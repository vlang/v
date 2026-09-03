module transform

import v3.flat
import v3.types

// Map literals normally add about 12 transform nodes per entry. Keep the
// estimate conservative because ownership cleanup and nested values can add
// more, and it is used only for parallel worker sizing/deferral.
const map_init_base_expansion_estimate = 12
const map_init_entry_expansion_estimate = 16
const map_init_owned_key_cleanup_expansion_estimate = 16
const map_init_owned_value_cleanup_expansion_estimate = 24
const array_literal_base_expansion_estimate = 12
const array_literal_entry_expansion_estimate = 16
const external_string_infix_expansion_estimate = 8

// A larger expansion is deliberately lowered after the bounded shared-worker
// phase. Large const maps can be referenced from a tiny function while their
// initializer lies outside its parsed subtree, so the complete worker reserve
// may be smaller than that single function's generated AST.
const deferred_map_expansion_threshold = 2048

// MapIndexInfo stores map index info metadata used by transform.
struct MapIndexInfo {
	base_id          flat.NodeId
	key_id           flat.NodeId
	base_type        string
	key_type         string
	key_storage_type string
	value_type       string
}

struct MapFixedArrayIndexInfo {
	map_info  MapIndexInfo
	index_ids []flat.NodeId
	elem_type string
}

fn (t &Transformer) map_init_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .map_init {
		return 0
	}
	if t.map_init_spread_requires_deferral(id, node) {
		return deferred_map_expansion_threshold + 1
	}
	entry_count := int(node.children_count) / 2
	mut entry_estimate := map_init_entry_expansion_estimate
	if !isnil(t.tc) {
		mut map_type := if node.value.len > 0 {
			node.value
		} else if node.typ.len > 0 {
			node.typ
		} else {
			t.node_type(id)
		}
		map_type = t.normalize_type_alias(t.resolve_type_text_import_aliases(map_type))
		if map_type.starts_with('map[') {
			key_type, value_type := t.map_type_parts(map_type)
			if t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type)) {
				entry_estimate += map_init_owned_key_cleanup_expansion_estimate
			}
			if t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type)) {
				entry_estimate += map_init_owned_value_cleanup_expansion_estimate
			}
		}
	}
	return map_init_base_expansion_estimate + entry_count * entry_estimate
}

fn (t &Transformer) map_init_spread_requires_deferral(id flat.NodeId, node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	first := t.a.child_node(&node, 0)
	if first.kind != .prefix || first.value != '...' || first.children_count == 0 {
		return false
	}
	mut map_type := if node.value.len > 0 {
		node.value
	} else if node.typ.len > 0 {
		node.typ
	} else {
		t.node_type(id)
	}
	map_type = t.normalize_type_alias(t.resolve_type_text_import_aliases(map_type))
	if !map_type.starts_with('map[') {
		return false
	}
	key_type, value_type := t.map_type_parts(map_type)
	return (t.normalize_type_alias(key_type).trim_space() != 'string'
		&& t.compiler_default_clone_type_needs_work(key_type))
		|| t.compiler_default_clone_type_needs_work(value_type)
}

fn (t &Transformer) array_literal_expansion_estimate(id flat.NodeId, node flat.Node, is_external bool) int {
	if node.kind != .array_literal {
		return 0
	}
	if t.array_literal_can_emit_direct(node) {
		// Direct literals already inside the function reuse their parsed storage.
		// An external const initializer is rebuilt at the use site, including its
		// child-ID span, so it must still fit in the bounded worker arena.
		return if is_external { int(node.children_count) + 1 } else { 0 }
	}
	if t.array_literal_spread_requires_deferral(id, node) {
		return deferred_map_expansion_threshold + 1
	}
	return array_literal_base_expansion_estimate +
		int(node.children_count) * array_literal_entry_expansion_estimate
}

fn (t &Transformer) array_literal_spread_requires_deferral(id flat.NodeId, node flat.Node) bool {
	mut has_spread := false
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
			has_spread = true
			break
		}
	}
	if !has_spread {
		return false
	}
	mut array_type := if node.typ.len > 0 {
		node.typ
	} else if node.value.len > 0 {
		node.value
	} else {
		t.node_type(id)
	}
	array_type = t.normalize_type_alias(t.resolve_type_text_import_aliases(array_type))
	return array_type.starts_with('[]') && t.compiler_default_clone_type_needs_work(array_type[2..])
}

fn (t &Transformer) fixed_array_init_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .array_init {
		return 0
	}
	raw_type := if node.typ.len > 0 {
		node.typ
	} else if node.value.len > 0 {
		node.value
	} else {
		t.node_type(id)
	}
	fixed_type := t.resolved_fixed_array_canonical_type(t.normalize_type_alias(raw_type))
	if !t.is_fixed_array_type(fixed_type) {
		return 0
	}
	if node.children_count == 0 {
		elem_type := fixed_array_elem_type(fixed_type)
		if !t.fixed_array_empty_init_may_expand(elem_type) {
			return 0
		}
		if t.fixed_array_empty_init_requires_deferral(elem_type) {
			return deferred_map_expansion_threshold + 1
		}
	}
	len_text := fixed_array_len_text(fixed_type)
	if !is_decimal_text(len_text) {
		return deferred_map_expansion_threshold + 1
	}
	len := len_text.int()
	if len > deferred_map_expansion_threshold / array_literal_entry_expansion_estimate {
		return deferred_map_expansion_threshold + 1
	}
	return array_literal_base_expansion_estimate + len * array_literal_entry_expansion_estimate
}

fn (t &Transformer) dynamic_array_init_requires_deferral(id flat.NodeId, node flat.Node) bool {
	if node.kind != .array_init {
		return false
	}
	raw_type := if node.typ.len > 0 {
		node.typ
	} else if node.value.len > 0 {
		node.value
	} else {
		t.node_type(id)
	}
	if t.is_fixed_array_type(t.resolved_fixed_array_canonical_type(t.normalize_type_alias(raw_type))) {
		return false
	}
	mut has_len := false
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		if field.value == 'init' {
			// Explicit element initializers are substituted and transformed inside a
			// synthesized fill loop, whose expansion is not bounded by this source node.
			return true
		}
		if field.value == 'len' {
			has_len = true
		}
	}
	if !has_len {
		return false
	}
	elem_value := t.array_init_elem_type_name(id, node)
	clean_value := t.normalize_type_alias(elem_value)
	elem_type := if node.typ.starts_with('[]') {
		node.typ[2..]
	} else if !elem_value.starts_with('[]') && clean_value.starts_with('[]') {
		clean_value[2..]
	} else {
		elem_value
	}
	return t.fixed_array_empty_init_may_expand(elem_type)
}

fn (t &Transformer) fixed_array_empty_init_requires_deferral(elem_type string) bool {
	clean_type := t.normalize_type_alias(elem_type)
	if t.is_fixed_array_type(clean_type) {
		return true
	}
	if t.sum_default_may_expand(clean_type) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	// Struct defaults can recursively synthesize collection literals that are not
	// represented by children of the external initializer.
	return types.unalias_type(t.tc.parse_type(clean_type)) is types.Struct
}

fn (t &Transformer) fixed_array_empty_init_may_expand(elem_type string) bool {
	clean_type := t.normalize_type_alias(elem_type)
	if clean_type.starts_with('[]') || clean_type.ends_with('[]') || clean_type.starts_with('map[')
		|| clean_type.starts_with('chan ') {
		return true
	}
	if t.is_fixed_array_type(clean_type) {
		fixed_type := t.resolved_fixed_array_canonical_type(clean_type)
		return t.fixed_array_empty_init_may_expand(fixed_array_elem_type(fixed_type))
	}
	if t.sum_default_may_expand(clean_type) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	// Struct fields can carry runtime defaults. Treat every struct conservatively here;
	// this estimate only decides whether lowering should leave the bounded worker arena.
	return types.unalias_type(t.tc.parse_type(clean_type)) is types.Struct
}

// external_map_tree_expansion_estimate counts writes caused by lowering an initializer
// that lives outside the current function's contiguous node range. The transformer
// recursively lowers each nested initializer at the use site. Struct reconstruction
// can also synthesize field defaults, so defer it rather than trying to predict an
// expansion that is not represented in this AST.
fn (mut t Transformer) external_map_tree_expansion_estimate(root flat.NodeId, lo int, hi int) int {
	if int(root) < 0 || int(root) >= t.a.nodes.len {
		return 0
	}
	mut estimate := 0
	mut pending := [root]
	mut cursor := 0
	for cursor < pending.len {
		id := pending[cursor]
		cursor++
		if int(id) < 0 || int(id) >= t.a.nodes.len {
			continue
		}
		if int(id) >= lo && int(id) < hi {
			continue
		}
		node := t.a.nodes[int(id)]
		if node.kind in [.struct_init, .assoc] {
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind in [.if_expr, .block] {
			// External conditionals and their branch blocks cannot rewrite their
			// child spans in place, so each is reconstructed at the use site.
			estimate += int(node.children_count) + 1
		}
		if node.kind == .match_stmt {
			// Match lowering synthesizes comparison, block, and nested conditional
			// trees that are not bounded by the source child count. Defer the whole
			// external tree instead of risking a shared-arena underestimate.
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .fn_literal {
			// Function-literal lifting appends declarations and closure signatures to
			// global output, which is not bounded by the source node's children.
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .map_init {
			estimate += t.map_init_expansion_estimate(id, node)
		}
		if node.kind == .array_literal {
			estimate += t.array_literal_expansion_estimate(id, node, true)
		}
		if node.kind == .array_init {
			estimate += t.fixed_array_init_expansion_estimate(id, node)
			if t.dynamic_array_init_requires_deferral(id, node) {
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind == .string_interp {
			interp_estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(node)
			estimate += interp_estimate
			if needs_deferred_lowering {
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind == .or_expr {
			// Or lowering synthesizes option temporaries, status checks, branches,
			// and value extraction beyond the source node's physical children.
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .dump_expr {
			// Dump lowering synthesizes a temporary plus formatting and output trees
			// whose size depends on the dumped type. Defer instead of estimating it.
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .in_expr {
			// Membership lowering can synthesize equality and OR trees whose size is
			// not bounded by the source node's physical children. Defer external
			// membership expressions instead of risking a shared-arena underestimate.
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind in [.is_expr, .as_expr] {
			// Interface tests and conversions expand from implementation metadata, so
			// their generated comparison/copy trees are not bounded by AST children.
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .call {
			// Calls outside the writable function span are always reconstructed by
			// transform_call_args, including a new child-ID span.
			estimate += int(node.children_count) + 1
			estimate += t.disabled_call_zero_value_expansion_estimate(id, node)
			if t.runtime_type_metadata_call_expands(id, node) {
				// Runtime type metadata calls expand into comparison chains derived from
				// sum variants or interface implementations, not physical call children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if info := t.compiler_default_clone_call_info(node) {
				if info.can_lower {
					// Compiler-provided aggregate clones expand recursively from type
					// metadata, which is not represented by the call's physical children.
					estimate += deferred_map_expansion_threshold + 1
				}
			}
			if t.compiler_collection_clone_call_expands(node) {
				// Ownership-aware collection clones synthesize element/key/value loops
				// whose size is derived from type metadata, not source children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.compiler_owned_map_items_call_expands(node) {
				// Ownership-aware map keys()/values() synthesize a clone loop whose
				// size is derived from item metadata, not source children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.compiler_array_search_call_expands(node) {
				// Builtin array equality/search calls can synthesize an element loop and recursively
				// expand aggregate equality from metadata absent from source children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.compiler_owned_array_accessor_call_expands(node) {
				// Ownership-aware first()/last() recursively clone aggregate elements
				// from metadata that is not represented by the physical call children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.compiler_owned_array_filter_call_expands(node) {
				// Ownership-aware filter() recursively clones accepted aggregate elements
				// from metadata that is not represented by the physical call children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.compiler_owned_array_map_call_expands(node) {
				// Ownership-aware map() recursively clones borrowed aggregate results from
				// metadata that is not represented by the physical call children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.compiler_collection_str_call_expands(node) {
				// Compiler-provided collection stringification synthesizes loops and
				// recursively formats elements that are absent from physical children.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.ownership_array_repeat_call_expands(node) {
				// Ownership-aware repeats synthesize a clone loop from element metadata.
				estimate += deferred_map_expansion_threshold + 1
			}
			if t.interface_array_literal_repeat_call_expands(node) {
				// Interface literal repeats duplicate the literal child span up to 32
				// times before ownership-aware repeat lowering runs.
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind == .cast_expr {
			// External casts cannot rewrite their child IDs in place, so even an
			// otherwise unchanged cast appends a replacement node and child span.
			estimate += int(node.children_count) + 1
			if t.interface_cast_expands_from_type_metadata(node) {
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind == .index {
			// A changed external index appends a replacement node and child span;
			// semantic constant-map edges below do not account for those writes.
			estimate += int(node.children_count) + 1
		}
		if node.kind == .range {
			// Changed external range bounds append a replacement node and child span.
			estimate += int(node.children_count) + 1
		}
		if node.kind == .selector {
			if t.external_selector_expands_from_type_metadata(node) {
				// Shared sum/interface fields expand into conditional trees whose size
				// comes from type metadata rather than the selector's physical children.
				estimate += deferred_map_expansion_threshold + 1
			} else {
				// A changed external selector base makes every selector ancestor append
				// a replacement node and child span at the constant's use site.
				estimate += int(node.children_count) + 1
			}
		}
		if node.kind in [.paren, .prefix] {
			// External wrappers cannot rewrite their child IDs in place, so each
			// wrapper appends a replacement node and child span.
			estimate += int(node.children_count) + 1
		}
		if node.kind == .infix && node.op in [.logical_and, .logical_or] {
			// Logical conditions are rebuilt through make_infix during smartcast lowering.
			// The new node has two child IDs, so charge the larger append pool.
			estimate += int(node.children_count)
		}
		if t.external_equality_expands_from_type_metadata(node) {
			// Struct and interface equality can expand from field/implementation
			// metadata that is not represented by the infix node's children.
			estimate += deferred_map_expansion_threshold + 1
		}
		is_string_infix := node.kind == .infix && node.op in [.plus, .eq, .ne, .lt, .gt, .le, .ge]
			&& node.children_count >= 2 && (t.is_string_type(t.a.child(&node, 0))
			|| t.is_string_type(t.a.child(&node, 1)))
		if is_string_infix {
			// String infix lowering emits a fresh literal or an identifier/call pair,
			// sometimes with conversions or a negating prefix.
			estimate += external_string_infix_expansion_estimate
		}
		if node.kind == .infix && node.op !in [.logical_and, .logical_or] && !is_string_infix {
			// A changed external operand makes ordinary infix lowering append both
			// the rebuilt node and its child span. Counting every external infix is
			// intentionally conservative; unchanged ones will simply reuse their ID.
			estimate += int(node.children_count) + 1
		}
		// External constant initializers can themselves index another constant map.
		// That substitution edge is semantic rather than a physical FlatAst child.
		if node.kind == .index && node.children_count > 0 {
			base_id := t.a.child(&node, 0)
			if const_expr := t.map_const_expr_for_ident(base_id) {
				pending << const_expr
			}
		}
		if node.kind == .in_expr && node.children_count > 1 {
			rhs_id := t.a.child(&node, 1)
			if const_expr := t.map_const_expr_for_ident(rhs_id) {
				pending << const_expr
			}
		}
		for ci in 0 .. int(node.children_count) {
			child := t.a.child(&node, ci)
			if int(child) >= 0 && int(child) < t.a.nodes.len
				&& (int(child) < lo || int(child) >= hi) {
				pending << child
			}
		}
	}
	return estimate
}

fn (t &Transformer) compiler_collection_clone_call_expands(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector
		|| fn_node.value !in ['clone', 'reverse', 'sorted', 'sorted_with_compare']
		|| fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	mut clean := t.normalize_type_alias(base_type).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	clean = clean.trim_left('&')
	if fn_node.value in ['reverse', 'sorted', 'sorted_with_compare'] {
		if !clean.starts_with('[]') || isnil(t.tc) {
			return false
		}
		elem_type := clean[2..]
		return t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
			&& t.compiler_default_clone_type_needs_work(elem_type)
			&& t.tc.ownership_default_clone_missing_method(t.tc.parse_type(elem_type)) == none
	}
	mut owned_types := []string{}
	if clean.starts_with('[]') {
		owned_types << clean[2..]
	} else if t.is_fixed_array_type(clean) {
		owned_types << fixed_array_elem_type(clean)
	} else if clean.starts_with('map[') {
		key_type, value_type := t.map_type_parts(clean)
		owned_types << key_type
		owned_types << value_type
	} else {
		return false
	}
	for typ in owned_types {
		if t.compiler_default_clone_type_needs_work(typ) {
			return true
		}
	}
	return false
}

fn (t &Transformer) compiler_owned_map_items_call_expands(node flat.Node) bool {
	if node.children_count == 0 || isnil(t.tc) {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['keys', 'values']
		|| fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := t.clean_map_type(base_type)
	if !clean_type.starts_with('map[') {
		return false
	}
	elem_type := if fn_node.value == 'keys' {
		t.map_key_type(clean_type)
	} else {
		t.map_value_type(clean_type)
	}
	if elem_type.len == 0 || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
		|| !t.compiler_default_clone_type_needs_work(elem_type) {
		return false
	}
	return fn_node.value == 'values' || t.normalize_type_alias(elem_type).trim_space() != 'string'
}

fn (t &Transformer) compiler_array_search_call_expands(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['equals', 'contains', 'index', 'last_index']
		|| fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := transform_unshared_receiver_type(t.normalize_type_alias(base_type)).trim_left('&')
	elem_type := if clean_type.starts_with('[]') {
		clean_type[2..]
	} else if t.is_fixed_array_type(clean_type) {
		fixed_array_elem_type(clean_type)
	} else {
		return false
	}
	return elem_type.len > 0 && t.array_elem_needs_element_eq(elem_type)
}

fn (t &Transformer) compiler_owned_array_accessor_call_expands(node flat.Node) bool {
	if node.children_count == 0 || isnil(t.tc) {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['first', 'last']
		|| fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := transform_unshared_receiver_type(t.normalize_type_alias(base_type)).trim_left('&')
	elem_type := if clean_type.starts_with('[]') {
		clean_type[2..]
	} else if t.is_fixed_array_type(clean_type) {
		fixed_array_elem_type(clean_type)
	} else {
		return false
	}
	if elem_type.len == 0 || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
		|| !t.compiler_default_clone_type_needs_work(elem_type) {
		return false
	}
	return t.tc.ownership_default_clone_missing_method(t.tc.parse_type(elem_type)) == none
}

fn (t &Transformer) compiler_owned_array_filter_call_expands(node flat.Node) bool {
	if node.children_count < 2 || isnil(t.tc) {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'filter' || fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := transform_unshared_receiver_type(t.normalize_type_alias(base_type)).trim_left('&')
	if !clean_type.starts_with('[]') {
		return false
	}
	elem_type := clean_type[2..]
	return elem_type.len > 0 && t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
		&& t.compiler_default_clone_type_needs_work(elem_type)
		&& t.tc.ownership_default_clone_missing_method(t.tc.parse_type(elem_type)) == none
}

fn (t &Transformer) compiler_owned_array_map_call_expands(node flat.Node) bool {
	if node.children_count < 2 || isnil(t.tc) {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'map' || fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_base := transform_unshared_receiver_type(t.normalize_type_alias(base_type)).trim_left('&')
	clean_result :=
		transform_unshared_receiver_type(t.normalize_type_alias(node.typ)).trim_left('&')
	if !clean_base.starts_with('[]') || !clean_result.starts_with('[]') {
		return false
	}
	result_elem_type := clean_result[2..]
	return result_elem_type.len > 0
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(result_elem_type))
		&& t.compiler_default_clone_type_needs_work(result_elem_type)
		&& t.tc.ownership_default_clone_missing_method(t.tc.parse_type(result_elem_type)) == none
}

fn (t &Transformer) compiler_collection_str_call_expands(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'str' || fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if _ := t.resolve_receiver_method_for_type(base_type, 'str') {
		return false
	}
	mut clean := t.normalize_type_alias(base_type).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	clean = clean.trim_left('&')
	if clean.starts_with('[]') || t.is_fixed_array_type(clean) {
		return true
	}
	if clean.starts_with('map[') {
		key_type, raw_value_type := t.map_type_parts(clean)
		fixed_value_type := t.fixed_array_map_value_type_text(raw_value_type)
		value_type := if fixed_value_type.len > 0 { fixed_value_type } else { raw_value_type }
		return t.map_str_types_need_typed_lowering(key_type, value_type)
	}
	return false
}

fn (t &Transformer) external_equality_expands_from_type_metadata(node flat.Node) bool {
	if node.kind != .infix || node.op !in [.eq, .ne] || node.children_count < 2 {
		return false
	}
	for i in 0 .. 2 {
		operand_id := t.a.child(&node, i)
		operand_type := t.node_type(operand_id)
		if !t.infix_operand_is_pointer(operand_id)
			&& t.equality_type_expands_from_metadata(operand_type, 0) {
			return true
		}
	}
	return false
}

fn (t &Transformer) equality_type_expands_from_metadata(typ string, depth int) bool {
	if depth >= 8 {
		return true
	}
	clean := t.normalize_type_alias(typ).trim_space()
	if clean.starts_with('&') {
		return false
	}
	if clean.starts_with('[]') {
		return t.equality_type_expands_from_metadata(clean[2..], depth + 1)
	}
	if t.is_fixed_array_type(clean) {
		return t.equality_type_expands_from_metadata(fixed_array_elem_type(clean), depth + 1)
	}
	if clean.starts_with('map[') {
		_, value_type := t.map_type_parts(clean)
		if t.zero_value_expansion_estimate(flat.NodeId(-1), value_type) > 0 {
			return true
		}
		return value_type.len > 0 && t.equality_type_expands_from_metadata(value_type, depth + 1)
	}
	return t.resolve_interface_type_name(clean).len > 0 || t.struct_lookup_name(clean).len > 0
}

fn (t &Transformer) external_selector_expands_from_type_metadata(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	if node.value == 'variant_types' && t.selector_base_is_comptime_type_value(t.a.child(&node, 0)) {
		return true
	}
	base_type := t.node_type(t.a.child(&node, 0))
	iface_name := t.resolve_interface_type_name(base_type)
	if iface_name.len > 0 && node.value !in ['_typ', '_object'] {
		if _ := t.interface_field_type_name(iface_name, node.value) {
			return true
		}
	}
	return t.sum_shared_field_type_name(base_type, node.value) != none
}

fn (t &Transformer) interface_cast_expands_from_type_metadata(node flat.Node) bool {
	return node.kind == .cast_expr && t.is_interface_type(node.value)
}

fn (mut t Transformer) compiler_call_expands_from_type_metadata(id flat.NodeId, node flat.Node) bool {
	if t.runtime_type_metadata_call_expands(id, node) {
		return true
	}
	if info := t.compiler_default_clone_call_info(node) {
		if info.can_lower {
			return true
		}
	}
	return t.compiler_collection_clone_call_expands(node)
		|| t.compiler_owned_map_items_call_expands(node)
		|| t.compiler_array_search_call_expands(node)
		|| t.compiler_owned_array_accessor_call_expands(node)
		|| t.compiler_owned_array_filter_call_expands(node)
		|| t.compiler_owned_array_map_call_expands(node)
		|| t.compiler_collection_str_call_expands(node)
		|| t.ownership_array_repeat_call_expands(node)
		|| t.interface_array_literal_repeat_call_expands(node)
		|| t.ownership_nested_map_delete_key_clone_expands(node)
}

fn (mut t Transformer) disabled_call_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .call || !t.is_disabled_fn_call(id, node) || t.is_cgen_magic_json_call(id, node) {
		return 0
	}
	mut result_type := t.node_type(id)
	if result_type.len == 0 || result_type in ['array', 'map', 'unknown']
		|| t.generic_arg_is_unresolved(result_type) {
		result_type = t.get_call_return_type(id, node)
	}
	if result_type == 'void' {
		return 0
	}
	if result_type.len == 0 || result_type in ['array', 'map', 'unknown']
		|| t.generic_arg_is_unresolved(result_type) {
		return deferred_map_expansion_threshold + 1
	}
	return t.zero_value_expansion_estimate(id, result_type)
}

fn (mut t Transformer) disabled_struct_operator_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .infix || node.children_count < 2 {
		return 0
	}
	lhs_id := t.a.child(&node, 0)
	mut lhs_type := t.node_type(lhs_id)
	checker_lhs_type := t.checker_node_type(lhs_id)
	lhs_key := t.expr_key(lhs_id)
	if lhs_key.len > 0 {
		if sc := t.find_smartcast(lhs_key) {
			lhs_type = t.smartcast_target_type(sc)
		}
	}
	if lhs_type.len == 0
		|| (t.generic_arg_is_unresolved(lhs_type) && !t.generic_arg_is_unresolved(checker_lhs_type)) {
		lhs_type = checker_lhs_type
	}
	lhs_node := t.a.nodes[int(lhs_id)]
	lhs_is_pointer := lhs_type.starts_with('&') || checker_lhs_type.starts_with('&')
		|| (lhs_node.kind == .ident && t.mut_param_values[lhs_node.value])
	if lhs_is_pointer && lhs_type.starts_with('&') {
		lhs_type = lhs_type[1..]
	}
	mut struct_type := ''
	mut is_alias_operator := false
	if alias_type := t.operator_alias_type_for_operand(lhs_id, node.op) {
		struct_type = alias_type
		is_alias_operator = true
	} else {
		struct_type = t.struct_lookup_name(lhs_type)
	}
	if struct_type.len == 0 {
		struct_type = t.generic_struct_instance_name(lhs_type)
	}
	if struct_type.len == 0 {
		return 0
	}
	call_info := t.struct_operator_call_info_for_operand(struct_type, node.op, is_alias_operator) or {
		return 0
	}
	if !t.is_disabled_fn_name(call_info.name) {
		return 0
	}
	result_type := t.struct_operator_return_type(call_info.name)
	if result_type == 'void' {
		return 0
	}
	if result_type.len == 0 || result_type in ['array', 'map', 'unknown']
		|| t.generic_arg_is_unresolved(result_type) {
		return deferred_map_expansion_threshold + 1
	}
	return t.zero_value_expansion_estimate(id, result_type)
}

fn (mut t Transformer) ownership_nested_map_delete_key_clone_expands(node flat.Node) bool {
	if node.kind != .call || node.children_count < 2 || isnil(t.tc) {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value != 'delete' || callee.children_count == 0 {
		return false
	}
	outer_info := t.map_index_info(t.a.child(callee, 0)) or { return false }
	inner_map_type := t.clean_map_type(outer_info.value_type)
	if !inner_map_type.starts_with('map[') {
		return false
	}
	if t.ownership_borrowed_map_key_clone_expands(outer_info.key_id, outer_info.key_type) {
		return true
	}
	inner_key_type, _ := t.map_type_parts(inner_map_type)
	return t.ownership_borrowed_map_key_clone_expands(t.a.child(&node, 1), inner_key_type)
}

fn (mut t Transformer) ownership_borrowed_map_key_clone_expands(key_id flat.NodeId, key_type_name string) bool {
	if isnil(t.tc) || t.normalize_type_alias(key_type_name).trim_space() == 'string'
		|| t.map_key_expr_creates_owned_value(key_id, key_type_name) {
		return false
	}
	key_type := t.tc.parse_type(key_type_name)
	return t.tc.ownership_type_requires_destruction(key_type)
		&& t.tc.ownership_default_clone_missing_method(key_type) == none
		&& t.compiler_default_clone_type_needs_work(key_type_name)
}

fn (mut t Transformer) ownership_for_in_binding_clone_expands(node flat.Node) bool {
	if node.kind != .for_in_stmt || node.children_count < 3 {
		return false
	}
	key_id := t.a.child(&node, 0)
	value_id := t.a.child(&node, 1)
	has_index := int(value_id) >= 0
	iter_type := t.normalize_type_alias(t.detect_for_in_type(node))
	map_iter_type := t.clean_map_type(iter_type)
	if map_iter_type.starts_with('map[') {
		key_type, value_type := t.map_type_parts(map_iter_type)
		if has_index {
			key_name := if int(key_id) >= 0 { t.a.nodes[int(key_id)].value } else { '' }
			if key_name !in ['', '_'] && t.normalize_type_alias(key_type).trim_space() != 'string'
				&& t.ownership_for_in_type_needs_clone(key_type) {
				return true
			}
			value_name := t.a.nodes[int(value_id)].value
			return value_name !in ['', '_'] && t.ownership_for_in_type_needs_clone(value_type)
		}
		value_name := if int(key_id) >= 0 { t.a.nodes[int(key_id)].value } else { '' }
		return value_name !in ['', '_'] && t.ownership_for_in_type_needs_clone(value_type)
	}
	if !iter_type.starts_with('[]') && !t.is_fixed_array_type(iter_type) {
		return false
	}
	binding_id := if has_index { value_id } else { key_id }
	binding_name := if int(binding_id) >= 0 { t.a.nodes[int(binding_id)].value } else { '' }
	if binding_name in ['', '_'] {
		return false
	}
	elem_type := t.infer_for_in_elem_type(iter_type, node)
	value_type := if node.op == .amp { '&${elem_type}' } else { elem_type }
	return t.ownership_for_in_type_needs_clone(value_type)
}

fn (mut t Transformer) ownership_for_in_map_snapshot_clone_expands(node flat.Node) bool {
	if node.kind != .for_in_stmt || node.children_count < 3 || isnil(t.tc) {
		return false
	}
	iter_type := t.clean_map_type(t.normalize_type_alias(t.detect_for_in_type(node)))
	if !iter_type.starts_with('map[') {
		return false
	}
	header_count := node.value.int()
	children := t.a.children_of(&node)
	if header_count < 3 || header_count > children.len {
		return false
	}
	container_id := t.a.child(&node, 2)
	if !t.for_in_body_contains_map_delete(children[header_count..], container_id) {
		return false
	}
	key_type, value_type := t.map_type_parts(iter_type)
	key_needs_clone := t.normalize_type_alias(key_type).trim_space() != 'string'
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type))
	value_needs_clone := t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type))
	if !key_needs_clone && !value_needs_clone {
		return false
	}
	if t.tc.ownership_default_clone_missing_method(t.tc.parse_type(key_type)) != none
		|| t.tc.ownership_default_clone_missing_method(t.tc.parse_type(value_type)) != none {
		return false
	}
	return (key_needs_clone && t.compiler_default_clone_type_needs_work(key_type))
		|| (value_needs_clone && t.compiler_default_clone_type_needs_work(value_type))
}

fn (mut t Transformer) ownership_map_assignment_clone_expands(node flat.Node) bool {
	if node.kind !in [.assign, .selector_assign, .index_assign] || node.children_count < 2
		|| isnil(t.tc) {
		return false
	}
	lhs_id := t.a.child(&node, 0)
	if t.map_assignment_lvalue_key_clone_expands(lhs_id) {
		return true
	}
	info := t.map_assignment_underlying_index_info(lhs_id) or { return false }
	if node.op != .assign {
		return false
	}
	rhs_id := t.a.child(&node, 1)
	if !t.tc.ownership_expr_moves_storage(rhs_id, lhs_id) {
		return false
	}
	value_type := t.tc.parse_type(info.value_type)
	return t.tc.ownership_type_requires_destruction(value_type)
		&& t.tc.ownership_default_clone_missing_method(value_type) == none
		&& t.compiler_default_clone_type_needs_work(info.value_type)
}

fn (mut t Transformer) map_assignment_underlying_index_info(id flat.NodeId) ?MapIndexInfo {
	mut current := id
	for _ in 0 .. 32 {
		if info := t.map_index_info(current) {
			return info
		}
		if int(current) < 0 || int(current) >= t.a.nodes.len {
			return none
		}
		node := t.a.nodes[int(current)]
		if node.kind !in [.selector, .index, .paren] || node.children_count == 0 {
			return none
		}
		current = t.a.child(&node, 0)
	}
	return none
}

fn (mut t Transformer) map_assignment_lvalue_key_clone_expands(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 32 {
		if info := t.map_index_info(current) {
			if t.ownership_borrowed_map_key_clone_expands(info.key_id, info.key_type) {
				return true
			}
		}
		if int(current) < 0 || int(current) >= t.a.nodes.len {
			return false
		}
		node := t.a.nodes[int(current)]
		if node.kind !in [.selector, .index, .paren] || node.children_count == 0 {
			return false
		}
		current = t.a.child(&node, 0)
	}
	return false
}

fn (mut t Transformer) ownership_method_value_clone_expands(id flat.NodeId, node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 || isnil(t.tc)
		|| !t.tc.expr_is_method_value(id) {
		return false
	}
	base_id := t.a.child(&node, 0)
	method_name := t.resolve_receiver_method_name(base_id, node.value)
	method_params := t.call_param_types(method_name)
	if method_params.len == 0 || method_params[0] is types.Pointer {
		return false
	}
	receiver_type_name := t.node_type(base_id)
	receiver_type := t.tc.parse_type(receiver_type_name)
	return t.tc.ownership_type_requires_destruction(receiver_type)
		&& t.tc.ownership_default_clone_missing_method(receiver_type) == none
		&& t.compiler_default_clone_type_needs_work(receiver_type_name)
}

fn (t &Transformer) collection_const_expr_for_ident(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	if _ := t.local_binding_before(node.value, id) {
		return none
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		if expr_id := t.const_expr_for_name('${t.cur_module}.${node.value}') {
			return expr_id
		}
	}
	return t.const_expr_for_name(node.value)
}

fn (t &Transformer) map_const_expr_for_ident(id flat.NodeId) ?flat.NodeId {
	if !t.clean_map_type(t.node_type(id)).starts_with('map[') {
		return none
	}
	return t.collection_const_expr_for_ident(id)
}

fn (t &Transformer) sum_default_may_expand(typ string) bool {
	resolved := t.resolve_sum_name(t.normalize_type_alias(typ))
	return resolved.len > 0
		&& (resolved in t.sum_types || (!isnil(t.tc) && resolved in t.tc.sum_types))
}

fn (t &Transformer) zero_value_expansion_estimate(id flat.NodeId, type_name string) int {
	clean_type := t.normalize_type_alias(type_name)
	if t.sum_default_may_expand(clean_type) {
		return deferred_map_expansion_threshold + 1
	}
	fixed_type := t.resolved_fixed_array_canonical_type(clean_type)
	if !t.is_fixed_array_type(fixed_type) {
		return 0
	}
	return t.fixed_array_init_expansion_estimate(id, flat.Node{
		kind: .array_init
		typ:  fixed_type
	})
}

fn (mut t Transformer) comptime_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .string_literal || node.children_count != 1
		|| node.value !in ['__v3_comptime_zero', '__v3_comptime_new'] {
		return 0
	}
	target_type := t.comptime_type_expr_type(t.a.child(&node, 0)) or {
		return deferred_map_expansion_threshold + 1
	}
	return t.zero_value_expansion_estimate(id, target_type)
}

fn (t &Transformer) map_index_zero_value_expansion_estimate(node flat.Node) int {
	if node.kind != .index || node.children_count == 0 {
		return 0
	}
	base_id := t.a.child(&node, 0)
	map_type := t.clean_map_type(t.node_type(base_id))
	if !map_type.starts_with('map[') {
		return 0
	}
	_, value_type := t.map_type_parts(map_type)
	return t.zero_value_expansion_estimate(base_id, value_type)
}

fn (t &Transformer) owned_array_index_zero_value_expansion_estimate(node flat.Node, moves_value bool) int {
	if !moves_value || node.kind != .index || node.children_count == 0 {
		return 0
	}
	base_id := t.a.child(&node, 0)
	base_type := t.normalize_type_alias(t.node_type(base_id).trim_left('&'))
	if !base_type.starts_with('[]') && !t.is_fixed_array_type(base_type) {
		return 0
	}
	return t.zero_value_expansion_estimate(base_id, node.typ)
}

fn (mut t Transformer) channel_receive_if_guard_zero_value_expansion_estimate(node flat.Node) int {
	if node.kind != .if_expr || node.children_count < 2 {
		return 0
	}
	condition := t.a.child_node(&node, 0)
	if condition.kind != .decl_assign || condition.children_count < 2
		|| t.multi_assign_lhs_ids(condition).len != 1 {
		return 0
	}
	rhs_id := t.a.child(condition, 1)
	info := t.channel_receive_info(rhs_id) or { return 0 }
	return t.zero_value_expansion_estimate(rhs_id, info.value_type)
}

fn (mut t Transformer) multi_return_if_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .if_expr {
		return 0
	}
	lhs_ids := t.multi_return_decl_lhs_ids(id)
	if lhs_ids.len == 0 {
		return 0
	}
	if !t.if_expr_has_tuple_tail_values(id, lhs_ids.len) {
		return 0
	}
	value_types := t.promoted_multi_if_value_types(id, node, lhs_ids.len)
	mut estimate := 0
	for value_type in value_types {
		estimate += t.zero_value_expansion_estimate(id, value_type)
	}
	return estimate
}

fn (t &Transformer) multi_return_decl_lhs_ids(id flat.NodeId) []flat.NodeId {
	parent_id := t.source_parent_id(int(id))
	if parent_id < 0 || parent_id >= t.a.nodes.len {
		return []
	}
	parent := t.a.nodes[parent_id]
	if parent.kind != .decl_assign || parent.children_count < 3
		|| t.multi_assign_rhs_count(parent) != 1 {
		return []
	}
	return t.multi_assign_lhs_ids(parent)
}

fn (mut t Transformer) multi_return_match_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .match_stmt || isnil(t.tc) {
		return 0
	}
	lhs_ids := t.multi_return_decl_lhs_ids(id)
	if lhs_ids.len == 0 {
		return 0
	}
	value_types := t.tc.multi_expr_tail_types_for_transform(id, lhs_ids.len) or { return 0 }
	mut estimate := 0
	for value_type in value_types {
		estimate += t.zero_value_expansion_estimate(id, value_type.name())
	}
	return estimate
}

fn (mut t Transformer) if_expr_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .if_expr {
		return 0
	}
	mut estimate := t.channel_receive_if_guard_zero_value_expansion_estimate(node)
	estimate += t.multi_return_if_zero_value_expansion_estimate(id, node)
	if node.children_count < 3 {
		return estimate
	}
	mut result_type := t.if_expr_result_type(id, node)
	if guard_result_type := t.if_expr_guard_result_type(node) {
		result_type = guard_result_type
	}
	if result_type.len == 0 || result_type == 'void' {
		return estimate
	}
	branch_type := t.if_expr_branch_result_type(node)
	if t.if_expr_branch_overrides_sum_target(branch_type, result_type) {
		result_type = branch_type
	}
	return estimate + t.zero_value_expansion_estimate(id, result_type)
}

fn (mut t Transformer) match_expr_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .match_stmt {
		return 0
	}
	mut estimate := t.multi_return_match_zero_value_expansion_estimate(id, node)
	mut result_type := t.match_expr_type(node)
	if (result_type.len == 0 || result_type == 'void' || t.generic_arg_is_unresolved(result_type))
		&& decl_type_is_usable(node.typ) && !t.generic_arg_is_unresolved(node.typ) {
		result_type = node.typ
	}
	estimate += t.zero_value_expansion_estimate(id, result_type)
	return estimate
}

fn (mut t Transformer) or_expr_zero_value_expansion_estimate(id flat.NodeId, node flat.Node) int {
	if node.kind != .or_expr || node.children_count < 2 {
		return 0
	}
	expr_id := t.a.child(&node, 0)
	fallback_type := if decl_type_is_usable(node.typ) && !t.generic_arg_is_unresolved(node.typ) {
		node.typ
	} else {
		t.stmt_value_type(t.a.child(&node, 1))
	}
	expr_type, value_type := t.or_expr_types(expr_id, fallback_type)
	mut estimate := if t.is_optional_type_name(expr_type) {
		0
	} else {
		t.nested_optional_leaf_zero_value_expansion_estimate(expr_id, 0)
	}
	if value_type in ['', 'void', 'Optional', '!', '?'] {
		return estimate
	}
	estimate += t.zero_value_expansion_estimate(id, value_type)
	return estimate
}

fn (mut t Transformer) nested_optional_leaf_zero_value_expansion_estimate(id flat.NodeId, depth int) int {
	if depth >= 64 || int(id) < 0 || int(id) >= t.a.nodes.len {
		return 0
	}
	source_id := t.nested_optional_leaf_source_id(id)
	expr_type, value_type := t.or_expr_types(source_id, '')
	if t.is_optional_type_name(expr_type) {
		return t.zero_value_expansion_estimate(source_id, value_type)
	}
	node := t.a.nodes[int(id)]
	mut estimate := 0
	for i in 0 .. node.children_count {
		estimate += t.nested_optional_leaf_zero_value_expansion_estimate(t.a.child(&node, i),

			depth + 1)
		if estimate > deferred_map_expansion_threshold {
			return deferred_map_expansion_threshold + 1
		}
	}
	return estimate
}

fn (mut t Transformer) ownership_array_append_expands(node flat.Node) bool {
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 || isnil(t.tc) {
		return false
	}
	lhs_id := t.a.child(&node, 0)
	array_type := t.clean_array_append_lhs_type(t.node_type(lhs_id))
	if !array_type.starts_with('[]') {
		return false
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind == .index && lhs.children_count > 0 {
		base_type := t.clean_map_type(t.node_type(t.a.child(&lhs, 0)))
		if base_type.starts_with('map[') {
			_, value_type := t.map_type_parts(base_type)
			parsed_value_type := t.tc.parse_type(value_type)
			if t.tc.ownership_type_requires_destruction(parsed_value_type)
				&& t.compiler_default_clone_type_needs_work(value_type)
				&& t.tc.ownership_default_clone_missing_method(parsed_value_type) == none {
				return true
			}
		}
	}
	elem_type := array_type[2..]
	return t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
		&& t.compiler_default_clone_type_needs_work(elem_type)
}

fn (mut t Transformer) builtin_call_auto_stringify_expands(id flat.NodeId, node flat.Node) bool {
	if node.kind != .call || node.children_count != 2 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value !in ['print', 'println', 'eprint', 'eprintln', 'panic'] {
		return false
	}
	resolved := t.call_name_for_node(id, node)
	if resolved !in [callee.value, 'builtin.${callee.value}'] {
		return false
	}
	argument_id := t.a.child(&node, 1)
	argument_type := t.reliable_stringify_type(argument_id)
	if argument_type.len == 0 || argument_type == 'unknown' {
		return true
	}
	return t.stringify_expansion_estimate(argument_type) > 0
}

fn (t &Transformer) array_membership_equality_expands(node flat.Node) bool {
	if node.kind != .in_expr || node.children_count < 2 {
		return false
	}
	rhs_id := t.a.child(&node, 1)
	mut rhs_type := t.node_type(rhs_id)
	if rhs_type.len == 0 {
		rhs_type = t.lvalue_type(rhs_id)
	}
	container_type := t.membership_container_type(rhs_type)
	element_type := if container_type.starts_with('[]') {
		container_type[2..]
	} else if t.is_fixed_array_type(container_type) {
		fixed_array_elem_type(container_type)
	} else {
		return false
	}
	return element_type.len > 0 && t.array_elem_needs_element_eq(element_type)
}

fn (mut t Transformer) forwarded_fixed_array_return_expansion_estimate(node flat.Node, fn_return_type string) int {
	if node.kind != .return_stmt || node.children_count != 1 || isnil(t.tc) {
		return 0
	}
	expected_name := if decl_type_is_usable(fn_return_type) { fn_return_type } else { node.typ }
	actual_name := t.node_type(t.a.child(&node, 0))
	if !decl_type_is_usable(actual_name) || !decl_type_is_usable(expected_name) {
		return 0
	}
	actual_type := t.tc.parse_type(actual_name)
	expected_type := t.tc.parse_type(expected_name)
	return t.forwarded_return_conversion_expansion_estimate(actual_type, expected_type, 0)
}

fn (mut t Transformer) forwarded_return_conversion_expansion_estimate(actual_type types.Type, expected_type types.Type, depth int) int {
	if depth >= 32 || !t.forwarded_slot_conversion_supported(actual_type, expected_type) {
		return 0
	}
	actual := forwarded_return_unalias_type(actual_type)
	expected := forwarded_return_unalias_type(expected_type)
	if actual is types.OptionType && expected is types.OptionType {
		return t.forwarded_return_conversion_expansion_estimate(actual.base_type,
			expected.base_type, depth + 1)
	}
	if actual is types.ResultType && expected is types.ResultType {
		return t.forwarded_return_conversion_expansion_estimate(actual.base_type,
			expected.base_type, depth + 1)
	}
	if expected is types.Array {
		if actual is types.Array {
			return t.forwarded_return_conversion_expansion_estimate(actual.elem_type,
				expected.elem_type, depth + 1)
		}
		if actual is types.ArrayFixed {
			return t.forwarded_return_conversion_expansion_estimate(actual.elem_type,
				expected.elem_type, depth + 1)
		}
	}
	if actual is types.Map && expected is types.Map {
		key_estimate := t.forwarded_return_conversion_expansion_estimate(actual.key_type,
			expected.key_type, depth + 1)
		if key_estimate > deferred_map_expansion_threshold {
			return key_estimate
		}
		value_estimate := t.forwarded_return_conversion_expansion_estimate(actual.value_type,
			expected.value_type, depth + 1)
		if value_estimate > deferred_map_expansion_threshold - key_estimate {
			return deferred_map_expansion_threshold + 1
		}
		conversion_estimate := key_estimate + value_estimate
		lookup_zero_estimate := t.zero_value_expansion_estimate(flat.NodeId(-1),
			t.semantic_type_name(actual.value_type))
		if lookup_zero_estimate > deferred_map_expansion_threshold - conversion_estimate {
			return deferred_map_expansion_threshold + 1
		}
		return conversion_estimate + lookup_zero_estimate
	}
	if actual !is types.ArrayFixed || expected !is types.ArrayFixed {
		return 0
	}
	actual_fixed := actual as types.ArrayFixed
	expected_fixed := expected as types.ArrayFixed
	if t.semantic_type_name(actual_fixed.elem_type) == t.semantic_type_name(expected_fixed.elem_type)
		|| !t.forwarded_slot_conversion_supported(actual_type, expected_type) {
		return 0
	}
	len := t.tc.fixed_array_len_value(expected_fixed) or { return 0 }
	nested_estimate := t.forwarded_return_conversion_expansion_estimate(actual_fixed.elem_type,
		expected_fixed.elem_type, depth + 1)
	if nested_estimate > deferred_map_expansion_threshold {
		return nested_estimate
	}
	entry_estimate := array_literal_entry_expansion_estimate + nested_estimate
	if len > deferred_map_expansion_threshold / entry_estimate {
		return deferred_map_expansion_threshold + 1
	}
	return array_literal_base_expansion_estimate + len * entry_estimate
}

// fn_span_map_expansion_estimate measures map-literal lowering generated by a
// function. Most literals are inside [lo, hi), but map-index and membership
// lowering can substitute a constant identifier with a large initializer
// outside that range.
fn (mut t Transformer) fn_span_map_expansion_estimate(lo int, hi int) int {
	mut estimate := 0
	fn_return_type := if hi >= 0 && hi < t.a.nodes.len && t.a.nodes[hi].kind == .fn_decl {
		t.fn_body_return_type(t.a.nodes[hi])
	} else {
		''
	}
	for idx in lo .. hi {
		if idx < 0 || idx >= t.a.nodes.len {
			continue
		}
		node := t.a.nodes[idx]
		estimate += t.comptime_zero_value_expansion_estimate(flat.NodeId(idx), node)
		estimate += t.forwarded_fixed_array_return_expansion_estimate(node, fn_return_type)
		if node.kind == .comptime_for {
			_, kind := comptime_for_parts(node.value)
			if kind in ['fields', 'values', 'variants', 'methods', 'params', 'attributes'] {
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind in [.struct_init, .assoc] {
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .if_expr {
			estimate += t.if_expr_zero_value_expansion_estimate(flat.NodeId(idx), node)
		}
		if node.kind == .match_stmt {
			estimate += t.match_expr_zero_value_expansion_estimate(flat.NodeId(idx), node)
		}
		if node.kind == .or_expr {
			estimate += t.or_expr_zero_value_expansion_estimate(flat.NodeId(idx), node)
		}
		if node.kind == .map_init {
			estimate += t.map_init_expansion_estimate(flat.NodeId(idx), node)
		}
		if node.kind == .array_literal {
			estimate += t.array_literal_expansion_estimate(flat.NodeId(idx), node, false)
		}
		if node.kind == .array_init {
			estimate += t.fixed_array_init_expansion_estimate(flat.NodeId(idx), node)
			if t.dynamic_array_init_requires_deferral(flat.NodeId(idx), node) {
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind == .dump_expr {
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .sql_expr {
			// SQL joins synthesize table field and column arrays from ORM metadata that
			// is not represented by the parsed expression's physical children.
			estimate += deferred_map_expansion_threshold + 1
		}
		if t.interface_cast_expands_from_type_metadata(node) {
			estimate += deferred_map_expansion_threshold + 1
		}
		if t.external_equality_expands_from_type_metadata(node) {
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind in [.is_expr, .as_expr]
			|| (node.kind == .selector && t.external_selector_expands_from_type_metadata(node)) {
			estimate += deferred_map_expansion_threshold + 1
		}
		if t.ownership_method_value_clone_expands(flat.NodeId(idx), node) {
			estimate += deferred_map_expansion_threshold + 1
		}
		if node.kind == .call {
			estimate += t.disabled_call_zero_value_expansion_estimate(flat.NodeId(idx), node)
			if t.compiler_call_expands_from_type_metadata(flat.NodeId(idx), node)
				|| t.builtin_call_auto_stringify_expands(flat.NodeId(idx), node) {
				estimate += deferred_map_expansion_threshold + 1
			}
		}
		if node.kind == .infix {
			estimate +=
				t.disabled_struct_operator_zero_value_expansion_estimate(flat.NodeId(idx), node)
		}
		if t.ownership_for_in_binding_clone_expands(node) {
			estimate += deferred_map_expansion_threshold + 1
		}
		if t.ownership_for_in_map_snapshot_clone_expands(node)
			|| t.ownership_map_assignment_clone_expands(node) {
			estimate += deferred_map_expansion_threshold + 1
		}
		if t.ownership_array_append_expands(node) {
			estimate += deferred_map_expansion_threshold + 1
		}
		// Map index lowering replaces a constant identifier with its initializer
		// through const_expr_for_ident(). That edge is semantic and is not present
		// in the parsed FlatAst, so account for it explicitly here.
		if node.kind == .index && node.children_count > 0 {
			estimate += t.map_index_zero_value_expansion_estimate(node)
			estimate += t.owned_array_index_zero_value_expansion_estimate(node, !isnil(t.tc)
				&& t.tc.ownership_index_read_moves_value(flat.NodeId(idx)))
			base_id := t.a.child(&node, 0)
			if const_expr := t.map_const_expr_for_ident(base_id) {
				estimate += t.external_map_tree_expansion_estimate(const_expr, lo, hi)
			}
		}
		if node.kind == .in_expr && node.children_count > 1 {
			if t.array_membership_equality_expands(node) {
				estimate += deferred_map_expansion_threshold + 1
			}
			rhs_id := t.a.child(&node, 1)
			if const_expr := t.map_const_expr_for_ident(rhs_id) {
				estimate += t.external_map_tree_expansion_estimate(const_expr, lo, hi)
			}
		}
		for ci in 0 .. int(node.children_count) {
			child := t.a.child(&node, ci)
			if int(child) >= lo && int(child) < hi {
				continue
			}
			estimate += t.external_map_tree_expansion_estimate(child, lo, hi)
		}
	}
	return estimate
}

// map_type_parts supports map type parts handling for Transformer.
fn (t &Transformer) map_type_parts(map_type string) (string, string) {
	clean := t.clean_map_type(map_type)
	if !clean.starts_with('map[') {
		return '', ''
	}
	return t.map_key_type(clean), t.map_value_type(clean)
}

fn (t &Transformer) map_key_storage_type(key_type string) string {
	if backing := t.map_key_backing_type(key_type) {
		return backing
	}
	return key_type
}

fn (t &Transformer) map_key_backing_type(key_type string) ?string {
	clean := t.normalize_type_alias(key_type).trim_space()
	raw := key_type.trim_space()
	for candidate in [clean, raw] {
		if candidate.len == 0 {
			continue
		}
		if backing := t.enum_backing_types[candidate] {
			return backing
		}
		if !candidate.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${candidate}'
			if backing := t.enum_backing_types[qname] {
				return backing
			}
		}
	}
	// A bare alias spelling from a foreign-module expansion (auto-stringified
	// fields keep their declaring module's spelling): resolve it through the
	// checker alias table like wrap_string_conversion does, and store keys as
	// the scalar base so the declared C type never names the unresolvable
	// alias. The transform's own struct mirror holds first-wins bare aliases
	// for unrelated modules, so only a checker-declared struct blocks this.
	if !isnil(t.tc) && !clean.contains('.') && clean !in t.tc.structs {
		mut alias_target := t.tc.type_aliases[clean] or { '' }
		if alias_target.len == 0 {
			suffix := '.${clean}'
			mut matches := 0
			for aname, target in t.tc.type_aliases {
				if aname.ends_with(suffix) {
					alias_target = target
					matches++
					if matches > 1 {
						alias_target = ''
						break
					}
				}
			}
		}
		if alias_target.len > 0 {
			base := t.normalize_type_alias(alias_target).trim_space()
			if base in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte', 'u16',
				'u32', 'u64', 'rune', 'char', 'string'] {
				return base
			}
		}
	}
	return none
}

// make_new_map_call builds make new map call data for transform.
fn (mut t Transformer) make_new_map_call(map_type string) flat.NodeId {
	key_type, value_type := t.map_type_parts(map_type)
	key_storage_type := t.map_key_storage_type(key_type)
	hash_fn, eq_fn, clone_fn, free_fn := t.map_callback_names_for_type(key_storage_type)
	mut args := []flat.NodeId{}
	args << t.make_sizeof_type(key_storage_type)
	args << t.make_sizeof_type(value_type)
	args << t.make_ident(hash_fn)
	args << t.make_ident(eq_fn)
	args << t.make_ident(clone_fn)
	args << t.make_ident(free_fn)
	return t.make_call_typed('new_map', args, map_type)
}

fn (t &Transformer) new_map_call_type(node flat.Node) string {
	if node.kind != .call || node.children_count < 3 {
		return ''
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != 'new_map' {
		return ''
	}
	key_size := t.a.child_node(&node, 1)
	value_size := t.a.child_node(&node, 2)
	if key_size.kind != .sizeof_expr || value_size.kind != .sizeof_expr || key_size.value.len == 0
		|| value_size.value.len == 0 {
		return ''
	}
	return 'map[${key_size.value}]${value_size.value}'
}

// map_callback_names supports map callback names handling for transform.
fn map_callback_names(key_type string) (string, string, string, string) {
	if key_type == 'string' {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	mut size_suffix := '4'
	if key_type in ['u8', 'i8', 'bool', 'char'] {
		size_suffix = '1'
	} else if key_type in ['u16', 'i16'] {
		size_suffix = '2'
	} else if key_type in ['i64', 'u64', 'isize', 'usize', 'f64', 'voidptr']
		|| key_type.contains('Arc[') || key_type.contains('Arc_') {
		size_suffix = '8'
	}

	return 'map_hash_int_${size_suffix}', 'map_eq_int_${size_suffix}', 'map_clone_int_${size_suffix}', 'map_free_nop'
}

fn (t &Transformer) map_callback_names_for_type(key_type string) (string, string, string, string) {
	if !isnil(t.tc) {
		clean := t.tc.parse_type(t.normalize_type_alias(key_type))
		if clean is types.ArrayFixed {
			base := '${t.tc.c_type(clean)}_map_key'
			return '${base}_hash', '${base}_eq', '${base}_clone', '${base}_free'
		}
	}
	return map_callback_names(key_type)
}

// map_index_info supports map index info handling for Transformer.
fn (mut t Transformer) map_index_info(index_id flat.NodeId) ?MapIndexInfo {
	if int(index_id) < 0 {
		return none
	}
	lhs := t.a.nodes[int(index_id)]
	if lhs.kind != .index || lhs.children_count < 2 || lhs.value == 'range' {
		return none
	}
	base_id := t.a.child(&lhs, 0)
	key_id := t.a.child(&lhs, 1)
	mut base_type := t.node_type(base_id)
	base_node := t.a.nodes[int(base_id)]
	if base_node.kind == .ident {
		local_type := t.var_type(base_node.value)
		local_map_type := t.clean_map_type(local_type)
		if local_map_type.starts_with('map[') && !local_map_type.contains('unknown')
			&& !t.generic_arg_is_unresolved(local_map_type) {
			base_type = if local_type.trim_space().starts_with('&') {
				'&${local_map_type}'
			} else {
				local_map_type
			}
		}
	}
	checker_base_type := t.raw_checker_node_type(base_id)
	checker_map_type := t.clean_map_type(checker_base_type)
	local_map_type := t.clean_map_type(base_type)
	local_map_is_concrete := local_map_type.starts_with('map[')
		&& !local_map_type.contains('unknown') && !t.generic_arg_is_unresolved(local_map_type)
	if !local_map_is_concrete && checker_map_type.starts_with('map[')
		&& !checker_map_type.contains('unknown') {
		base_type = if checker_base_type.trim_space().starts_with('&') {
			'&${checker_map_type}'
		} else {
			checker_map_type
		}
	}
	map_type := t.clean_map_type(base_type)
	if !map_type.starts_with('map[') {
		return none
	}
	key_type, raw_value_type := t.map_type_parts(map_type)
	if key_type.len == 0 || raw_value_type.len == 0 {
		return none
	}
	// Qualify a bare local sum type (`Value` -> `eval.Value`). A bare name can clash
	// with an imported type of the same name and resolve to the wrong sum type when the
	// value is later wrapped into the map element type.
	mut value_type := raw_value_type
	if !value_type.contains('.') && !value_type.contains('[') && !value_type.starts_with('&')
		&& t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qualified := '${t.cur_module}.${value_type}'
		if qualified in t.sum_types || (!isnil(t.tc) && qualified in t.tc.sum_types) {
			value_type = qualified
		}
	}
	return MapIndexInfo{
		base_id:          base_id
		key_id:           key_id
		base_type:        base_type
		key_type:         key_type
		key_storage_type: t.map_key_storage_type(key_type)
		value_type:       value_type
	}
}

// make_map_get_expr builds make map get expr data for transform.
fn (mut t Transformer) make_map_get_expr(map_expr flat.NodeId, base_type string, key_name string, zero_name string, value_type string) flat.NodeId {
	fixed_value_type := t.fixed_array_map_value_type_text(value_type)
	effective_value_type := if fixed_value_type.len > 0 { fixed_value_type } else { value_type }
	clean_value_type := if t.is_fixed_array_type(effective_value_type) {
		if t.fixed_array_type_contains_map(effective_value_type) {
			effective_value_type
		} else {
			fixed_array_canonical_type(effective_value_type)
		}
	} else {
		effective_value_type
	}
	call := t.make_call_typed('map__get', [t.runtime_addr(map_expr, base_type),
		t.make_prefix(.amp, t.make_ident(key_name)), t.make_prefix(.amp, t.make_ident(zero_name))],
		'voidptr')
	cast := t.make_cast('&${clean_value_type}', call, '&${clean_value_type}')
	result := t.make_prefix(.mul, cast)
	t.set_node_typ(int(result), clean_value_type)
	return result
}

fn (t &Transformer) fixed_array_type_contains_map(typ string) bool {
	clean := t.normalize_type_alias(typ).trim_space()
	if clean.starts_with('map[') {
		return true
	}
	if !t.is_fixed_array_type(clean) {
		return false
	}
	return t.fixed_array_type_contains_map(fixed_array_elem_type(clean))
}

// make_map_get_check_expr builds make map get check expr data for transform.
fn (mut t Transformer) make_map_get_check_expr(map_expr flat.NodeId, base_type string, key_name string) flat.NodeId {
	return t.make_call_typed('map__get_check', [t.runtime_addr(map_expr, base_type),
		t.make_prefix(.amp, t.make_ident(key_name))], 'voidptr')
}

fn (mut t Transformer) make_map_get_key_check_expr(map_expr flat.NodeId, base_type string, key_name string) flat.NodeId {
	return t.make_call_typed('map__get_key_check', [t.runtime_addr(map_expr, base_type),
		t.make_prefix(.amp, t.make_ident(key_name))], 'voidptr')
}

// make_map_exists_expr builds make map exists expr data for transform.
fn (mut t Transformer) make_map_exists_expr(map_expr flat.NodeId, base_type string, key_name string) flat.NodeId {
	return t.make_call_typed('map__exists', [t.runtime_addr(map_expr, base_type),
		t.make_prefix(.amp, t.make_ident(key_name))], 'bool')
}

// make_map_set_stmt builds make map set stmt data for transform.
fn (mut t Transformer) make_map_set_stmt(map_expr flat.NodeId, base_type string, key_name string, value_name string) flat.NodeId {
	call := t.make_call_typed('map__set', [t.runtime_addr(map_expr, base_type),
		t.make_prefix(.amp, t.make_ident(key_name)), t.make_prefix(.amp, t.make_ident(value_name))],
		'void')
	return t.make_expr_stmt(call)
}

fn (mut t Transformer) stable_map_lvalue_for_reuse(id flat.NodeId) flat.NodeId {
	if t.expr_can_take_address(id) {
		return t.transform_lvalue(id)
	}
	return t.stable_expr_for_reuse(id)
}

// const_expr_for_ident supports const expr for ident handling for Transformer.
fn (t &Transformer) const_expr_for_ident(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	if t.var_type(node.value).len > 0 {
		return none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${node.value}'
		if expr_id := t.tc.const_exprs[qname] {
			return expr_id
		}
	}
	if expr_id := t.tc.const_exprs[node.value] {
		return expr_id
	}
	return none
}

// lower_map_membership_expr builds lower map membership expr data for transform.
fn (mut t Transformer) lower_map_membership_expr(map_id flat.NodeId, key_id flat.NodeId, map_type string) ?flat.NodeId {
	clean_type := t.clean_map_type(map_type)
	mut key_type := ''
	if clean_type.starts_with('map[') {
		key_type = t.map_key_type(clean_type)
	}
	if key_type.len == 0 {
		key_type = t.node_type(key_id)
	}
	if key_type.len == 0 {
		key := t.a.nodes[int(key_id)]
		if key.kind == .selector {
			key_type = t.selector_field_type(key)
		}
	}
	if key_type.len == 0 {
		return none
	}
	map_source_id := t.const_expr_for_ident(map_id) or { map_id }
	// Spill the typed key before materializing the container so a side-effecting key
	// evaluates before a value-branch map's hoisted propagation prelude, preserving
	// source order, e.g. `tr.key() in (match node { First { tr.map_first(node)! } ... })`.
	key_name := t.new_temp('map_key')
	t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(key_id,
		key_type), t.map_key_storage_type(key_type))
	// Route a value `match`/`if` map container through value lowering so a propagating
	// arm tail is materialized as a value (no-op for the common non-branch containers).
	map_expr := if t.is_value_match_or_if_operand(map_source_id) {
		t.transform_value_operand(map_source_id)
	} else {
		t.stable_expr_for_reuse(map_source_id)
	}
	exists := t.make_map_exists_expr(map_expr, map_type, key_name)
	cleanup_key := !isnil(t.tc) && t.map_key_expr_creates_owned_value(key_id, key_type)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type))
	if cleanup_key {
		result_name := t.new_temp('map_exists')
		t.pending_stmts << t.make_decl_assign_typed(result_name, exists, 'bool')
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(key_name),
		], 'void'))
		return t.make_ident(result_name)
	}
	return exists
}

// try_lower_map_index_expr supports try lower map index expr handling for Transformer.
fn (mut t Transformer) try_lower_map_index_expr(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return none
	}
	base_id := t.a.child(&node, 0)
	key_id := t.a.child(&node, 1)
	base_type := t.node_type(base_id)
	map_type := t.clean_map_type(base_type)
	if !map_type.starts_with('map[') {
		return none
	}
	key_type, value_type := t.map_type_parts(map_type)
	if key_type.len == 0 || value_type.len == 0 {
		return none
	}
	map_source_id := t.const_expr_for_ident(base_id) or { base_id }
	source_is_owned_temporary := !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(map_type))
		&& !base_type.starts_with('&') && !t.expr_can_take_address(map_source_id)
	// Route a value `match`/`if` map-index base through value lowering (e.g.
	// `(match n { First { make_map_first(n)! } ... })['key']`); otherwise the propagating
	// arm tail is lowered in a value-less statement context and emits an empty expression.
	// `transform_value_operand` materializes it into a value temp (stable for the repeated
	// use below); non-branch bases keep `stable_expr_for_reuse`. The base is evaluated before
	// the key: if the key hoists a value branch whose prelude can reassign a syntactically
	// stable base (`items[match node { First { replace(mut items)! } ... }]`), snapshot the
	// base's source-order value so the lookup uses the map evaluated before that prelude.
	map_expr := if t.is_value_match_or_if_operand(map_source_id) {
		t.transform_value_operand(map_source_id)
	} else if t.operand_hoists_value_branch(key_id)
		&& t.operand_needs_ordering_snapshot(map_source_id) {
		t.snapshot_expr_for_reuse(map_source_id)
	} else {
		t.stable_expr_for_reuse(map_source_id)
	}
	key_name := t.new_temp('map_key')
	t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(key_id,
		key_type), t.map_key_storage_type(key_type))
	cleanup_key := !isnil(t.tc) && t.map_key_expr_creates_owned_value(key_id, key_type)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type))
	if !isnil(t.tc) && t.tc.ownership_index_read_moves_value(id)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type)) {
		result := t.lower_owned_map_index_move(map_source_id, map_expr, base_type, key_name,
			value_type)
		if cleanup_key {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(key_name),
			], 'void'))
		}
		return result
	}
	zero_name := t.new_temp('map_zero')
	t.pending_stmts << t.make_decl_assign_typed(zero_name, t.zero_value_for_type(value_type),
		value_type)
	value := t.make_map_get_expr(map_expr, base_type, key_name, zero_name, value_type)
	if cleanup_key || source_is_owned_temporary {
		result_name := t.new_temp('map_index_value')
		t.pending_stmts << t.make_decl_assign_typed(result_name, value, value_type)
		if cleanup_key {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(key_name),
			], 'void'))
		}
		if source_is_owned_temporary {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				map_expr,
			], 'void'))
		}
		result := t.make_ident(result_name)
		t.set_node_typ(int(result), value_type)
		return result
	}
	return value
}

// lower_owned_map_index_move materializes an indexed value that ownership analysis
// consumed, then clears the stored value so the map destructor cannot destroy it again.
fn (mut t Transformer) lower_owned_map_index_move(source_id flat.NodeId, map_expr flat.NodeId, map_type string, key_name string, value_type string) flat.NodeId {
	ptr_name := t.new_temp('owned_index_map_ptr')
	result_name := t.new_temp('owned_index_map_value')
	ptr := t.make_map_get_check_expr(map_expr, map_type, key_name)
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, ptr, 'voidptr')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.zero_value_for_type(value_type),
		value_type)
	clean_value_type := if t.is_fixed_array_type(value_type) {
		fixed_array_canonical_type(value_type)
	} else {
		value_type
	}
	stored_read := t.make_prefix(.mul, t.make_cast('&${clean_value_type}', t.make_ident(ptr_name),
		'&${clean_value_type}'))
	body := [t.make_assign(t.make_ident(result_name), stored_read),
		t.make_clear_map_ptr_value(ptr_name, value_type)]
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	body_block := t.make_block(body)
	start := t.a.children.len
	t.a.children << cond
	t.a.children << body_block
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:                 .if_expr
		children_start:       start
		children_count:       2
		skip_ownership_drops: true
	})
	if !map_type.starts_with('&') && !t.expr_can_take_address(source_id) {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [map_expr], 'void'))
	}
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), value_type)
	return result
}

// is_map_index_or_expr reports whether is map index or expr applies in transform.
fn (mut t Transformer) is_map_index_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	expr := t.a.child_node(&node, 0)
	if expr.kind != .index || expr.children_count < 2 || expr.value == 'range' {
		return false
	}
	base_id := t.a.child(expr, 0)
	base_type := t.node_type(base_id)
	return t.clean_map_type(base_type).starts_with('map[')
}

// transform_map_index_or_expr transforms transform map index or expr data for transform.
fn (mut t Transformer) transform_map_index_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	info := t.map_index_info(expr_id) or { return id }
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	ptr_name := t.new_temp('map_ptr')
	val_name := t.new_temp('map_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	key_expr := t.transform_expr_for_type(info.key_id, info.key_type)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	mut result_type := info.value_type
	mut wrap_found_value := false
	source_is_optional := t.map_value_type_is_optional(info.value_type)
	mut source_value_type := info.value_type
	if source_is_optional {
		source_value_type = t.map_optional_value_base_type(info.value_type)
		result_type = source_value_type
	}
	if source_is_optional && t.or_body_is_none(body_id) {
		result_type = info.value_type
		wrap_found_value = true
	}
	if t.map_value_type_is_optional(node.typ) {
		optional_target := t.map_optional_target_type(node.typ)
		body_type := t.stmt_value_type(body_id)
		body_keeps_optional := t.or_body_is_none(body_id) || t.map_value_type_is_optional(body_type)
		if (!source_is_optional || body_keeps_optional)
			&& t.normalize_type_alias(t.map_optional_value_base_type(optional_target)) == t.normalize_type_alias(source_value_type) {
			result_type = optional_target
			wrap_found_value = true
		}
	}
	prelude << t.make_decl_assign_typed(key_name, key_expr, info.key_storage_type)
	prelude << t.make_decl_assign_typed(ptr_name, t.make_map_get_check_expr(map_expr,
		info.base_type, key_name), 'voidptr')
	if !isnil(t.tc) && t.map_key_expr_creates_owned_value(info.key_id, info.key_type)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(info.key_type)) {
		prelude << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(key_name),
		], 'void'))
	}
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(result_type), result_type)
	move_found_value := !isnil(t.tc) && t.tc.ownership_index_read_moves_value(expr_id)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(info.value_type))

	ptr_ident := t.make_ident(ptr_name)
	found_cond := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))
	else_block := t.make_block(t.lower_map_or_body_to_stmts(body_id, val_name, result_type,
		node.value, t.make_ierror_none()))
	ptr_value := t.make_prefix(.mul, t.make_cast('&${info.value_type}', t.make_ident(ptr_name),
		'&${info.value_type}'))
	then_block := if source_is_optional {
		opt_name := t.new_temp('map_opt')
		opt_decl := t.make_decl_assign_typed(opt_name, ptr_value, info.value_type)
		opt_value := t.make_selector(t.make_ident(opt_name), 'value', source_value_type)
		found_value := if wrap_found_value {
			t.make_optional_some(opt_value, result_type)
		} else {
			opt_value
		}
		assign_found := t.make_assign(t.make_ident(val_name), found_value)
		mut ok_stmts := [assign_found]
		if move_found_value {
			ok_stmts << t.make_clear_map_ptr_value(ptr_name, info.value_type)
		}
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', 'bool')
		opt_err_expr := t.make_selector(t.make_ident(opt_name), 'err', 'IError')
		mut opt_else_stmts := []flat.NodeId{}
		if move_found_value && node.value in ['?', '!'] {
			// Propagation transfers the failed wrapper's error through `opt_name`, so the map
			// slot must relinquish it before the return/panic branch consumes that copy.
			opt_else_stmts << t.make_clear_map_ptr_value(ptr_name, info.value_type)
		}
		opt_else_stmts << t.lower_map_or_body_to_stmts(body_id, val_name, result_type, node.value,
			opt_err_expr)
		opt_else_block := t.make_block(opt_else_stmts)
		t.make_block([opt_decl, t.make_if(ok_cond, t.make_block(ok_stmts), opt_else_block)])
	} else {
		found_value := if wrap_found_value {
			t.make_optional_some(ptr_value, result_type)
		} else {
			ptr_value
		}
		mut found_stmts := [t.make_assign(t.make_ident(val_name), found_value)]
		if move_found_value {
			found_stmts << t.make_clear_map_ptr_value(ptr_name, info.value_type)
		}
		t.make_block(found_stmts)
	}
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << t.make_if(found_cond, then_block, else_block)
	return t.make_ident(val_name)
}

// make_clear_map_ptr_value zeroes a value after ownership was moved out of a
// map slot returned by map__get_check.
fn (mut t Transformer) make_clear_map_ptr_value(ptr_name string, value_type string) flat.NodeId {
	clean_value_type := if t.is_fixed_array_type(value_type) {
		fixed_array_canonical_type(value_type)
	} else {
		value_type
	}
	stored := t.make_prefix(.mul, t.make_cast('&${clean_value_type}', t.make_ident(ptr_name),
		'&${clean_value_type}'))
	return t.make_assign(stored, t.zero_value_for_type(value_type))
}

// lower_map_or_body_to_stmts converts lower map or body to stmts data for transform.
fn (mut t Transformer) lower_map_or_body_to_stmts(body_id flat.NodeId, target_name string, target_type string, mode string, err_expr flat.NodeId) []flat.NodeId {
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return [t.make_none_return_stmt_with_err_expr(err_expr)]
		}
		return [t.make_panic_stmt('option/result propagation failed')]
	}
	if int(body_id) < 0 {
		return []flat.NodeId{}
	}
	body := t.a.nodes[int(body_id)]
	if body.kind != .block {
		if body.kind == .call && t.is_error_call(body) && t.is_optional_type_name(t.cur_fn_ret_type) {
			return [t.make_return(body_id, t.cur_fn_ret_type)]
		}
		if body.kind == .none_expr && t.map_value_type_is_optional(target_type) {
			return [
				t.make_assign(t.make_ident(target_name), t.make_optional_none(target_type)),
			]
		}
		if body.kind == .none_expr && !t.is_optional_type_name(target_type)
			&& t.is_optional_type_name(t.cur_fn_ret_type) {
			return [t.make_none_return_stmt()]
		}
		return [t.make_assign(t.make_ident(target_name), t.transform_expr(body_id))]
	}
	mut result := []flat.NodeId{}
	if body.children_count == 0 {
		return result
	}
	saved_var_types := t.var_types.clone()
	t.set_implicit_err_var_type()
	err_value := if int(err_expr) >= 0 {
		err_expr
	} else {
		t.make_struct_init('IError')
	}
	result << t.make_decl_assign_typed('err', err_value, 'IError')
	for i in 0 .. body.children_count {
		child_id := t.a.child(&body, i)
		child := t.a.nodes[int(child_id)]
		is_last := i == body.children_count - 1
		if is_last && child.kind == .expr_stmt && child.children_count > 0 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .none_expr && t.map_value_type_is_optional(target_type) {
				result << t.make_assign(t.make_ident(target_name),
					t.make_optional_none(target_type))
				continue
			}
			if inner.kind == .none_expr && !t.is_optional_type_name(target_type)
				&& t.is_optional_type_name(t.cur_fn_ret_type) {
				result << t.make_none_return_stmt()
				continue
			}
			if inner.kind == .call && t.is_error_call(inner)
				&& t.is_optional_type_name(t.cur_fn_ret_type) {
				result << t.make_return(inner_id, t.cur_fn_ret_type)
				continue
			}
			if t.node_type(inner_id) == 'void' {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else {
				value := t.transform_expr(inner_id)
				t.drain_pending(mut result)
				result << t.make_assign(t.make_ident(target_name), value)
			}
		} else {
			expanded := t.transform_stmt(child_id)
			t.drain_pending(mut result)
			for eid in expanded {
				result << eid
			}
		}
	}
	_ = target_type
	t.restore_var_types(saved_var_types)
	return result
}

fn (t &Transformer) map_value_type_is_optional(typ string) bool {
	clean := t.normalize_type_alias(typ).trim_space()
	return t.is_optional_type_name(clean) || clean == 'Optional'
}

fn (t &Transformer) map_optional_target_type(typ string) string {
	clean := t.normalize_type_alias(typ).trim_space()
	if t.is_optional_type_name(clean) {
		return t.qualify_optional_type(clean)
	}
	return typ
}

fn (t &Transformer) map_optional_value_base_type(typ string) string {
	clean := t.normalize_type_alias(typ).trim_space()
	if t.is_optional_type_name(clean) {
		return t.optional_base_type(t.qualify_optional_type(clean))
	}
	return typ
}

// try_lower_map_index_assign supports try lower map index assign handling for Transformer.
fn (mut t Transformer) try_lower_map_index_assign(id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind !in [.assign, .index_assign] || node.children_count < 2 {
		return none
	}
	info := t.map_index_info(t.a.child(&node, 0)) or { return none }
	map_expr := t.stable_map_lvalue_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	mut key_value := t.transform_expr_for_type(info.key_id, info.key_type)
	mut key_is_owned := t.map_key_expr_creates_owned_value(info.key_id, info.key_type)
	if !key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(info.key_type).trim_space() != 'string' {
		key_type := t.tc.parse_type(info.key_type)
		if t.tc.ownership_type_requires_destruction(key_type) {
			if _ := t.tc.ownership_default_clone_missing_method(key_type) {
				return []flat.NodeId{}
			}
			key_value = t.make_compiler_default_clone_value(key_value, info.key_type, true)
			key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, key_value, info.key_storage_type)
	rhs_id := t.a.child(&node, 1)
	if node.op == .assign {
		value_name := t.new_temp('map_val')
		mut value := if info.value_type.starts_with('&') && t.is_sum_type_name(info.value_type[1..]) {
			t.transform_expr_for_type(rhs_id, info.value_type)
		} else if info.value_type in t.sum_types
			|| t.resolve_sum_name(info.value_type) in t.sum_types {
			t.wrap_sum_value(rhs_id, info.value_type)
		} else {
			t.transform_expr_for_type(rhs_id, info.value_type)
		}
		mut assignment_is_valid := true
		value, assignment_is_valid = t.clone_map_assignment_rhs_if_needed(value, rhs_id,
			t.a.child(&node, 0), info.value_type)
		t.drain_pending(mut result)
		if !assignment_is_valid {
			return []flat.NodeId{}
		}
		result << t.make_decl_assign_typed(value_name, value, info.value_type)
		cleanup_key, existing_key_name := t.prepare_owned_map_set_key_cleanup(key_is_owned,
			info.key_type, map_expr, info.base_type, key_name, mut result)
		// A map-derived or possibly aliasing replacement was cloned above. The old slot can now
		// be destroyed normally; only a true move/reinitialization suppresses this drop.
		if isnil(t.tc) || !t.tc.ownership_assignment_reinitializes_moved_value(id) {
			t.append_map_value_drop_before_set(map_expr, info.base_type, key_name, info.value_type, mut
				result)
		}
		result << t.make_map_set_stmt(map_expr, info.base_type, key_name, value_name)
		if int(id) in t.local_closure_field_cleanups {
			result << t.make_local_closure_cleanup_defer(value_name)
		}
		t.append_owned_map_set_key_cleanup(key_name, cleanup_key, existing_key_name, mut result)
		return result
	}
	if node.op == .left_shift_assign && info.value_type.starts_with('[]') {
		cleanup_key, existing_key_name := t.prepare_owned_map_set_key_cleanup(key_is_owned,
			info.key_type, map_expr, info.base_type, key_name, mut result)
		if !t.lower_map_index_append_with_info(info, map_expr, key_name, rhs_id, mut result) {
			return []flat.NodeId{}
		}
		t.append_owned_map_set_key_cleanup(key_name, cleanup_key, existing_key_name, mut result)
		return result
	}
	op := map_compound_to_infix_op(node.op) or { return none }
	cleanup_key, existing_key_name := t.prepare_owned_map_set_key_cleanup(key_is_owned,
		info.key_type, map_expr, info.base_type, key_name, mut result)
	t.lower_map_index_compound_with_info(info, map_expr, key_name, op, rhs_id, mut result)
	t.append_owned_map_set_key_cleanup(key_name, cleanup_key, existing_key_name, mut result)
	return result
}

// prepare_owned_map_set_key_cleanup records whether map__set receives an independently
// owned key. Non-string keys transfer into a new slot by byte copy, so
// only an existing-key update leaves the incoming owner unused. String keys are cloned by
// the runtime for new slots and therefore always leave a fresh incoming owner to destroy.
fn (mut t Transformer) prepare_owned_map_set_key_cleanup(key_is_owned bool, key_type_name string, map_expr flat.NodeId, map_type string, key_name string, mut result []flat.NodeId) (bool, string) {
	if isnil(t.tc) || !key_is_owned {
		return false, ''
	}
	key_type := t.tc.parse_type(key_type_name)
	if !t.tc.ownership_type_requires_destruction(key_type) {
		return false, ''
	}
	if t.normalize_type_alias(key_type_name).trim_space() == 'string' {
		return true, ''
	}
	existing_name := t.new_temp('map_key_existed')
	result << t.make_decl_assign_typed(existing_name, t.make_map_exists_expr(map_expr, map_type,
		key_name), 'bool')
	return true, existing_name
}

fn (mut t Transformer) append_owned_map_set_key_cleanup(key_name string, cleanup bool, existing_name string, mut result []flat.NodeId) {
	if !cleanup {
		return
	}
	drop_stmt := t.make_expr_stmt(t.make_call_typed('drop_owned', [
		t.make_ident(key_name),
	], 'void'))
	if existing_name.len == 0 {
		result << drop_stmt
		return
	}
	result << t.make_if(t.make_ident(existing_name), t.make_block([drop_stmt]), t.make_empty())
}

// map_key_expr_creates_owned_value includes ownership-bearing expressions recognized by
// the checker plus string forms lowered directly by the transformer. Concatenation and
// interpolation allocate independent string storage that map__set only borrows/clones.
fn (t &Transformer) map_key_expr_creates_owned_value(id flat.NodeId, key_type_name string) bool {
	if isnil(t.tc) {
		return false
	}
	if t.tc.ownership_expr_creates_owned_value(id) {
		return true
	}
	if t.normalize_type_alias(key_type_name).trim_space() != 'string' {
		return false
	}
	mut expr_id := id
	for int(expr_id) >= 0 {
		expr := t.a.nodes[int(expr_id)]
		if expr.kind in [.paren, .expr_stmt, .cast_expr] && expr.children_count > 0 {
			expr_id = t.a.child(&expr, 0)
			continue
		}
		return expr.kind == .string_interp || (expr.kind == .infix && expr.op == .plus)
	}
	return false
}

// Map literals consume named string storage, even though an identifier does not create a
// fresh value. map__set clones string keys, so the consumed input still needs destruction.
fn (t &Transformer) map_literal_key_expr_creates_owned_value(id flat.NodeId, key_type_name string) bool {
	if t.map_key_expr_creates_owned_value(id, key_type_name) {
		return true
	}
	if isnil(t.tc) || t.normalize_type_alias(key_type_name).trim_space() != 'string' {
		return false
	}
	mut expr_id := id
	for int(expr_id) >= 0 {
		expr := t.a.nodes[int(expr_id)]
		if expr.kind in [.paren, .expr_stmt, .cast_expr] && expr.children_count > 0 {
			expr_id = t.a.child(&expr, 0)
			continue
		}
		return expr.kind in [.ident, .selector, .index]
	}
	return false
}

// clone_map_assignment_rhs_if_needed makes a borrowed or map-derived replacement independent
// before the stored owner is destroyed. The checker rejects a required clone that cannot be
// made, and the false result prevents unsafe lowering of that invalid assignment.
fn (mut t Transformer) clone_map_assignment_rhs_if_needed(value flat.NodeId, rhs_id flat.NodeId, lhs_id flat.NodeId, value_type_name string) (flat.NodeId, bool) {
	cloned := t.clone_borrowed_projection(rhs_id, value, value_type_name)
	if cloned != value {
		return cloned, true
	}
	if isnil(t.tc) || (!t.tc.ownership_expr_moves_storage(rhs_id, lhs_id)
		&& !t.tc.ownership_expr_clones_borrowed_storage(rhs_id)) {
		return value, true
	}
	value_type := t.tc.parse_type(value_type_name)
	if !t.tc.ownership_type_requires_destruction(value_type) {
		return value, true
	}
	if _ := t.tc.ownership_default_clone_missing_method(value_type) {
		return value, false
	}
	return t.make_compiler_default_borrowed_clone_value(value, value_type_name, true), true
}

// append_map_value_drop_before_set destroys an existing owned map value before
// map__set overwrites its storage. The replacement has already been saved by the
// caller, so cloning from the old value remains valid.
fn (mut t Transformer) append_map_value_drop_before_set(map_expr flat.NodeId, map_type string, key_name string, value_type_name string, mut result []flat.NodeId) {
	if isnil(t.tc) {
		return
	}
	value_type := t.tc.parse_type(value_type_name)
	if !t.tc.ownership_type_requires_destruction(value_type) {
		return
	}
	ptr_name := t.new_temp('map_old_value')
	ptr := t.make_map_get_check_expr(map_expr, map_type, key_name)
	result << t.make_decl_assign_typed(ptr_name, ptr, 'voidptr')
	ptr_ident := t.make_ident(ptr_name)
	old_value_ptr := t.make_cast('&${value_type_name}', ptr_ident, '&${value_type_name}')
	old_value := t.make_prefix(.mul, old_value_ptr)
	t.set_node_typ(int(old_value), value_type_name)
	drop_call := t.make_call_typed('drop_owned', [old_value], 'void')
	found := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))
	result << t.make_if(found, t.make_block([t.make_expr_stmt(drop_call)]), t.make_empty())
}

// try_lower_nested_map_index_assign lowers `m[k1][k2] = value` by updating the
// inner map value and storing it back into the outer map.
fn (mut t Transformer) try_lower_nested_map_index_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind !in [.assign, .index_assign] || node.children_count < 2
		|| (node.op != .assign && map_compound_to_infix_op(node.op) == none) {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count < 2 {
		return none
	}
	outer_index_id := t.a.child(&lhs, 0)
	outer_index := t.a.nodes[int(outer_index_id)]
	if outer_index.kind != .index {
		return none
	}
	outer_info := t.map_index_info(outer_index_id) or { return none }
	inner_map_type := t.clean_map_type(outer_info.value_type)
	if !inner_map_type.starts_with('map[') {
		return none
	}
	inner_key_type, inner_value_type := t.map_type_parts(inner_map_type)
	if inner_key_type.len == 0 || inner_value_type.len == 0 {
		return none
	}
	map_expr := t.stable_map_lvalue_for_reuse(outer_info.base_id)
	outer_key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	mut outer_key_value := t.transform_expr_for_type(outer_info.key_id, outer_info.key_type)
	mut outer_key_is_owned := t.map_key_expr_creates_owned_value(outer_info.key_id,
		outer_info.key_type)
	if !outer_key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(outer_info.key_type).trim_space() != 'string' {
		outer_key_type := t.tc.parse_type(outer_info.key_type)
		if t.tc.ownership_type_requires_destruction(outer_key_type) {
			if _ := t.tc.ownership_default_clone_missing_method(outer_key_type) {
				return []flat.NodeId{}
			}
			outer_key_value = t.make_compiler_default_clone_value(outer_key_value,
				outer_info.key_type, true)
			outer_key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(outer_key_name, outer_key_value, outer_info.key_storage_type)
	cleanup_outer_key, outer_key_existed_name := t.prepare_owned_map_set_key_cleanup(outer_key_is_owned,
		outer_info.key_type, map_expr, outer_info.base_type, outer_key_name, mut result)
	inner_name := t.load_map_index_current(outer_info, map_expr, outer_key_name, mut result)
	inner_key_name := t.new_temp('map_key')
	inner_key_storage_type := t.map_key_storage_type(inner_key_type)
	inner_key_id := t.a.child(&lhs, 1)
	mut inner_key_value := t.transform_expr_for_type(inner_key_id, inner_key_type)
	mut inner_key_is_owned := t.map_key_expr_creates_owned_value(inner_key_id, inner_key_type)
	if !inner_key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(inner_key_type).trim_space() != 'string' {
		inner_key_parsed_type := t.tc.parse_type(inner_key_type)
		if t.tc.ownership_type_requires_destruction(inner_key_parsed_type)
			&& t.tc.ownership_default_clone_missing_method(inner_key_parsed_type) == none {
			inner_key_value =
				t.make_compiler_default_clone_value(inner_key_value, inner_key_type, true)
			inner_key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(inner_key_name, inner_key_value, inner_key_storage_type)
	inner_map_expr := t.make_ident(inner_name)
	cleanup_inner_key, inner_key_existed_name := t.prepare_owned_map_set_key_cleanup(inner_key_is_owned,
		inner_key_type, inner_map_expr, inner_map_type, inner_key_name, mut result)
	rhs_id := t.a.child(&node, 1)
	if node.op == .assign {
		inner_value_name := t.new_temp('map_val')
		mut inner_value := if inner_value_type.starts_with('&')
			&& t.is_sum_type_name(inner_value_type[1..]) {
			t.transform_expr_for_type(rhs_id, inner_value_type)
		} else if inner_value_type in t.sum_types
			|| t.resolve_sum_name(inner_value_type) in t.sum_types {
			t.wrap_sum_value(rhs_id, inner_value_type)
		} else {
			t.transform_expr_for_type(rhs_id, inner_value_type)
		}
		mut assignment_is_valid := true
		inner_value, assignment_is_valid = t.clone_map_assignment_rhs_if_needed(inner_value,
			rhs_id, lhs_id, inner_value_type)
		t.drain_pending(mut result)
		if !assignment_is_valid {
			return []flat.NodeId{}
		}
		result << t.make_decl_assign_typed(inner_value_name, inner_value, inner_value_type)
		t.append_map_value_drop_before_set(inner_map_expr, inner_map_type, inner_key_name,
			inner_value_type, mut result)
		result << t.make_map_set_stmt(inner_map_expr, inner_map_type, inner_key_name,
			inner_value_name)
	} else {
		inner_info := MapIndexInfo{
			base_id:          outer_index_id
			key_id:           inner_key_id
			base_type:        inner_map_type
			key_type:         inner_key_type
			key_storage_type: inner_key_storage_type
			value_type:       inner_value_type
		}
		op := map_compound_to_infix_op(node.op) or { return none }
		t.lower_map_index_compound_with_info(inner_info, inner_map_expr, inner_key_name, op,
			rhs_id, mut result)
	}
	t.append_owned_map_set_key_cleanup(inner_key_name, cleanup_inner_key, inner_key_existed_name, mut
		result)
	result << t.make_map_set_stmt(map_expr, outer_info.base_type, outer_key_name, inner_name)
	t.append_owned_map_set_key_cleanup(outer_key_name, cleanup_outer_key, outer_key_existed_name, mut
		result)
	return result
}

fn (mut t Transformer) try_lower_nested_map_index_postfix_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .postfix || node.children_count == 0 || node.op !in [.inc, .dec] {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count < 2 {
		return none
	}
	outer_index := t.a.child_node(&lhs, 0)
	if outer_index.kind != .index {
		return none
	}
	start := t.a.children.len
	t.a.children << lhs_id
	t.a.children << t.make_int_literal(1)
	return t.try_lower_nested_map_index_assign(flat.Node{
		kind:           .index_assign
		op:             if node.op == .dec { flat.Op.minus_assign } else { flat.Op.plus_assign }
		children_start: start
		children_count: 2
		pos:            node.pos
	})
}

fn (mut t Transformer) try_lower_nested_map_delete_call(node flat.Node, base_id flat.NodeId, inner_map_type string) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	outer_info := t.map_index_info(base_id) or { return none }
	if t.clean_map_type(outer_info.value_type) != inner_map_type {
		return none
	}
	inner_key_type, inner_value_type := t.map_type_parts(inner_map_type)
	if inner_key_type.len == 0 || inner_value_type.len == 0 {
		return none
	}
	map_expr := t.stable_expr_for_reuse(outer_info.base_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	outer_key_name := t.new_temp('map_key')
	mut outer_key_value := t.transform_expr_for_type(outer_info.key_id, outer_info.key_type)
	mut outer_key_is_owned := t.map_key_expr_creates_owned_value(outer_info.key_id,
		outer_info.key_type)
	if !outer_key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(outer_info.key_type).trim_space() != 'string' {
		outer_key_type := t.tc.parse_type(outer_info.key_type)
		if t.tc.ownership_type_requires_destruction(outer_key_type) {
			if _ := t.tc.ownership_default_clone_missing_method(outer_key_type) {
				return t.make_empty()
			}
			outer_key_value = t.make_compiler_default_clone_value(outer_key_value,
				outer_info.key_type, true)
			outer_key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(outer_key_name, outer_key_value, outer_info.key_storage_type)
	cleanup_outer_key, outer_key_existed_name := t.prepare_owned_map_set_key_cleanup(outer_key_is_owned,
		outer_info.key_type, map_expr, outer_info.base_type, outer_key_name, mut result)
	inner_name := t.load_map_index_current(outer_info, map_expr, outer_key_name, mut result)
	inner_map_expr := t.make_ident(inner_name)
	inner_key_id := t.a.child(&node, 1)
	inner_key_name := t.new_temp('map_key')
	mut inner_key_value := t.transform_expr_for_type(inner_key_id, inner_key_type)
	mut inner_key_is_owned := t.map_key_expr_creates_owned_value(inner_key_id, inner_key_type)
	if !inner_key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(inner_key_type).trim_space() != 'string' {
		inner_key_parsed_type := t.tc.parse_type(inner_key_type)
		if t.tc.ownership_type_requires_destruction(inner_key_parsed_type) {
			if _ := t.tc.ownership_default_clone_missing_method(inner_key_parsed_type) {
				return t.make_empty()
			}
			inner_key_value =
				t.make_compiler_default_clone_value(inner_key_value, inner_key_type, true)
			inner_key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(inner_key_name, inner_key_value,
		t.map_key_storage_type(inner_key_type))
	for stmt in result {
		t.pending_stmts << stmt
	}
	t.mark_fn_used('map__delete')
	handled_delete := t.append_owned_map_entry_delete_with_drops(inner_map_expr, inner_map_type,
		inner_key_name, inner_key_type, inner_value_type)
	if !handled_delete {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('map__delete', [
			t.runtime_addr(inner_map_expr, inner_map_type),
			t.make_prefix(.amp, t.make_ident(inner_key_name)),
		], 'void'))
	}
	if inner_key_is_owned && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(inner_key_type)) {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(inner_key_name),
		], 'void'))
	}
	t.pending_stmts << t.make_map_set_stmt(map_expr, outer_info.base_type, outer_key_name,
		inner_name)
	mut cleanup := []flat.NodeId{}
	t.append_owned_map_set_key_cleanup(outer_key_name, cleanup_outer_key, outer_key_existed_name, mut
		cleanup)
	for stmt in cleanup {
		t.pending_stmts << stmt
	}
	return t.make_empty()
}

fn (mut t Transformer) try_lower_map_index_fixed_array_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind !in [.assign, .index_assign] || node.children_count < 2 || node.op != .assign {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	path := t.map_fixed_array_index_path(lhs_id) or { return none }
	map_expr := t.stable_map_lvalue_for_reuse(path.map_info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(path.map_info.key_id,
		path.map_info.key_type), path.map_info.key_storage_type)
	current_name := t.load_map_index_current(path.map_info, map_expr, key_name, mut result)
	mut target := t.make_ident(current_name)
	mut cur_type := path.map_info.value_type
	for index_id in path.index_ids {
		clean := t.resolved_fixed_array_canonical_type(cur_type)
		elem_type := fixed_array_elem_type(clean)
		if elem_type.len == 0 {
			return none
		}
		target = t.make_index(target, t.transform_expr(index_id), elem_type)
		cur_type = elem_type
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.transform_expr_for_type(rhs_id, path.elem_type)
	result << t.make_assign(target, t.clone_borrowed_assignment_value(rhs_id, rhs, path.elem_type))
	result << t.make_map_set_stmt(map_expr, path.map_info.base_type, key_name, current_name)
	return result
}

fn (mut t Transformer) map_fixed_array_index_path(lhs_id flat.NodeId) ?MapFixedArrayIndexInfo {
	mut ids_rev := []flat.NodeId{}
	mut cur_id := lhs_id
	mut info := MapIndexInfo{}
	mut found := false
	for {
		if int(cur_id) < 0 || int(cur_id) >= t.a.nodes.len {
			return none
		}
		cur := t.a.nodes[int(cur_id)]
		if cur.kind != .index || cur.children_count < 2 || cur.value == 'range' {
			return none
		}
		parent_id := t.a.child(&cur, 0)
		ids_rev << t.a.child(&cur, 1)
		if map_info := t.map_index_info(parent_id) {
			info = map_info
			found = true
			break
		}
		cur_id = parent_id
	}
	if !found || ids_rev.len == 0 {
		return none
	}
	mut index_ids := []flat.NodeId{cap: ids_rev.len}
	for i := ids_rev.len; i > 0; i-- {
		index_ids << ids_rev[i - 1]
	}
	mut cur_type := info.value_type
	for _ in index_ids {
		clean := t.resolved_fixed_array_canonical_type(cur_type)
		if !t.is_fixed_array_type(clean) {
			return none
		}
		cur_type = fixed_array_elem_type(clean)
		if cur_type.len == 0 {
			return none
		}
	}
	return MapFixedArrayIndexInfo{
		map_info:  info
		index_ids: index_ids
		elem_type: cur_type
	}
}

// try_lower_map_index_selector_assign lowers `m[k].field = value` by updating a
// temporary map value and writing it back to the map.
fn (mut t Transformer) try_lower_map_index_selector_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind !in [.assign, .selector_assign, .index_assign] || node.children_count < 2
		|| node.op != .assign {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .selector || lhs.children_count == 0 || lhs.value.len == 0 {
		return none
	}
	base_id := t.a.child(&lhs, 0)
	info := t.map_index_info(base_id) or { return none }
	field_type := t.lvalue_type(lhs_id)
	if field_type.len == 0 {
		return none
	}
	map_expr := t.stable_map_lvalue_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	mut key_value := t.transform_expr_for_type(info.key_id, info.key_type)
	mut key_is_owned := t.map_key_expr_creates_owned_value(info.key_id, info.key_type)
	if !key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(info.key_type).trim_space() != 'string' {
		key_type := t.tc.parse_type(info.key_type)
		if t.tc.ownership_type_requires_destruction(key_type)
			&& t.tc.ownership_default_clone_missing_method(key_type) == none {
			key_value = t.make_compiler_default_clone_value(key_value, info.key_type, true)
			key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, key_value, info.key_storage_type)
	cleanup_key, existing_key_name := t.prepare_owned_map_set_key_cleanup(key_is_owned,
		info.key_type, map_expr, info.base_type, key_name, mut result)
	mut current_existed := flat.empty_node
	if !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(field_type)) {
		if existing_key_name.len > 0 {
			current_existed = t.make_ident(existing_key_name)
		} else {
			current_existed_name := t.new_temp('map_value_existed')
			result << t.make_decl_assign_typed(current_existed_name, t.make_map_exists_expr(map_expr,
				info.base_type, key_name), 'bool')
			current_existed = t.make_ident(current_existed_name)
		}
	}
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	field := t.make_selector(t.make_ident(current_name), lhs.value, field_type)
	rhs_id := t.a.child(&node, 1)
	mut rhs := t.transform_expr_for_type(rhs_id, field_type)
	mut assignment_is_valid := true
	rhs, assignment_is_valid = t.clone_map_assignment_rhs_if_needed(rhs, rhs_id, lhs_id, field_type)
	t.drain_pending(mut result)
	if !assignment_is_valid {
		return []flat.NodeId{}
	}
	rhs_name := t.new_temp('map_field_value')
	result << t.make_decl_assign_typed(rhs_name, rhs, field_type)
	t.append_owned_lvalue_drop_before_assign_if(field, field_type, current_existed, mut result)
	result << t.make_assign_after_owned_drop(field, t.make_ident(rhs_name))
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, current_name)
	t.append_owned_map_set_key_cleanup(key_name, cleanup_key, existing_key_name, mut result)
	return result
}

// append_owned_lvalue_drop_before_assign destroys an owned value after its
// replacement has been saved and before its storage is overwritten.
fn (mut t Transformer) append_owned_lvalue_drop_before_assign(lvalue flat.NodeId, type_name string, mut result []flat.NodeId) {
	t.append_owned_lvalue_drop_before_assign_if(lvalue, type_name, flat.empty_node, mut result)
}

// append_owned_lvalue_drop_before_assign_if destroys an owned value only when its
// storage existed before the assignment. Map-index field updates use this to avoid
// dropping the zero fallback loaded for an absent key.
fn (mut t Transformer) append_owned_lvalue_drop_before_assign_if(lvalue flat.NodeId, type_name string, guard flat.NodeId, mut result []flat.NodeId) {
	if isnil(t.tc) || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(type_name)) {
		return
	}
	drop_call := t.make_call_typed('drop_owned', [lvalue], 'void')
	drop_stmt := t.make_expr_stmt(drop_call)
	if int(guard) >= 0 {
		result << t.make_if(guard, t.make_block([drop_stmt]), t.make_empty())
		return
	}
	result << drop_stmt
}

// map_compound_to_infix_op converts map compound to infix op data for transform.
fn map_compound_to_infix_op(op flat.Op) ?flat.Op {
	match op {
		.plus_assign { return flat.Op.plus }
		.minus_assign { return flat.Op.minus }
		.mul_assign { return flat.Op.mul }
		.power_assign { return flat.Op.power }
		.div_assign { return flat.Op.div }
		.mod_assign { return flat.Op.mod }
		.amp_assign { return flat.Op.amp }
		.pipe_assign { return flat.Op.pipe }
		.xor_assign { return flat.Op.xor }
		.left_shift_assign { return flat.Op.left_shift }
		.right_shift_assign { return flat.Op.right_shift }
		.right_shift_unsigned_assign { return flat.Op.right_shift_unsigned }
		else { return none }
	}
}

// load_map_index_current reads load map index current input for transform.
fn (mut t Transformer) load_map_index_current(info MapIndexInfo, map_expr flat.NodeId, key_name string, mut result []flat.NodeId) string {
	zero_name := t.new_temp('map_zero')
	current_name := t.new_temp('map_val')
	result << t.make_decl_assign_typed(zero_name, t.zero_value_for_type(info.value_type),
		info.value_type)
	get_expr := t.make_map_get_expr(map_expr, info.base_type, key_name, zero_name, info.value_type)
	result << t.make_decl_assign_typed(current_name, get_expr, info.value_type)
	return current_name
}

// lower_map_index_compound_with_info builds lower map index compound with info data for transform.
fn (mut t Transformer) lower_map_index_compound_with_info(info MapIndexInfo, map_expr flat.NodeId, key_name string, op flat.Op, rhs_id flat.NodeId, mut result []flat.NodeId) {
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	rhs := t.transform_expr(rhs_id)
	new_value := if info.value_type == 'string' && op == .plus {
		t.make_call_typed('string__plus', [t.make_ident(current_name), rhs], 'string')
	} else {
		t.make_infix(op, t.make_ident(current_name), rhs)
	}
	result << t.make_assign(t.make_ident(current_name), new_value)
	t.append_map_value_drop_before_set(map_expr, info.base_type, key_name, info.value_type, mut
		result)
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, current_name)
}

// lower_map_index_postfix_with_info builds lower map index postfix with info data for transform.
fn (mut t Transformer) lower_map_index_postfix_with_info(info MapIndexInfo, map_expr flat.NodeId, key_name string, op flat.Op, mut result []flat.NodeId) {
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	infix_op := if op == .dec { flat.Op.minus } else { flat.Op.plus }
	new_value := t.make_infix(infix_op, t.make_ident(current_name), t.make_int_literal(1))
	result << t.make_assign(t.make_ident(current_name), new_value)
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, current_name)
}

// lower_map_index_append_with_info builds lower map index append with info data for transform.
fn (mut t Transformer) lower_map_index_append_with_info(info MapIndexInfo, map_expr flat.NodeId, key_name string, rhs_id flat.NodeId, mut result []flat.NodeId) bool {
	return t.lower_map_index_append_with_info_and_prelude(info, map_expr, key_name, rhs_id,
		[]flat.NodeId{}, mut result)
}

fn (mut t Transformer) lower_map_index_append_with_info_and_prelude(info MapIndexInfo, map_expr flat.NodeId, key_name string, rhs_id flat.NodeId, pre_append_stmts []flat.NodeId, mut result []flat.NodeId) bool {
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	mut working_name := current_name
	if !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(info.value_type)) {
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(info.value_type)) {
			return false
		}
		pending_start := t.pending_stmts.len
		cloned :=
			t.make_compiler_default_clone_value(t.make_ident(current_name), info.value_type, true)
		for stmt in t.pending_stmts[pending_start..].clone() {
			result << stmt
		}
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		working_name = t.new_temp('map_append_value')
		result << t.make_decl_assign_typed(working_name, cloned, info.value_type)
	}
	for stmt in pre_append_stmts {
		result << stmt
	}
	append := t.make_infix(.left_shift, t.make_ident(working_name), rhs_id)
	t.annotate_left_shift(append)
	if lowered := t.try_lower_array_append_stmt(append) {
		for stmt in lowered {
			result << stmt
		}
	} else {
		result << t.make_expr_stmt(append)
	}
	t.append_map_value_drop_before_set(map_expr, info.base_type, key_name, info.value_type, mut
		result)
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, working_name)
	return true
}

// try_lower_map_index_postfix_stmt
// supports helper handling in transform.
fn (mut t Transformer) try_lower_map_index_postfix_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .postfix || node.children_count == 0 || node.op !in [.inc, .dec] {
		return none
	}
	info := t.map_index_info(t.a.child(&node, 0)) or { return none }
	map_expr := t.stable_map_lvalue_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(info.key_id,
		info.key_type), info.key_storage_type)
	t.lower_map_index_postfix_with_info(info, map_expr, key_name, node.op, mut result)
	return result
}

// try_lower_map_index_append_stmt
// supports helper handling in transform.
fn (mut t Transformer) try_lower_map_index_append_stmt(id flat.NodeId) ?[]flat.NodeId {
	return t.try_lower_map_index_append_stmt_with_prelude(id, []flat.NodeId{})
}

fn (mut t Transformer) try_lower_map_index_append_stmt_with_prelude(id flat.NodeId, pre_append_stmts []flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 {
		return none
	}
	info := t.map_index_info(t.a.child(&node, 0)) or { return none }
	if !info.value_type.starts_with('[]') {
		return none
	}
	map_expr := t.stable_map_lvalue_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	mut key_value := t.transform_expr_for_type(info.key_id, info.key_type)
	mut key_is_owned := t.map_key_expr_creates_owned_value(info.key_id, info.key_type)
	if !key_is_owned && !isnil(t.tc)
		&& t.normalize_type_alias(info.key_type).trim_space() != 'string' {
		key_type := t.tc.parse_type(info.key_type)
		if t.tc.ownership_type_requires_destruction(key_type) {
			if _ := t.tc.ownership_default_clone_missing_method(key_type) {
				return []flat.NodeId{}
			}
			key_value = t.make_compiler_default_clone_value(key_value, info.key_type, true)
			key_is_owned = true
		}
	}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, key_value, info.key_storage_type)
	cleanup_key, existing_key_name := t.prepare_owned_map_set_key_cleanup(key_is_owned,
		info.key_type, map_expr, info.base_type, key_name, mut result)
	if !t.lower_map_index_append_with_info_and_prelude(info, map_expr, key_name,
		t.a.child(&node, 1), pre_append_stmts, mut result) {
		return []flat.NodeId{}
	}
	t.append_owned_map_set_key_cleanup(key_name, cleanup_key, existing_key_name, mut result)
	return result
}

fn (t &Transformer) fixed_array_map_value_type_text(value_type string) string {
	if value_type.starts_with('map[') {
		elem, dims := transform_postfix_fixed_array_parts(value_type)
		if dims.len > 0 && (is_decimal_text(dims[0]) || (!isnil(t.tc)
			&& t.tc.const_int_value_in_module(dims[0], t.cur_module, []string{}) != none)) {
			return '[${dims[0]}]${elem}'
		}
		return ''
	}
	if value_type.starts_with('[') || (value_type.contains('[') && value_type.ends_with(']')) {
		return value_type
	}
	return ''
}

// lower_map_init_to_runtime converts lower map init to runtime data for transform.
// resolve_type_text_import_aliases rewrites `alias.Type` segments in a type
// text to their canonical module (`json.Any` -> `json2.Any` under
// `import x.json2 as json`), so texts that survive into cgen stay resolvable
// outside the importing file's context.
fn (t &Transformer) resolve_type_text_import_aliases(typ string) string {
	if isnil(t.tc) || !typ.contains('.') {
		return typ
	}
	if typ.starts_with('[]') {
		return '[]' + t.resolve_type_text_import_aliases(typ[2..])
	}
	if typ.starts_with('&') {
		return '&' + t.resolve_type_text_import_aliases(typ[1..])
	}
	if typ.starts_with('?') || typ.starts_with('!') {
		return typ[..1] + t.resolve_type_text_import_aliases(typ[1..])
	}
	if typ.starts_with('map[') {
		bracket_end := generic_matching_bracket(typ, 3)
		if bracket_end < typ.len {
			key := t.resolve_type_text_import_aliases(typ[4..bracket_end])
			val := t.resolve_type_text_import_aliases(typ[bracket_end + 1..])
			return 'map[${key}]${val}'
		}
		return typ
	}
	if typ.starts_with('[') {
		idx := typ.index_u8(`]`)
		if idx > 0 {
			return typ[..idx + 1] + t.resolve_type_text_import_aliases(typ[idx + 1..])
		}
	}
	return t.tc.resolve_imported_type_text_in_file(typ, t.cur_file)
}

fn (mut t Transformer) lower_map_init_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	mut map_type := if node.value.len > 0 {
		node.value
	} else if node.typ.len > 0 {
		node.typ
	} else {
		t.node_type(id)
	}
	map_type = t.normalize_type_alias(t.resolve_type_text_import_aliases(map_type))
	if t.generic_arg_is_unresolved(map_type) {
		inferred_type :=
			t.normalize_type_alias(t.resolve_type_text_import_aliases(t.infer_map_init_entry_type(node)))
		if inferred_type.starts_with('map[') && !t.generic_arg_is_unresolved(inferred_type) {
			map_type = inferred_type
		}
	}
	if !map_type.starts_with('map[') {
		return id
	}
	map_type = t.refine_map_init_fixed_array_value_type(node, map_type)
	mut init_call := t.make_new_map_call(map_type)
	if node.children_count == 0 {
		return init_call
	}
	key_type, value_type := t.map_type_parts(map_type)
	mut start_i := 0
	mut has_spread := false
	first_id := t.a.child(&node, 0)
	first := t.a.nodes[int(first_id)]
	if first.kind == .prefix && first.value == '...' && first.children_count > 0 {
		if !isnil(t.tc) {
			// The checker reports these errors. Do not lower a rejected spread to a
			// map containing shallow ownership-bearing entries.
			if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(key_type)) {
				return t.make_empty()
			}
			if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(value_type)) {
				return t.make_empty()
			}
		}
		source_id := t.a.child(&first, 0)
		source_is_owned_temporary := !t.expr_can_take_address(source_id)
		source_expr := t.transform_expr(source_id)
		init_call = t.make_compiler_default_map_clone_value(source_expr, map_type,
			source_is_owned_temporary)
		start_i = 2
		has_spread = true
	}
	tmp_name := t.new_temp('map_lit')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, init_call, map_type)
	key_storage_type := t.map_key_storage_type(key_type)
	needs_entry_cleanup := has_spread || (!isnil(t.tc)
		&& (t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type))
		|| t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type))))
	for i := start_i; i + 1 < node.children_count; i += 2 {
		key_id := t.a.child(&node, i)
		key_name := t.new_temp('map_key')
		value_name := t.new_temp('map_val')
		mut key_expr := t.transform_map_entry_expr_for_type(key_id, key_type)
		key_expr = t.clone_borrowed_projection(key_id, key_expr, key_type)
		t.pending_stmts << t.make_decl_assign_typed(key_name, key_expr, key_storage_type)
		value_id := t.a.child(&node, i + 1)
		mut value := t.transform_map_entry_expr_for_type(value_id, value_type)
		value = t.clone_borrowed_projection(value_id, value, value_type)
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, value_type)
		mut cleanup_key := false
		mut existing_key_name := ''
		if needs_entry_cleanup {
			mut drop_stmts := []flat.NodeId{}
			key_is_owned := t.map_literal_key_expr_creates_owned_value(key_id, key_type)
			cleanup_key, existing_key_name = t.prepare_owned_map_set_key_cleanup(key_is_owned,
				key_type, t.make_ident(tmp_name), map_type, key_name, mut drop_stmts)
			t.append_map_value_drop_before_set(t.make_ident(tmp_name), map_type, key_name,
				value_type, mut drop_stmts)
			for stmt in drop_stmts {
				t.pending_stmts << stmt
			}
		}
		call := t.make_call_typed('map__set', [
			t.make_prefix(.amp, t.make_ident(tmp_name)),
			t.make_prefix(.amp, t.make_ident(key_name)),
			t.make_prefix(.amp, t.make_ident(value_name)),
		], 'void')
		t.pending_stmts << t.make_expr_stmt(call)
		if int(value_id) in t.local_closure_field_cleanups {
			t.pending_stmts << t.make_local_closure_cleanup_defer(value_name)
		}
		if needs_entry_cleanup {
			mut cleanup_stmts := []flat.NodeId{}
			t.append_owned_map_set_key_cleanup(key_name, cleanup_key, existing_key_name, mut
				cleanup_stmts)
			for stmt in cleanup_stmts {
				t.pending_stmts << stmt
			}
		}
	}
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) transform_map_entry_expr_for_type(id flat.NodeId, typ string) flat.NodeId {
	prefix := t.pending_stmts
	t.pending_stmts = []flat.NodeId{}
	value := if typ.starts_with('&') && t.is_sum_type_name(typ[1..]) {
		t.transform_expr_for_type(id, typ)
	} else if typ in t.sum_types || t.resolve_sum_name(typ) in t.sum_types {
		t.transform_sum_value_for_type(id, typ)
	} else {
		t.transform_expr_for_type(id, typ)
	}
	entry_pending := t.pending_stmts
	t.pending_stmts = prefix
	for stmt in entry_pending {
		t.pending_stmts << stmt
	}
	return value
}

fn (t &Transformer) refine_map_init_fixed_array_value_type(node flat.Node, map_type string) string {
	key_type, value_type := t.map_type_parts(map_type)
	if key_type.len == 0 || value_type.len == 0 {
		return map_type
	}
	if t.is_fixed_array_type(value_type) {
		return t.refine_fixed_array_map_init_scalar_value_type(node, key_type, value_type, map_type)
	}
	mut i := 0
	for i + 1 < node.children_count {
		key_id := t.a.child(&node, i)
		key := t.a.nodes[int(key_id)]
		if key.kind == .prefix && key.value == '...' && key.children_count > 0 {
			i += 2
			continue
		}
		value_id := t.a.child(&node, i + 1)
		value_typ := t.fixed_array_literal_type_containing_map(value_id) or {
			t.node_type(value_id)
		}
		if t.is_fixed_array_type(value_typ) && t.fixed_array_type_contains_map(value_typ) {
			return 'map[${key_type}]${value_typ}'
		}
		i += 2
	}
	return map_type
}

fn (t &Transformer) refine_fixed_array_map_init_scalar_value_type(node flat.Node, key_type string, value_type string, map_type string) string {
	elem_type := t.normalize_type_alias(fixed_array_elem_type(value_type))
	if elem_type.len == 0 {
		return map_type
	}
	mut saw_value := false
	mut i := 0
	for i + 1 < node.children_count {
		key_id := t.a.child(&node, i)
		key := t.a.nodes[int(key_id)]
		if key.kind == .prefix && key.value == '...' && key.children_count > 0 {
			i += 2
			continue
		}
		value_id := t.a.child(&node, i + 1)
		if fixed_value := t.fixed_array_literal_type_from_syntax(value_id) {
			if t.normalize_type_alias(fixed_value) == value_type {
				return map_type
			}
		}
		mut actual := t.node_type(value_id)
		if actual.len == 0 {
			actual = t.resolve_expr_type(value_id)
		}
		actual = t.normalize_type_alias(actual)
		if actual.len == 0 || actual != elem_type {
			return map_type
		}
		saw_value = true
		i += 2
	}
	if saw_value {
		return 'map[${key_type}]${elem_type}'
	}
	return map_type
}

fn (t &Transformer) fixed_array_literal_type_containing_map(id flat.NodeId) ?string {
	value_type := t.fixed_array_literal_type_from_syntax(id) or { return none }
	if t.is_fixed_array_type(value_type) && t.fixed_array_type_contains_map(value_type) {
		return value_type
	}
	return none
}

fn (t &Transformer) fixed_array_literal_type_from_syntax(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] {
		if node.children_count == 0 {
			return none
		}
		return t.fixed_array_literal_type_from_syntax(t.a.child(&node, 0))
	}
	if node.kind != .postfix || node.op != .not || node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind != .array_literal {
		return none
	}
	elem_type := if child.children_count > 0 {
		t.fixed_array_literal_elem_type_from_syntax(t.a.child(&child, 0)) or { return none }
	} else {
		'int'
	}
	return '[${child.children_count}]${elem_type}'
}

fn (t &Transformer) fixed_array_literal_elem_type_from_syntax(id flat.NodeId) ?string {
	if fixed_type := t.fixed_array_literal_type_from_syntax(id) {
		return fixed_type
	}
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] {
		if node.children_count == 0 {
			return none
		}
		return t.fixed_array_literal_elem_type_from_syntax(t.a.child(&node, 0))
	}
	if node.kind == .map_init {
		mut map_type := if node.value.starts_with('map[') {
			node.value
		} else if node.typ.starts_with('map[') {
			node.typ
		} else {
			t.node_type(id)
		}
		map_type = t.normalize_type_alias(t.resolve_type_text_import_aliases(map_type))
		if map_type.starts_with('map[') {
			return t.refine_map_init_fixed_array_value_type(node, map_type)
		}
	}
	node_type := t.node_type(id)
	if node_type.len > 0 {
		return node_type
	}
	return none
}
