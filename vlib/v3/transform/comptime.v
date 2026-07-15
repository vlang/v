module transform

import os
import strconv
import strings
import v3.flat

const comptime_unsupported_late_generic_call = '__v3_comptime_unsupported_late_generic_call'

// vmod_root supports vmod root handling for Transformer.
fn (t &Transformer) vmod_root() string {
	mut dir := if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
		}
		dir = parent
	}
	return dir
}

// Compile-time reflection: `$for field in T.fields { ... }`.
//
// The parser emits a `comptime_for` node (value `<var>|<kind>`, typ = base type) and defers the
// loop body verbatim. Once the base type is concrete (after generic monomorphization) the loop is
// unrolled here: the body is cloned once per field with the loop variable's compile-time members
// substituted, then transformed normally. Members mirror V's `FieldData`.

// FieldMeta is the compile-time metadata exposed by the `$for field in T.fields` loop variable.
struct FieldMeta {
	name               string
	typ                string
	unaliased_typ      string
	comptime_typ       string
	comptime_unaliased string
	typ_id             int
	unaliased_id       int
	is_option          bool
	is_embed           bool
	is_array           bool
	is_map             bool
	is_chan            bool
	is_struct          bool
	is_enum            bool
	is_alias           bool
	is_shared          bool
	is_atomic          bool
	is_mut             bool
	is_pub             bool
	attrs              []string
	indirections       int
}

struct EnumValueMeta {
	name      string
	value     i64
	attrs     []string
	enum_name string
}

struct VariantMeta {
	typ    string
	typ_id int
}

struct MethodMeta {
	name        string
	receiver    string
	module_name string
	location    string
	return_type string
	is_pub      bool
	params      []ParamMeta
	attrs       []string
	attributes  []AttributeMeta
}

struct ParamMeta {
	name        string
	typ         string
	module_name string
}

struct AttributeMeta {
	name    string
	arg     string
	has_arg bool
	kind    int
}

struct RawAttributeData {
	attrs []string
	kinds []int
}

struct EnumDeclFieldValue {
	name    string
	expr_id flat.NodeId
	attrs   []string
}

fn comptime_for_parts(value string) (string, string) {
	if idx := value.index('|') {
		return value[..idx], value[idx + 1..]
	}
	return value, 'fields'
}

fn comptime_for_declares_var(node flat.Node, var_name string) bool {
	if node.kind != .comptime_for {
		return false
	}
	loop_var, _ := comptime_for_parts(node.value)
	return loop_var == var_name
}

// comptime_for_base_type resolves the loop source type to a concrete name. Generic `T` was already
// substituted to the concrete type in `node.typ` during monomorphization.
fn (t &Transformer) comptime_for_base_type(raw string) string {
	source := if value_type := t.comptime_for_value_source_type(raw) {
		value_type
	} else {
		raw
	}
	return t.comptime_normalize_type_alias_chain(t.comptime_resolve_selective_import_type(source))
}

fn (t &Transformer) comptime_for_value_source_type(raw string) ?string {
	clean := raw.trim_space()
	if clean.len == 0 {
		return none
	}
	parts := clean.split('.')
	if parts.len == 0 {
		return none
	}
	mut typ := t.comptime_for_var_source_type(parts[0]) or { return none }
	for field in parts[1..] {
		typ = t.comptime_for_field_source_type(typ, field) or { return none }
	}
	return typ
}

fn (t &Transformer) comptime_for_var_source_type(name string) ?string {
	raw_typ := t.raw_var_type(name).trim_space()
	if raw_typ.len > 0 {
		return t.comptime_for_value_type_base(raw_typ)
	}
	typ := t.var_type(name).trim_space()
	if typ.len > 0 {
		return t.comptime_for_value_type_base(typ)
	}
	return none
}

fn (t &Transformer) comptime_for_field_source_type(owner_type string, field_name string) ?string {
	base := t.comptime_normalize_type_alias_chain(t.comptime_for_value_type_base(owner_type))
	info := t.lookup_struct_info(base) or {
		t.generic_struct_info_for_stringify(base) or { return none }
	}
	for field in info.fields {
		if field.name == field_name {
			ftyp := if field.typ.len > 0 { field.typ } else { field.raw_typ }
			return t.comptime_for_value_type_base(ftyp)
		}
	}
	return none
}

fn (t &Transformer) comptime_for_value_type_base(raw string) string {
	mut typ := raw.trim_space()
	if typ.starts_with('mut ') {
		typ = typ[4..].trim_space()
	}
	for typ.starts_with('&') {
		typ = typ[1..].trim_space()
	}
	return typ
}

fn (t &Transformer) comptime_resolve_selective_import_type(raw string) string {
	clean := raw.trim_space()
	if clean.len == 0 || isnil(t.tc) || t.cur_file.len == 0 {
		return clean
	}
	if clean.contains('.') {
		if imported := t.resolve_imported_type_name(clean) {
			return imported
		}
		return clean
	}
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, clean)] or {
		[]string{}
	} {
		if candidate in t.tc.type_aliases || candidate in t.tc.structs
			|| candidate in t.tc.interface_names || candidate in t.tc.flag_enums
			|| candidate in t.tc.enum_names || candidate in t.tc.sum_types {
			return candidate
		}
	}
	return clean
}

fn (t &Transformer) comptime_resolve_selective_import_reflection_source(raw string) string {
	clean := raw.trim_space()
	if clean.len == 0 || isnil(t.tc) || t.cur_file.len == 0 {
		return clean
	}
	if local := t.comptime_local_reflection_source(clean) {
		return local
	}
	if clean.contains('.') {
		return t.resolve_imported_type_name(clean) or { clean }
	}
	resolved_type := t.comptime_resolve_selective_import_type(clean)
	if resolved_type != clean {
		return resolved_type
	}
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, clean)] or {
		[]string{}
	} {
		if candidate in t.tc.fn_ret_types || candidate in t.tc.fn_param_types {
			return candidate
		}
	}
	return clean
}

fn (t &Transformer) comptime_local_reflection_source(name string) ?string {
	if name.len == 0 || t.cur_module.len == 0 || t.cur_module in ['main', 'builtin'] {
		return none
	}
	qualified := '${t.cur_module}.${name}'
	if qualified in t.tc.fn_ret_types || qualified in t.tc.fn_param_types
		|| qualified in t.tc.type_aliases || qualified in t.tc.structs
		|| qualified in t.tc.interface_names || qualified in t.tc.flag_enums
		|| qualified in t.tc.enum_names || qualified in t.tc.sum_types {
		return qualified
	}
	return none
}

fn (mut t Transformer) cache_comptime_param_reflection_metadata() {
	if !t.has_used_fn_filter() || isnil(t.tc) || t.tc.top_level_idx.len == 0 {
		return
	}
	old_file := t.cur_file
	old_module := t.cur_module
	old_fn_name := t.cur_fn_name
	mut cur_file := ''
	mut cur_module := ''
	mut previous_top_level := -1
	for top_level_idx in t.tc.top_level_idx {
		node := t.a.nodes[top_level_idx]
		if node.kind == .file {
			cur_file = node.value
			cur_module = ''
		} else if node.kind == .module_decl {
			cur_module = node.value
		} else if node.kind == .fn_decl {
			t.cur_file = cur_file
			t.cur_module = cur_module
			t.cur_fn_name = node.value
			if t.should_transform_fn(node) {
				for idx in previous_top_level + 1 .. top_level_idx {
					candidate := t.a.nodes[idx]
					if candidate.kind != .comptime_for {
						continue
					}
					_, kind := comptime_for_parts(candidate.value)
					if kind != 'params' {
						continue
					}
					source := t.comptime_reflection_source(candidate.typ, flat.NodeId(idx))
					resolved := t.comptime_resolve_selective_import_reflection_source(source)
					if resolved != source && resolved !in t.comptime_reflected_params {
						params := t.comptime_param_metas(resolved)
						t.comptime_reflected_params[resolved] = params
					}
				}
			}
		}
		previous_top_level = top_level_idx
	}
	t.cur_file = old_file
	t.cur_module = old_module
	t.cur_fn_name = old_fn_name
}

fn (t &Transformer) comptime_normalize_type_alias_chain(raw string) string {
	mut typ := raw.trim_space()
	mut seen := map[string]bool{}
	for typ.len > 0 && typ !in seen {
		seen[typ] = true
		next := t.normalize_type_alias(typ).trim_space()
		if next == typ {
			break
		}
		typ = next
	}
	return typ
}

// expand_comptime_for unrolls the supported compile-time reflection loops into concrete
// statements.
fn (mut t Transformer) expand_comptime_for(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return []flat.NodeId{}
	}
	var_name, kind := comptime_for_parts(node.value)
	if kind !in ['fields', 'values', 'variants', 'methods', 'params', 'attributes'] {
		return []flat.NodeId{}
	}
	// Resolve the raw source only after checking for an open generic. Earlier
	// transform passes can still have a concrete type cached for the same
	// one-letter name from another function, but this template's `T` must not be
	// expanded until its specialization is cloned.
	if is_generic_fn_placeholder_name(node.typ) && t.generic_arg_is_unresolved(node.typ) {
		return arr1(id)
	}
	base_type := if kind == 'methods' {
		source := t.comptime_for_value_source_type(node.typ) or { node.typ }
		t.comptime_resolve_selective_import_type(source)
	} else {
		t.comptime_for_base_type(node.typ)
	}
	// An open generic template must survive the pre-monomorph transform pass.
	// Its metadata and `$zero(field.typ)` children are needed when the concrete
	// specialization is cloned later; erasing them here leaves empty child ids.
	if is_generic_fn_placeholder_name(base_type) || t.generic_arg_is_unresolved(base_type) {
		return arr1(id)
	}
	t.ignore_comptime_for_subtree(id)
	body_id := t.a.child(&node, 0)
	body := t.a.nodes[int(body_id)]
	body_stmts := t.a.children_of(&body).clone()
	if kind == 'values' {
		return t.expand_comptime_for_values(var_name, base_type, body_stmts)
	}
	if kind == 'variants' {
		return t.expand_comptime_for_variants(var_name, base_type, body_stmts)
	}
	if kind == 'methods' {
		return t.expand_comptime_for_methods(var_name, base_type, body_stmts)
	}
	if kind == 'params' {
		return t.expand_comptime_for_params(var_name, node.typ, body_stmts, id)
	}
	if kind == 'attributes' {
		return t.expand_comptime_for_attributes(var_name, node.typ, body_stmts, id)
	}
	// Comptime `match field.typ { int {} ... }` (type match) is not modelled yet; skip such
	// loops rather than mis-lower them. `typeof(receiver.$(field.name))` is checked after
	// substitution, when the dynamic selector has become a concrete field selector.
	if t.comptime_body_has_unsupported(body_stmts, var_name, false) {
		return []flat.NodeId{}
	}
	mut out := []flat.NodeId{}
	for fm in t.comptime_field_metas(base_type) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_field_subst(sid, var_name, fm) {
				cloned << cid
			}
		}
		if t.comptime_body_has_unsupported(cloned, var_name, true) {
			return []flat.NodeId{}
		}
		// One block per iteration so per-field temps get their own scope.
		block := t.make_block(cloned)
		for s in t.transform_stmt(block) {
			out << s
		}
	}
	return out
}

fn (mut t Transformer) expand_comptime_for_attributes(var_name string, source string, body_stmts []flat.NodeId, loop_id flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	for attr in t.comptime_attribute_metas(source, loop_id) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			cloned << t.clone_attribute_subst(sid, var_name, attr)
		}
		block := t.make_block(cloned)
		for stmt in t.transform_stmt(block) {
			out << stmt
		}
	}
	return out
}

fn (t &Transformer) comptime_attribute_metas(source string, loop_id flat.NodeId) []AttributeMeta {
	raw_name := t.comptime_reflection_source(source, loop_id)
	name := t.comptime_resolve_selective_import_reflection_source(raw_name)
	mut module_name := ''
	for idx, node in t.a.nodes {
		if node.kind == .file {
			module_name = ''
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind !in [.fn_decl, .struct_decl, .enum_decl, .interface_decl, .type_decl] {
			continue
		}
		qualified := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
			'${module_name}.${node.value}'
		} else {
			node.value
		}
		if qualified == name || (module_name == t.cur_module && node.value == name) {
			return t.comptime_node_attribute_metas(idx)
		}
	}
	return []AttributeMeta{}
}

fn (t &Transformer) comptime_reflection_source(source string, loop_id flat.NodeId) string {
	clean := source.trim_space()
	rhs_id := t.comptime_reflection_local_rhs(loop_id, clean) or { return clean }
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind == .ident {
		return rhs.value
	}
	if rhs.kind == .selector && rhs.children_count > 0 {
		base := t.a.child_node(&rhs, 0)
		if base.kind == .ident {
			return '${base.value}.${rhs.value}'
		}
		mut found := ''
		for candidate in t.a.nodes {
			if candidate.kind == .fn_decl && candidate.value.contains('.')
				&& candidate.value.all_after_last('.') == rhs.value {
				if found.len > 0 {
					return clean
				}
				found = candidate.value
			}
		}
		if found.len > 0 {
			return found
		}
	}
	return clean
}

fn (t &Transformer) comptime_reflection_local_rhs(loop_id flat.NodeId, name string) ?flat.NodeId {
	if int(loop_id) < 0 || int(loop_id) >= t.a.nodes.len {
		return none
	}
	for idx, node in t.a.nodes {
		if node.kind != .fn_decl || (t.cur_fn_name.len > 0 && node.value != t.cur_fn_name) {
			continue
		}
		mut path := []flat.NodeId{}
		if !t.comptime_reflection_node_path(flat.NodeId(idx), loop_id, mut path) {
			continue
		}
		mut rhs_id := flat.empty_node
		for depth in 0 .. path.len - 1 {
			scope := t.a.nodes[int(path[depth])]
			if scope.kind !in [.fn_decl, .block, .for_stmt, .for_in_stmt, .match_branch,
				.select_branch] {
				continue
			}
			child_on_path := path[depth + 1]
			for i in 0 .. scope.children_count {
				child_id := t.a.child(&scope, i)
				if child_id == child_on_path {
					break
				}
				if candidate := t.comptime_reflection_decl_rhs(child_id, name) {
					rhs_id = candidate
				}
			}
		}
		if rhs_id != flat.empty_node {
			return rhs_id
		}
		return none
	}
	return none
}

fn (t &Transformer) comptime_reflection_node_path(id flat.NodeId, target flat.NodeId, mut path []flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	path << id
	if id == target {
		return true
	}
	node := t.a.nodes[int(id)]
	for i in 0 .. node.children_count {
		if t.comptime_reflection_node_path(t.a.child(&node, i), target, mut path) {
			return true
		}
	}
	path.pop()
	return false
}

fn (t &Transformer) comptime_reflection_decl_rhs(id flat.NodeId, name string) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .decl_assign || node.children_count < 2 {
		return none
	}
	for i := 0; i + 1 < int(node.children_count); i += 2 {
		lhs := t.a.child_node(&node, i)
		if lhs.kind == .ident && lhs.value == name {
			return t.a.child(&node, i + 1)
		}
	}
	return none
}

fn (t &Transformer) comptime_node_raw_attributes(node_id int) []string {
	return t.comptime_node_raw_attribute_data(node_id).attrs
}

fn (t &Transformer) comptime_node_raw_attribute_data(node_id int) RawAttributeData {
	marker := '@attributes:${node_id}'
	for node in t.a.nodes {
		if node.kind == .directive && node.value == marker {
			return RawAttributeData{
				attrs: node.generic_params.clone()
				kinds: if node.typ.len > 0 {
					node.typ.split(',').map(it.int())
				} else {
					[]int{}
				}
			}
		}
	}
	return RawAttributeData{}
}

fn comptime_attribute_metas_from_raw(raw_attrs []string, raw_kinds []int) []AttributeMeta {
	mut attrs := []AttributeMeta{}
	for attr_idx, raw in raw_attrs {
		clean := raw.trim_space()
		if clean.len == 0 {
			continue
		}
		recorded_kind := if attr_idx < raw_kinds.len { raw_kinds[attr_idx] } else { -1 }
		colon := clean.index_u8(`:`)
		has_arg := colon >= 0 && !(recorded_kind == 1
			&& !comptime_attr_is_string_literal(clean[colon + 1..].trim_space()))
		if has_arg {
			idx := colon
			name := clean[..idx].trim_space()
			raw_arg := clean[idx + 1..].trim_space()
			arg := comptime_attr_unquote(raw_arg)
			kind := if recorded_kind >= 0 {
				recorded_kind
			} else if raw_arg.len >= 2 && raw_arg[0] in [`'`, `\"`] {
				1
			} else if raw_arg == 'true' || raw_arg == 'false' {
				3
			} else if comptime_is_int(raw_arg) {
				2
			} else {
				0
			}
			attrs << AttributeMeta{
				name:    name
				arg:     arg
				has_arg: true
				kind:    kind
			}
		} else {
			attrs << AttributeMeta{
				name: clean
				kind: if recorded_kind >= 0 { recorded_kind } else { 0 }
			}
		}
	}
	return attrs
}

fn comptime_attr_is_string_literal(raw string) bool {
	return (raw.len >= 2 && raw[0] in [`'`, `"`] && raw[raw.len - 1] == raw[0])
		|| (raw.len >= 3 && raw[0] == `r` && raw[1] in [`'`, `"`] && raw[raw.len - 1] == raw[1])
}

fn (t &Transformer) comptime_node_attribute_metas(node_id int) []AttributeMeta {
	data := t.comptime_node_raw_attribute_data(node_id)
	return comptime_attribute_metas_from_raw(data.attrs, data.kinds)
}

fn comptime_attr_unquote(s string) string {
	if s.len >= 3 && s[0] == `r` && s[1] in [`'`, `\"`] && s[s.len - 1] == s[1] {
		return s[2..s.len - 1]
	}
	if s.len >= 2 && s[0] in [`'`, `\"`] && s[s.len - 1] == s[0] {
		return comptime_cond_unescape(s[1..s.len - 1])
	}
	return s
}

fn (mut t Transformer) clone_attribute_subst(id flat.NodeId, var_name string, attr AttributeMeta) flat.NodeId {
	return t.clone_attribute_subst_scoped(id, var_name, attr, []string{})
}

fn (mut t Transformer) clone_attribute_subst_scoped(id flat.NodeId, var_name string, attr AttributeMeta, inner_vars []string) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .comptime_if {
		cond := t.subst_attribute_cond(node.value, var_name, attr)
		if comptime_cond_references_ident(node.value, var_name)
			&& !comptime_cond_has_loop_member_ref(cond, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return t.make_block([]flat.NodeId{})
				}
				return t.clone_attribute_subst_scoped(t.a.child(&node, branch_idx), var_name, attr,
					inner_vars)
			}
		}
		return t.clone_attribute_subst_children_with_value(node, var_name, attr, inner_vars, cond)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_attribute_literal(attr)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return match node.value {
				'name' { t.make_string_literal(attr.name) }
				'arg' { t.make_string_literal(attr.arg) }
				'has_arg' { t.make_bool_literal(attr.has_arg) }
				'kind' { t.make_int_literal_typed(attr.kind.str(), 'AttributeKind') }
				else { t.clone_attribute_subst_children(node, var_name, attr, inner_vars) }
			}
		}
	}
	return t.clone_attribute_subst_children(node, var_name, attr, inner_vars)
}

fn (t &Transformer) subst_attribute_cond(cond string, var_name string, attr AttributeMeta) string {
	mut result := comptime_cond_replace_unquoted(cond, '${var_name}.has_arg', attr.has_arg.str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.name',
		comptime_cond_string_literal(attr.name))
	result = comptime_cond_replace_unquoted(result, '${var_name}.arg',
		comptime_cond_string_literal(attr.arg))
	kind_value := comptime_attribute_kind_cond_value(attr.kind)
	result = comptime_cond_replace_unquoted(result, '${var_name}.kind ==.', '${kind_value} == .')
	result = comptime_cond_replace_unquoted(result, '${var_name}.kind !=.', '${kind_value} != .')
	result = comptime_cond_replace_unquoted(result, '${var_name}.kind', kind_value)
	return result
}

fn comptime_cond_string_literal(value string) string {
	mut out := strings.new_builder(value.len + 2)
	out.write_u8(`'`)
	for i := 0; i < value.len; i++ {
		if value[i] == `\\` || value[i] == `'` {
			out.write_u8(`\\`)
		}
		out.write_u8(value[i])
	}
	out.write_u8(`'`)
	return out.str()
}

fn comptime_attribute_kind_cond_value(kind int) string {
	return match kind {
		1 { '.string' }
		2 { '.number' }
		3 { '.bool' }
		4 { '.comptime_define' }
		else { '.plain' }
	}
}

fn (mut t Transformer) clone_attribute_subst_children(node flat.Node, var_name string, attr AttributeMeta, inner_vars []string) flat.NodeId {
	return t.clone_attribute_subst_children_with_value(node, var_name, attr, inner_vars, node.value)
}

fn (mut t Transformer) clone_attribute_subst_children_with_value(node flat.Node, var_name string, attr AttributeMeta, inner_vars []string, value string) flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.clone_attribute_subst_scoped(t.a.child(&node, i), var_name, attr,
			child_inner_vars)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          value
		typ:            node.typ
		generic_params: node.generic_params.clone()
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn (mut t Transformer) make_attribute_literal(attr AttributeMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(attr.name), 'string'),
		t.make_named_field_init('has_arg', t.make_bool_literal(attr.has_arg), 'bool'),
		t.make_named_field_init('arg', t.make_string_literal(attr.arg), 'string'),
		t.make_named_field_init('kind', t.make_int_literal_typed(attr.kind.str(), 'AttributeKind'),
			'AttributeKind'),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'VAttribute'
		typ:            'VAttribute'
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (mut t Transformer) expand_comptime_for_params(var_name string, fn_name string, body_stmts []flat.NodeId, loop_id flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	source := t.comptime_reflection_source(fn_name, loop_id)
	for param in t.comptime_param_metas(source) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_param_subst(sid, var_name, param) {
				cloned << cid
			}
		}
		block := t.make_block(cloned)
		for stmt in t.transform_stmt(block) {
			out << stmt
		}
	}
	return out
}

fn (t &Transformer) comptime_param_metas(fn_name string) []ParamMeta {
	raw_wanted := fn_name.trim_space()
	wanted := t.comptime_resolve_selective_import_reflection_source(raw_wanted)
	if params := t.comptime_reflected_params[wanted] {
		return params.clone()
	}
	mut module_name := ''
	mut file_name := ''
	mut signature_fallback := []ParamMeta{}
	for node in t.a.nodes {
		if node.kind == .file {
			module_name = ''
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		qualified := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
			'${module_name}.${node.value}'
		} else {
			node.value
		}
		is_scoped_match := qualified == wanted
			|| (module_name == t.cur_module && node.value == wanted)
		mut params := []ParamMeta{}
		for i in 0 .. node.children_count {
			param := t.a.child_node(&node, i)
			if param.kind != .param {
				continue
			}
			if i == 0 && param.op == .dot {
				continue
			}
			params << ParamMeta{
				name:        param.value
				typ:         param.typ
				module_name: module_name
			}
		}
		if is_scoped_match {
			return params
		}
		if wanted_params, wanted_ret := fn_type_text_parts(wanted) {
			if !comptime_params_match_signature(params, node.typ, wanted_params, wanted_ret) {
				continue
			}
			if file_name != t.cur_file {
				if signature_fallback.len == 0 {
					signature_fallback = params.clone()
				}
				continue
			}
		} else {
			continue
		}
		return params
	}
	return signature_fallback
}

fn comptime_params_match_signature(params []ParamMeta, return_type string, wanted_params []string, wanted_ret string) bool {
	if params.len != wanted_params.len {
		return false
	}
	for i, param in params {
		if param.typ.trim_space() != generic_fn_type_param_payload(wanted_params[i]).trim_space() {
			return false
		}
	}
	actual_ret := if return_type.len > 0 { return_type } else { 'void' }
	expected_ret := if wanted_ret.len > 0 { wanted_ret } else { 'void' }
	return actual_ret == expected_ret
}

fn (mut t Transformer) clone_param_subst(id flat.NodeId, var_name string, param ParamMeta) ?flat.NodeId {
	return t.clone_param_subst_scoped(id, var_name, param, []string{})
}

fn (mut t Transformer) clone_param_subst_scoped(id flat.NodeId, var_name string, param ParamMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_param_data_literal(param)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return match node.value {
				'name' {
					t.make_string_literal(param.name)
				}
				'typ' {
					t.make_int_literal(t.comptime_field_type_id(param.typ, param.module_name))
				}
				else {
					t.clone_param_subst_children(node, var_name, param, inner_vars)
				}
			}
		}
		if base.kind == .typeof_expr && t.typeof_arg_is_param_typ(t.a.child(&node, 0), var_name) {
			return match node.value {
				'name' {
					t.make_string_literal(param.typ)
				}
				'idx' {
					t.make_int_literal(t.comptime_field_type_id(param.typ, param.module_name))
				}
				else {
					t.clone_param_subst_children(node, var_name, param, inner_vars)
				}
			}
		}
	}
	if node.kind == .typeof_expr && t.typeof_arg_is_param_typ(id, var_name) {
		return t.make_string_literal(param.typ)
	}
	if node.kind == .comptime_if {
		param_typ := if param.typ == '&void' {
			'voidptr'
		} else {
			t.comptime_field_type_id_key(param.typ, param.module_name)
		}
		mut cond := comptime_cond_replace_unquoted(node.value, '${var_name}.typ', param_typ)
		cond = comptime_cond_replace_unquoted(cond, '${var_name}.name', "'${param.name}'")
		cond = comptime_cond_replace_unquoted(cond, ' is &void', ' is voidptr')
		cond = comptime_cond_replace_unquoted(cond, ' !is &void', ' !is voidptr')
		if comptime_cond_references_ident(node.value, var_name)
			&& !comptime_cond_has_loop_member_ref(cond, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_param_subst_scoped(t.a.child(&node, branch_idx), var_name, param,
					inner_vars)
			}
		}
		return t.clone_param_subst_children_with_value(node, var_name, param, inner_vars, cond)
	}
	return t.clone_param_subst_children(node, var_name, param, inner_vars)
}

fn (t &Transformer) typeof_arg_is_param_typ(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .typeof_expr || node.children_count == 0 {
		return false
	}
	return t.param_typ_expr_references(t.a.child(&node, 0), var_name)
}

fn (t &Transformer) param_typ_expr_references(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector && node.value == 'typ' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.param_typ_expr_references(t.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) clone_param_subst_children(node flat.Node, var_name string, param ParamMeta, inner_vars []string) flat.NodeId {
	return t.clone_param_subst_children_with_value(node, var_name, param, inner_vars, node.value)
}

fn (mut t Transformer) clone_param_subst_children_with_value(node flat.Node, var_name string, param ParamMeta, inner_vars []string, value string) flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		if child := t.clone_param_subst_scoped(t.a.child(&node, i), var_name, param,
			child_inner_vars)
		{
			children << child
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          value
		typ:            node.typ
		generic_params: node.generic_params.clone()
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn (mut t Transformer) make_param_data_literal(param ParamMeta) flat.NodeId {
	return t.make_param_data_literal_in_module(param, param.module_name)
}

fn (mut t Transformer) make_param_data_literal_in_module(param ParamMeta, module_name string) flat.NodeId {
	name_field := t.make_named_field_init('name', t.make_string_literal(param.name), 'string')
	typ_field := t.make_named_field_init('typ', t.make_int_literal(t.comptime_field_type_id(param.typ,
		module_name)), 'int')
	start := t.a.children.len
	t.a.children << name_field
	t.a.children << typ_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'FunctionParam'
		typ:            'FunctionParam'
		children_start: start
		children_count: 2
	})
}

// make_method_data_literal builds a runtime `FunctionData` value for a bare methods loop variable.
fn (mut t Transformer) make_method_data_literal(method MethodMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(method.name), 'string'),
		t.make_named_field_init('location', t.make_string_literal(method.location), 'string'),
		t.make_named_field_init('attrs', t.make_string_array_literal(method.attrs), '[]string'),
		t.make_named_field_init('attributes', t.make_attribute_array_literal(method.attributes),
			'[]VAttribute'),
		t.make_named_field_init('args', t.make_param_array_literal(method.params,
			method.module_name), '[]FunctionParam'),
		t.make_named_field_init('return_type', t.make_int_literal(t.comptime_field_type_id(method.return_type,
			method.module_name)), 'int'),
		t.make_named_field_init('typ', t.make_int_literal(t.comptime_method_type_id(method)), 'int'),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'FunctionData'
		typ:            'FunctionData'
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (mut t Transformer) expand_comptime_for_methods(var_name string, base_type string, body_stmts []flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	for method in t.comptime_method_metas(base_type) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_method_subst(sid, var_name, method) {
				cloned << cid
			}
		}
		block := t.make_block(cloned)
		for stmt in t.transform_stmt(block) {
			out << stmt
		}
	}
	return out
}

fn comptime_method_receiver_base(raw string) string {
	mut name := comptime_method_receiver_type(raw)
	bracket := name.index_u8(`[`)
	if bracket >= 0 {
		name = name[..bracket]
	}
	return name
}

fn comptime_method_receiver_type(raw string) string {
	mut name := raw.trim_space()
	if name.starts_with('mut ') {
		name = name[4..].trim_space()
	}
	for name.starts_with('&') {
		name = name[1..].trim_space()
	}
	return name
}

fn comptime_method_receiver_name(raw string, module_name string) string {
	name := comptime_method_receiver_base(raw)
	if name.len == 0 || name.contains('.') || module_name.len == 0
		|| module_name in ['main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn comptime_source_line_offsets(path string) []int {
	source := os.read_file(path) or { return []int{} }
	mut offsets := []int{cap: source.len / 40 + 1}
	offsets << 0
	for i, ch in source {
		if ch == `\n` {
			offsets << i + 1
		}
	}
	return offsets
}

fn comptime_source_location(path string, encoded_offset int, line_offsets []int) string {
	if path.len == 0 || encoded_offset <= 0 || line_offsets.len == 0 {
		return ''
	}
	offset := encoded_offset - 1
	mut lo := 0
	mut hi := line_offsets.len
	for lo < hi {
		mid := (lo + hi) / 2
		if line_offsets[mid] <= offset {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	line_index := if lo > 0 { lo - 1 } else { 0 }
	return '${path}:${line_index + 1}:${offset - line_offsets[line_index]}'
}

fn comptime_method_receiver_matches(receiver string, requested string, normalized string, receiver_module string, requested_module string) bool {
	receiver_name := comptime_method_receiver_name(receiver, receiver_module)
	for candidate in [requested, normalized] {
		if receiver_name == comptime_method_receiver_name(candidate, requested_module) {
			return true
		}
	}
	return false
}

fn (t &Transformer) comptime_method_metas(base_type string) []MethodMeta {
	normalized := t.comptime_normalize_type_alias_chain(base_type)
	mut module_name := ''
	mut file_name := ''
	mut methods := []MethodMeta{}
	mut seen := map[string]bool{}
	mut loaded_source_files := map[string]bool{}
	mut line_offsets_by_file := map[string][]int{}
	for node_id, node in t.a.nodes {
		if node.kind == .file {
			module_name = ''
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .fn_decl || !node.value.contains('.') || node.children_count == 0 {
			continue
		}
		first := t.a.child_node(&node, 0)
		if first.kind != .param || first.op != .dot || first.value.len == 0
			|| !comptime_method_receiver_matches(first.typ, base_type, normalized, module_name, t.cur_module) {
			continue
		}
		name := node.value.all_after_last('.')
		if name.len == 0 || name in seen {
			continue
		}
		seen[name] = true
		generic_args, generic_params := t.comptime_method_receiver_generic_args(first.typ,
			base_type, normalized)
		mut params := []ParamMeta{}
		for i in 1 .. node.children_count {
			param := t.a.child_node(&node, i)
			if param.kind == .param {
				params << ParamMeta{
					name:        param.value
					typ:         substitute_generic_type_text_with_params(param.typ, generic_args,
						generic_params)
					module_name: module_name
				}
			}
		}
		return_type := substitute_generic_type_text_with_params(if node.typ.len > 0 {
			node.typ
		} else {
			'void'
		}, generic_args, generic_params)
		raw_attr_data := t.comptime_node_raw_attribute_data(node_id)
		if file_name !in loaded_source_files {
			loaded_source_files[file_name] = true
			line_offsets_by_file[file_name] = comptime_source_line_offsets(file_name)
		}
		methods << MethodMeta{
			name:        name
			receiver:    first.typ
			module_name: module_name
			location:    comptime_source_location(file_name, node.pos.offset,
				line_offsets_by_file[file_name])
			return_type: return_type
			is_pub:      node.op == .arrow
			params:      params
			attrs:       raw_attr_data.attrs
			attributes:  comptime_attribute_metas_from_raw(raw_attr_data.attrs, raw_attr_data.kinds)
		}
	}
	return methods
}

fn (t &Transformer) comptime_method_receiver_generic_args(receiver string, requested string, normalized string) ([]string, []string) {
	_, params, is_generic := generic_app_parts(comptime_method_receiver_type(receiver))
	if !is_generic || params.len == 0 {
		return []string{}, []string{}
	}
	mut candidates := [requested, normalized]
	for candidate in [requested, normalized] {
		if source := t.generic_specialized_source_type_name(comptime_method_receiver_type(candidate)) {
			candidates << source
		}
	}
	for candidate in candidates {
		_, args, ok := generic_app_parts(comptime_method_receiver_type(candidate))
		if ok && args.len == params.len && !t.generic_args_have_placeholders(args) {
			return args, params
		}
	}
	return []string{}, []string{}
}

fn (mut t Transformer) clone_method_subst(id flat.NodeId, var_name string, method MethodMeta) ?flat.NodeId {
	return t.clone_method_subst_scoped(id, var_name, method, []string{})
}

fn (mut t Transformer) clone_method_subst_scoped(id flat.NodeId, var_name string, method MethodMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_method_data_literal(method)
	}
	if idx := t.comptime_method_param_index(id, var_name) {
		if idx >= 0 && idx < method.params.len {
			return t.make_param_data_literal_in_module(method.params[idx], method.module_name)
		}
	}
	if node.kind == .selector && node.value == '$' && node.children_count >= 2
		&& t.comptime_method_name_expr_matches(t.a.child(&node, 1), var_name) {
		receiver := t.clone_method_subst_scoped(t.a.child(&node, 0), var_name, method, inner_vars) or {
			return none
		}
		receiver_name := comptime_method_receiver_name(method.receiver, method.module_name)
		t.mark_fn_used('${receiver_name}.${method.name}')
		return t.make_selector(receiver, method.name, method.return_type)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return match node.value {
				'name' {
					t.make_string_literal(method.name)
				}
				'location' {
					t.make_string_literal(method.location)
				}
				'is_pub' {
					t.make_bool_literal(method.is_pub)
				}
				'return_type' {
					t.make_int_literal(t.comptime_field_type_id(method.return_type,
						method.module_name))
				}
				'typ' {
					t.make_int_literal(t.comptime_method_type_id(method))
				}
				'args', 'params' {
					t.make_param_array_literal(method.params, method.module_name)
				}
				'attrs' {
					t.make_string_array_literal(method.attrs)
				}
				'attributes' {
					t.make_attribute_array_literal(method.attributes)
				}
				else {
					t.clone_method_subst_children(node, var_name, method, inner_vars)
				}
			}
		}
	}
	if node.kind == .comptime_if {
		cond := t.subst_method_cond(node.value, var_name, method)
		if comptime_cond_references_ident(node.value, var_name)
			&& !comptime_cond_has_loop_member_ref(cond, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_method_subst_scoped(t.a.child(&node, branch_idx), var_name, method,
					inner_vars)
			}
		}
		return t.clone_method_subst_children_with_value(node, var_name, method, inner_vars, cond)
	}
	return t.clone_method_subst_children(node, var_name, method, inner_vars)
}

fn (mut t Transformer) make_attribute_array_literal(attrs []AttributeMeta) flat.NodeId {
	if attrs.len == 0 {
		return t.zero_value_for_type('[]VAttribute')
	}
	mut ids := []flat.NodeId{cap: attrs.len}
	for attr in attrs {
		ids << t.make_attribute_literal(attr)
	}
	return t.make_array_literal_typed(ids, '[]VAttribute')
}

fn (mut t Transformer) make_param_array_literal(params []ParamMeta, module_name string) flat.NodeId {
	if params.len == 0 {
		return t.zero_value_for_type('[]FunctionParam')
	}
	mut ids := []flat.NodeId{cap: params.len}
	for param in params {
		ids << t.make_param_data_literal_in_module(param, module_name)
	}
	return t.make_array_literal_typed(ids, '[]FunctionParam')
}

fn (t &Transformer) comptime_method_param_index(id flat.NodeId, var_name string) ?int {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.comptime_method_param_index(t.a.child(&node, 0), var_name)
	}
	if node.kind != .index || node.children_count < 2 {
		return none
	}
	base := t.a.child_node(&node, 0)
	index := t.a.child_node(&node, 1)
	if base.kind != .selector || base.value !in ['args', 'params'] || base.children_count == 0
		|| index.kind != .int_literal
		|| !t.comptime_method_param_owner_matches(t.a.child(base, 0), var_name) {
		return none
	}
	return index.value.int()
}

fn (t &Transformer) comptime_method_param_owner_matches(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.comptime_method_param_owner_matches(t.a.child(&node, 0), var_name)
	}
	return node.kind == .ident && node.value == var_name
}

fn (t &Transformer) comptime_method_name_expr_matches(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value == var_name
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		return base.kind == .ident && base.value == var_name
	}
	return false
}

fn (mut t Transformer) clone_method_subst_children(node flat.Node, var_name string, method MethodMeta, inner_vars []string) flat.NodeId {
	return t.clone_method_subst_children_with_value(node, var_name, method, inner_vars, node.value)
}

fn (mut t Transformer) clone_method_subst_children_with_value(node flat.Node, var_name string, method MethodMeta, inner_vars []string, value string) flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		if child := t.clone_method_subst_scoped(t.a.child(&node, i), var_name, method,
			child_inner_vars)
		{
			children << child
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	mut typ := node.typ
	if node.kind == .comptime_for {
		receiver_name := comptime_method_receiver_name(method.receiver, method.module_name)
		typ = comptime_cond_replace_bare_ident(typ, var_name, '${receiver_name}.${method.name}')
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          value
		typ:            typ
		generic_params: node.generic_params.clone()
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn (t &Transformer) subst_method_cond(cond string, var_name string, method MethodMeta) string {
	mut result := t.subst_method_param_cond(cond, var_name, method)
	result = comptime_cond_replace_unquoted(result, '${var_name}.args.len', method.params.len.str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.params.len',
		method.params.len.str())
	method_type := t.comptime_method_type_text(method)
	result = comptime_cond_replace_unquoted(result, '${var_name}.location',
		comptime_cond_string_literal(method.location))
	result = comptime_cond_replace_unquoted(result, '${var_name}.return_type', t.comptime_field_type_id_key(method.return_type,
		method.module_name))
	result = comptime_cond_replace_unquoted(result, '${var_name}.typ', method_type)
	result = comptime_cond_replace_unquoted(result, '${var_name}.is_pub', method.is_pub.str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.name', "'${method.name}'")
	result = comptime_cond_replace_bare_ident(result, var_name, method_type)
	for op in [' !is ', ' is '] {
		if idx := comptime_top_index(result, op) {
			expected := result[idx + op.len..].trim_space()
			normalized := if !isnil(t.tc)
				&& (expected.starts_with('fn(') || expected.starts_with('fn (')) {
				t.tc.parse_type(expected).name()
			} else {
				t.comptime_normalize_type_alias_chain(expected)
			}
			if normalized != expected {
				result = result[..idx + op.len] + normalized
			}
			break
		}
	}
	return result
}

fn (t &Transformer) comptime_method_type_text(method MethodMeta) string {
	mut param_types := []string{cap: method.params.len}
	for param in method.params {
		param_types << t.comptime_field_type_id_key(param.typ, method.module_name)
	}
	ret := if method.return_type.len > 0 && method.return_type != 'void' {
		' ${t.comptime_field_type_id_key(method.return_type, method.module_name)}'
	} else {
		''
	}
	return 'fn(${param_types.join(', ')})${ret}'
}

fn (t &Transformer) comptime_method_type_id(method MethodMeta) int {
	return t.comptime_field_type_id(t.comptime_method_type_text(method), '')
}

// subst_method_param_cond materializes indexed FunctionParam members in serialized `$if` guards.
fn (t &Transformer) subst_method_param_cond(cond string, var_name string, method MethodMeta) string {
	mut result := cond
	for collection in ['args', 'params'] {
		prefix := '${var_name}.${collection}['
		mut offset := 0
		for offset < result.len {
			if result[offset] == `'` || result[offset] == `"` {
				offset = comptime_cond_skip_string(result, offset)
				continue
			}
			if !result[offset..].starts_with(prefix)
				|| (offset > 0 && comptime_cond_name_char(result[offset - 1])) {
				offset++
				continue
			}
			start := offset
			index_start := start + prefix.len
			rel_end := result[index_start..].index_u8(`]`)
			if rel_end < 0 {
				break
			}
			index_end := index_start + rel_end
			member_start := index_end + 1
			member := if result[member_start..].starts_with('.typ') {
				'typ'
			} else if result[member_start..].starts_with('.name') {
				'name'
			} else {
				''
			}
			member_end := member_start + member.len + 1
			if member.len == 0
				|| (member_end < result.len && comptime_cond_name_char(result[member_end])) {
				offset = member_start
				continue
			}
			index_text := result[index_start..index_end].trim_space()
			if !comptime_is_int(index_text) || index_text.starts_with('-') {
				offset = member_end
				continue
			}
			index := index_text.int()
			replacement := if index >= 0 && index < method.params.len {
				if member == 'name' {
					"'${method.params[index].name}'"
				} else if method.params[index].typ == '&void' {
					'voidptr'
				} else {
					t.comptime_field_type_id_key(method.params[index].typ, method.module_name)
				}
			} else if member == 'name' {
				"''"
			} else {
				// Missing slots must not accidentally match a real type such as `void`.
				'__v3_missing_method_param_type'
			}
			result = result[..start] + replacement + result[member_end..]
			offset = start + replacement.len
		}
	}
	return result
}

// expand_comptime_for_values unrolls `$for value in Enum.values { ... }`. The loop variable
// exposes `value.name` / `value.value` and, used bare, materializes as `EnumData`.
fn (mut t Transformer) expand_comptime_for_values(var_name string, base_type string, body_stmts []flat.NodeId) []flat.NodeId {
	values := t.comptime_enum_members(base_type)
	mut out := []flat.NodeId{}
	for item in values {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_value_subst(sid, var_name, item) {
				cloned << cid
			}
		}
		block := t.make_block(cloned)
		for s in t.transform_stmt(block) {
			out << s
		}
	}
	return out
}

// expand_comptime_for_variants unrolls `$for variant in Sum.variants`. The loop variable is a
// VariantData value at runtime, while its `.typ` member also carries the concrete compile-time
// type used by `is` checks and `typeof(variant.typ)`.
fn (mut t Transformer) expand_comptime_for_variants(var_name string, base_type string, body_stmts []flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	for item in t.comptime_sum_variants(base_type) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_variant_subst(sid, var_name, item) {
				cloned << cid
			}
		}
		block := t.make_block(cloned)
		for s in t.transform_stmt(block) {
			out << s
		}
	}
	return out
}

fn (t &Transformer) comptime_sum_variants(base_type string) []VariantMeta {
	resolved := t.comptime_resolve_sum_type_name(base_type)
	mut variants := if !base_type.contains('.') {
		t.comptime_local_sum_variants(base_type.trim_space()) or { []string{} }
	} else if isnil(t.tc) {
		[]string{}
	} else {
		t.tc.sum_types[resolved] or { []string{} }
	}
	if variants.len == 0 && !isnil(t.tc) {
		variants = t.tc.sum_types[resolved] or { []string{} }
	}
	if variants.len == 0 {
		variants = t.sum_types[resolved] or { []string{} }
	}
	mut metas := []VariantMeta{cap: variants.len}
	for variant in variants {
		metas << VariantMeta{
			typ:    variant
			typ_id: t.comptime_field_type_id(variant, t.cur_module)
		}
	}
	return metas
}

fn (t &Transformer) comptime_resolve_sum_type_name(base_type string) string {
	base := base_type.trim_space()
	if base.len == 0 || isnil(t.tc) {
		return base
	}
	if base.contains('.') {
		return t.resolve_imported_type_name(base) or { base }
	}
	local := if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		'${t.cur_module}.${base}'
	} else {
		base
	}
	if local in t.tc.sum_types {
		return local
	}
	// A specialization declared in another module can carry an unqualified
	// caller-owned sum type (notably a type from `main`). Prefer that exact
	// registered type before qualifying it as if it belonged to the generic
	// function's module.
	if base in t.tc.sum_types {
		return base
	}
	imported := t.comptime_resolve_selective_import_type(base)
	if imported != base {
		return imported
	}
	return local
}

// collect_types keeps legacy short-name entries for imported sums, so locate a bare sum's
// declaration in the current module before consulting that ambiguous cache.
fn (t &Transformer) comptime_local_sum_variants(name string) ?[]string {
	if name.len == 0 || name.contains('.') {
		return none
	}
	mut module_name := ''
	for node in t.a.nodes {
		if node.kind == .file {
			module_name = ''
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .type_decl || node.value != name || module_name != t.cur_module
			|| node.children_count == 0 {
			continue
		}
		mut variants := []string{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			variant := t.a.child_node(&node, i)
			variants << t.normalize_sum_variant_type(variant.value, module_name,
				node.generic_params)
		}
		return variants
	}
	return none
}

// comptime_enum_members returns the member metadata for an enum (resolving an enum alias to its
// underlying enum).
fn (t &Transformer) comptime_enum_members(base_type string) []EnumValueMeta {
	mut names := []string{}
	mut resolved := base_type
	if got := t.enum_types[base_type] {
		names = got.clone()
	} else {
		qname := t.qualified_alias_name(base_type)
		if got := t.enum_types[qname] {
			names = got.clone()
			resolved = qname
		}
	}
	if names.len == 0 {
		return []EnumValueMeta{}
	}
	metas := t.enum_decl_value_metas(resolved)
	if metas.len > 0 {
		return metas
	}
	mut fallback := []EnumValueMeta{cap: names.len}
	for idx, name in names {
		fallback << EnumValueMeta{
			name:      name
			value:     i64(idx)
			enum_name: resolved
		}
	}
	return fallback
}

// enum_decl_value_metas locates the enum declaration matching `enum_name` (either a bare `Enum`
// or a `module.Enum` qualified name) and evaluates each member's metadata. Explicit values are
// treated like the C backend: normal enums use the integer directly; `[flag]` enums use it as the
// bit index and materialize `1 << index`.
fn (t &Transformer) enum_decl_value_metas(enum_name string) []EnumValueMeta {
	mut cur_mod := ''
	for idx in 0 .. t.a.nodes.len {
		kind := t.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = t.a.nodes[idx].value
			continue
		}
		if kind != .enum_decl {
			continue
		}
		node := t.a.nodes[idx]
		qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
			'${cur_mod}.${node.value}'
		} else {
			node.value
		}
		if enum_name != node.value && enum_name != qualified {
			continue
		}
		is_flag := node.typ == 'flag'
		mut fields := []EnumDeclFieldValue{}
		mut field_exprs := map[string]flat.NodeId{}
		for i in 0 .. node.children_count {
			f := t.a.child_node(&node, i)
			if f.kind != .enum_field {
				continue
			}
			expr_id := if f.children_count > 0 { t.a.child(f, 0) } else { flat.NodeId(-1) }
			fields << EnumDeclFieldValue{
				name:    f.value
				expr_id: expr_id
				attrs:   f.generic_params.clone()
			}
			if int(expr_id) >= 0 {
				field_exprs[f.value] = expr_id
			}
		}
		mut values := []EnumValueMeta{}
		mut field_values := map[string]i64{}
		mut resolving := map[string]bool{}
		mut next_val := i64(0)
		for f in fields {
			mut val := next_val
			if int(f.expr_id) >= 0 {
				if ev := t.enum_field_int_value_with_enum(f.expr_id, cur_mod, qualified, mut
					field_values, field_exprs, mut resolving)
				{
					val = ev
				}
			}
			field_values[f.name] = val
			values << EnumValueMeta{
				name:      f.name
				value:     if is_flag { i64(u64(1) << u64(val)) } else { val }
				attrs:     f.attrs.clone()
				enum_name: qualified
			}
			next_val = val + 1
		}
		return values
	}
	return []EnumValueMeta{}
}

// enum_field_int_value evaluates an enum member's value expression using the transformer's
// current module for any const reference.
fn (t &Transformer) enum_field_int_value(id flat.NodeId) ?i64 {
	return t.enum_field_int_value_in_module(id, t.cur_module)
}

// enum_field_int_value_in_module evaluates an enum member's value expression (`x = 1`,
// `x = 1 << 2`, `x = SomeConst`) to an i64, following the same forms as the C backend.
// `enum_module` is the enum's declaring module, so a const referenced by a member (`a = base`)
// resolves in that module rather than the caller's.
fn (t &Transformer) enum_field_int_value_in_module(id flat.NodeId, enum_module string) ?i64 {
	mut field_values := map[string]i64{}
	field_exprs := map[string]flat.NodeId{}
	mut resolving := map[string]bool{}
	return t.enum_field_int_value_with_enum(id, enum_module, '', mut field_values, field_exprs, mut
		resolving)
}

fn (t &Transformer) enum_field_int_value_with_enum(id flat.NodeId, enum_module string, enum_name string, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			clean := node.value.replace('_', '')
			parsed := strconv.common_parse_int(clean, 0, 64, true, true) or { return none }
			return parsed
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return t.enum_field_int_value_with_enum(t.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			v := t.enum_field_int_value_with_enum(t.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)?
			return match node.op {
				.plus { v }
				.minus { -v }
				.bit_not { ~v }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			l := t.enum_field_int_value_with_enum(t.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)?
			r := t.enum_field_int_value_with_enum(t.a.child(&node, 1), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)?
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] && (r < 0 || r >= 64) {
				return none
			}
			return match node.op {
				.plus {
					l + r
				}
				.minus {
					l - r
				}
				.mul {
					l * r
				}
				.div {
					if r == 0 {
						none
					} else {
						l / r
					}
				}
				.mod {
					if r == 0 {
						none
					} else {
						l % r
					}
				}
				.left_shift {
					i64(u64(l) << u64(r))
				}
				.right_shift {
					l >> r
				}
				.right_shift_unsigned {
					i64(u64(l) >> u64(r))
				}
				.amp {
					l & r
				}
				.pipe {
					l | r
				}
				.xor {
					l ^ r
				}
				else {
					none
				}
			}
		}
		.enum_val {
			return t.enum_decl_field_ref_value(node.value, enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
		}
		.selector {
			if field := t.enum_decl_selector_ref_field(id, enum_module, enum_name) {
				return t.enum_decl_field_ref_value(field, enum_module, enum_name, mut field_values,
					field_exprs, mut resolving)
			}
			return none
		}
		.ident {
			if ev := t.enum_decl_field_ref_value(node.value, enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				return ev
			}
			if !isnil(t.tc) {
				lookup_module := if enum_module.len > 0 { enum_module } else { t.cur_module }
				return i64(t.tc.const_int_value_in_module(node.value, lookup_module, []string{})?)
			}
			return none
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) enum_decl_field_ref_value(field_name string, enum_module string, enum_name string, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if val := field_values[field_name] {
		return val
	}
	expr_id := field_exprs[field_name] or { return none }
	if resolving[field_name] {
		return none
	}
	resolving[field_name] = true
	maybe_val := t.enum_field_int_value_with_enum(expr_id, enum_module, enum_name, mut
		field_values, field_exprs, mut resolving)
	resolving.delete(field_name)
	val := maybe_val?
	field_values[field_name] = val
	return val
}

fn (t &Transformer) enum_decl_selector_ref_field(id flat.NodeId, enum_module string, enum_name string) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	prefix := t.enum_decl_selector_base_text(t.a.child(&node, 0))
	if !enum_ref_prefix_matches(prefix, enum_module, enum_name) {
		return none
	}
	return node.value
}

fn (t &Transformer) enum_decl_selector_base_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := t.enum_decl_selector_base_text(t.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		else {
			return ''
		}
	}
}

fn enum_ref_prefix_matches(prefix string, enum_module string, enum_name string) bool {
	if prefix.len == 0 || enum_name.len == 0 {
		return false
	}
	short := enum_name.all_after_last('.')
	if prefix == enum_name || prefix == short {
		return true
	}
	if enum_module.len > 0 && prefix == '${enum_module}.${short}' {
		return true
	}
	return false
}

// clone_value_subst clones a `$for value in Enum.values` body, substituting `value.name`,
// `value.value`, `value.attrs`, and a bare `value` (an `EnumData` literal).
fn (mut t Transformer) clone_value_subst(id flat.NodeId, var_name string, item EnumValueMeta) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_enum_data_literal(item)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			match node.value {
				'name' {
					return t.make_string_literal(item.name)
				}
				'value' {
					return t.make_comptime_enum_value(item)
				}
				'attrs' {
					return t.make_string_array_literal(item.attrs)
				}
				else {}
			}
		}
	}
	// `$if`/`$else $if` referencing the loop variable (`value.name`, `value.value`): evaluate now
	// and keep the taken branch, mirroring the field-loop path so the guard is not left as an
	// unsupported `comptime_if` for the C backend.
	if node.kind == .comptime_if && comptime_cond_references_ident(node.value, var_name) {
		cond := t.subst_value_cond(node.value, var_name, item.name, item.value)
		if !comptime_cond_has_loop_member_ref(cond, var_name) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_value_subst(t.a.child(&node, branch_idx), var_name, item)
			}
		}
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if c := t.clone_value_subst(child_id, var_name, item) {
			children << c
		}
	}
	start := t.a.children.len
	for c in children {
		t.a.children << c
	}
	clone_id := t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
	return clone_id
}

fn (mut t Transformer) comptime_field_call_generic_args(node flat.Node, mut children []flat.NodeId, fm FieldMeta) string {
	if node.kind != .call || node.value.len > 0 || children.len < 2 {
		return node.value
	}
	callee := t.a.nodes[int(children[0])]
	if callee.kind !in [.ident, .selector] {
		return node.value
	}
	decls := t.cached_generic_fn_decls()
	mut decl := GenericFnDecl{}
	mut found := false
	if callee.kind == .ident {
		for candidate in t.generic_plain_call_candidates(callee.value, t.cur_module) {
			if got := decls[candidate] {
				decl = got
				found = true
				break
			}
		}
	} else {
		for key in t.generic_receiver_methods_by_name[callee.value] {
			candidate := decls[key] or { continue }
			if candidate.module == t.cur_module {
				decl = candidate
				found = true
				break
			}
		}
	}
	if !found {
		return node.value
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return comptime_unsupported_late_generic_call
	}
	mut inferred := map[string]string{}
	mut param_idx := 0
	is_receiver := callee.kind == .selector && t.generic_decl_is_receiver_method(decl.node)
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			continue
		}
		arg_id := if is_receiver && param_idx == 0 {
			if callee.children_count == 0 { flat.empty_node } else { t.a.child(&callee, 0) }
		} else {
			arg_pos := if is_receiver { param_idx } else { param_idx + 1 }
			if arg_pos < children.len {
				children[arg_pos]
			} else {
				flat.empty_node
			}
		}
		if int(arg_id) < 0 {
			break
		}
		arg := t.a.nodes[int(arg_id)]
		mut arg_type := if arg.kind == .ident {
			t.local_decl_type_before(arg.value, arg_id) or {
				t.generic_call_arg_type_for_inference(arg_id)
			}
		} else {
			t.generic_call_arg_type_for_inference(arg_id)
		}
		if arg_type.len == 0 || arg_type in ['array', 'map', 'unknown', 'generic']
			|| is_generic_fn_placeholder_name(arg_type) {
			arg_type = fm.comptime_typ
		}
		infer_generic_type_args(param.typ, arg_type, mut inferred)
		param_idx++
	}
	mut inferred_args := []string{cap: param_names.len}
	for name in param_names {
		inferred_args << inferred[name] or { return comptime_unsupported_late_generic_call }
	}
	args := t.canonical_generic_specialization_args(inferred_args)
	if args.len != param_names.len || t.generic_args_have_placeholders(args) {
		return comptime_unsupported_late_generic_call
	}
	spec_value := specialized_generic_fn_value(decl.node.value, args)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	if spec_name !in t.fn_ret_types {
		clone_id := t.emit_generic_fn_specialization(decl, args)
		t.generated_fn_used_names(decl, clone_id, args)
	}
	if is_receiver {
		receiver := t.a.child(&callee, 0)
		children[0] = t.make_ident(spec_name)
		children.insert(1, receiver)
	} else {
		t.set_node_value(int(children[0]), spec_name)
	}
	return ''
}

fn (mut t Transformer) make_comptime_enum_value(item EnumValueMeta) flat.NodeId {
	literal := t.make_int_literal_typed(item.value.str(), 'i64')
	return t.make_cast('i64', literal, 'i64')
}

// clone_variant_subst clones a `$for variant in Sum.variants` body and gives the variant loop
// variable its dual meaning: a VariantData value in ordinary expressions and a concrete type in
// `is`/`$if`/`typeof(variant.typ)` compile-time positions.
fn (mut t Transformer) clone_variant_subst(id flat.NodeId, var_name string, item VariantMeta) ?flat.NodeId {
	return t.clone_variant_subst_with_smartcast(id, var_name, item, '')
}

fn (mut t Transformer) clone_variant_subst_with_smartcast(id flat.NodeId, var_name string, item VariantMeta, smartcast_name string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && node.children_count > 0
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		target_expr := t.a.child_node(&node, 0)
		if target_expr.kind == .selector && target_expr.value == 'typ'
			&& target_expr.children_count > 0 {
			base := t.a.child_node(target_expr, 0)
			// The marker can share its loop-variable leaf with the discarded
			// generic template. The marker itself is sufficient to recover the
			// variant type after that leaf has been pruned.
			if base.kind == .empty || (base.kind == .ident && base.value == var_name) {
				return if node.value == '__v3_comptime_new' {
					t.comptime_new_value(item.typ)
				} else {
					t.zero_value_for_type(item.typ)
				}
			}
		}
	}
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_variant_data_literal(item)
	}
	if node.kind == .cast_expr && node.children_count > 0 {
		operand := t.a.child_node(&node, 0)
		if operand.kind == .ident && operand.value == var_name {
			// `T(v)` wraps a zero value of the variant's type in the sum type
			// (the VariantData literal is only the loop var's runtime carrier).
			zero_id := t.zero_value_for_type(item.typ)
			variant_zero := if item.typ in t.enum_types
				|| t.qualified_alias_name(item.typ) in t.enum_types {
				t.make_cast(item.typ, zero_id, item.typ)
			} else {
				zero_id
			}
			return t.make_cast(node.value, variant_zero, node.value)
		}
	}
	if node.kind == .selector && node.children_count > 0
		&& t.typeof_arg_is_variant_typ(t.a.child(&node, 0), var_name) {
		match node.value {
			'name' { return t.make_string_literal(item.typ) }
			'idx' { return t.make_int_literal(item.typ_id) }
			else {}
		}
	}
	if node.kind == .typeof_expr && t.typeof_arg_is_variant_typ(id, var_name) {
		return t.make_string_literal(item.typ)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name && node.value == 'typ' {
			return t.make_int_literal(item.typ_id)
		}
	}
	if node.kind == .comptime_if && (comptime_cond_references_ident(node.value, var_name)
		|| (smartcast_name.len > 0 && comptime_cond_references_ident(node.value, smartcast_name))) {
		mut cond := t.subst_variant_cond(node.value, var_name, item)
		if smartcast_name.len > 0 {
			cond = comptime_cond_replace_bare_ident(cond, smartcast_name, item.typ)
		}
		if !comptime_cond_has_loop_member_ref(cond, var_name) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_variant_subst_with_smartcast(t.a.child(&node, branch_idx), var_name,
					item, smartcast_name)
			}
		}
	}
	mut branch_smartcast := ''
	if node.kind == .if_expr && node.children_count >= 2 {
		cond := t.a.child_node(&node, 0)
		if cond.kind == .is_expr && cond.value == var_name && cond.children_count > 0 {
			base := t.a.child_node(cond, 0)
			if base.kind == .ident {
				branch_smartcast = base.value
			}
		}
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_smartcast := if i == 1 && branch_smartcast.len > 0 {
			branch_smartcast
		} else {
			smartcast_name
		}
		if child := t.clone_variant_subst_with_smartcast(t.a.child(&node, i), var_name, item,
			child_smartcast)
		{
			children << child
		}
	}
	if node.kind == .cast_expr && children.len == 1 {
		target_sum := t.resolve_sum_name(node.value)
		child := t.a.nodes[int(children[0])]
		child_type := t.node_type(children[0])
		if target_sum in t.sum_types && child.kind == .ident
			&& t.sum_target_accepts_variant_type(target_sum, item.typ)
			&& (child_type.len == 0 || t.resolve_sum_name(child_type) == target_sum
			|| t.normalize_type_alias(child_type) == t.normalize_type_alias(item.typ)) {
			t.set_node_typ(int(children[0]), item.typ)
			return t.make_sum_literal(target_sum, item.typ, children[0])
		}
	}
	mut typ := node.typ
	if node.kind == .ident && smartcast_name.len > 0 && node.value == smartcast_name {
		typ = item.typ
	} else if node.kind == .ident && t.mut_param_values[node.value] {
		storage_type := t.var_type(node.value)
		if storage_type.starts_with('&') {
			typ = storage_type[1..]
		} else if typ.starts_with('&') {
			typ = typ[1..]
		}
	} else if node.kind == .ident && node.value.len > 0 {
		local_type := t.raw_var_type(node.value)
		if local_type.len > 0 && !t.generic_arg_is_unresolved(local_type) {
			typ = local_type
		}
	}
	if node.kind == .decl_assign && children.len >= 2 {
		rhs := t.a.nodes[int(children[1])]
		rhs_typ := if rhs.kind == .ident && smartcast_name.len > 0 && rhs.value == smartcast_name
			&& rhs.typ.len > 0 {
			rhs.typ
		} else {
			t.node_type(children[1])
		}
		if rhs_typ.len > 0 && rhs_typ !in ['unknown', 'generic'] {
			typ = rhs_typ
			t.set_node_typ(int(children[0]), rhs_typ)
			lhs := t.a.nodes[int(children[0])]
			if lhs.kind == .ident && lhs.value.len > 0 {
				t.set_var_type(lhs.value, rhs_typ)
			}
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	clone_id := t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          if node.kind == .is_expr && node.value == var_name {
			item.typ
		} else {
			node.value
		}
		typ:            typ
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
	if node.kind == .ident && t.mut_param_values[node.value] {
		t.mut_value_ident_nodes[int(clone_id)] = true
	}
	return clone_id
}

fn (t &Transformer) typeof_arg_is_variant_typ(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .typeof_expr || node.children_count == 0 {
		return false
	}
	arg := t.a.child_node(&node, 0)
	if arg.kind != .selector || arg.value != 'typ' || arg.children_count == 0 {
		return false
	}
	base := t.a.child_node(arg, 0)
	return base.kind == .ident && base.value == var_name
}

fn (t &Transformer) subst_variant_cond(cond string, var_name string, item VariantMeta) string {
	mut result := cond.replace('${var_name}.typ', item.typ)
	result = comptime_cond_replace_bare_ident(result, var_name, item.typ)
	return result
}

// make_enum_data_literal builds `EnumData{name: '<name>', value: <value>, attrs: [...]}`.
fn (mut t Transformer) make_enum_data_literal(item EnumValueMeta) flat.NodeId {
	name_field := t.make_named_field_init('name', t.make_string_literal(item.name), 'string')
	value_field := t.make_named_field_init('value', t.make_int_literal_typed(item.value.str(),
		'i64'), 'i64')
	attrs_field := t.make_named_field_init('attrs', t.make_string_array_literal(item.attrs),
		'[]string')
	start := t.a.children.len
	t.a.children << name_field
	t.a.children << value_field
	t.a.children << attrs_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'EnumData'
		typ:            'EnumData'
		children_start: start
		children_count: 3
	})
}

fn (mut t Transformer) make_variant_data_literal(item VariantMeta) flat.NodeId {
	typ_field := t.make_named_field_init('typ', t.make_int_literal(item.typ_id), 'int')
	start := t.a.children.len
	t.a.children << typ_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'VariantData'
		typ:            'VariantData'
		children_start: start
		children_count: 1
	})
}

// make_field_data_literal builds a runtime `FieldData` value for a bare `$for field` variable.
fn (mut t Transformer) make_field_data_literal(fm FieldMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(fm.name), 'string'),
		t.make_named_field_init('typ', t.make_int_literal(fm.typ_id), 'int'),
		t.make_named_field_init('unaliased_typ', t.make_int_literal(fm.unaliased_id), 'int'),
		t.make_named_field_init('attrs', t.make_string_array_literal(fm.attrs), '[]string'),
		t.make_named_field_init('is_pub', t.make_bool_literal(fm.is_pub), 'bool'),
		t.make_named_field_init('is_mut', t.make_bool_literal(fm.is_mut), 'bool'),
		t.make_named_field_init('is_embed', t.make_bool_literal(fm.is_embed), 'bool'),
		t.make_named_field_init('is_shared', t.make_bool_literal(fm.is_shared), 'bool'),
		t.make_named_field_init('is_atomic', t.make_bool_literal(fm.is_atomic), 'bool'),
		t.make_named_field_init('is_option', t.make_bool_literal(fm.is_option), 'bool'),
		t.make_named_field_init('is_array', t.make_bool_literal(fm.is_array), 'bool'),
		t.make_named_field_init('is_map', t.make_bool_literal(fm.is_map), 'bool'),
		t.make_named_field_init('is_chan', t.make_bool_literal(fm.is_chan), 'bool'),
		t.make_named_field_init('is_enum', t.make_bool_literal(fm.is_enum), 'bool'),
		t.make_named_field_init('is_struct', t.make_bool_literal(fm.is_struct), 'bool'),
		t.make_named_field_init('is_alias', t.make_bool_literal(fm.is_alias), 'bool'),
		t.make_named_field_init('indirections', t.make_int_literal(fm.indirections), 'u8'),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'FieldData'
		typ:            'FieldData'
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (mut t Transformer) make_named_field_init(field string, value flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		value:          field
		typ:            typ
		children_start: start
		children_count: 1
	})
}

fn (mut t Transformer) clone_node_preserving_children(node flat.Node) flat.NodeId {
	return t.clone_node_preserving_children_with_type(node, node.typ)
}

fn (mut t Transformer) clone_node_preserving_children_with_type(node flat.Node, typ string) flat.NodeId {
	start := t.a.children.len
	for i in 0 .. node.children_count {
		t.a.children << t.a.child(&node, i)
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          node.value
		typ:            typ
		is_mut:         node.is_mut
		children_start: start
		children_count: node.children_count
	})
}

// comptime_body_has_unsupported_late_generic reports whether inference marked a generic call
// that could not be specialized after the normal monomorphization pass.
fn (t &Transformer) comptime_body_has_unsupported_late_generic(stmts []flat.NodeId) bool {
	for sid in stmts {
		if t.subtree_has_unsupported_late_generic(sid) {
			return true
		}
	}
	return false
}

// comptime_body_has_unsupported reports whether any statement uses a comptime construct the unroll
// cannot yet model: a `match <var>.typ` type match, or (after substitution) `typeof(...)` that
// still references the loop variable.
fn (t &Transformer) comptime_body_has_unsupported(stmts []flat.NodeId, var_name string, reject_typeof bool) bool {
	for sid in stmts {
		if t.subtree_has_unsupported_comptime(sid, var_name, reject_typeof) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_has_unsupported_comptime(id flat.NodeId, var_name string, reject_typeof bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	// ORM (`sql db { select ... }`) mixes comptime field names with a query DSL.
	if node.kind in [.sql_expr, .select_stmt] {
		return true
	}
	if reject_typeof && node.kind == .typeof_expr && t.subtree_references_var(id, var_name) {
		return true
	}
	// A `field.member` / `field.$(...)` selector is a supported comptime access: check its other
	// children but not the base loop-var ident (which is not a bare use).
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			for i in 1 .. node.children_count {
				if t.subtree_has_unsupported_comptime(t.a.child(&node, i), var_name, reject_typeof) {
					return true
				}
			}
			return false
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_unsupported_comptime(t.a.child(&node, i), var_name, reject_typeof) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_references_var(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == var_name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.subtree_references_var(t.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_has_unsupported_late_generic(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call && node.value == comptime_unsupported_late_generic_call {
		return true
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_unsupported_late_generic(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

// FieldDeclMeta holds the per-field mutability/visibility/attributes recorded by the parser on a
// `field_decl` node, keyed by field name (see struct_field_decl_metas).
struct FieldDeclMeta {
	is_mut bool
	is_pub bool
	attrs  []string
}

// comptime_field_metas derives FieldData for every field of the concrete struct type.
fn (t &Transformer) comptime_field_metas(base_type string) []FieldMeta {
	// A monomorphized generic struct instance (`Box[int]`) is stored in the struct table under
	// its generic declaration name (`Box`), so a direct lookup misses; resolve it through the
	// generic-struct field substitution path before giving up.
	info := t.lookup_struct_info(base_type) or {
		t.generic_struct_info_for_stringify(base_type) or { return []FieldMeta{} }
	}
	decl_metas := t.struct_field_decl_metas(base_type)
	mut metas := []FieldMeta{cap: info.fields.len}
	for f in info.fields {
		// V's `field.typ` is the type as written (`MyInt`, `?[]int`); `raw_typ` preserves that,
		// while `f.typ` was already alias-resolved in the struct's declaring module during
		// collect_types. Classify using `f.typ` (and `info.module` for the alias check) so a
		// field type declared in another module is not mis-resolved against the caller's module.
		ftyp := if f.raw_typ.len > 0 { f.raw_typ } else { f.typ }
		// Fields whose declaration node we could not introspect default to the previous
		// public/immutable behaviour; found fields carry their real modifiers/attrs.
		extra := decl_metas[f.name] or {
			FieldDeclMeta{
				is_pub: true
			}
		}
		metas << t.field_meta_for(f.name, ftyp, f.typ, info.module, f.is_embedded, extra)
	}
	return metas
}

// struct_field_decl_metas scans the declaration of `base_type` (resolving a generic instance such
// as `Box[int]` to its base `Box`, module-aware like enum lookup) and returns each field's real
// `is_mut`/`is_pub`/attrs, which the parser recorded on the `field_decl` node.
fn (t &Transformer) struct_field_decl_metas(base_type string) map[string]FieldDeclMeta {
	mut out := map[string]FieldDeclMeta{}
	mut decl_name := base_type.trim_space()
	if idx := decl_name.index('[') {
		decl_name = decl_name[..idx]
	}
	mut cur_mod := ''
	for idx in 0 .. t.a.nodes.len {
		kind := t.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = t.a.nodes[idx].value
			continue
		}
		if kind != .struct_decl {
			continue
		}
		node := t.a.nodes[idx]
		qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
			'${cur_mod}.${node.value}'
		} else {
			node.value
		}
		if decl_name != node.value && decl_name != qualified {
			continue
		}
		for i in 0 .. node.children_count {
			f := t.a.child_node(&node, i)
			if f.kind != .field_decl {
				continue
			}
			// The parser packs field metadata into generic_params: element 0 is a flag string
			// (`m` = mut, `p` = pub), the rest are attributes. An empty list is the default
			// (private, immutable, no attrs).
			mut is_mut := false
			mut is_pub := false
			mut attrs := []string{}
			if f.generic_params.len > 0 {
				flags := f.generic_params[0]
				is_mut = flags.contains('m')
				is_pub = flags.contains('p')
				attrs = f.generic_params[1..].clone()
			}
			out[f.value] = FieldDeclMeta{
				is_mut: is_mut
				is_pub: is_pub
				attrs:  attrs
			}
		}
		return out
	}
	return out
}

fn (t &Transformer) field_meta_for(name string, ftyp string, resolved_typ string, decl_module string, is_embed bool, extra FieldDeclMeta) FieldMeta {
	is_option := ftyp.starts_with('?')
	mut core := if is_option { ftyp[1..].trim_space() } else { ftyp }
	mut indir := 0
	mut is_shared := false
	mut is_atomic := false
	// A `shared T` field is stored behind a lock guard; V reports one level of indirection.
	if core.starts_with('shared ') {
		is_shared = true
		indir++
		core = core[7..].trim_space()
	} else if core.starts_with('atomic ') {
		is_atomic = true
		core = core[7..].trim_space()
	}
	for core.starts_with('&') {
		indir++
		core = core[1..]
	}
	// `resolved_typ` is already alias-resolved in the declaring module and still carries wrappers
	// like `?`, `shared`, and `&`; preserve them for `FieldData.unaliased_typ`.
	unaliased := if resolved_typ.len > 0 {
		resolved_typ.trim_space()
	} else {
		t.comptime_normalize_type_alias_chain(ftyp)
	}
	unaliased_core := comptime_strip_field_wrappers(unaliased)
	is_alias := t.field_type_is_alias(core, decl_module)
	return FieldMeta{
		name:               name
		typ:                ftyp
		unaliased_typ:      unaliased
		comptime_typ:       t.comptime_field_type_id_key(ftyp, decl_module)
		comptime_unaliased: t.comptime_field_type_id_key(unaliased, decl_module)
		typ_id:             t.comptime_field_type_id(ftyp, decl_module)
		unaliased_id:       t.comptime_field_type_id(unaliased, decl_module)
		is_option:          is_option
		is_embed:           is_embed
		is_array:           unaliased_core.starts_with('[]')
			|| t.is_fixed_array_type(unaliased_core)
		is_map:             unaliased_core.starts_with('map[')
		is_chan:            unaliased_core.starts_with('chan ')
		is_struct:          t.comptime_field_type_is_struct(unaliased_core)
		is_enum:            unaliased_core in t.enum_types
		is_alias:           is_alias
		is_shared:          is_shared
		is_atomic:          is_atomic
		is_mut:             extra.is_mut
		is_pub:             extra.is_pub
		attrs:              extra.attrs
		indirections:       indir
	}
}

fn (t &Transformer) comptime_field_type_is_struct(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 || comptime_is_primitive_type(clean) {
		return false
	}
	return t.comptime_struct_type_known(clean)
}

fn comptime_generic_type_base(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('[') || !clean.contains('[') {
		return ''
	}
	bracket := clean.index_u8(`[`)
	bracket_end := generic_matching_bracket(clean, bracket)
	if bracket <= 0 || bracket_end <= bracket {
		return ''
	}
	return clean[..bracket].trim_space()
}

fn (t &Transformer) comptime_field_type_id(typ string, decl_module string) int {
	key := t.comptime_field_type_id_key(typ, decl_module)
	if key.len == 0 {
		return 0
	}
	builtin_idx := comptime_builtin_type_idx(key)
	if builtin_idx > 0 {
		return builtin_idx
	}
	return comptime_type_id_hash(key)
}

// comptime_builtin_type_idx maps a builtin type name to V's stable ast type index
// (vlib/v/ast/types.v `*_type_idx` consts), so user code comparing `field.typ` /
// `typeof[T]().idx` against `v.ast` constants (e.g. `int(ast.bool_type)` == 19) sees the
// same values the reference compiler produces.
fn comptime_builtin_type_idx(name string) int {
	return match name {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8', 'byte' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'bool' { 19 }
		'none' { 20 }
		'string' { 21 }
		'rune' { 22 }
		'float literal' { 27 }
		'int literal' { 28 }
		'thread' { 29 }
		'nil' { 31 }
		else { 0 }
	}
}

fn (t &Transformer) comptime_field_type_id_key(typ string, decl_module string) string {
	mut core := typ.trim_space()
	if core.len == 0 {
		return ''
	}
	if core.starts_with('?') {
		return '?' + t.comptime_field_type_id_key(core[1..], decl_module)
	}
	if core.starts_with('shared ') {
		return 'shared ' + t.comptime_field_type_id_key(core[7..], decl_module)
	}
	if core.starts_with('atomic ') {
		return 'atomic ' + t.comptime_field_type_id_key(core[7..], decl_module)
	}
	mut refs := ''
	for core.starts_with('&') {
		refs += '&'
		core = core[1..].trim_space()
	}
	if refs.len > 0 {
		return refs + t.comptime_field_type_id_key(core, decl_module)
	}
	if core.starts_with('[]') {
		return '[]' + t.comptime_field_type_id_key(core[2..], decl_module)
	}
	if core.starts_with('map[') {
		bracket_end := generic_matching_bracket(core, 3)
		if bracket_end > 3 && bracket_end + 1 < core.len {
			key := t.comptime_field_type_id_key(core[4..bracket_end], decl_module)
			val := t.comptime_field_type_id_key(core[bracket_end + 1..], decl_module)
			return 'map[${key}]${val}'
		}
	}
	if core.starts_with('[') {
		bracket_end := generic_matching_bracket(core, 0)
		if bracket_end > 0 && bracket_end + 1 < core.len {
			elem := t.comptime_field_type_id_key(core[bracket_end + 1..], decl_module)
			return core[..bracket_end + 1] + elem
		}
	}
	if t.is_fixed_array_type(core) {
		elem, dims := transform_postfix_fixed_array_parts(core)
		if elem.len > 0 && dims.len > 0 {
			mut out := t.comptime_field_type_id_key(elem, decl_module)
			for dim in dims {
				out += '[${dim}]'
			}
			return out
		}
	}
	if comptime_is_primitive_type(core) || core.contains('.') || core.contains('[')
		|| core.contains(' ') || decl_module.len == 0 || decl_module in ['main', 'builtin'] {
		return core
	}
	return '${decl_module}.${core}'
}

// V3 does not have a runtime TypeInfo table yet; keep FieldData TypeIDs stable and nonzero.
// Hashed ids start above 65536 so they can never collide with the reserved builtin
// `*_type_idx` range returned by comptime_builtin_type_idx.
fn comptime_type_id_hash(key string) int {
	mut h := u64(1469598103934665603)
	for i in 0 .. key.len {
		h = ((h ^ u64(key[i])) * 1099511628211) % 2147418111
	}
	return int(h) + 65536
}

fn comptime_is_primitive_type(typ string) bool {
	return typ in ['string', 'bool', 'rune', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize',
		'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal',
		'voidptr', 'byteptr', 'charptr', 'nil', 'void']
}

// comptime_strip_field_wrappers removes the `?` option, `shared`/`atomic`, and `&` reference
// decorations from an (already alias-resolved) field type, yielding the core type used for
// metadata classification.
fn comptime_strip_field_wrappers(typ string) string {
	mut core := typ.trim_space()
	if core.starts_with('?') {
		core = core[1..].trim_space()
	}
	if core.starts_with('shared ') {
		core = core[7..].trim_space()
	} else if core.starts_with('atomic ') {
		core = core[7..].trim_space()
	}
	for core.starts_with('&') {
		core = core[1..]
	}
	return core
}

// field_type_is_alias reports whether `core` names a `type X = Y` alias. An unqualified name is
// resolved in the struct's declaring module (`decl_module`) rather than the caller's cur_module,
// so cross-module reflection reports `is_alias` correctly.
fn (t &Transformer) field_type_is_alias(core string, decl_module string) bool {
	if isnil(t.tc) {
		return false
	}
	if core in t.tc.type_aliases {
		return true
	}
	if !core.contains('.') && decl_module.len > 0 && decl_module != 'main'
		&& decl_module != 'builtin' {
		return '${decl_module}.${core}' in t.tc.type_aliases
	}
	return false
}

fn (t &Transformer) qualified_alias_name(name string) string {
	if name.contains('.') || t.cur_module.len == 0 || t.cur_module in ['main', 'builtin'] {
		return name
	}
	return '${t.cur_module}.${name}'
}

// clone_field_subst deep-clones a body node, substituting `<var>.member` references and folding
// `$if` conditions that reference the loop variable. Returns none when a folded branch is empty.
fn (mut t Transformer) clone_field_subst(id flat.NodeId, var_name string, fm FieldMeta) ?flat.NodeId {
	return t.clone_field_subst_scoped(id, var_name, fm, []string{})
}

fn (mut t Transformer) clone_field_subst_scoped(id flat.NodeId, var_name string, fm FieldMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && node.children_count > 0
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		target := t.comptime_field_type_accessor(t.a.child(&node, 0), var_name, fm) or {
			return t.clone_field_subst_children(node, var_name, fm, inner_vars)
		}
		return if node.value == '__v3_comptime_new' {
			t.comptime_new_value(target)
		} else {
			t.zero_value_for_type(target)
		}
	}
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children_with_type(node, t.clone_field_subst_type_text(node,
			var_name, fm))
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_field_data_literal(fm)
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0
		&& t.subtree_has_reflected_typeof_idx(t.a.child(&node, 0), var_name) {
		mut pointee := fm.comptime_typ.trim_space()
		if pointee.starts_with('&') {
			pointee = pointee[1..].trim_space()
		}
		zero := t.zero_value_for_type(pointee)
		start := t.a.children.len
		t.a.children << zero
		return t.a.add_node(flat.Node{
			kind:           .prefix
			op:             .amp
			children_start: start
			children_count: 1
		})
	}
	if node.kind == .for_in_stmt && node.children_count >= 3 && fm.is_option
		&& fm.comptime_typ.starts_with('?')
		&& t.direct_reflected_field_selector(t.a.child(&node, 2), var_name) {
		mut children := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			child := t.clone_field_subst_scoped(t.a.child(&node, i), var_name, fm, inner_vars) or {
				flat.empty_node
			}
			if i == 2 {
				children << t.make_selector(child, 'value', fm.comptime_typ[1..])
			} else {
				children << child
			}
		}
		start := t.a.children.len
		for child in children {
			t.a.children << child
		}
		return t.a.add_node(flat.Node{
			kind:           node.kind
			kind_id:        node.kind_id
			op:             node.op
			pos:            node.pos
			value:          node.value
			typ:            node.typ
			is_mut:         node.is_mut
			children_start: start
			children_count: flat.child_count(children.len)
		})
	}
	if node.kind == .call && node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.value == 'str' && callee.children_count > 0 {
			base := t.a.child_node(callee, 0)
			if base.kind == .ident && base.value == var_name {
				return t.make_string_literal('FieldData{name: ${fm.name}, typ: ${fm.typ}}')
			}
		}
	}
	// `typeof(<var>)` / `typeof(<var>).name` / `typeof(<var>).idx`: the field's own type,
	// not the FieldData metadata struct.
	if node.kind == .typeof_expr && t.typeof_arg_is_var(id, var_name) {
		return t.make_string_literal(fm.typ)
	}
	if node.kind == .selector && node.children_count > 0
		&& t.typeof_arg_is_var(t.a.child(&node, 0), var_name) {
		match node.value {
			'name' { return t.make_string_literal(fm.typ) }
			'unaliased_typ' { return t.make_string_literal(fm.unaliased_typ) }
			'idx' { return t.make_int_literal(fm.typ_id) }
			else {}
		}
	}
	// `<var>.member` compile-time member access.
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			if v := t.field_member_value(node.value, fm) {
				return v
			}
			// Unknown FieldData member (e.g. a typo): leave the selector unresolved so it
			// surfaces as an error instead of silently becoming the field name.
			return t.clone_field_subst_children(node, var_name, fm, inner_vars)
		}
		// `receiver.$(<var>.name)` compile-time field selector - only fold when the name
		// expression is *this* loop variable's `.name`. A nested loop's `$(inner.name)` is left
		// untouched so its own unroll pass resolves it against the right field.
		if node.value == '$' && node.children_count >= 2
			&& t.dollar_selector_names_var(t.a.child(&node, 1), var_name) {
			receiver := t.clone_field_subst_scoped(t.a.child(&node, 0), var_name, fm, inner_vars) or {
				return none
			}
			return t.make_selector(receiver, fm.name, fm.comptime_typ)
		}
	}
	// `$if`/`$else $if` referencing the loop variable: evaluate now, keep the taken branch.
	if node.kind == .comptime_if {
		substituted := t.subst_field_cond(node.value, var_name, fm)
		cond := t.subst_reflected_field_selector_cond(node.value, substituted, var_name, fm)
		if (comptime_cond_references_ident(node.value, var_name)
			|| cond != node.value || comptime_cond_is_static_literal_expr(cond))
			&& !comptime_cond_has_loop_member_ref(cond, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_field_subst_scoped(t.a.child(&node, branch_idx), var_name, fm,
					inner_vars)
			}
		}
		return t.clone_field_subst_children_with_value(node, var_name, fm, inner_vars, cond)
	}
	return t.clone_field_subst_children(node, var_name, fm, inner_vars)
}

// direct_reflected_field_selector reports whether an iterable is exactly
// `receiver.$(<var>.name)` (with optional parentheses). Expressions that merely contain that
// selector, such as `receiver.$(<var>.name) or { fallback }`, already produce an unwrapped value.
fn (t &Transformer) direct_reflected_field_selector(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count == 1 {
		return t.direct_reflected_field_selector(t.a.child(&node, 0), var_name)
	}
	return node.kind == .selector && node.value == '$' && node.children_count >= 2
		&& t.dollar_selector_names_var(t.a.child(&node, 1), var_name)
}

fn (t &Transformer) subtree_has_reflected_typeof_idx(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector && node.value == 'idx' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .typeof_expr && t.subtree_references_var(t.a.child(&node, 0), var_name) {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_reflected_typeof_idx(t.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

// subst_reflected_field_selector_cond resolves type membership for an actual dynamic reflected
// selector. For the legacy static form (`val.test in [...]` inside the `test` iteration), it
// resolves the receiver's field type instead of assuming that every `.test` belongs to `fm`.
fn (t &Transformer) subst_reflected_field_selector_cond(original string, substituted string, var_name string, fm FieldMeta) string {
	clean := original.trim_space()
	for op in [' !in', ' in'] {
		if op_idx := comptime_top_index(clean, op) {
			left := clean[..op_idx].trim_space()
			mut selector_type := ''
			if comptime_reflected_selector_left(left, var_name) {
				selector_type = fm.comptime_typ
			} else {
				static_suffix := '.${fm.name}'
				if !left.ends_with(static_suffix) {
					continue
				}
				receiver := left[..left.len - static_suffix.len].trim_space()
				if !comptime_plain_ident(receiver) {
					continue
				}
				receiver_type := t.var_type(receiver).trim_space().trim_left('&')
				selector_type = t.lookup_struct_field_type(receiver_type, fm.name) or { continue }
			}
			resolved := substituted.trim_space()
			if resolved_op_idx := comptime_top_index(resolved, op) {
				return selector_type + resolved[resolved_op_idx..]
			}
		}
	}
	return substituted
}

fn comptime_reflected_selector_left(left string, var_name string) bool {
	if !left.ends_with(')') {
		return false
	}
	open_idx := left.last_index('.$(') or { return false }
	return comptime_cond_references_ident(left, var_name)
		&& left[open_idx + 3..left.len - 1].trim_space() == '${var_name}.name'
}

fn comptime_plain_ident(value string) bool {
	if value.len == 0 || !(value[0].is_letter() || value[0] == `_`) {
		return false
	}
	for c in value {
		if !(c.is_letter() || c.is_digit() || c == `_`) {
			return false
		}
	}
	return true
}

// typeof_arg_is_var reports whether `id` is a `typeof(<var_name>)` expression over the
// comptime loop variable.
fn (t &Transformer) typeof_arg_is_var(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .typeof_expr || node.children_count == 0 {
		return false
	}
	arg := t.a.child_node(&node, 0)
	return arg.kind == .ident && arg.value == var_name
}

// dollar_selector_names_var reports whether a `$(...)` selector's name expression is the current
// field loop's `<var>.name`, so only the matching loop rewrites the selector and a nested loop's
// `$(inner.name)` is deferred to its own unroll.
fn (t &Transformer) dollar_selector_names_var(name_id flat.NodeId, var_name string) bool {
	if int(name_id) < 0 {
		return false
	}
	name_expr := t.a.nodes[int(name_id)]
	if name_expr.kind != .selector || name_expr.value != 'name' || name_expr.children_count == 0 {
		return false
	}
	base := t.a.child_node(&name_expr, 0)
	return base.kind == .ident && base.value == var_name
}

fn (mut t Transformer) clone_field_subst_children(node flat.Node, var_name string, fm FieldMeta, inner_vars []string) ?flat.NodeId {
	return t.clone_field_subst_children_with_value(node, var_name, fm, inner_vars, node.value)
}

fn (mut t Transformer) clone_field_subst_children_with_value(node flat.Node, var_name string, fm FieldMeta, inner_vars []string, value string) ?flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		if c := t.clone_field_subst_scoped(t.a.child(&node, i), var_name, fm, child_inner_vars) {
			children << c
		}
	}
	cloned_value := if node.kind == .comptime_if {
		value
	} else {
		t.comptime_field_call_generic_args(node, mut children, fm)
	}
	mut typ := t.clone_field_subst_type_text(node, var_name, fm)
	if node.kind == .prefix && children.len == 1 {
		child_type := t.node_type(children[0])
		if child_type.len > 0 && child_type !in ['unknown', 'generic'] {
			if node.op == .amp {
				typ = '&${child_type}'
			} else if node.op == .mul && child_type.starts_with('&') {
				typ = child_type[1..]
			}
		}
	}
	if node.kind == .call && children.len > 0 {
		callee := t.a.nodes[int(children[0])]
		if callee.kind == .ident {
			if ret := t.fn_ret_types[callee.value] {
				typ = ret
			} else if !isnil(t.tc) {
				if ret := t.tc.fn_ret_types[callee.value] {
					typ = ret.name()
				}
			}
		}
	}
	if node.kind == .decl_assign && children.len >= 2 {
		rhs_typ := t.node_type(children[1])
		if rhs_typ.len > 0 && rhs_typ !in ['unknown', 'generic'] {
			typ = rhs_typ
			t.set_node_typ(int(children[0]), typ)
		}
	}
	start := t.a.children.len
	for c in children {
		t.a.children << c
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          cloned_value
		typ:            typ
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn (mut t Transformer) comptime_field_type_accessor(id flat.NodeId, var_name string, fm FieldMeta) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == var_name {
		return fm.comptime_typ
	}
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := t.a.child_node(&node, 0)
	if base.kind == .ident && base.value == var_name {
		return match node.value {
			'typ' { fm.comptime_typ }
			'unaliased_typ' { fm.comptime_unaliased }
			else { none }
		}
	}
	inner := t.comptime_field_type_accessor(t.a.child(&node, 0), var_name, fm) or { return none }
	return match node.value {
		'payload_type' {
			if inner.starts_with('?') || inner.starts_with('!') {
				inner[1..]
			} else {
				inner
			}
		}
		'pointee_type' {
			inner.trim_left('&')
		}
		else {
			none
		}
	}
}

fn (mut t Transformer) comptime_new_value(typ string) flat.NodeId {
	zero := t.zero_value_for_type(typ)
	addr := t.make_prefix(.amp, zero)
	dup := t.make_call_typed('memdup', arr2(addr, t.make_sizeof_type(typ)), 'voidptr')
	return t.make_cast('&${typ}', dup, '&${typ}')
}

fn (t &Transformer) clone_field_subst_type_text(node flat.Node, var_name string, fm FieldMeta) string {
	if node.kind != .comptime_for {
		return node.typ
	}
	mut typ := node.typ
	typ = typ.replace('${var_name}.unaliased_typ', fm.comptime_unaliased)
	typ = typ.replace('${var_name}.typ', fm.comptime_typ)
	return comptime_cond_replace_bare_ident(typ, var_name, fm.comptime_typ)
}

// field_member_value replaces `<var>.member` with its concrete compile-time value. Returns none
// for an unknown member (e.g. a typo `field.nmae`) so the caller leaves it unresolved rather than
// silently substituting the field name.
fn (mut t Transformer) field_member_value(member string, fm FieldMeta) ?flat.NodeId {
	return match member {
		'name' { t.make_string_literal(fm.name) }
		'is_option', 'is_opt' { t.make_bool_literal(fm.is_option) }
		'is_embed' { t.make_bool_literal(fm.is_embed) }
		'is_array' { t.make_bool_literal(fm.is_array) }
		'is_map' { t.make_bool_literal(fm.is_map) }
		'is_chan' { t.make_bool_literal(fm.is_chan) }
		'is_struct' { t.make_bool_literal(fm.is_struct) }
		'is_enum' { t.make_bool_literal(fm.is_enum) }
		'is_alias' { t.make_bool_literal(fm.is_alias) }
		'is_shared' { t.make_bool_literal(fm.is_shared) }
		'is_atomic' { t.make_bool_literal(fm.is_atomic) }
		'is_mut' { t.make_bool_literal(fm.is_mut) }
		'is_pub' { t.make_bool_literal(fm.is_pub) }
		'indirections' { t.make_int_literal(fm.indirections) }
		'attrs' { t.make_string_array_literal(fm.attrs) }
		'typ' { t.make_int_literal(fm.typ_id) }
		'unaliased_typ' { t.make_int_literal(fm.unaliased_id) }
		else { return none }
	}
}

// make_string_array_literal builds a `[]string` literal (empty when `values` is empty) for
// materializing `field.attrs`.
fn (mut t Transformer) make_string_array_literal(values []string) flat.NodeId {
	if values.len == 0 {
		return t.zero_value_for_type('[]string')
	}
	mut ids := []flat.NodeId{cap: values.len}
	for v in values {
		ids << t.make_string_literal(v)
	}
	return t.make_array_literal_typed(ids, '[]string')
}

// subst_value_cond textually substitutes `<var>.name`/`<var>.value` inside a comptime condition
// string for a `$for value in Enum.values` iteration, so `eval_field_cond` can fold it.
fn (t &Transformer) subst_value_cond(cond string, var_name string, name string, value i64) string {
	mut c := cond
	c = c.replace('${var_name}.value', value.str())
	c = c.replace('${var_name}.name', "'${name}'")
	return c
}

// subst_field_cond textually substitutes `<var>.member` and bare `<var>` type-guard shorthand
// inside a comptime condition string. Longer members are replaced first so `.typ` does not
// clobber `.unaliased_typ`.
fn (t &Transformer) subst_field_cond(cond string, var_name string, fm FieldMeta) string {
	if !cond.contains("'") && !cond.contains('"') {
		return t.subst_unquoted_field_cond(cond, var_name, fm)
	}
	mut result := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			result += cond[offset..end]
			offset = end
			continue
		}
		start := offset
		for offset < cond.len && cond[offset] !in [`'`, `"`] {
			offset++
		}
		result += t.subst_unquoted_field_cond(cond[start..offset], var_name, fm)
	}
	return result
}

fn (t &Transformer) subst_unquoted_field_cond(cond string, var_name string, fm FieldMeta) string {
	mut c := cond
	if t.cur_module.len > 0 {
		c = c.replace('${t.cur_module}.${var_name}', var_name)
	}
	c = c.replace('${var_name}.unaliased_typ', fm.comptime_unaliased)
	c = c.replace('${var_name}.indirections', fm.indirections.str())
	c = c.replace('${var_name}.is_option', fm.is_option.str())
	c = c.replace('${var_name}.is_opt', fm.is_option.str())
	c = c.replace('${var_name}.is_embed', fm.is_embed.str())
	c = c.replace('${var_name}.is_array', fm.is_array.str())
	c = c.replace('${var_name}.is_map', fm.is_map.str())
	c = c.replace('${var_name}.is_chan', fm.is_chan.str())
	c = c.replace('${var_name}.is_struct', fm.is_struct.str())
	c = c.replace('${var_name}.is_enum', fm.is_enum.str())
	c = c.replace('${var_name}.is_alias', fm.is_alias.str())
	c = c.replace('${var_name}.is_shared', fm.is_shared.str())
	c = c.replace('${var_name}.is_atomic', fm.is_atomic.str())
	c = c.replace('${var_name}.is_mut', fm.is_mut.str())
	c = c.replace('${var_name}.is_pub', fm.is_pub.str())
	c = c.replace('${var_name}.typ', fm.comptime_typ)
	c = c.replace('${var_name}.name', "'${fm.name}'")
	c = comptime_cond_replace_bare_ident(c, var_name, fm.comptime_typ)
	return c
}

fn comptime_cond_replace_unquoted(cond string, needle string, replacement string) string {
	if needle.len == 0 || !cond.contains(needle) {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + needle.len <= cond.len && cond[offset..offset + needle.len] == needle {
			before_ok := !comptime_cond_name_char(needle[0]) || offset == 0
				|| !comptime_cond_name_char(cond[offset - 1])
			after := offset + needle.len
			after_ok := !comptime_cond_name_char(needle[needle.len - 1]) || after >= cond.len
				|| !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset].ascii_str()
		offset++
	}
	return out
}

fn comptime_cond_replace_bare_ident(cond string, ident string, replacement string) string {
	if ident.len == 0 {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + ident.len <= cond.len && cond[offset..offset + ident.len] == ident {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + ident.len
			after_ok := after >= cond.len
				|| (!comptime_cond_name_char(cond[after]) && cond[after] != `.`)
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset..offset + 1]
		offset++
	}
	return out
}

fn comptime_cond_references_ident(cond string, ident string) bool {
	if ident.len == 0 {
		return false
	}
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + ident.len <= cond.len && cond[offset..offset + ident.len] == ident {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + ident.len
			after_ok := after >= cond.len || !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				return true
			}
		}
		offset++
	}
	return false
}

fn comptime_cond_has_loop_member_ref(cond string, var_name string) bool {
	prefix := '${var_name}.'
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + prefix.len > cond.len || cond[offset..offset + prefix.len] != prefix {
			offset++
			continue
		}
		if offset > 0 && comptime_cond_name_char(cond[offset - 1]) {
			offset++
			continue
		}
		member_start := offset + prefix.len
		if member_start < cond.len && comptime_cond_name_char(cond[member_start]) {
			return true
		}
		offset = member_start
	}
	return false
}

fn comptime_cond_has_any_loop_member_ref(cond string, var_names []string) bool {
	for var_name in var_names {
		if comptime_cond_has_loop_member_ref(cond, var_name) {
			return true
		}
	}
	return false
}

fn comptime_nested_loop_vars(node flat.Node, var_name string, inner_vars []string) []string {
	if node.kind != .comptime_for {
		return inner_vars
	}
	loop_var, _ := comptime_for_parts(node.value)
	if loop_var == var_name || loop_var in inner_vars {
		return inner_vars
	}
	mut scoped_vars := inner_vars.clone()
	scoped_vars << loop_var
	return scoped_vars
}

fn comptime_cond_skip_string(cond string, start int) int {
	quote := cond[start]
	mut i := start + 1
	for i < cond.len {
		if cond[i] == `\\` {
			i += 2
			continue
		}
		if cond[i] == quote {
			return i + 1
		}
		i++
	}
	return cond.len
}

fn comptime_cond_name_char(ch u8) bool {
	return ch.is_letter() || ch.is_digit() || ch == `_`
}

fn comptime_cond_is_static_literal_expr(cond string) bool {
	clean := comptime_condition_strip_outer_parens(cond.trim_space())
	for op in ['||', '&&'] {
		if op_idx := comptime_top_index(clean, op) {
			return comptime_cond_is_static_literal_expr(clean[..op_idx])
				&& comptime_cond_is_static_literal_expr(clean[op_idx + op.len..])
		}
	}
	if clean.starts_with('!') {
		return comptime_cond_is_static_literal_expr(clean[1..])
	}
	if clean in ['true', 'false'] {
		return true
	}
	for op in ['!=', '=='] {
		if op_idx := comptime_top_index(clean, op) {
			return comptime_cond_is_quoted_literal(clean[..op_idx])
				&& comptime_cond_is_quoted_literal(clean[op_idx + op.len..])
		}
	}
	return false
}

fn comptime_cond_is_quoted_literal(value string) bool {
	clean := value.trim_space()
	return clean.len >= 2 && clean[0] in [`'`, `"`, `\``] && clean[clean.len - 1] == clean[0]
}

// eval_field_cond evaluates a fully-substituted comptime condition (`is`/`!is`, `==`/`!=`,
// `&&`/`||`/`!`, bare bool). Returns none when it cannot be decided statically.
fn (mut t Transformer) eval_field_cond(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond.trim_space())
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	if or_idx := comptime_top_index(clean, '||') {
		left := t.eval_field_cond(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return t.eval_field_cond(clean[or_idx + 2..])
	}
	if and_idx := comptime_top_index(clean, '&&') {
		left := t.eval_field_cond(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return t.eval_field_cond(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		if op_idx := comptime_top_index(clean, op) {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			matches := if left.starts_with('$') && !right.starts_with('$') {
				t.comptime_type_matches(right, left) or { return none }
			} else {
				t.comptime_type_matches(left, right) or { return none }
			}
			return if op == ' is ' { matches } else { !matches }
		}
	}
	for op in [' != ', ' == '] {
		if op_idx := comptime_top_index(clean, op) {
			// String operands may be quoted on one side (`'txt'` from a substituted `field.name`)
			// and bare on the other (`txt` as captured in the condition); compare unquoted.
			left := comptime_unquote(clean[..op_idx].trim_space())
			right := comptime_unquote(clean[op_idx + op.len..].trim_space())
			eq := left == right
			return if op == ' == ' { eq } else { !eq }
		}
	}
	// `field.name in ['id', 'name']` membership. The serialized condition drops the space
	// between `in` and the list literal (`in[...]`), so match the operator token and accept a
	// following space, `[`, or `(`.
	for op in [' !in', ' in'] {
		if op_idx := comptime_top_index(clean, op) {
			after := op_idx + op.len
			if after < clean.len && clean[after] != ` ` && clean[after] != `[`
				&& clean[after] != `(` {
				continue
			}
			needle := comptime_unquote(clean[..op_idx].trim_space())
			found := comptime_list_contains(clean[after..].trim_space(), needle)
			return if op == ' in' { found } else { !found }
		}
	}
	// Integer ordering (e.g. `field.indirections < 2`); longer operators first.
	for op in [' <= ', ' >= ', ' < ', ' > '] {
		if op_idx := comptime_top_index(clean, op) {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			if !comptime_is_int(left) || !comptime_is_int(right) {
				return none
			}
			l := left.int()
			r := right.int()
			return match op {
				' <= ' { l <= r }
				' >= ' { l >= r }
				' < ' { l < r }
				else { l > r }
			}
		}
	}
	if clean.starts_with('!') {
		inner := t.eval_field_cond(clean[1..]) or { return none }
		return !inner
	}
	return none
}

fn comptime_list_contains(list_text string, needle string) bool {
	clean := list_text.trim_space()
	if !clean.starts_with('[') || !clean.ends_with(']') {
		return false
	}
	inner := clean[1..clean.len - 1]
	for part in inner.split(',') {
		if comptime_unquote(part.trim_space()) == needle {
			return true
		}
	}
	return false
}

fn comptime_is_int(s string) bool {
	if s.len == 0 {
		return false
	}
	start := if s[0] == `-` || s[0] == `+` { 1 } else { 0 }
	if start >= s.len {
		return false
	}
	for i in start .. s.len {
		if !s[i].is_digit() {
			return false
		}
	}
	return true
}

fn comptime_unquote(s string) string {
	if s.len >= 2 && (s[0] == `'` || s[0] == `"`) && s[s.len - 1] == s[0] {
		return comptime_cond_unescape(s[1..s.len - 1])
	}
	return s
}

fn comptime_cond_unescape(value string) string {
	if !value.contains('\\') {
		return value
	}
	mut out := strings.new_builder(value.len)
	mut i := 0
	for i < value.len {
		if value[i] != `\\` || i + 1 >= value.len {
			out.write_u8(value[i])
			i++
			continue
		}
		next := value[i + 1]
		if next == `\n` {
			i += 2
			for i < value.len && value[i] in [` `, `\t`, `\r`] {
				i++
			}
			continue
		}
		if next == `\r` && i + 2 < value.len && value[i + 2] == `\n` {
			i += 3
			for i < value.len && value[i] in [` `, `\t`] {
				i++
			}
			continue
		}
		if next == `x` && i + 3 < value.len {
			if code := comptime_cond_fixed_hex(value, i + 2, 2) {
				out.write_u8(u8(code))
				i += 4
				continue
			}
		}
		if next == `u` && i + 5 < value.len {
			if code := comptime_cond_fixed_hex(value, i + 2, 4) {
				out.write_rune(rune(code))
				i += 6
				continue
			}
		}
		if next == `U` && i + 9 < value.len {
			if code := comptime_cond_fixed_hex(value, i + 2, 8) {
				out.write_rune(rune(code))
				i += 10
				continue
			}
		}
		decoded := match next {
			`n` { int(`\n`) }
			`t` { int(`\t`) }
			`r` { int(`\r`) }
			`\\` { int(`\\`) }
			`'` { int(`'`) }
			`"` { int(`"`) }
			`$` { int(`$`) }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			else { -1 }
		}

		if decoded >= 0 {
			out.write_u8(u8(decoded))
		} else {
			out.write_u8(`\\`)
			out.write_u8(next)
		}
		i += 2
	}
	return out.str()
}

fn comptime_cond_fixed_hex(value string, start int, count int) ?u32 {
	mut code := u32(0)
	for i in 0 .. count {
		if start + i >= value.len {
			return none
		}
		digit := comptime_cond_hex_digit(value[start + i]) or { return none }
		code = (code << 4) | digit
	}
	return code
}

fn comptime_cond_hex_digit(c u8) ?u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u32(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u32(c - `A` + 10)
	}
	return none
}

fn comptime_top_index(s string, op string) ?int {
	idx := comptime_condition_top_level_index(s, op)
	if idx >= 0 {
		return idx
	}
	return none
}
