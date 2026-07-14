module transform

import v3.flat
import v3.types

struct GenericStructDecl {
	id     flat.NodeId
	node   flat.Node
	file   string
	module string
	key    string
}

struct GenericSumDecl {
	id     flat.NodeId
	node   flat.Node
	file   string
	module string
	key    string
}

struct GenericSpecContext {
	base   string
	file   string
	module string
}

struct GenericCallSite {
	id     flat.NodeId
	module string
}

fn (mut t Transformer) is_generic_fn(name string) bool {
	if t.skip_generics {
		return false
	}
	decls := t.cached_generic_fn_decls()
	return name in decls || transform_qualified_fn_name(t.cur_module, name) in decls
}

fn (mut t Transformer) is_generic_struct(name string) bool {
	if t.skip_generics {
		return false
	}
	base, _, ok := generic_app_parts(name)
	if ok {
		return base in t.structs
	}
	if info := t.lookup_struct_info(name) {
		return info.name.contains('[')
	}
	return false
}

fn (mut t Transformer) monomorphize_pass() []string {
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return []string{}
	}
	struct_decls := t.collect_generic_struct_decls()
	t.seed_generic_specialization_args(struct_decls)
	mut ignored_nodes := t.monomorphize_ignored_nodes(decls)
	mut emitted := map[string]bool{}
	mut generated := []string{}
	mut generic_call_sites := []GenericCallSite{}
	mut recorded_call_sites := map[int]bool{}
	mut changed := true
	mut scan_start := 0
	mut used_fns_at_scan := t.used_fns.len
	t.in_monomorphize_scan = true
	defer {
		t.in_monomorphize_scan = false
	}
	for changed {
		changed = false
		t.ensure_node_module_map()
		node_count := t.a.nodes.len
		// A previous round can mark an until-then unused fn as used (a clone
		// or comptime unroll now calls it); its subtree left the ignore set
		// and must be scanned too, or its generic calls stay unspecialized.
		mut rescan := []int{}
		if scan_start > 0 && t.used_fns.len != used_fns_at_scan {
			new_ignored := t.monomorphize_ignored_nodes(decls)
			for i in 0 .. scan_start {
				if i < ignored_nodes.len && ignored_nodes[i] && !(i < new_ignored.len
					&& new_ignored[i]) {
					rescan << i
				}
			}
			ignored_nodes = unsafe { new_ignored }
		}
		used_fns_at_scan = t.used_fns.len
		for scan_idx in 0 .. rescan.len + (node_count - scan_start) {
			i := if scan_idx < rescan.len {
				rescan[scan_idx]
			} else {
				scan_start + (scan_idx - rescan.len)
			}
			if (i < ignored_nodes.len && ignored_nodes[i])
				|| (i < t.ignored_comptime_for_nodes.len && t.ignored_comptime_for_nodes[i]) {
				continue
			}
			node := t.a.nodes[i]
			match node.kind {
				.call {
					// An infix operator on a generic instance was already lowered to a
					// direct call (`Vec_int__plus(a, b)`) before this pass. Record the
					// callee so `specialize_generic_struct_methods` emits an operator
					// overload only for instances whose operator is actually called.
					t.record_called_fn_name(node)
					call_module := t.node_module_or(i, '')
					decl_key, args := t.cached_generic_call_specialization(flat.NodeId(i), node,
						call_module, decls) or { continue }
					decl := decls[decl_key] or { continue }
					if !t.call_has_source_generic_args(node)
						&& t.generic_args_contain_alias(args, decl.module) {
						t.set_node_value(i, args.join(', '))
					}
					if i !in recorded_call_sites {
						generic_call_sites << GenericCallSite{
							id:     flat.NodeId(i)
							module: call_module
						}
						recorded_call_sites[i] = true
					}
					concrete_args := t.canonical_generic_specialization_args(args)
					spec_key := generic_fn_spec_key(decl_key, concrete_args)
					if emitted[spec_key] {
						continue
					}
					generated << t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl,
						concrete_args), concrete_args)
					emitted[spec_key] = true
					changed = true
				}
				else {}
			}
		}
		scan_start = node_count
		// Specialize methods of instantiated generic structs that are never reached
		// through an explicit call node — notably operator overloads (`a + b`), which
		// are infix expressions lowered to method calls only later in the pipeline.
		if t.specialize_generic_struct_methods(struct_decls, decls, mut emitted, mut generated) {
			changed = true
		}
	}
	t.rewrite_generic_call_sites(decls, generic_call_sites)
	if t.refresh_decl_assign_types_after_generic_rewrite() {
		t.generic_call_spec_cache = map[int]GenericCallSpec{}
		t.generic_call_spec_misses = map[int]bool{}
		extra_call_sites := t.collect_generic_call_sites_after_type_refresh(decls, ignored_nodes, mut
			emitted, mut generated, mut recorded_call_sites)
		if extra_call_sites.len > 0 {
			t.rewrite_generic_call_sites(decls, extra_call_sites)
			t.refresh_decl_assign_types_after_generic_rewrite()
		}
	}
	t.erase_generic_fn_decls(decls)
	return generated
}

fn (mut t Transformer) collect_generic_call_sites_after_type_refresh(decls map[string]GenericFnDecl, ignored_nodes []bool, mut emitted map[string]bool, mut generated []string, mut recorded_call_sites map[int]bool) []GenericCallSite {
	mut sites := []GenericCallSite{}
	t.ensure_node_module_map()
	old_in_scan := t.in_monomorphize_scan
	t.in_monomorphize_scan = true
	defer {
		t.in_monomorphize_scan = old_in_scan
	}
	node_count := t.a.nodes.len
	for i in 0 .. node_count {
		if (i < ignored_nodes.len && ignored_nodes[i])
			|| (i < t.ignored_comptime_for_nodes.len && t.ignored_comptime_for_nodes[i]) {
			continue
		}
		node := t.a.nodes[i]
		if node.kind != .call {
			continue
		}
		call_module := t.node_module_or(i, '')
		decl_key, args := t.cached_generic_call_specialization(flat.NodeId(i), node, call_module,
			decls) or { continue }
		decl := decls[decl_key] or { continue }
		if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args, decl.module) {
			t.set_node_value(i, args.join(', '))
		}
		if i !in recorded_call_sites {
			sites << GenericCallSite{
				id:     flat.NodeId(i)
				module: call_module
			}
			recorded_call_sites[i] = true
		}
		concrete_args := t.canonical_generic_specialization_args(args)
		spec_key := generic_fn_spec_key(decl_key, concrete_args)
		if emitted[spec_key] {
			continue
		}
		generated << t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl,
			concrete_args), concrete_args)
		emitted[spec_key] = true
	}
	return sites
}

fn (mut t Transformer) seed_generic_specialization_args(decls map[string]GenericStructDecl) {
	if decls.len == 0 {
		return
	}
	specs, _ := t.collect_generic_materialization_specs(decls, map[string]GenericSumDecl{})
	for spec, base in specs {
		decl := decls[base] or { continue }
		_, args, ok := generic_app_parts(spec)
		if ok && args.len > 0 {
			t.record_generic_specialization_args_in_module(base, decl.module, args)
		}
	}
}

pub fn erase_generic_templates(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	mut t := new_transformer(mut a, tc, used_fns)
	decls := t.collect_generic_fn_decls_for_erasure()
	if decls.len != 0 {
		keep := t.building_v_type_erased_generic_keep_set(decls)
		mut erased := map[string]GenericFnDecl{}
		for key, decl in decls {
			if keep[key] {
				continue
			}
			erased[key] = decl
		}
		t.erase_generic_fn_decls(erased)
	}
	return t.used_fns
}

fn (mut t Transformer) collect_generic_fn_decls_for_erasure() map[string]GenericFnDecl {
	mut decls := map[string]GenericFnDecl{}
	mut cur_file := ''
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if !t.generic_fn_decl_needs_erasure_scan(node, cur_module) {
					continue
				}
				key := t.generic_fn_decl_key(node, cur_module)
				decls[key] = GenericFnDecl{
					id:     flat.NodeId(i)
					node:   node
					file:   cur_file
					module: cur_module
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn (mut t Transformer) building_v_type_erased_generic_keep_set(decls map[string]GenericFnDecl) map[string]bool {
	mut keep := map[string]bool{}
	mut queue := []string{}
	for key, _ in decls {
		if building_v_type_erased_generic_keep_root(key) {
			keep[key] = true
			queue << key
		}
	}
	if queue.len == 0 {
		return keep
	}
	t.generic_fn_decls_cache = decls.clone()
	t.build_generic_receiver_method_index()
	mut cursor := 0
	for cursor < queue.len {
		key := queue[cursor]
		cursor++
		decl := decls[key] or { continue }
		mut called := map[string]bool{}
		t.collect_type_erased_generic_template_calls(decl.id, decl.module, decls, mut called)
		for called_key, _ in called {
			if called_key !in keep {
				keep[called_key] = true
				queue << called_key
			}
		}
	}
	return keep
}

fn (mut t Transformer) collect_type_erased_generic_template_calls(id flat.NodeId, module_name string, decls map[string]GenericFnDecl, mut called map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		if key := t.type_erased_generic_template_call_decl_key(id, node, module_name, decls) {
			called[key] = true
		}
	}
	for i in 0 .. node.children_count {
		t.collect_type_erased_generic_template_calls(t.a.child(&node, i), module_name, decls, mut
			called)
	}
}

fn (mut t Transformer) type_erased_generic_template_call_decl_key(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	if key := t.generic_call_decl_key(id, node, module_name, decls) {
		return key
	}
	if node.children_count == 0 {
		return none
	}
	mut callee := t.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee = t.a.child_node(callee, 0)
	}
	if callee.kind == .ident {
		for candidate in t.generic_plain_call_candidates(callee.value, module_name) {
			if key := t.type_erased_generic_template_candidate_key(candidate, node, decls) {
				return key
			}
		}
	} else if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		if base.kind == .ident {
			for candidate in ['${base.value}.${callee.value}',
				'${t.import_alias_module(base.value)}.${callee.value}'] {
				if key := t.type_erased_generic_template_candidate_key(candidate, node, decls) {
					return key
				}
			}
		}
	}
	return none
}

fn (t &Transformer) type_erased_generic_template_candidate_key(candidate string, node flat.Node, decls map[string]GenericFnDecl) ?string {
	key := generic_fn_decl_base_value(candidate)
	decl := decls[key] or { return none }
	if !t.generic_call_arg_count_matches_decl(node, decl) {
		return none
	}
	return key
}

fn (mut t Transformer) generic_fn_decl_needs_erasure_scan(node flat.Node, module_name string) bool {
	if generic_params_have_runtime_type_param(node.generic_params)
		|| t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.type_text_has_generic_placeholder(node.value, module_name) {
		return true
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .param && t.type_text_has_generic_placeholder(child.typ, module_name) {
			return true
		}
	}
	return false
}

fn building_v_type_erased_generic_keep_root(key string) bool {
	return key in ['token.new_keywords_matcher_trie', 'sync.pool.PoolProcessor.work_on_items']
}

// is_operator_method_name reports whether a method-name part is an overloaded
// operator symbol (`+`, `-`, `*`, `/`, `%`, `==`, `<`, `>`, `<=`, `>=`).
fn is_operator_method_name(name string) bool {
	return name in ['+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>=']
}

// record_called_fn_name records the callee name of a direct call so operator overloads
// (lowered to direct calls before this pass) can be specialized only when actually
// called. Operator method names are mangled (`Vec_int__plus`), matching the name
// `specialize_generic_struct_methods` would emit for that instance and operator.
fn (mut t Transformer) record_called_fn_name(node flat.Node) {
	if node.children_count < 1 {
		return
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind == .ident && fn_node.value.len > 0 {
		t.used_struct_operator_fns[fn_node.value] = true
	}
}

// specialize_generic_struct_methods specializes every generic method of each
// instantiated generic struct (e.g. for `Vec4[f32]`, generate `vec__Vec4_f32__plus`,
// `vec__Vec4_f32__one`, ...). It only handles methods whose generic parameters are
// exactly the struct's parameters (no extra method-level `[U]`); those are left to
// the call-driven path. Returns whether any new specialization was emitted.
fn (mut t Transformer) specialize_generic_struct_methods(struct_decls map[string]GenericStructDecl, decls map[string]GenericFnDecl, mut emitted map[string]bool, mut generated []string) bool {
	if !t.needs_generic_struct_method_specialization(decls) {
		return false
	}
	mut specs := map[string]string{}
	t.collect_generic_struct_specs(struct_decls, mut specs)
	mut any := false
	for spec, base in specs {
		_, args, ok := generic_app_parts(spec)
		if !ok || args.len == 0 {
			continue
		}
		for decl_key, decl in decls {
			if !decl_key.contains('.') || decl_key.all_before_last('.') != base {
				continue
			}
			// Only operator overloads need struct-instantiation-driven specialization:
			// they are reached through infix expressions, not call nodes. Regular
			// methods are specialized on demand by the call-driven path, so emitting
			// every method here would generate unused (and possibly unresolved) bodies.
			// The exception is a method used as a *value* (`b.method`): it has no call
			// node to drive specialization, so specialize the ones the checker recorded.
			method := decl_key.all_after_last('.')
			if is_operator_method_name(method) {
				// Specialize an operator overload only for an instance whose operator is
				// actually applied somewhere (recorded by record_used_struct_operator).
				// Otherwise a stored-but-never-operated instance whose type argument does
				// not support the operation would emit a body that fails C compilation.
				if c_name('${spec}.${method}') !in t.used_struct_operator_fns {
					continue
				}
			} else if !t.generic_struct_method_needed_for_interface(spec, method) {
				mvkey := '${spec}.${method}'
				if !t.generic_struct_method_used_for_spec(spec, decl, args, method) {
					if isnil(t.tc) || mvkey !in t.tc.generic_method_value_info {
						continue
					}
					// Only specialize a method value the checker recorded inside a *reachable*
					// function: markused seeds the concrete instance key (`Box[int].report`) into
					// `used_fns` per reachable function, so one used only in dead code is skipped —
					// its body may be invalid for that type argument and would fail C compilation.
					if t.used_fns.len > 0 && mvkey !in t.used_fns && c_name(mvkey) !in t.used_fns {
						continue
					}
				}
			}
			if t.generic_decl_has_method_level_params(decl) {
				// Method has its own generic parameters beyond the struct's; leave it
				// to the call-driven specialization which can infer them.
				continue
			}
			concrete_args := t.canonical_generic_specialization_args(args)
			spec_key := generic_fn_spec_key(decl_key, concrete_args)
			if emitted[spec_key] {
				continue
			}
			generated << t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl,
				concrete_args), concrete_args)
			emitted[spec_key] = true
			any = true
		}
	}
	return any
}

fn (t &Transformer) generic_struct_spec_has_emitted_method(base string, args []string, decls map[string]GenericFnDecl, emitted map[string]bool) bool {
	concrete_args := t.canonical_generic_specialization_args(args)
	for decl_key, _ in decls {
		if !decl_key.contains('.') || decl_key.all_before_last('.') != base {
			continue
		}
		if emitted[generic_fn_spec_key(decl_key, concrete_args)] {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_struct_method_used_for_spec(spec string, decl GenericFnDecl, args []string, method string) bool {
	mut aliases := ['${spec}.${method}', c_name('${spec}.${method}')]
	aliases << specialized_generic_fn_signature_aliases(decl, args)
	receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	if receiver.len > 0 {
		short_flat := '${receiver}_${generic_type_suffixes(args)}'
		qualified_short_flat := transform_qualified_fn_name(decl.module, short_flat)
		full_flat := '${receiver}_${generic_type_full_suffixes(args)}'
		qualified_full_flat := transform_qualified_fn_name(decl.module, full_flat)
		for flat_receiver in [short_flat, qualified_short_flat, full_flat, qualified_full_flat] {
			aliases << '${flat_receiver}.${method}'
			aliases << c_name('${flat_receiver}.${method}')
		}
	}
	for alias in aliases {
		if alias in t.used_fns {
			return true
		}
	}
	return false
}

fn (t &Transformer) needs_generic_struct_method_specialization(decls map[string]GenericFnDecl) bool {
	if !isnil(t.tc) && t.tc.generic_method_value_info.len > 0 {
		return true
	}
	if !isnil(t.tc) && t.tc.interface_names.len > 0 {
		return true
	}
	for decl_key, _ in decls {
		if decl_key.contains('.') && is_operator_method_name(decl_key.all_after_last('.')) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) generic_struct_method_needed_for_interface(spec string, method string) bool {
	if isnil(t.tc) || spec.len == 0 || method.len == 0 {
		return false
	}
	for iface_name, _ in t.tc.interface_names {
		if method !in t.tc.interface_abstract_method_names(iface_name) {
			continue
		}
		if !t.tc.named_type_implements_interface(spec, iface_name) {
			continue
		}
		if !t.has_used_fn_filter()
			|| (t.interface_dispatch_method_used(iface_name, method)
			&& t.interface_boxed_type_used(iface_name, spec)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) interface_dispatch_method_used(iface_name string, method string) bool {
	name := '${iface_name}.${method}'
	if t.used_interface_dispatch_key(name) {
		return true
	}
	if decl_key := t.tc.interface_method_signature_key(iface_name, method) {
		if decl_key != name && t.used_interface_dispatch_key(decl_key) {
			return true
		}
		decl_short_name := '${decl_key.all_before_last('.').all_after_last('.')}.${method}'
		if decl_short_name != decl_key && t.interface_dispatch_short_name_allowed(iface_name)
			&& t.used_interface_dispatch_key(decl_short_name) {
			return true
		}
	}
	for alias in t.interface_alias_names(iface_name) {
		alias_name := '${alias}.${method}'
		if t.used_interface_dispatch_key(alias_name) {
			return true
		}
		short_alias_name := '${alias.all_after_last('.')}.${method}'
		if short_alias_name != alias_name && t.interface_dispatch_short_name_allowed(alias)
			&& t.used_interface_dispatch_key(short_alias_name) {
			return true
		}
	}
	short_name := '${iface_name.all_after_last('.')}.${method}'
	return short_name != name && t.interface_dispatch_short_name_allowed(iface_name)
		&& t.used_interface_dispatch_key(short_name)
}

fn (t &Transformer) interface_alias_names(iface_name string) []string {
	mut aliases := []string{}
	for alias, target in t.tc.type_aliases {
		qtarget := t.tc.qualify_name(target)
		if target == iface_name || qtarget == iface_name {
			aliases << alias
		}
	}
	return aliases
}

fn (t &Transformer) used_interface_dispatch_key(name string) bool {
	return t.used_fn_contains_name(name) || t.used_fn_contains_name(c_name(name))
}

fn (t &Transformer) interface_dispatch_short_name_allowed(iface_name string) bool {
	return !iface_name.contains('.')
}

fn (mut t Transformer) interface_boxed_type_used(iface_name string, concrete_type string) bool {
	t.collect_interface_boxed_types()
	return t.interface_boxed_type_marked(iface_name, concrete_type)
}

fn (t &Transformer) interface_boxed_type_marked(iface_name string, concrete_type string) bool {
	mut iface_names := [iface_name]
	short_name := iface_name.all_after_last('.')
	if short_name != iface_name {
		iface_names << short_name
	}
	resolved := t.resolve_interface_type_name(iface_name)
	if resolved.len > 0 && resolved != iface_name {
		iface_names << resolved
	}
	for iface in iface_names {
		if t.interface_boxed_types[interface_boxed_type_key(iface, concrete_type)]
			|| t.interface_boxed_types[interface_boxed_type_key(iface, c_name(concrete_type))] {
			return true
		}
	}
	return false
}

fn (mut t Transformer) collect_interface_boxed_types() {
	if t.interface_boxed_types_done || isnil(t.tc) {
		return
	}
	t.interface_boxed_types_done = true
	for idx, node in t.a.nodes {
		if node.kind == .fn_decl && t.interface_box_type_text_maybe(node.typ) {
			return_type := t.tc.parse_type(node.typ)
			if interface_box_expected_type(return_type) {
				for i in 0 .. node.children_count {
					t.collect_interface_return_boxes(t.a.child(&node, i), return_type)
				}
			}
		}
		if node.kind in [.fn_literal, .lambda_expr] {
			literal_id := flat.NodeId(idx)
			return_type := t.interface_box_fn_literal_return_type(literal_id, node) or { continue }
			if interface_box_expected_type(return_type) {
				for i in 0 .. node.children_count {
					t.collect_interface_return_boxes(t.a.child(&node, i), return_type)
				}
				if node.kind == .lambda_expr && node.children_count > 0 {
					t.collect_interface_boxed_value(t.a.child(&node, node.children_count - 1),
						return_type)
				}
			}
		}
		if node.kind in [.assign, .decl_assign, .selector_assign, .index_assign] {
			t.collect_interface_assign_boxes(node)
		}
		if node.kind == .select_branch && node.value == 'recv_assign' {
			t.collect_interface_select_receive_boxes(node)
		}
		if node.kind == .infix && node.children_count >= 2 {
			if node.op == .left_shift {
				t.collect_interface_append_boxes(node)
			} else if node.op == .arrow {
				t.collect_interface_channel_send_boxes(node)
			}
		}
		if node.kind == .cast_expr && node.children_count == 1
			&& t.interface_box_type_text_maybe(node.value) {
			expected := t.tc.parse_type(node.value)
			if interface_box_expected_type(expected) {
				t.collect_interface_boxed_value(t.a.child(&node, 0), expected)
			}
		}
		if node.kind == .call && node.children_count > 0 {
			t.collect_interface_call_boxes(flat.NodeId(idx), node)
		}
		if node.kind != .struct_init {
			continue
		}
		t.collect_interface_boxed_value(flat.NodeId(idx), t.tc.parse_type(node.value))
		iface_name := t.interface_literal_name(node.value) or { continue }
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind != .field_init || field.value != '_object' {
				continue
			}
			concrete_type := if field.typ.starts_with('&') { field.typ[1..] } else { field.typ }
			t.mark_interface_boxed_type(iface_name, concrete_type)
		}
	}
}

fn (t &Transformer) interface_box_fn_literal_return_type(id flat.NodeId, node flat.Node) ?types.Type {
	if typ := t.tc.expr_type(id) {
		if return_type := interface_box_fn_type_return_type(typ) {
			return return_type
		}
	}
	if node.kind == .fn_literal && node.typ.len > 0 {
		return t.tc.parse_type(node.typ)
	}
	return none
}

fn interface_box_fn_type_return_type(typ types.Type) ?types.Type {
	if typ is types.FnType {
		return typ.return_type
	}
	if typ is types.Alias {
		return interface_box_fn_type_return_type(typ.base_type)
	}
	return none
}

fn (mut t Transformer) collect_interface_call_boxes(call_id flat.NodeId, node flat.Node) {
	mut call_name := t.tc.resolved_call_name(call_id) or { '' }
	callee := t.a.child_node(&node, 0)
	if call_name.len == 0 {
		if callee.kind == .ident {
			call_name = callee.value
		}
	}
	params := t.tc.fn_param_types_for_name(call_name)
	if params.len == 0 {
		return
	}
	explicit_args := int(node.children_count) - 1
	mut field_init_args := 0
	for arg_idx in 0 .. explicit_args {
		if t.a.child_node(&node, arg_idx + 1).kind == .field_init {
			field_init_args++
		}
	}
	params_may_box := params.any(t.interface_box_call_param_maybe(it))
	last_param := params[params.len - 1]
	variadic_elem_may_box := if field_init_args > 0 && last_param is types.Array {
		t.interface_box_call_param_maybe(last_param.elem_type)
	} else {
		false
	}
	if !params_may_box && !variadic_elem_may_box {
		return
	}
	is_variadic := t.call_is_variadic(call_name)
	variadic_idx := if is_variadic && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	if !params_may_box && variadic_idx < 0 {
		return
	}
	param_offset := if callee.kind == .selector {
		t.call_param_offset(call_name, node, params)
	} else {
		0
	}
	logical_args := explicit_args - field_init_args + if field_init_args > 0 { 1 } else { 0 }
	hidden_ctx_offset := if t.tc.fn_implicit_veb_ctx[call_name]
		&& params.len - param_offset > logical_args {
		1
	} else {
		0
	}
	logical_params := params.len - param_offset - hidden_ctx_offset
	mut omitted_params_struct := ''
	if variadic_idx < 0 && logical_params != logical_args {
		if logical_params == logical_args + 1 {
			omitted_params_struct = t.params_struct_type_name(params[params.len - 1].name()) or {
				return
			}
		} else {
			return
		}
	}
	if variadic_idx >= 0 && logical_args < variadic_idx - param_offset - hidden_ctx_offset {
		return
	}
	for arg_idx in 0 .. explicit_args {
		arg_id := t.a.child(&node, arg_idx + 1)
		param_idx := arg_idx + param_offset + hidden_ctx_offset
		arg := t.a.nodes[int(arg_id)]
		if arg.kind == .field_init {
			if param_idx < params.len {
				mut param_type := params[param_idx].name()
				if param_idx == variadic_idx {
					variadic_type := params[param_idx]
					if variadic_type is types.Array {
						param_type = variadic_type.elem_type.name()
					}
				}
				struct_type := t.params_struct_type_name(param_type) or {
					t.struct_arg_type_name(param_type) or { '' }
				}
				if struct_type.len > 0 {
					t.collect_interface_struct_call_fields(node, arg_idx + 1, struct_type)
				}
			}
			break
		}
		mut expected := if variadic_idx >= 0 && param_idx >= variadic_idx {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				variadic_type.elem_type
			} else {
				variadic_type
			}
		} else if param_idx < params.len {
			params[param_idx]
		} else {
			continue
		}
		mut value_id := arg_id
		if variadic_idx >= 0 && param_idx >= variadic_idx && arg.kind == .prefix
			&& arg.value == '...' && arg.children_count > 0 {
			expected = params[variadic_idx]
			value_id = t.a.child(&arg, 0)
		}
		if interface_box_expected_type(expected) {
			t.collect_interface_boxed_value(value_id, expected)
		}
	}
	if omitted_params_struct.len > 0 {
		t.collect_interface_struct_default_boxes(omitted_params_struct, []string{})
	}
}

fn (t &Transformer) interface_box_call_param_maybe(param types.Type) bool {
	if interface_box_expected_type(param) {
		return true
	}
	if param !is types.Struct && param !is types.Alias {
		return false
	}
	struct_type := t.params_struct_type_name(param.name()) or {
		t.struct_arg_type_name(param.name()) or { return false }
	}
	info := t.lookup_struct_info(struct_type) or { return false }
	for field in info.fields {
		field_type_text := t.lookup_struct_field_type(struct_type, field.name) or { field.typ }
		if interface_box_expected_type(t.tc.parse_type(field_type_text)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) collect_interface_struct_call_fields(node flat.Node, field_start int, struct_type string) {
	info := t.lookup_struct_info(struct_type) or { return }
	mut field_index := 0
	mut provided := []string{}
	for i in field_start .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if field_index < info.fields.len {
			info.fields[field_index].name
		} else {
			field_index++
			continue
		}
		provided << field_name
		if field.children_count == 0 {
			field_index++
			continue
		}
		field_type_text := t.lookup_struct_field_type(struct_type, field_name) or {
			field_index++
			continue
		}
		field_type := t.tc.parse_type(field_type_text)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(t.a.child(field, 0), field_type)
		}
		field_index++
	}
	t.collect_interface_struct_default_boxes(struct_type, provided)
}

fn (mut t Transformer) collect_interface_struct_default_boxes(struct_type string, provided []string) {
	info := t.lookup_struct_info(struct_type) or { return }
	for field in info.fields {
		if field.name in provided || int(field.default_expr) < 0 {
			continue
		}
		field_type_text := t.lookup_struct_field_type(struct_type, field.name) or { field.typ }
		field_type := t.tc.parse_type(field_type_text)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(field.default_expr, field_type)
		}
	}
}

fn (mut t Transformer) collect_interface_return_boxes(id flat.NodeId, return_type types.Type) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return
	}
	if node.kind == .return_stmt {
		if node.children_count == 1 {
			if expected_types := multi_return_types_from_type(return_type, 0) {
				value_id := t.a.child(&node, 0)
				if actual_types := t.multi_return_types_for_expr(value_id, expected_types.len) {
					for i, expected in expected_types {
						if interface_box_expected_type(expected) {
							t.collect_interface_boxed_type(actual_types[i], expected)
						}
					}
					return
				}
			}
		}
		if return_types := multi_return_types_from_type(return_type, int(node.children_count)) {
			for i, expected in return_types {
				t.collect_interface_boxed_value(t.a.child(&node, i), expected)
			}
			return
		}
		for i in 0 .. node.children_count {
			t.collect_interface_boxed_value(t.a.child(&node, i), return_type)
		}
		return
	}
	for i in 0 .. node.children_count {
		t.collect_interface_return_boxes(t.a.child(&node, i), return_type)
	}
}

fn (mut t Transformer) collect_interface_assign_boxes(node flat.Node) {
	lhs_ids := t.multi_assign_lhs_ids(node)
	rhs_count := t.multi_assign_rhs_count(node)
	if lhs_ids.len > 1 && rhs_count == 1 {
		rhs_id := t.multi_assign_rhs_id(node, 0)
		if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
			for i, lhs_id in lhs_ids {
				expected := t.interface_box_lhs_type(lhs_id)
				if interface_box_expected_type(expected) {
					t.collect_interface_boxed_type(rhs_types[i], expected)
				}
			}
			return
		}
	}
	for i in 0 .. rhs_count {
		lhs_id := t.multi_assign_lhs_id(node, i)
		rhs_id := t.multi_assign_rhs_id(node, i)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.typ.len > 0 && !t.interface_box_type_text_maybe(lhs.typ) {
			continue
		}
		lhs_type := t.interface_box_lhs_type(lhs_id)
		mut expected := lhs_type
		if node.op == .left_shift_assign {
			expected = t.interface_box_append_expected_type(lhs_type, lhs_id, rhs_id)
		}
		if interface_box_expected_type(expected) {
			t.collect_interface_boxed_value(rhs_id, expected)
		}
	}
}

fn (t &Transformer) interface_box_lhs_type(lhs_id flat.NodeId) types.Type {
	lhs := t.a.nodes[int(lhs_id)]
	return if lhs.typ.len > 0 {
		t.tc.parse_type(lhs.typ)
	} else {
		t.tc.expr_type(lhs_id) or { t.tc.resolve_type(lhs_id) }
	}
}

fn (mut t Transformer) collect_interface_boxed_type(actual types.Type, expected types.Type) {
	if actual is types.OptionType {
		t.collect_interface_boxed_type(actual.base_type, expected)
		return
	}
	if actual is types.ResultType {
		t.collect_interface_boxed_type(actual.base_type, expected)
		return
	}
	match expected {
		types.Alias {
			t.collect_interface_boxed_type(actual, expected.base_type)
		}
		types.OptionType {
			actual_base := if actual is types.OptionType { actual.base_type } else { actual }
			t.collect_interface_boxed_type(actual_base, expected.base_type)
		}
		types.ResultType {
			actual_base := if actual is types.ResultType { actual.base_type } else { actual }
			t.collect_interface_boxed_type(actual_base, expected.base_type)
		}
		types.Interface {
			iface_name := t.resolve_interface_type_name(expected.name)
			actual_name := actual.name()
			if iface_name.len > 0 && actual_name !in ['', 'unknown', 'void']
				&& t.resolve_interface_type_name(actual_name).len == 0 {
				t.mark_interface_boxed_type(iface_name, t.trim_pointer_type(actual_name))
			}
		}
		types.Array {
			actual_base := interface_box_unalias_type(actual)
			if actual_base is types.Array {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			} else if actual_base is types.ArrayFixed {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			}
		}
		types.ArrayFixed {
			actual_base := interface_box_unalias_type(actual)
			if actual_base is types.ArrayFixed {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			}
		}
		types.Map {
			actual_base := interface_box_unalias_type(actual)
			if actual_base is types.Map {
				t.collect_interface_boxed_type(actual_base.key_type, expected.key_type)
				t.collect_interface_boxed_type(actual_base.value_type, expected.value_type)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_interface_append_boxes(node flat.Node) {
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs_type := t.tc.expr_type(lhs_id) or { t.tc.resolve_type(lhs_id) }
	expected := t.interface_box_append_expected_type(lhs_type, lhs_id, rhs_id)
	if interface_box_expected_type(expected) {
		t.collect_interface_boxed_value(rhs_id, expected)
	}
}

fn (mut t Transformer) collect_interface_channel_send_boxes(node flat.Node) {
	channel_id := t.a.child(&node, 0)
	value_id := t.a.child(&node, 1)
	channel_type := interface_box_unalias_type(t.tc.expr_type(channel_id) or {
		t.tc.resolve_type(channel_id)
	})
	if channel_type is types.Channel && interface_box_expected_type(channel_type.elem_type) {
		t.collect_interface_boxed_value(value_id, channel_type.elem_type)
	}
}

fn (mut t Transformer) collect_interface_select_receive_boxes(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	recv := t.a.child_node(&node, 1)
	if recv.kind != .prefix || recv.op != .arrow || recv.children_count == 0 {
		return
	}
	expected := t.interface_box_lhs_type(lhs_id)
	if !interface_box_expected_type(expected) {
		return
	}
	channel_id := t.a.child(recv, 0)
	channel_type := interface_box_unalias_type(t.tc.expr_type(channel_id) or {
		t.tc.resolve_type(channel_id)
	})
	if channel_type is types.Channel {
		t.collect_interface_boxed_type(channel_type.elem_type, expected)
	}
}

fn (t &Transformer) interface_box_append_expected_type(lhs_type types.Type, lhs_id flat.NodeId, rhs_id flat.NodeId) types.Type {
	clean := interface_box_unalias_type(lhs_type)
	if clean is types.Array {
		rhs_type := t.tc.expr_type(rhs_id) or { t.tc.resolve_type(rhs_id) }
		if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type.name(), clean.elem_type.name()) {
			return lhs_type
		}
		return clean.elem_type
	}
	return lhs_type
}

fn interface_box_unalias_type(typ types.Type) types.Type {
	if typ is types.Alias {
		return interface_box_unalias_type(typ.base_type)
	}
	return typ
}

fn (t &Transformer) interface_box_type_text_maybe(raw_type string) bool {
	if raw_type.len == 0 {
		return false
	}
	clean := t.normalize_type_alias(raw_type).trim_space()
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.interface_box_type_text_maybe(clean[1..])
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		for part in split_generic_args(clean[1..clean.len - 1]) {
			if t.interface_box_type_text_maybe(part) {
				return true
			}
		}
		return false
	}
	return t.resolve_interface_type_name(clean).len > 0 || clean.starts_with('[]')
		|| clean.starts_with('[') || clean.starts_with('map[')
}

fn interface_box_expected_type(expected types.Type) bool {
	return match expected {
		types.Interface {
			true
		}
		types.Alias {
			interface_box_expected_type(expected.base_type)
		}
		types.OptionType {
			interface_box_expected_type(expected.base_type)
		}
		types.ResultType {
			interface_box_expected_type(expected.base_type)
		}
		types.MultiReturn {
			expected.types.any(interface_box_expected_type(it))
		}
		types.Array {
			interface_box_expected_type(expected.elem_type)
		}
		types.ArrayFixed {
			interface_box_expected_type(expected.elem_type)
		}
		types.Map {
			interface_box_expected_type(expected.key_type)
				|| interface_box_expected_type(expected.value_type)
		}
		else {
			false
		}
	}
}

fn (mut t Transformer) collect_interface_boxed_value(id flat.NodeId, expected types.Type) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .expr_stmt {
			if node.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&node, 0), expected)
			}
			return
		}
		.if_expr, .match_stmt {
			for i in 1 .. node.children_count {
				t.collect_interface_boxed_value(t.a.child(&node, i), expected)
			}
			return
		}
		.or_expr {
			if node.children_count > 0 {
				source_id := t.a.child(&node, 0)
				source_type := t.tc.expr_type(source_id) or { t.tc.resolve_type(source_id) }
				t.collect_interface_boxed_type(source_type, expected)
			}
			if node.children_count >= 2 {
				t.collect_interface_boxed_value(t.a.child(&node, 1), expected)
			}
			return
		}
		.block, .match_branch {
			if node.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&node, node.children_count - 1), expected)
			}
			return
		}
		else {}
	}

	match expected {
		types.Alias {
			t.collect_interface_boxed_value(id, expected.base_type)
		}
		types.OptionType {
			actual := interface_box_unalias_type(t.tc.expr_type(id) or { t.tc.resolve_type(id) })
			if actual is types.OptionType {
				t.collect_interface_boxed_type(actual.base_type, expected.base_type)
			} else {
				t.collect_interface_boxed_value(id, expected.base_type)
			}
		}
		types.ResultType {
			actual := interface_box_unalias_type(t.tc.expr_type(id) or { t.tc.resolve_type(id) })
			if actual is types.ResultType {
				t.collect_interface_boxed_type(actual.base_type, expected.base_type)
			} else {
				t.collect_interface_boxed_value(id, expected.base_type)
			}
		}
		types.Interface {
			iface_name := t.resolve_interface_type_name(expected.name)
			if iface_name.len > 0 {
				t.collect_interface_boxed_interface_value(id, iface_name)
			}
		}
		types.Array {
			t.collect_interface_boxed_array_value(node, expected.elem_type, expected)
		}
		types.ArrayFixed {
			t.collect_interface_boxed_array_value(node, expected.elem_type, expected)
		}
		types.Map {
			if node.kind != .map_init {
				return
			}
			for i := 0; i + 1 < node.children_count; i += 2 {
				t.collect_interface_boxed_value(t.a.child(&node, i), expected.key_type)
				t.collect_interface_boxed_value(t.a.child(&node, i + 1), expected.value_type)
			}
		}
		types.Struct {
			t.collect_interface_boxed_struct_value(node, expected.name)
		}
		else {}
	}
}

fn (mut t Transformer) collect_interface_boxed_struct_value(node flat.Node, struct_name string) {
	if node.kind != .struct_init {
		return
	}
	literal_name := if node.value.len > 0 { node.value } else { struct_name }
	info := t.lookup_struct_info(literal_name) or { return }
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		field_type_text := t.lookup_struct_field_type(literal_name, field_name) or {
			t.lookup_struct_field_type(struct_name, field_name) or { continue }
		}
		field_type := t.tc.parse_type(field_type_text)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(t.a.child(field, 0), field_type)
		}
	}
	for field in info.fields {
		if int(field.default_expr) < 0
			|| t.interface_box_struct_field_provided(node, info, field.name) {
			continue
		}
		field_type_text := t.lookup_struct_field_type(literal_name, field.name) or {
			t.lookup_struct_field_type(struct_name, field.name) or { continue }
		}
		field_type := t.tc.parse_type(field_type_text)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(field.default_expr, field_type)
		}
	}
}

fn (t &Transformer) interface_box_struct_field_provided(node flat.Node, info StructInfo, name string) bool {
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		if field_name == name {
			return true
		}
	}
	return false
}

fn (mut t Transformer) collect_interface_boxed_array_value(node flat.Node, elem_type types.Type, array_type types.Type) {
	if node.kind == .postfix && node.children_count > 0 {
		t.collect_interface_boxed_value(t.a.child(&node, 0), array_type)
		return
	}
	if node.kind !in [.array_literal, .array_init] {
		return
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init {
			if child.value == 'init' && child.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&child, 0), elem_type)
			}
			continue
		}
		if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
			t.collect_interface_boxed_value(t.a.child(&child, 0), array_type)
			continue
		}
		t.collect_interface_boxed_value(child_id, elem_type)
	}
}

fn (mut t Transformer) collect_interface_boxed_interface_value(id flat.NodeId, iface_name string) {
	node := t.a.nodes[int(id)]
	actual := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	actual_name := if actual.name() in ['', 'unknown'] && node.kind == .struct_init {
		node.value
	} else {
		actual.name()
	}
	if actual_name !in ['', 'unknown', 'void']
		&& t.resolve_interface_type_name(actual_name).len == 0 {
		t.mark_interface_boxed_type(iface_name, t.trim_pointer_type(actual_name))
	}
}

fn (t &Transformer) interface_literal_name(name string) ?string {
	if name in t.tc.interface_names {
		return name
	}
	qname := t.tc.qualify_name(name)
	if qname in t.tc.interface_names {
		return qname
	}
	return none
}

fn (mut t Transformer) mark_interface_boxed_type(iface_name string, concrete_type string) {
	if iface_name.len == 0 || concrete_type.len == 0 {
		return
	}
	mut iface_names := [iface_name]
	resolved := t.resolve_interface_type_name(iface_name)
	if resolved.len > 0 && resolved != iface_name {
		iface_names << resolved
	}
	for iface in iface_names {
		t.interface_boxed_types[interface_boxed_type_key(iface, concrete_type)] = true
		t.interface_boxed_types[interface_boxed_type_key(iface, c_name(concrete_type))] = true
	}
}

fn interface_boxed_type_key(iface_name string, concrete_type string) string {
	return '${iface_name}\n${concrete_type}'
}

fn (mut t Transformer) materialize_generic_structs() {
	if isnil(t.tc) {
		return
	}
	decls := t.collect_generic_struct_decls()
	sum_decls := t.collect_generic_sum_decls()
	if decls.len == 0 && sum_decls.len == 0 {
		return
	}
	specs, sum_specs := t.collect_generic_materialization_specs(decls, sum_decls)
	for spec, context in sum_specs {
		decl := sum_decls[context.base] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	for spec, base in specs {
		decl := decls[base] or { continue }
		t.materialize_generic_struct_spec(spec, decl)
	}
	t.erase_generic_sum_templates(sum_decls)
	for _, decl in decls {
		t.tc.structs.delete(decl.key)
		t.tc.unions.delete(decl.key)
		t.tc.params_structs.delete(decl.key)
	}
	t.tc.invalidate_short_type_name_index()
}

fn (mut t Transformer) materialize_generic_sum_types(erase_templates bool) {
	if isnil(t.tc) {
		return
	}
	decls := t.collect_generic_struct_decls()
	sum_decls := t.collect_generic_sum_decls()
	if sum_decls.len == 0 {
		return
	}
	_, sum_specs := t.collect_generic_materialization_specs(decls, sum_decls)
	for spec, context in sum_specs {
		decl := sum_decls[context.base] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	if erase_templates {
		t.erase_generic_sum_templates(sum_decls)
	}
}

fn (mut t Transformer) erase_generic_sum_templates(sum_decls map[string]GenericSumDecl) {
	for _, decl in sum_decls {
		t.tc.sum_types.delete(decl.key)
		t.sum_types.delete(decl.key)
		t.tc.sum_generic_params.delete(decl.key)
		if decl.key != decl.node.value {
			t.tc.sum_generic_params.delete(decl.node.value)
		}
	}
	t.tc.invalidate_short_type_name_index()
}

fn (mut t Transformer) collect_generic_materialization_specs(decls map[string]GenericStructDecl, sum_decls map[string]GenericSumDecl) (map[string]string, map[string]GenericSpecContext) {
	mut specs := map[string]string{}
	mut sum_specs := map[string]GenericSpecContext{}
	t.collect_generic_struct_specs(decls, mut specs)
	t.collect_generic_sum_specs(sum_decls, mut sum_specs)
	for _ in 0 .. 40 {
		before := specs.len
		before_sums := sum_specs.len
		current := specs.clone()
		for spec, base in current {
			decl := decls[base] or { continue }
			_, args, ok := generic_app_parts(spec)
			if !ok {
				continue
			}
			for i in 0 .. decl.node.children_count {
				field := t.a.child_node(&decl.node, i)
				if field.kind != .field_decl {
					continue
				}
				field_type := substitute_generic_type_text_with_params(field.typ, args,
					decl.node.generic_params)
				t.collect_generic_struct_spec_from_type(field_type, decl.module, decl.file, decls, mut
					specs)
				t.collect_generic_sum_spec_from_type(field_type, decl.module, decl.file, sum_decls, mut
					sum_specs)
			}
		}
		current_sums := sum_specs.clone()
		for spec, context in current_sums {
			decl := sum_decls[context.base] or { continue }
			_, args, ok := generic_app_parts(spec)
			if !ok {
				continue
			}
			for i in 0 .. decl.node.children_count {
				variant := t.a.child_node(&decl.node, i)
				variant_type := substitute_generic_type_text_with_params(variant.value, args,
					decl.node.generic_params)
				t.collect_generic_struct_spec_from_type(variant_type, decl.module, decl.file,
					decls, mut specs)
				t.collect_generic_sum_spec_from_type(variant_type, decl.module, decl.file,
					sum_decls, mut sum_specs)
			}
		}
		if specs.len == before && sum_specs.len == before_sums {
			break
		}
	}
	return specs, sum_specs
}

fn (mut t Transformer) collect_generic_struct_decls() map[string]GenericStructDecl {
	mut decls := map[string]GenericStructDecl{}
	t.ensure_node_module_map()
	mut cur_file := ''
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
			}
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				if node.generic_params.len == 0 && 'generic' !in node.typ.split(',') {
					continue
				}
				module_name := t.node_module_or(i, cur_module)
				key := generic_struct_decl_key(node.value, module_name)
				decls[key] = GenericStructDecl{
					id:     flat.NodeId(i)
					node:   node
					file:   cur_file
					module: module_name
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn generic_struct_decl_key(name string, module_name string) string {
	if name.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' {
		return name
	}
	return '${module_name}.${name}'
}

fn (mut t Transformer) collect_generic_sum_decls() map[string]GenericSumDecl {
	mut decls := map[string]GenericSumDecl{}
	t.ensure_node_module_map()
	mut cur_file := ''
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
			}
			.module_decl {
				cur_module = node.value
			}
			.type_decl {
				if node.generic_params.len == 0 || node.children_count == 0 {
					continue
				}
				module_name := t.node_module_map_cache[i] or { cur_module }
				key := generic_type_decl_key(node.value, module_name)
				decls[key] = GenericSumDecl{
					id:     flat.NodeId(i)
					node:   node
					file:   cur_file
					module: module_name
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn generic_type_decl_key(name string, module_name string) string {
	return generic_struct_decl_key(name, module_name)
}

fn (mut t Transformer) collect_generic_struct_specs(decls map[string]GenericStructDecl, mut specs map[string]string) {
	for _, target in t.tc.type_aliases {
		t.collect_generic_struct_spec_from_type(target, '', '', decls, mut specs)
	}
	mut file_name := ''
	mut module_name := ''
	for node in t.a.nodes {
		match node.kind {
			.file {
				file_name = node.value
				module_name = ''
			}
			.module_decl {
				module_name = node.value
			}
			else {}
		}

		if node.typ.len > 0 {
			t.collect_generic_struct_spec_from_type(node.typ, module_name, file_name, decls, mut
				specs)
		}
		match node.kind {
			.struct_init, .array_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr, .is_expr {
				t.collect_generic_struct_spec_from_type(node.value, module_name, file_name, decls, mut
					specs)
			}
			else {}
		}
	}
}

fn (mut t Transformer) collect_generic_struct_spec_from_type(typ string, module_name string, file_name string, decls map[string]GenericStructDecl, mut specs map[string]string) {
	clean := typ.trim_space()
	if clean.len == 0 || !clean.contains('[') {
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_struct_spec_from_type(clean[1..], module_name, file_name, decls, mut
			specs)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_struct_spec_from_type(clean[4..], module_name, file_name, decls, mut
			specs)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_struct_spec_from_type(clean[1..], module_name, file_name, decls, mut
			specs)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_struct_spec_from_type(clean[3..], module_name, file_name, decls, mut
			specs)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_struct_spec_from_type(clean[2..], module_name, file_name, decls, mut
			specs)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_struct_spec_from_type(clean[4..bracket_end], module_name, file_name,
				decls, mut specs)
			t.collect_generic_struct_spec_from_type(clean[bracket_end + 1..], module_name,
				file_name, decls, mut specs)
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			t.collect_generic_struct_spec_from_type(clean[bracket_end + 1..], module_name,
				file_name, decls, mut specs)
		}
		return
	}
	base, args, ok := generic_app_parts(clean)
	if !ok {
		return
	}
	for arg in args {
		t.collect_generic_struct_spec_from_type(arg, module_name, file_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(args) {
		return
	}
	spec_base := t.generic_struct_spec_base_name(base, module_name, file_name, decls) or { return }
	scoped_args := t.generic_struct_args_in_scope(args, module_name, file_name)
	spec_name := '${spec_base}[${scoped_args.join(', ')}]'
	specs[spec_name] = spec_base
	spec_module := if decl := decls[spec_base] { decl.module } else { module_name }
	t.record_generic_specialization_args_in_module(spec_base, spec_module, scoped_args)
}

fn (mut t Transformer) collect_generic_sum_specs(decls map[string]GenericSumDecl, mut specs map[string]GenericSpecContext) {
	for _, target in t.tc.type_aliases {
		t.collect_generic_sum_spec_from_type(target, '', '', decls, mut specs)
	}
	mut file_name := ''
	mut module_name := ''
	for node in t.a.nodes {
		match node.kind {
			.file {
				file_name = node.value
				module_name = ''
			}
			.module_decl {
				module_name = node.value
			}
			else {}
		}

		if node.typ.len > 0 {
			t.collect_generic_sum_spec_from_type(node.typ, module_name, file_name, decls, mut specs)
		}
		match node.kind {
			.struct_init, .array_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr, .is_expr {
				t.collect_generic_sum_spec_from_type(node.value, module_name, file_name, decls, mut
					specs)
			}
			else {}
		}
	}
}

fn (mut t Transformer) collect_generic_sum_spec_from_type(typ string, module_name string, file_name string, decls map[string]GenericSumDecl, mut specs map[string]GenericSpecContext) {
	clean := typ.trim_space()
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_sum_spec_from_type(clean[1..], module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_sum_spec_from_type(clean[4..], module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_sum_spec_from_type(clean[1..], module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_sum_spec_from_type(clean[3..], module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_sum_spec_from_type(clean[2..], module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_sum_spec_from_type(clean[4..bracket_end], module_name, file_name,
				decls, mut specs)
			t.collect_generic_sum_spec_from_type(clean[bracket_end + 1..], module_name, file_name,
				decls, mut specs)
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			t.collect_generic_sum_spec_from_type(clean[bracket_end + 1..], module_name, file_name,
				decls, mut specs)
		}
		return
	}
	base, args, ok := generic_app_parts(clean)
	if !ok {
		return
	}
	for arg in args {
		t.collect_generic_sum_spec_from_type(arg, module_name, file_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(args) {
		return
	}
	spec_base := t.generic_sum_spec_base_name(base, module_name, file_name, decls) or { return }
	spec_name := t.generic_sum_spec_name_in_scope(spec_base, args, module_name, file_name)
	specs[spec_name] = GenericSpecContext{
		base:   spec_base
		file:   file_name
		module: module_name
	}
}

fn (t &Transformer) generic_struct_spec_base_name(base string, module_name string, file_name string, decls map[string]GenericStructDecl) ?string {
	if base in decls {
		return base
	}
	if base.contains('.') {
		return none
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		qbase := '${module_name}.${base}'
		if qbase in decls {
			return qbase
		}
	}
	if resolved := t.selective_import_generic_struct_base(base, file_name, decls) {
		return resolved
	}
	mut found := ''
	for key, _ in decls {
		if key.all_after_last('.') == base {
			if found.len > 0 && found != key {
				return none
			}
			found = key
		}
	}
	if found.len > 0 {
		return found
	}
	return none
}

fn (t &Transformer) generic_sum_spec_base_name(base string, module_name string, file_name string, decls map[string]GenericSumDecl) ?string {
	if base in decls {
		return base
	}
	if base.contains('.') {
		return none
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		qbase := '${module_name}.${base}'
		if qbase in decls {
			return qbase
		}
	}
	if resolved := t.selective_import_generic_sum_base(base, file_name, decls) {
		return resolved
	}
	mut found := ''
	for key, _ in decls {
		if key.all_after_last('.') == base {
			if found.len > 0 && found != key {
				return none
			}
			found = key
		}
	}
	if found.len > 0 {
		return found
	}
	return none
}

fn (mut t Transformer) generic_sum_spec_name_in_scope(base string, args []string, module_name string, file_name string) string {
	scoped_args := t.generic_sum_args_in_scope(args, module_name, file_name)
	return '${base}[${scoped_args.join(', ')}]'
}

fn (mut t Transformer) generic_struct_args_in_scope(args []string, module_name string, file_name string) []string {
	if isnil(t.tc) {
		return args.clone()
	}
	old_module := t.tc.cur_module
	old_file := t.tc.cur_file
	t.tc.cur_module = module_name
	t.tc.cur_file = file_name
	mut scoped := []string{cap: args.len}
	for arg in args {
		parsed := t.tc.parse_resolution_type(arg)
		scoped << if parsed is types.Unknown {
			t.normalize_sum_variant_type(arg, module_name, [])
		} else if parsed is types.Alias && parsed.base_type is types.ArrayFixed {
			types.Type(parsed.base_type).name()
		} else {
			parsed.name()
		}
	}
	t.tc.cur_module = old_module
	t.tc.cur_file = old_file
	return scoped
}

fn (mut t Transformer) generic_sum_args_in_scope(args []string, module_name string, file_name string) []string {
	if isnil(t.tc) {
		return args.clone()
	}
	old_module := t.tc.cur_module
	old_file := t.tc.cur_file
	t.tc.cur_module = module_name
	t.tc.cur_file = file_name
	mut scoped := []string{cap: args.len}
	for arg in args {
		parsed := t.tc.parse_resolution_type(arg)
		scoped << if parsed is types.Unknown {
			t.normalize_sum_variant_type(arg, module_name, [])
		} else {
			parsed.name()
		}
	}
	t.tc.cur_module = old_module
	t.tc.cur_file = old_file
	return scoped
}

fn (t &Transformer) selective_import_generic_struct_base(base string, file_name string, decls map[string]GenericStructDecl) ?string {
	if isnil(t.tc) || base.contains('.') || file_name.len == 0 {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file_name, base)] or { return none }
	for candidate in candidates {
		if candidate in decls {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) selective_import_generic_sum_base(base string, file_name string, decls map[string]GenericSumDecl) ?string {
	if isnil(t.tc) || base.contains('.') || file_name.len == 0 {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file_name, base)] or { return none }
	for candidate in candidates {
		if candidate in decls {
			return candidate
		}
	}
	return none
}

fn (mut t Transformer) materialize_generic_sum_spec(spec_name string, decl GenericSumDecl, context GenericSpecContext) {
	_, args, ok := generic_app_parts(spec_name)
	if !ok || args.len == 0 {
		return
	}
	if variants := t.tc.sum_types[spec_name] {
		t.sum_types[spec_name] = variants
		return
	}
	if variants := t.sum_types[spec_name] {
		t.tc.sum_types[spec_name] = variants
		t.tc.invalidate_short_type_name_index()
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	scoped_args := t.generic_sum_args_in_scope(args, context.module, context.file)
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	mut variants := []string{}
	for i in 0 .. decl.node.children_count {
		variant := t.a.child_node(&decl.node, i)
		variant_type := substitute_generic_type_text_with_params(variant.value, scoped_args,
			decl.node.generic_params)
		parsed := t.tc.parse_resolution_type(variant_type)
		variants << if parsed is types.Unknown { variant_type } else { parsed.name() }
	}
	t.tc.sum_types[spec_name] = variants
	t.sum_types[spec_name] = variants
	t.tc.invalidate_short_type_name_index()
	if !isnil(t.sum_cache) {
		t.sum_cache.entries.clear()
	}
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
}

fn (mut t Transformer) materialize_generic_struct_spec(spec_name string, decl GenericStructDecl) {
	base, args, ok := generic_app_parts(spec_name)
	if !ok || args.len == 0 {
		return
	}
	t.record_generic_specialization_args_in_module(base, decl.module, args)
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	mut fields := []types.StructField{}
	for i in 0 .. decl.node.children_count {
		field := t.a.child_node(&decl.node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type := substitute_generic_type_text_with_params(field.typ, args,
			decl.node.generic_params)
		fields << types.StructField{
			name: field.value
			typ:  t.tc.parse_resolution_type(field_type)
		}
	}
	t.tc.structs[spec_name] = fields
	if decl.key in t.tc.unions {
		t.tc.unions[spec_name] = true
	}
	if decl.key in t.tc.params_structs {
		t.tc.params_structs[spec_name] = true
	}
	t.tc.invalidate_short_type_name_index()
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
}

fn (mut t Transformer) collect_generic_fn_decls() map[string]GenericFnDecl {
	mut decls := map[string]GenericFnDecl{}
	t.ensure_node_module_map()
	mut cur_file := ''
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				fn_module := t.node_module_or(i, cur_module)
				if !t.fn_decl_has_unresolved_generics(node, fn_module) {
					continue
				}
				key := t.generic_fn_decl_key(node, fn_module)
				decls[key] = GenericFnDecl{
					id:     flat.NodeId(i)
					node:   node
					file:   cur_file
					module: fn_module
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn (mut t Transformer) monomorphize_ignored_nodes(decls map[string]GenericFnDecl) []bool {
	mut nodes := []bool{len: t.a.nodes.len}
	mut stack := []flat.NodeId{cap: 256}
	for _, decl in decls {
		t.collect_node_subtree_flags(decl.id, mut nodes, mut stack)
	}
	if !t.has_used_fn_filter() {
		return nodes
	}
	old_module := t.cur_module
	t.cur_module = ''
	for i, node in t.a.nodes {
		match node.kind {
			.module_decl {
				t.cur_module = node.value
			}
			.fn_decl {
				if node.value.starts_with('test_') {
					continue
				}
				if !t.should_transform_fn(node) {
					t.collect_node_subtree_flags(flat.NodeId(i), mut nodes, mut stack)
				}
			}
			else {}
		}
	}
	t.cur_module = old_module
	return nodes
}

fn (mut t Transformer) ensure_node_module_map() {
	if t.node_module_map_nodes == t.a.nodes.len {
		return
	}
	if t.node_module_map_nodes < 0 || t.node_module_map_nodes > t.a.nodes.len {
		t.node_module_map_cache = []string{len: t.a.nodes.len}
		t.node_module_map_nodes = 0
	} else if t.node_module_map_cache.len < t.a.nodes.len {
		t.node_module_map_cache << []string{len: t.a.nodes.len - t.node_module_map_cache.len}
	}
	mut cur_module := ''
	for i := t.node_module_map_nodes; i < t.a.nodes.len; i++ {
		node := t.a.nodes[i]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl, .const_decl, .global_decl, .struct_decl, .type_decl, .enum_decl,
			.interface_decl {
				t.mark_node_module(flat.NodeId(i), cur_module)
			}
			else {}
		}
	}
	t.node_module_map_nodes = t.a.nodes.len
}

fn (t &Transformer) node_module_or(idx int, fallback string) string {
	if idx >= 0 && idx < t.node_module_map_cache.len {
		module_name := t.node_module_map_cache[idx]
		if module_name.len > 0 {
			return module_name
		}
	}
	return fallback
}

fn (mut t Transformer) mark_node_module(id flat.NodeId, module_name string) {
	mut stack := [id]
	for stack.len > 0 {
		cur := stack.pop()
		idx := int(cur)
		if idx < 0 || idx >= t.a.nodes.len {
			continue
		}
		if idx < t.node_module_map_cache.len && t.node_module_map_cache[idx].len > 0 {
			continue
		}
		t.node_module_map_cache[idx] = module_name
		node := t.a.nodes[idx]
		start := node.children_start
		end := start + int(node.children_count)
		if start < 0 || end > t.a.children.len {
			continue
		}
		for i in start .. end {
			// The child range is checked above; avoid per-child bounds checks while mapping modules.
			child_id := unsafe { t.a.children[i] }
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

fn (mut t Transformer) collect_node_subtree_ids(id flat.NodeId, mut nodes map[int]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len || nodes[int(id)] {
		return
	}
	nodes[int(id)] = true
	node := t.a.nodes[int(id)]
	for i in 0 .. node.children_count {
		t.collect_node_subtree_ids(t.a.child(&node, i), mut nodes)
	}
}

fn (mut t Transformer) collect_node_subtree_flags(id flat.NodeId, mut nodes []bool, mut stack []flat.NodeId) {
	stack.clear()
	stack << id
	for stack.len > 0 {
		current := stack.pop()
		idx := int(current)
		if idx < 0 || idx >= t.a.nodes.len || idx >= nodes.len || nodes[idx] {
			continue
		}
		nodes[idx] = true
		node := t.a.nodes[idx]
		start := node.children_start
		end := start + int(node.children_count)
		if start < 0 || end > t.a.children.len {
			continue
		}
		for i in start .. end {
			// The child range is checked above; avoid bounds checks in this hot walk.
			child_id := unsafe { t.a.children[i] }
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

fn (mut t Transformer) emit_generic_fn_specialization(decl GenericFnDecl, args []string) flat.NodeId {
	concrete_args := t.canonical_generic_specialization_args(args)
	spec_key := t.generic_specialization_progress_key(decl, concrete_args)
	if t.generic_fn_specs_in_progress[spec_key] {
		return flat.empty_node
	}
	t.generic_fn_specs_in_progress[spec_key] = true
	defer {
		t.generic_fn_specs_in_progress.delete(spec_key)
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	t.cur_module = decl.module
	t.cur_file = decl.file
	if !isnil(t.tc) {
		t.tc.cur_module = decl.module
		t.tc.cur_file = decl.file
	}
	if decl.module.len > 0 {
		t.a.add_node(flat.Node{
			kind:  .module_decl
			value: decl.module
		})
	}
	old_params := t.active_generic_params
	t.active_generic_params = t.generic_fn_param_names(decl.node, decl.module)
	if decl.node.value.contains('.') {
		receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
		if receiver.len > 0 {
			t.record_generic_specialization_args_in_module(receiver, decl.module, concrete_args)
		}
	}
	old_clone_var_types := t.var_types.clone()
	// Seed the template's params (with substituted types) for the duration of
	// the clone: nested generic calls are retargeted while cloning, and their
	// arg-type inference must see the declared value type of a `mut val T`
	// param, not the checker's internal `&T` annotation on the ident — the
	// latter would bind T to `&Concrete` while the later monomorphize scan
	// binds it to `Concrete`, leaving the call-site type and the emitted
	// specialization in disagreement.
	t.reset_var_types()
	for i in 0 .. decl.node.children_count {
		param_child := t.a.child_node(&decl.node, i)
		if node_kind_id(param_child) != 75 || param_child.value.len == 0 || param_child.typ.len == 0 {
			continue
		}
		mut param_raw := param_child.typ
		if param_raw.starts_with('mut ') {
			param_raw = param_raw[4..]
		}
		if param_raw.starts_with('...') {
			param_raw = '[]' + param_raw[3..]
		}
		// `mut val T` params are recorded as `&T`; the declared language-level
		// type is `T`, and that is what a by-value use of the param must infer.
		if (param_child.is_mut || param_child.op == .amp) && param_raw.starts_with('&') {
			param_raw = param_raw[1..]
		}
		param_substituted := t.subst_type(param_raw, concrete_args)
		if param_substituted.len > 0 {
			t.set_var_type(param_child.value, param_substituted)
		}
		if param_child.is_mut || param_child.op == .amp || param_child.typ.starts_with('mut ') {
			t.mut_param_values[param_child.value] = true
		}
	}
	t.cloning_generic_fn_depth++
	old_clone_ret_type := t.cur_fn_ret_type
	t.cur_fn_ret_type = t.specialized_signature_type_text(decl, decl.node.typ, concrete_args,
		t.active_generic_params)
	clone_id := t.clone_generic_fn_node(decl.node, concrete_args)
	t.cur_fn_ret_type = old_clone_ret_type
	t.cloning_generic_fn_depth--
	t.a.specialized_fn_nodes[int(clone_id)] = true
	t.restore_var_types(old_clone_var_types)
	t.specialize_cloned_fn_signature(clone_id, decl, concrete_args)
	clone := t.a.nodes[int(clone_id)]
	t.register_specialized_fn_signature(decl, clone, concrete_args)
	t.materialize_generic_sum_types(false)
	t.active_generic_params = old_params
	t.transform_specialized_fn_body(clone_id, decl.module, decl.file)
	t.cur_module = old_module
	t.cur_file = old_file
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return clone_id
}

fn (mut t Transformer) transform_specialized_fn_body(clone_id flat.NodeId, module_name string, file_name string) {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	old_fn_name := t.cur_fn_name
	old_ret_type := t.cur_fn_ret_type
	old_var_types := t.var_types.clone()
	old_is_generic := t.cur_fn_is_generic
	t.cur_module = module_name
	t.cur_file = file_name
	if !isnil(t.tc) {
		t.tc.cur_file = file_name
	}
	// The body transform establishes a real function scope (params seeded into
	// var_types), so scope lookups are trustworthy again inside it.
	old_in_scan := t.in_monomorphize_scan
	old_validating_specialization := t.validating_generic_spec
	t.in_monomorphize_scan = false
	t.validating_generic_spec = true
	t.transform_fn_body(int(clone_id))
	t.in_monomorphize_scan = old_in_scan
	t.validating_generic_spec = old_validating_specialization
	t.cur_module = old_module
	t.cur_file = old_file
	if !isnil(t.tc) {
		t.tc.cur_file = old_tc_file
	}
	t.cur_fn_name = old_fn_name
	t.cur_fn_ret_type = old_ret_type
	t.restore_var_types(old_var_types)
	t.cur_fn_is_generic = old_is_generic
}

fn (mut t Transformer) generated_fn_used_names(decl GenericFnDecl, clone_id flat.NodeId, args []string) []string {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return []string{}
	}
	clone := t.a.nodes[int(clone_id)]
	qname := transform_qualified_fn_name(decl.module, clone.value)
	mut names := [clone.value, qname, c_name(clone.value), c_name(qname)]
	names << specialized_generic_fn_signature_aliases(decl, args)
	t.record_generic_specialization_args_for_names(names, args)
	old_module := t.cur_module
	old_file := t.cur_file
	t.cur_module = decl.module
	t.cur_file = decl.file
	names << t.generated_fn_body_call_names(clone_id)
	for name in names {
		t.mark_fn_used_name(name)
	}
	t.cur_module = old_module
	t.cur_file = old_file
	return names
}

fn (mut t Transformer) generated_fn_body_call_names(root flat.NodeId) []string {
	mut names := []string{}
	mut seen := map[string]bool{}
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	saved_vars := t.var_types.clone()
	saved_mut_param_values := t.mut_param_values.clone()
	t.seed_generated_fn_body_context(root)
	t.collect_generated_fn_body_call_names(root, mut names, mut seen)
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	t.restore_var_types(saved_vars)
	t.mut_param_values = saved_mut_param_values.clone()
	return names
}

fn (mut t Transformer) seed_generated_fn_body_context(root flat.NodeId) {
	if int(root) < 0 || int(root) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(root)]
	if node.kind != .fn_decl {
		return
	}
	t.cur_fn_name = node.value
	t.cur_fn_ret_type = if node.typ.len > 0 { node.typ } else { t.fn_body_return_type(node) }
	t.reset_var_types()
	param_count := t.fn_body_param_count(node)
	param_types := t.fn_body_param_types(node, param_count)
	mut param_idx := 0
	for i in 0 .. node.children_count {
		child_id := t.a.children[node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) != 75 || child.value.len == 0 {
			continue
		}
		raw_source_typ := if child.typ.starts_with('...') {
			'[]' + child.typ[3..]
		} else {
			child.typ
		}
		raw_typ := if child.typ.len > 0 {
			if child.typ.starts_with('...') {
				'[]' + t.normalize_type_alias(child.typ[3..])
			} else {
				t.normalize_type_alias(child.typ)
			}
		} else {
			''
		}
		typ := if raw_typ.len > 0 {
			raw_typ
		} else if param_idx < param_types.len {
			t.normalize_type_alias(param_types[param_idx].name())
		} else if param_idx == 0 {
			t.fn_body_receiver_type(node.value)
		} else {
			''
		}
		if typ.len > 0 {
			t.set_var_type_with_raw(child.value, typ, raw_source_typ)
		}
		if child.is_mut || child.op == .amp || child.typ.starts_with('mut ') {
			t.mut_param_values[child.value] = true
		}
		param_idx++
	}
}

fn (mut t Transformer) collect_generated_fn_body_call_names(id flat.NodeId, mut names []string, mut seen map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .decl_assign {
		t.seed_generated_decl_assign_binding(node)
	}
	if node.kind == .call {
		call_name := t.generated_call_name_for_used(id, node)
		if call_name.len > 0 {
			t.push_generated_used_name(call_name, mut names, mut seen)
		}
	} else if fn_value_name := t.generated_fn_value_name_for_used(id, node) {
		t.push_generated_used_name(fn_value_name, mut names, mut seen)
	}
	for i in 0 .. node.children_count {
		t.collect_generated_fn_body_call_names(t.a.child(&node, i), mut names, mut seen)
	}
}

fn (mut t Transformer) seed_generated_decl_assign_binding(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
		return
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	typ := t.generated_decl_assign_binding_type(node, lhs, rhs_id)
	if typ.len > 0 {
		t.set_var_type_with_raw(lhs.value, typ, typ)
	}
}

fn (mut t Transformer) generated_decl_assign_binding_type(node flat.Node, lhs flat.Node, rhs_id flat.NodeId) string {
	for candidate in [lhs.typ, node.typ] {
		if candidate.len > 0 {
			return t.normalize_type_alias(candidate)
		}
	}
	if int(rhs_id) >= 0 && int(rhs_id) < t.a.nodes.len {
		rhs := t.a.nodes[int(rhs_id)]
		if rhs.typ.len > 0 {
			return t.normalize_type_alias(rhs.typ)
		}
		rhs_type := t.node_type(rhs_id)
		if rhs_type.len > 0 {
			return t.normalize_type_alias(rhs_type)
		}
	}
	return ''
}

fn (mut t Transformer) generated_fn_value_name_for_used(id flat.NodeId, node flat.Node) ?string {
	if !isnil(t.tc) {
		if name := t.tc.resolved_fn_value_name(id) {
			return name
		}
	}
	if node.kind == .ident && node.value.len > 0 {
		if t.var_type(node.value).len > 0 {
			return none
		}
		if t.generated_used_name_is_known_fn(node.value) {
			return node.value
		}
		if !node.value.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value}'
			if t.generated_used_name_is_known_fn(qname) {
				return qname
			}
		}
		return none
	}
	if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind != .ident || base.value.len == 0 {
			return none
		}
		full := '${base.value}.${node.value}'
		if t.generated_used_name_is_known_fn(full) {
			return full
		}
		if !isnil(t.tc) && t.cur_file.len > 0 {
			if mod := t.tc.file_imports[file_import_key(t.cur_file, base.value)] {
				resolved := '${mod}.${node.value}'
				if t.generated_used_name_is_known_fn(resolved) {
					return resolved
				}
			}
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !full.contains('${t.cur_module}.') {
			qname := '${t.cur_module}.${full}'
			if t.generated_used_name_is_known_fn(qname) {
				return qname
			}
		}
	}
	return none
}

fn (t &Transformer) generated_used_name_is_known_fn(name string) bool {
	if name.len == 0 {
		return false
	}
	if name in t.fn_ret_types {
		return true
	}
	return !isnil(t.tc) && (name in t.tc.fn_ret_types || name in t.tc.fn_param_types)
}

fn (mut t Transformer) generated_call_name_for_used(id flat.NodeId, node flat.Node) string {
	if node.children_count > 0 {
		fn_id := t.a.child(&node, 0)
		fn_node := t.a.nodes[int(fn_id)]
		if fn_node.kind == .selector && fn_node.value.len > 0 && fn_node.children_count > 0 {
			base_id := t.a.child(&fn_node, 0)
			base_type := t.node_type(base_id).trim_left('&')
			_, _, base_is_generic := generic_app_parts(base_type)
			if base_is_generic {
				return '${base_type}.${fn_node.value}'
			}
		}
		if fn_node.kind == .ident && fn_node.value.len > 0 && t.var_type(fn_node.value).len > 0 {
			return ''
		}
	}
	call_name := t.call_name_for_node(id, node)
	if call_name.len > 0 {
		if !t.generated_call_matches_selector_receiver(call_name, node) {
			if receiver_call := t.generated_selector_receiver_call_name(node) {
				return receiver_call
			}
			return ''
		}
		return t.generated_known_call_name(call_name) or { call_name }
	}
	if node.children_count == 0 {
		return ''
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .ident && fn_node.value.len > 0 {
		if t.var_type(fn_node.value).len > 0 {
			return ''
		}
		return fn_node.value
	}
	if fn_node.kind == .selector && fn_node.value.len > 0 && fn_node.children_count > 0 {
		base_id := t.a.child(&fn_node, 0)
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 {
			return method_name
		}
		base_type := t.node_type(base_id).trim_left('&')
		_, _, base_is_generic := generic_app_parts(base_type)
		if base_is_generic {
			return '${base_type}.${fn_node.value}'
		}
	}
	return ''
}

fn (t &Transformer) generated_call_matches_selector_receiver(call_name string, node flat.Node) bool {
	if call_name.len == 0 || node.children_count == 0 {
		return true
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return true
	}
	receiver_name := call_name.all_before_last('.')
	if receiver_name.len == 0 {
		return true
	}
	base_type := t.generated_selector_receiver_type(fn_node)
	if base_type.len == 0 {
		return true
	}
	if receiver_name == base_type {
		return true
	}
	if !base_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && receiver_name == '${t.cur_module}.${base_type}' {
		return true
	}
	if receiver_name.all_after_last('.') != base_type.all_after_last('.') {
		return true
	}
	return false
}

fn (t &Transformer) generated_selector_receiver_call_name(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value.len == 0 {
		return none
	}
	base_type := t.generated_selector_receiver_type(fn_node)
	if base_type.len == 0 {
		return none
	}
	return '${base_type}.${fn_node.value}'
}

fn (t &Transformer) generated_selector_receiver_type(fn_node flat.Node) string {
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	mut base_type := ''
	if base_node.kind == .ident {
		base_type = t.var_type(base_node.value)
	}
	if base_type.len == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type.len == 0 && !isnil(t.tc) {
		base_type = t.tc.resolve_type(base_id).name()
	}
	for base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	return t.normalize_type_alias(base_type)
}

fn (t &Transformer) generated_known_call_name(name string) ?string {
	if name.len == 0 {
		return none
	}
	if t.generated_used_name_is_known_fn(name) {
		return name
	}
	if !name.contains('.') || t.cur_module.len == 0 || t.cur_module == 'main'
		|| t.cur_module == 'builtin' || name.starts_with('${t.cur_module}.') {
		return none
	}
	qname := '${t.cur_module}.${name}'
	if t.generated_used_name_is_known_fn(qname) {
		return qname
	}
	return none
}

fn (mut t Transformer) push_generated_used_name(name string, mut names []string, mut seen map[string]bool) {
	if name.len == 0 || seen[name] {
		return
	}
	seen[name] = true
	names << name
	cn := c_name(name)
	if cn != name && !seen[cn] {
		seen[cn] = true
		names << cn
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
		&& !name.contains('.') && !name.contains('__') {
		qname := '${t.cur_module}.${name}'
		if !seen[qname] {
			seen[qname] = true
			names << qname
		}
		qcn := c_name(qname)
		if qcn != qname && !seen[qcn] {
			seen[qcn] = true
			names << qcn
		}
	}
}

fn (mut t Transformer) specialize_cloned_fn_signature(clone_id flat.NodeId, decl GenericFnDecl, args []string) {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	t.set_node_typ(int(clone_id), t.specialized_signature_type_text(decl, decl.node.typ, args,
		params))
	t.set_node_generic_params(int(clone_id), []string{})
	mut dst_params := []flat.NodeId{}
	clone := t.a.nodes[int(clone_id)]
	for i in 0 .. clone.children_count {
		dst_id := t.a.child(&clone, i)
		if t.a.nodes[int(dst_id)].kind == .param {
			dst_params << dst_id
		}
	}
	mut param_idx := 0
	for i in 0 .. decl.node.children_count {
		src := t.a.child_node(&decl.node, i)
		if src.kind != .param {
			continue
		}
		if param_idx >= dst_params.len {
			continue
		}
		dst_id := dst_params[param_idx]
		t.set_node_typ(int(dst_id), t.specialized_signature_type_text(decl, src.typ, args, params))
		param_idx++
	}
}

fn (mut t Transformer) register_specialized_fn_signature(decl GenericFnDecl, clone flat.Node, args []string) {
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	t.cur_module = decl.module
	t.cur_file = decl.file
	if !isnil(t.tc) {
		t.tc.cur_module = decl.module
		t.tc.cur_file = decl.file
	}
	generic_params := t.generic_fn_param_names(decl.node, decl.module)
	ret_name := t.specialized_signature_type_text(decl, decl.node.typ, args, generic_params)
	ret := if !isnil(t.tc) {
		t.tc.parse_resolution_type(ret_name)
	} else {
		types.Type(types.void_)
	}
	mut params := []types.Type{}
	mut variadic := false
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		param_type := t.specialized_signature_type_text(decl, child.typ, args, generic_params)
		if param_type.starts_with('...') {
			variadic = true
		}
		if !isnil(t.tc) {
			params << t.tc.parse_resolution_type(param_type)
		}
	}
	qname := transform_qualified_fn_name(decl.module, clone.value)
	t.fn_ret_types[clone.value] = ret_name
	t.fn_ret_types[qname] = ret_name
	t.add_receiver_method_suffix_index(clone.value)
	t.add_receiver_method_suffix_index(qname)
	if !isnil(t.tc) {
		mut names := [clone.value, qname, c_name(clone.value),
			c_name(qname)]
		names << specialized_generic_fn_signature_aliases(decl, args)
		for name in names {
			t.tc.fn_ret_types[name] = ret
			t.tc.fn_param_types[name] = params.clone()
			t.tc.fn_variadic[name] = variadic
			t.add_receiver_method_suffix_index(name)
			t.tc.specialized_generic_fns[name] = true
		}
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	t.cur_module = old_module
	t.cur_file = old_file
}

fn (mut t Transformer) specialized_fn_return_type_text(decl GenericFnDecl, args []string) string {
	return t.specialized_signature_type_text(decl, decl.node.typ, args, t.generic_fn_param_names(decl.node,
		decl.module))
}

fn (mut t Transformer) specialized_fn_return_display_type_text(decl GenericFnDecl, args []string) string {
	substituted := substitute_generic_type_text_with_params(decl.node.typ, args, t.generic_fn_param_names(decl.node,
		decl.module))
	return t.qualify_specialized_signature_type_text(substituted, decl)
}

fn (mut t Transformer) specialized_signature_type_text(decl GenericFnDecl, typ string, args []string, params []string) string {
	if direct := t.specialized_direct_generic_type_text(typ, args, params) {
		return direct
	}
	substituted := substitute_generic_type_text_with_params(typ, args, params)
	qualified := t.qualify_specialized_signature_type_text(substituted, decl)
	is_shared := qualified.trim_space().starts_with('shared ')
	if isnil(t.tc) {
		return qualified
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	parsed := t.tc.parse_resolution_type(qualified)
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
	if parsed is types.Unknown {
		return qualified
	}
	if is_shared {
		return qualified
	}
	return specialized_signature_storage_type_name(parsed)
}

fn (t &Transformer) specialized_direct_generic_type_text(typ string, args []string, params []string) ?string {
	clean := typ.trim_space()
	for i, param in params {
		if clean == param && i < args.len {
			return args[i]
		}
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
		if clean.starts_with(prefix) {
			inner := t.specialized_direct_generic_type_text(clean[prefix.len..], args, params) or {
				return none
			}
			return prefix + inner
		}
	}
	return none
}

fn specialized_signature_storage_type_name(typ types.Type) string {
	if typ is types.Alias {
		return typ.name
	}
	if typ is types.OptionType {
		return '?' + specialized_signature_storage_type_name(typ.base_type)
	}
	if typ is types.ResultType {
		return '!' + specialized_signature_storage_type_name(typ.base_type)
	}
	if typ is types.Pointer {
		return '&' + specialized_signature_storage_type_name(typ.base_type)
	}
	return typ.name()
}

fn (t &Transformer) qualify_specialized_signature_type_text(typ string, decl GenericFnDecl) string {
	clean := typ.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return typ
	}
	if clean.starts_with('&') {
		return '&' + t.qualify_specialized_signature_type_text(clean[1..], decl)
	}
	if clean.starts_with('mut ') {
		inner := t.qualify_specialized_signature_type_text(clean[4..], decl)
		if inner.starts_with('&') {
			return inner
		}
		return '&' + inner
	}
	if clean.starts_with('?') {
		return '?' + t.qualify_specialized_signature_type_text(clean[1..], decl)
	}
	if clean.starts_with('!') {
		return '!' + t.qualify_specialized_signature_type_text(clean[1..], decl)
	}
	if clean.starts_with('...') {
		return '...' + t.qualify_specialized_signature_type_text(clean[3..], decl)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.qualify_specialized_signature_type_text(clean[7..], decl)
	}
	if clean.starts_with('[]') {
		return '[]' + t.qualify_specialized_signature_type_text(clean[2..], decl)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := t.qualify_specialized_signature_type_text(clean[4..bracket_end], decl)
			val := t.qualify_specialized_signature_type_text(clean[bracket_end + 1..], decl)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] +
				t.qualify_specialized_signature_type_text(clean[bracket_end + 1..], decl)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			parts << t.qualify_specialized_signature_type_text(part, decl)
		}
		return '(' + parts.join(', ') + ')'
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := generic_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_generic_args(clean[bracket + 1..bracket_end]) {
				parts << t.qualify_specialized_signature_type_text(part, decl)
			}
			base := t.qualify_specialized_signature_type_text(clean[..bracket], decl)
			return base + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	if clean.contains('.') || types.is_builtin_type_name(clean)
		|| clean in ['void', 'none', 'nil', 'C', 'JS'] {
		return clean
	}
	return t.selective_signature_type_symbol(decl.file, clean) or { clean }
}

fn (t &Transformer) selective_signature_type_symbol(file string, name string) ?string {
	if isnil(t.tc) || file.len == 0 || name.contains('.') {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file, name)] or { return none }
	for candidate in candidates {
		if candidate in t.tc.type_aliases || candidate in t.tc.structs
			|| candidate in t.tc.interface_names || candidate in t.tc.flag_enums
			|| candidate in t.tc.enum_names || candidate in t.tc.sum_types {
			return candidate
		}
	}
	return none
}

fn specialized_generic_fn_signature_aliases(decl GenericFnDecl, args []string) []string {
	if !decl.node.value.contains('.') {
		return []string{}
	}
	receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	method := decl.node.value.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return []string{}
	}
	flat_receiver := '${receiver}_${generic_type_suffixes(args)}'
	qflat_receiver := transform_qualified_fn_name(decl.module, flat_receiver)
	return [
		'${flat_receiver}.${method}',
		'${qflat_receiver}.${method}',
		c_name('${flat_receiver}.${method}'),
		c_name('${qflat_receiver}.${method}'),
		'${c_name(flat_receiver)}__${c_name(method)}',
		c_name('${decl.module}.${flat_receiver}.${method}'),
	]
}

fn (mut t Transformer) rewrite_generic_call_sites(decls map[string]GenericFnDecl, call_sites []GenericCallSite) {
	for site in call_sites {
		i := int(site.id)
		if i < 0 || i >= t.a.nodes.len {
			continue
		}
		node := t.a.nodes[i]
		if node.kind != .call {
			continue
		}
		decl_key, args := t.cached_generic_call_specialization(site.id, node, site.module, decls) or {
			continue
		}
		decl := decls[decl_key] or { continue }
		if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args, decl.module) {
			t.set_node_value(i, args.join(', '))
		}
		concrete_args := t.canonical_generic_specialization_args(args)
		if t.generic_decl_is_receiver_method(decl.node)
			&& !t.generic_call_is_static_assoc_selector(node, decl) {
			t.rewrite_generic_method_call(site.id, node, decl, concrete_args)
		} else {
			t.rewrite_generic_plain_call(site.id, node, decl, concrete_args)
		}
	}
}

fn (mut t Transformer) refresh_decl_assign_types_after_generic_rewrite() bool {
	mut changed := false
	for i in 0 .. t.a.nodes.len {
		node := t.a.nodes[i]
		if node.kind != .decl_assign || node.children_count != 2 {
			continue
		}
		lhs_id := t.a.child(&node, 0)
		rhs_id := t.a.child(&node, 1)
		if int(lhs_id) < 0 || int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
			continue
		}
		rhs := t.a.nodes[int(rhs_id)]
		if rhs.kind != .call || rhs.typ.len == 0 || t.generic_arg_is_unresolved(rhs.typ) {
			continue
		}
		if decl_type_is_usable(node.typ) && !t.generic_arg_is_unresolved(node.typ) {
			continue
		}
		t.set_node_typ(i, rhs.typ)
		changed = true
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident {
			t.set_node_typ(int(lhs_id), rhs.typ)
			t.set_var_type(lhs.value, rhs.typ)
		}
	}
	return changed
}

fn (mut t Transformer) concrete_generic_call_return_type(id flat.NodeId, node flat.Node) string {
	if t.skip_generics {
		return ''
	}
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return ''
	}
	decl_key := t.generic_call_decl_key(id, node, t.cur_module, decls) or { return '' }
	decl := decls[decl_key] or { return '' }
	if t.should_skip_generic_call_specialization(decl_key) {
		return ''
	}
	mut args := []string{}
	if explicit := t.explicit_generic_call_args(node, t.cur_module) {
		args = explicit.clone()
	} else {
		args = t.infer_generic_call_args_from_params(decl, node, t.cur_module) or { return '' }
	}
	if args.len == 0 || t.generic_args_have_placeholders(args) {
		return ''
	}
	if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args, t.cur_module) {
		t.set_node_value(int(id), args.join(', '))
	}
	ret := t.specialized_fn_return_type_text(decl, args)
	if ret.len == 0 || t.generic_arg_is_unresolved(ret) {
		return ''
	}
	return t.normalize_type_alias(ret)
}

fn (mut t Transformer) raw_generic_call_return_type(id flat.NodeId, node flat.Node) string {
	if t.skip_generics {
		return ''
	}
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return ''
	}
	decl_key := t.generic_call_decl_key(id, node, t.cur_module, decls) or { return '' }
	decl := decls[decl_key] or { return '' }
	if t.should_skip_generic_call_specialization(decl_key) {
		return ''
	}
	args := t.explicit_generic_call_args(node, t.cur_module) or { return '' }
	if args.len == 0 || t.generic_args_have_placeholders(args) {
		return ''
	}
	ret := t.specialized_fn_return_display_type_text(decl, args)
	if ret.len == 0 || t.generic_arg_is_unresolved(ret) {
		return ''
	}
	return ret
}

fn (mut t Transformer) concrete_generic_call_param_types(id flat.NodeId, node flat.Node) ?[]types.Type {
	if t.skip_generics {
		return none
	}
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return none
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return none
	}
	decl_key := t.generic_call_decl_key(id, node, t.cur_module, decls) or { return none }
	decl := decls[decl_key] or { return none }
	if t.should_skip_generic_call_specialization(decl_key) {
		return none
	}
	mut args := []string{}
	if explicit := t.explicit_generic_call_args(node, t.cur_module) {
		args = explicit.clone()
	} else {
		args = t.infer_generic_call_args_from_params(decl, node, t.cur_module) or { return none }
	}
	if args.len == 0 || t.generic_args_have_placeholders(args) {
		return none
	}
	if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args, t.cur_module) {
		t.set_node_value(int(id), args.join(', '))
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	mut result := []types.Type{}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		param_type := t.specialized_signature_type_text(decl, child.typ, args, params)
		result << t.tc.parse_type(param_type)
	}
	if result.len == 0 {
		return none
	}
	return result
}

fn (mut t Transformer) cached_generic_fn_decls() map[string]GenericFnDecl {
	if t.skip_generics {
		if !t.generic_fn_decls_ready {
			t.generic_fn_decls_cache = map[string]GenericFnDecl{}
			t.generic_receiver_methods_by_name = map[string][]string{}
			t.generic_fn_decls_ready = true
		}
		return t.generic_fn_decls_cache
	}
	if !t.generic_fn_decls_ready {
		t.generic_fn_decls_cache = t.collect_generic_fn_decls()
		t.build_generic_receiver_method_index()
		t.generic_fn_decls_ready = true
	}
	return t.generic_fn_decls_cache
}

fn (mut t Transformer) build_generic_receiver_method_index() {
	mut by_name := map[string][]string{}
	for key, decl in t.generic_fn_decls_cache {
		if !t.generic_decl_is_receiver_method(decl.node) {
			continue
		}
		method := key.all_after_last('.')
		by_name[method] << key
	}
	t.generic_receiver_methods_by_name = by_name.move()
}

fn (mut t Transformer) infer_generic_call_args_from_params(decl GenericFnDecl, node flat.Node, call_module string) ?[]string {
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return none
	}
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	mut inferred := map[string]string{}
	mut param_idx := 0
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		is_recv_param := is_receiver && param_idx == 0
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		arg_type := generic_arg_type_for_param(child.typ,
			t.generic_call_arg_type_for_inference(arg_id))
		if arg_type.len > 0 {
			infer_generic_type_args(child.typ, arg_type, mut inferred)
			if is_recv_param {
				t.infer_generic_receiver_suffix_args(child.typ, arg_type, mut inferred)
				t.infer_generic_embedded_receiver_args(child.typ, arg_type, mut inferred)
			}
		}
		t.infer_generic_struct_init_args(child.typ, arg_id, mut inferred)
		param_idx++
	}
	mut args := []string{cap: param_names.len}
	for name in param_names {
		arg := inferred[name] or { return none }
		args << t.generic_arg_for_call_and_decl_module(arg, call_module, decl.module)
	}
	return args
}

fn (mut t Transformer) rewrite_generic_plain_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args []string) {
	if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && t.plain_concrete_callee_shadows_decl(callee.value, decl) {
			return
		}
	}
	spec_value := specialized_generic_fn_value(decl.node.value, args)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	ret_typ := t.specialized_fn_return_type_text(decl, args)
	param_types := t.specialized_generic_call_param_type_texts(decl, args)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << t.make_ident(spec_name)
	for i in 1 .. node.children_count {
		arg_id := t.a.child(&node, i)
		param_idx := i - 1
		param_type := if param_idx < param_types.len { param_types[param_idx] } else { '' }
		children << t.retype_generic_call_literal_arg(arg_id, param_type)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	t.set_node(int(id), flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          ''
		typ:            ret_typ
	})
	t.clear_resolved_call(id)
}

fn (t &Transformer) plain_concrete_callee_shadows_decl(name string, decl GenericFnDecl) bool {
	if name.len == 0 || name.contains('.') {
		return false
	}
	if decl.key == name {
		return false
	}
	if decl.node.value == name || decl.key == transform_qualified_fn_name(decl.module, name) {
		return false
	}
	if !isnil(t.tc) {
		if t.concrete_fn_return_known(name) {
			return true
		}
		return false
	}
	return name in t.fn_ret_types
}

fn (t &Transformer) concrete_fn_return_known(name string) bool {
	ret := t.tc.fn_ret_types[name] or { return false }
	return ret !is types.Unknown
}

fn (mut t Transformer) specialized_generic_call_param_type_texts(decl GenericFnDecl, args []string) []string {
	params := t.generic_fn_param_names(decl.node, decl.module)
	mut result := []string{}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		result << t.specialized_call_target_type_text(decl, child.typ, args, params)
	}
	return result
}

fn (mut t Transformer) specialized_call_target_type_text(decl GenericFnDecl, typ string, args []string, params []string) string {
	return t.specialized_signature_type_text(decl, typ, args, params)
}

fn (mut t Transformer) retype_generic_call_literal_arg(arg_id flat.NodeId, param_type string) flat.NodeId {
	if int(arg_id) < 0 || param_type.len == 0 {
		return arg_id
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .none_expr {
		if t.is_optional_type_name(param_type) {
			return t.make_optional_none(t.resolve_substituted_type_text(t.qualify_optional_type(param_type)))
		}
		if param_type == 'Optional' || param_type.starts_with('Optional_') {
			return t.make_optional_none(param_type)
		}
	}
	if node.kind == .struct_init && node.value == 'Optional'
		&& (t.is_optional_type_name(param_type) || param_type.starts_with('Optional_')) {
		optional_type := if t.is_optional_type_name(param_type) {
			t.resolve_substituted_type_text(t.qualify_optional_type(param_type))
		} else {
			param_type
		}
		return t.make_optional_none(optional_type)
	}
	if node.kind == .array_literal
		&& t.generic_call_array_literal_can_retype_inline(node, param_type) {
		mut values := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			values << t.a.child(&node, i)
		}
		return t.make_array_literal_typed(values, param_type)
	}
	if node.kind == .ident && node.value.contains('arr_lit') && node.typ.starts_with('[]')
		&& param_type.starts_with('[]') && node.typ != param_type {
		if t.retype_lowered_array_literal_temp(node.value, param_type) {
			t.set_node_typ(int(arg_id), param_type)
			t.set_var_type(node.value, param_type)
		}
	}
	return arg_id
}

fn (t &Transformer) generic_call_array_literal_can_retype_inline(node flat.Node, param_type string) bool {
	if !param_type.starts_with('[]') {
		return false
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind !in [.int_literal, .float_literal, .bool_literal, .char_literal,
			.string_literal, .cast_expr, .as_expr] {
			return false
		}
	}
	return true
}

fn (mut t Transformer) retype_lowered_array_literal_temp(name string, array_type string) bool {
	if !array_type.starts_with('[]') {
		return false
	}
	elem_type := array_type[2..]
	mut changed := false
	for i in 0 .. t.a.nodes.len {
		node := t.a.nodes[i]
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs_id := t.a.child(&node, 0)
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value == name {
				rhs_id := t.a.child(&node, 1)
				t.set_node_typ(i, array_type)
				t.set_node_typ(int(lhs_id), array_type)
				t.set_var_type(name, array_type)
				t.retype_array_new_call(rhs_id, elem_type, array_type)
				changed = true
			}
		}
		if node.kind == .call && t.call_is_array_push_to_name(node, name) {
			value_name := t.array_push_value_name(node) or { continue }
			if t.retype_decl_assign_name(value_name, elem_type) {
				changed = true
			}
		}
	}
	return changed
}

fn (mut t Transformer) retype_array_new_call(call_id flat.NodeId, elem_type string, array_type string) {
	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len {
		return
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 2 {
		return
	}
	callee := t.a.child_node(&call, 0)
	if callee.kind != .ident || callee.value != 'array_new' {
		return
	}
	sizeof_id := t.a.child(&call, 1)
	if int(sizeof_id) >= 0 && int(sizeof_id) < t.a.nodes.len {
		if t.a.nodes[int(sizeof_id)].kind == .sizeof_expr {
			t.set_node_value(int(sizeof_id), elem_type)
		}
	}
	t.set_node_typ(int(call_id), array_type)
}

fn (t &Transformer) call_is_array_push_to_name(node flat.Node, name string) bool {
	if node.children_count < 3 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != 'array_push' {
		return false
	}
	arg := t.a.child_node(&node, 1)
	if arg.kind != .prefix || arg.children_count == 0 {
		return false
	}
	base := t.a.child_node(arg, 0)
	return base.kind == .ident && base.value == name
}

fn (t &Transformer) array_push_value_name(node flat.Node) ?string {
	if node.children_count < 3 {
		return none
	}
	arg := t.a.child_node(&node, 2)
	if arg.kind != .prefix || arg.children_count == 0 {
		return none
	}
	base := t.a.child_node(arg, 0)
	if base.kind != .ident || base.value.len == 0 {
		return none
	}
	return base.value
}

fn (mut t Transformer) retype_decl_assign_name(name string, typ string) bool {
	mut changed := false
	for i in 0 .. t.a.nodes.len {
		node := t.a.nodes[i]
		if node.kind != .decl_assign || node.children_count < 1 {
			continue
		}
		lhs_id := t.a.child(&node, 0)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value != name {
			continue
		}
		t.set_node_typ(i, typ)
		t.set_node_typ(int(lhs_id), typ)
		t.set_var_type(name, typ)
		changed = true
	}
	return changed
}

// call_is_selector_form reports whether a call node embeds its receiver inside the
// callee (`recv.method(args)`) rather than passing it as an explicit child
// (`Type.method(recv, args)`, the ident-lowered form).
fn (t &Transformer) call_is_selector_form(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	mut callee := t.a.nodes[int(t.a.child(&node, 0))]
	if callee.kind == .index && callee.children_count > 0 {
		callee = t.a.nodes[int(t.a.child(&callee, 0))]
	}
	return callee.kind == .selector
}

fn (mut t Transformer) rewrite_generic_method_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args []string) {
	if node.children_count == 0 {
		return
	}
	t.rewrite_method_level_generic_call(id, node, decl, args)
}

// rewrite_method_level_generic_call rewrites a call to a method-level generic into
// an explicit call of the specialized function: `SpecName(receiver, args...)`. It
// handles both the selector form (receiver inside the callee) and the
// ident-lowered form (receiver already an explicit child).
fn (mut t Transformer) rewrite_method_level_generic_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args []string) {
	old_params := t.active_generic_params
	t.active_generic_params = t.generic_fn_param_names(decl.node, decl.module)
	spec_value := specialized_generic_fn_value(decl.node.value, args)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	ret_typ := t.specialized_signature_type_text(decl, decl.node.typ, args, t.active_generic_params)
	t.active_generic_params = old_params
	param_types := t.specialized_generic_call_param_type_texts(decl, args)

	mut children := []flat.NodeId{cap: int(node.children_count) + 1}
	children << t.make_ident(spec_name)
	if t.call_is_selector_form(node) {
		// Receiver is embedded in the selector callee; explicit args are children 1..n.
		if recv_id := t.generic_call_receiver_id(node) {
			children << recv_id
		}
		for i in 1 .. node.children_count {
			arg_id := t.a.child(&node, i)
			param_type := if i < param_types.len { param_types[i] } else { '' }
			children << t.retype_generic_call_literal_arg(arg_id, param_type)
		}
	} else {
		// Ident-lowered form: receiver and args are already explicit children 1..n.
		for i in 1 .. node.children_count {
			arg_id := t.a.child(&node, i)
			param_idx := i - 1
			param_type := if param_idx < param_types.len { param_types[param_idx] } else { '' }
			children << t.retype_generic_call_literal_arg(arg_id, param_type)
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	t.set_node(int(id), flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          ''
		typ:            ret_typ
	})
	t.clear_resolved_call(id)
}

fn (mut t Transformer) clear_resolved_call(id flat.NodeId) {
	if isnil(t.tc) {
		return
	}
	idx := int(id)
	if idx >= 0 && idx < t.tc.resolved_call_names.len {
		t.tc.resolved_call_names[idx] = ''
		t.tc.resolved_call_set[idx] = false
	}
}

fn (mut t Transformer) erase_generic_fn_decls(decls map[string]GenericFnDecl) {
	for _, decl in decls {
		t.unregister_generic_fn_signature(decl)
		if int(decl.id) < 0 || int(decl.id) >= t.a.nodes.len {
			continue
		}
		mut subtree := map[int]bool{}
		t.collect_node_subtree_ids(decl.id, mut subtree)
		for idx, _ in subtree {
			old := t.a.nodes[idx]
			t.set_node(idx, flat.Node{
				kind: .empty
				pos:  old.pos
			})
			t.clear_typechecker_node_cache(idx)
		}
	}
}

fn (mut t Transformer) unregister_generic_fn_signature(decl GenericFnDecl) {
	if isnil(t.tc) {
		return
	}
	mut names := [decl.key, c_name(decl.key)]
	if decl.module.len == 0 || decl.module == 'main' || decl.module == 'builtin' {
		names << decl.node.value
		names << c_name(decl.node.value)
	}
	if decl.module.len > 0 && decl.module != 'main' && decl.module != 'builtin' {
		qname := transform_qualified_fn_name(decl.module, decl.node.value)
		names << qname
		names << c_name(qname)
	}
	for name in names {
		t.tc.fn_ret_types.delete(name)
		t.tc.fn_param_types.delete(name)
		t.tc.fn_variadic.delete(name)
	}
}

fn (mut t Transformer) cached_generic_call_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []string) {
	idx := int(id)
	if spec := t.generic_call_spec_cache[idx] {
		return spec.decl_key, spec.args
	}
	if t.generic_call_spec_misses[idx] {
		return none
	}
	decl_key, args := t.generic_call_specialization(id, node, module_name, decls) or {
		t.generic_call_spec_misses[idx] = true
		return none
	}
	t.generic_call_spec_cache[idx] = GenericCallSpec{
		decl_key: decl_key
		args:     args
	}
	return decl_key, args
}

fn (mut t Transformer) generic_call_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []string) {
	if node.children_count == 0 {
		return none
	}
	decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
	decl := decls[decl_key] or { return none }
	if t.should_skip_generic_call_specialization(decl_key) {
		return none
	}
	if t.call_has_source_generic_args(node) || node.value.len > 0 {
		if args := t.explicit_generic_call_args(node, module_name) {
			if args.len > 0 && !t.generic_args_have_placeholders(args) {
				return decl_key, args
			}
		}
	} else if args := t.explicit_generic_call_args(node, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args)
			&& t.generic_args_contain_alias(args, module_name) {
			return decl_key, args
		}
	}
	if args := t.infer_generic_call_args(decl, id, node, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	if args := t.infer_generic_call_args_from_receiver_record(decl, node, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	if args := t.specialized_plain_generic_call_args(node, decl, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	if args := t.explicit_generic_call_args(node, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	return none
}

fn (mut t Transformer) infer_generic_call_args_from_receiver_record(decl GenericFnDecl, node flat.Node, call_module string) ?[]string {
	if !t.generic_decl_is_receiver_method(decl.node) || node.children_count == 0
		|| t.generic_call_is_static_assoc_selector(node, decl) {
		return none
	}
	recv_id := t.generic_call_receiver_id(node) or { return none }
	recv_type := t.generic_call_arg_type_for_inference(recv_id)
	recorded := t.recorded_generic_specialization_args(recv_type) or { return none }
	if recorded.len == 0 {
		return none
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len == 0 {
		return none
	}
	mut args := []string{}
	mut recorded_idx := 0
	for param in params {
		if !is_generic_fn_placeholder_name(param) {
			continue
		}
		if recorded_idx >= recorded.len {
			return none
		}
		args << t.generic_arg_for_call_and_decl_module(recorded[recorded_idx], call_module,
			decl.module)
		recorded_idx++
	}
	if args.len == 0 {
		return none
	}
	return args
}

fn (t &Transformer) call_has_source_generic_args(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.kind == .index && fn_node.children_count >= 2 && fn_node.value != 'range'
		&& !t.index_callee_is_value_index(fn_node)
}

fn (t &Transformer) should_skip_generic_call_specialization(decl_key string) bool {
	// The old `json` module's decode/encode are C-magic (cJSON) and handled by a
	// cgen shortcut; `json2`/`x.json2` are pure V and monomorphize normally.
	return decl_key in ['json.decode', 'json.encode', 'veb.run_at', 'veb.run_new']
}

fn (mut t Transformer) generic_call_decl_key(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	if node.children_count == 0 {
		return none
	}
	mut callee_id := t.a.child(&node, 0)
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		if t.index_callee_is_value_index(callee) {
			return none
		}
		callee_id = t.a.child(&callee, 0)
		callee = t.a.nodes[int(callee_id)]
	}
	if !isnil(t.tc) {
		if resolved := t.tc.resolved_call_name(id) {
			key := t.generic_resolved_call_decl_key(resolved, callee, node, module_name, decls) or {
				return none
			}
			if decl := decls[key] {
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					return none
				}
				return key
			}
			return none
		}
	}
	if callee.kind == .ident {
		direct_key := generic_fn_decl_base_value(callee.value)
		if decl := decls[direct_key] {
			if t.generic_call_arg_count_matches_decl(node, decl) {
				return direct_key
			}
		}
		if key := t.generic_flat_receiver_call_decl_key(callee.value, module_name, decls) {
			if decl := decls[key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if key := t.specialized_plain_generic_call_decl_key(callee.value, module_name, decls) {
			if decl := decls[key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if t.local_concrete_fn_shadows_generic(callee.value, module_name, decls)
			|| t.plain_concrete_fn_known(callee.value, module_name, decls) {
			return none
		}
		for candidate in t.generic_plain_call_candidates(callee.value, module_name) {
			key := generic_fn_decl_base_value(candidate)
			if decl := decls[key] {
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					continue
				}
				return key
			}
		}
	} else if callee.kind == .selector && callee.children_count > 0 {
		base_id := t.a.child(&callee, 0)
		if static_key := t.generic_static_assoc_call_decl_key(base_id, callee.value, decls) {
			if decl := decls[static_key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return static_key
				}
			}
		}
		base := t.a.nodes[int(base_id)]
		if base.kind == .ident && t.ident_is_import_alias(base.value) {
			mod_name := t.import_alias_module(base.value)
			for candidate in ['${mod_name}.${callee.value}', '${base.value}.${callee.value}'] {
				key := generic_fn_decl_base_value(candidate)
				if decl := decls[key] {
					if !t.generic_call_arg_count_matches_decl(node, decl) {
						continue
					}
					return key
				}
			}
		}
		method_keys := t.generic_receiver_methods_by_name[callee.value] or { return none }
		mut base_type := t.syntactic_static_call_return_type(base_id)
		if base_type.len == 0 {
			base_type = t.node_type(base_id)
		}
		if base_type.starts_with('&') {
			base_type = base_type[1..]
		}
		base_name, _, ok := generic_app_parts(base_type)
		if ok {
			key := t.generic_receiver_decl_key(base_name, callee.value, decls)
			if decl := decls[key] {
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					return none
				}
				return key
			}
		}
		for key in method_keys {
			if decl := decls[key] {
				if !t.generic_receiver_decl_matches_type(base_type, decl, module_name) {
					continue
				}
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					continue
				}
				return key
			}
		}
	}
	return none
}

fn (t &Transformer) generic_receiver_decl_matches_type(base_type string, decl GenericFnDecl, module_name string) bool {
	decl_receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	if decl_receiver.len == 0 {
		return false
	}
	mut clean_base := base_type.trim_space()
	if clean_base.starts_with('&') {
		clean_base = clean_base[1..]
	}
	if clean_base.len == 0 {
		return false
	}
	base, _, ok := generic_app_parts(clean_base)
	if ok {
		clean_base = base
	}
	call_qualified := t.normalize_type_in_module(clean_base, module_name)
	decl_qualified := t.normalize_type_in_module(decl_receiver, decl.module)
	if call_qualified.contains('.') && decl_qualified.contains('.')
		&& call_qualified != decl_qualified {
		// Receiver short names are not unique across modules. Once both sides can
		// be resolved, do not let `json2.Decoder` match `decoder2.Decoder` merely
		// because both declarations are named `Decoder`.
		return false
	}
	mut call_receivers := []string{}
	for candidate in [clean_base, t.normalize_type_alias(clean_base),
		t.normalize_type_in_module(clean_base, module_name)] {
		if candidate.len > 0 && candidate !in call_receivers {
			call_receivers << candidate
		}
		if candidate.contains('.') {
			short := candidate.all_after_last('.')
			if short.len > 0 && short !in call_receivers {
				call_receivers << short
			}
		}
	}
	mut decl_receivers := []string{}
	for candidate in [decl_receiver, t.normalize_type_alias(decl_receiver),
		t.normalize_type_in_module(decl_receiver, decl.module)] {
		if candidate.len > 0 && candidate !in decl_receivers {
			decl_receivers << candidate
		}
		if candidate.contains('.') {
			short := candidate.all_after_last('.')
			if short.len > 0 && short !in decl_receivers {
				decl_receivers << short
			}
		} else if decl.module.len > 0 && decl.module !in ['main', 'builtin'] {
			qualified := '${decl.module}.${candidate}'
			if qualified !in decl_receivers {
				decl_receivers << qualified
			}
		}
	}
	for call_receiver in call_receivers {
		if call_receiver in decl_receivers {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_static_assoc_call_decl_key(base_id flat.NodeId, method string, decls map[string]GenericFnDecl) ?string {
	if method.len == 0 || int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return none
	}
	for type_name in t.generic_static_assoc_type_candidates(base_id) {
		key := generic_fn_decl_base_value('${type_name}.${method}')
		if key in decls {
			return key
		}
	}
	return none
}

fn (t &Transformer) generic_call_is_static_assoc_selector(node flat.Node, decl GenericFnDecl) bool {
	if node.children_count == 0 || !decl.node.value.contains('.') {
		return false
	}
	callee_id := t.a.child(&node, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	method := decl.node.value.all_after_last('.')
	if callee.value != method {
		return false
	}
	base_id := t.a.child(callee, 0)
	for type_name in t.generic_static_assoc_type_candidates(base_id) {
		key := generic_fn_decl_base_value('${type_name}.${method}')
		if key == decl.key || key == generic_fn_decl_base_value(decl.node.value) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_static_assoc_type_candidates(base_id flat.NodeId) []string {
	base := t.a.nodes[int(base_id)]
	mut candidates := []string{}
	if base.kind == .ident {
		if base.value == 'C' || t.ident_is_import_alias(base.value) {
			return candidates
		}
		candidates << base.value
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind == .ident {
			if t.ident_is_import_alias(inner.value) {
				mod_name := t.import_alias_module(inner.value)
				candidates << '${mod_name}.${base.value}'
			}
			candidates << '${inner.value}.${base.value}'
		}
	}
	mut result := []string{}
	for candidate in candidates {
		for type_name in t.static_assoc_type_candidates(candidate) {
			if type_name !in result {
				result << type_name
			}
		}
	}
	return result
}

fn (t &Transformer) plain_concrete_fn_known(name string, module_name string, decls map[string]GenericFnDecl) bool {
	if name.len == 0 {
		return false
	}
	qname := transform_qualified_fn_name(module_name, name)
	for candidate in [qname, name] {
		if decl := decls[candidate] {
			if generic_decl_module_matches_call_module(decl.module, module_name) {
				return false
			}
		}
		if candidate in t.fn_ret_types {
			return true
		}
		if !isnil(t.tc) && t.concrete_fn_return_known(candidate) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_flat_receiver_call_decl_key(name string, module_name string, decls map[string]GenericFnDecl) ?string {
	if !name.contains('_') {
		return none
	}
	receiver, method := generic_flat_receiver_call_parts(name) or { return none }
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	method_keys := t.generic_receiver_methods_by_name[method] or { return none }
	for key in method_keys {
		decl := decls[key] or { continue }
		decl_receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
		if decl_receiver.len == 0 {
			continue
		}
		if generic_flat_receiver_matches(receiver, decl_receiver, module_name, decl.module) {
			return key
		}
	}
	return none
}

fn (t &Transformer) specialized_plain_generic_call_decl_key(name string, module_name string, decls map[string]GenericFnDecl) ?string {
	if !name.contains('_T_') {
		return none
	}
	base := name.all_before('_T_')
	if base.len == 0 {
		return none
	}
	mut candidates := [base]
	if base.contains('__') {
		candidates << base.replace('__', '.')
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !base.contains('.') && !base.contains('__') {
		candidates << '${module_name}.${base}'
	}
	for candidate in candidates {
		key := generic_fn_decl_base_value(candidate)
		if key in decls {
			return key
		}
	}
	return none
}

fn (mut t Transformer) specialized_plain_generic_call_args(node flat.Node, decl GenericFnDecl, module_name string) ?[]string {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident {
		return none
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if recorded := t.recorded_generic_specialization_args(callee.value) {
		if recorded.len == params.len {
			return recorded.clone()
		}
	}
	if !callee.value.contains('_T_') {
		return none
	}
	suffix := callee.value.all_after('_T_')
	if suffix.len == 0 {
		return none
	}
	if params.len != 1 {
		return none
	}
	arg := generic_type_arg_from_suffix(suffix)
	if arg.len == 0 {
		return none
	}
	return [t.generic_arg_for_call_and_decl_module(arg, module_name, decl.module)]
}

fn generic_flat_receiver_call_parts(name string) ?(string, string) {
	if name.contains('.') {
		receiver := name.all_before_last('.')
		method := name.all_after_last('.')
		if receiver.len > 0 && method.len > 0 {
			return receiver, method
		}
	}
	if name.contains('__') {
		receiver := name.all_before_last('__')
		method := name.all_after_last('__')
		if receiver.len > 0 && method.len > 0 {
			return receiver, method
		}
	}
	return none
}

fn generic_flat_receiver_matches(receiver string, decl_receiver string, module_name string, decl_module string) bool {
	mut bases := []string{}
	bases << decl_receiver
	if decl_module.len > 0 && decl_module != 'main' && decl_module != 'builtin'
		&& !decl_receiver.contains('.') {
		bases << '${decl_module}.${decl_receiver}'
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !decl_receiver.contains('.') {
		bases << '${module_name}.${decl_receiver}'
	}
	if decl_receiver.contains('.') {
		short_receiver := decl_receiver.all_after_last('.')
		bases << short_receiver
		bases << '${decl_receiver.all_before_last('.')}.${short_receiver}'
	}
	for base in bases {
		if base.len == 0 {
			continue
		}
		if receiver.starts_with('${base}_') {
			return true
		}
		cbase := c_name(base)
		if cbase != base && receiver.starts_with('${cbase}_') {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_resolved_call_decl_key(resolved string, callee flat.Node, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	key := generic_fn_decl_base_value(resolved)
	if decl := decls[key] {
		if !t.resolved_generic_decl_matches_callee_receiver(callee, node, decl, module_name) {
			return none
		}
		return key
	}
	if flat_key := t.generic_flat_receiver_call_decl_key(key, module_name, decls) {
		return flat_key
	}
	if flat_key := t.generic_flat_receiver_call_decl_key(resolved, module_name, decls) {
		return flat_key
	}
	if callee.kind == .ident
		&& (t.local_concrete_fn_shadows_generic(callee.value, module_name, decls)
		|| t.resolved_call_is_concrete_fn(resolved, key)) {
		return none
	}
	resolved_mod := if key.contains('.') { key.all_before_last('.') } else { module_name }
	short := key.all_after_last('.')
	if resolved_mod.len > 0 {
		qshort := '${resolved_mod}.${short}'
		if qshort in decls {
			return qshort
		}
	}
	if decl := decls[short] {
		if decl.module == resolved_mod || decl.module == module_name {
			return short
		}
	}
	if callee.kind == .ident
		&& !t.local_concrete_fn_shadows_generic(callee.value, module_name, decls) {
		for candidate in t.generic_plain_call_candidates(callee.value, module_name) {
			candidate_key := generic_fn_decl_base_value(candidate)
			if candidate_key in decls {
				return candidate_key
			}
		}
	}
	return none
}

fn (t &Transformer) resolved_generic_decl_matches_callee_receiver(callee flat.Node, node flat.Node, decl GenericFnDecl, module_name string) bool {
	if !t.generic_decl_is_receiver_method(decl.node) {
		return true
	}
	mut base_id := flat.empty_node
	if callee.kind == .selector && callee.children_count > 0 {
		base_id = t.a.child(&callee, 0)
	} else if callee.kind == .ident && node.children_count > 1 {
		base_id = t.a.child(&node, 1)
	} else {
		return true
	}
	mut base_type := t.syntactic_static_call_return_type(base_id)
	if base_type.len == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type.len == 0 {
		return true
	}
	return t.generic_receiver_decl_matches_type(base_type, decl, module_name)
}

fn (t &Transformer) syntactic_static_call_return_type(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return ''
	}
	base := t.a.child_node(callee, 0)
	mut owner := ''
	if base.kind == .ident {
		owner = base.value
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(base, 0)
		if inner.kind == .ident {
			owner = '${inner.value}.${base.value}'
		}
	}
	if owner.len == 0 {
		return ''
	}
	name := '${owner}.${callee.value}'
	for candidate in [name, transform_qualified_fn_name(t.cur_module, name)] {
		if ret := t.fn_ret_types[candidate] {
			return ret
		}
		if !isnil(t.tc) {
			if typ := t.tc.fn_ret_types[candidate] {
				ret_name := typ.name()
				if ret_name.len > 0 && ret_name != 'unknown' && ret_name != 'void' {
					return ret_name
				}
			}
		}
	}
	if callee.value == 'new' {
		for type_name in t.static_assoc_type_candidates(owner) {
			return type_name
		}
		if t.syntactic_new_owner_is_type_like(base, owner) {
			return owner
		}
	}
	return ''
}

fn (t &Transformer) syntactic_new_owner_is_type_like(base flat.Node, owner string) bool {
	short_owner := owner.all_after_last('.')
	if short_owner.len == 0 || !v3_type_name_starts_upper(short_owner) {
		return false
	}
	if base.kind == .ident {
		return t.var_type(base.value).len == 0 && !t.ident_is_import_alias(base.value)
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind != .ident || t.var_type(inner.value).len > 0 {
			return false
		}
		return t.ident_is_import_alias(inner.value) || v3_type_name_starts_upper(inner.value)
	}
	return false
}

fn v3_type_name_starts_upper(name string) bool {
	if name.len == 0 {
		return false
	}
	return name[0] >= `A` && name[0] <= `Z`
}

fn (t &Transformer) resolved_call_is_concrete_fn(resolved string, key string) bool {
	for candidate in [resolved, key] {
		if candidate.len == 0 {
			continue
		}
		if candidate in t.fn_ret_types {
			return true
		}
		if !isnil(t.tc) && t.concrete_fn_return_known(candidate) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_call_arg_count_matches_decl(node flat.Node, decl GenericFnDecl) bool {
	mut param_count := 0
	mut is_variadic := false
	mut has_trailing_params_struct := false
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		param_count++
		if child.typ.starts_with('...') {
			is_variadic = true
		}
		has_trailing_params_struct = t.generic_param_is_params_struct(child.typ, decl.module)
	}
	// param_count includes the receiver for methods. The call's child count
	// depends on form: the selector form (`recv.method(args)`) keeps the receiver
	// inside the callee child, so children = [callee, args...] and the callee child
	// offsets the receiver param (actual == children_count). The ident-lowered form
	// (`Type.method(recv, args)`) carries the receiver as a real child, so
	// children = [callee, recv, args...] and actual == children_count - 1.
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	actual_args := t.generic_call_effective_arg_count(node)
	actual := if is_receiver && t.call_is_selector_form(node) {
		actual_args + 1
	} else {
		actual_args
	}
	if is_variadic {
		return actual >= param_count - 1
	}
	if has_trailing_params_struct {
		return actual >= param_count - 1
	}
	return actual == param_count
}

fn (t &Transformer) generic_param_is_params_struct(param_type string, decl_module string) bool {
	if param_type.len == 0 {
		return false
	}
	if _ := t.params_struct_type_name(param_type) {
		return true
	}
	if !param_type.contains('.') && decl_module.len > 0 && decl_module != 'main'
		&& decl_module != 'builtin' {
		if _ := t.params_struct_type_name('${decl_module}.${param_type}') {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_call_effective_arg_count(node flat.Node) int {
	if node.children_count <= 1 {
		return 0
	}
	mut count := 0
	mut i := 1
	for i < node.children_count {
		arg := t.a.child_node(&node, i)
		if arg.kind == .field_init {
			count++
			for i < node.children_count {
				field := t.a.child_node(&node, i)
				if field.kind != .field_init {
					break
				}
				i++
			}
			continue
		}
		count++
		i++
	}
	return count
}

fn (t &Transformer) local_concrete_fn_shadows_generic(name string, module_name string, decls map[string]GenericFnDecl) bool {
	qname := transform_qualified_fn_name(module_name, name)
	if decl := decls[name] {
		if generic_decl_module_matches_call_module(decl.module, module_name) {
			return false
		}
	}
	if decl := decls[qname] {
		if generic_decl_module_matches_call_module(decl.module, module_name) {
			return false
		}
	}
	if qname in t.fn_ret_types || name in t.fn_ret_types {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return qname in t.tc.fn_param_types || qname in t.tc.fn_ret_types || name in t.tc.fn_param_types
		|| name in t.tc.fn_ret_types
}

fn generic_decl_module_matches_call_module(decl_module string, call_module string) bool {
	return decl_module == call_module
		|| (decl_module in ['', 'main'] && call_module in ['', 'main'])
}

fn (t &Transformer) generic_plain_call_candidates(name string, module_name string) []string {
	mut candidates := []string{}
	candidates << name
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		candidates << '${module_name}.${name}'
	}
	if !isnil(t.tc) {
		qname := t.tc.qualify_fn_name(name)
		candidates << qname
	}
	return candidates
}

fn (t &Transformer) generic_receiver_decl_key(base_name string, method string, decls map[string]GenericFnDecl) string {
	direct := '${base_name}.${method}'
	if direct in decls {
		return direct
	}
	short := base_name.all_after_last('.')
	for key, _ in decls {
		if key == '${short}.${method}' || key.ends_with('.${short}.${method}') {
			return key
		}
	}
	return direct
}

fn (t &Transformer) explicit_generic_call_args(node flat.Node, module_name string) ?[]string {
	if node.value.len > 0 {
		return normalize_generic_args(split_generic_args(node.value), module_name)
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return none
	}
	type_arg := t.generic_call_type_args_name(fn_node)
	if type_arg.len == 0 {
		return none
	}
	return normalize_generic_args(split_generic_args(type_arg), module_name)
}

fn (t &Transformer) generic_call_arg_id_for_param(node flat.Node, param_idx int, is_receiver bool) ?flat.NodeId {
	is_recv_param := is_receiver && param_idx == 0
	selector_form := is_receiver && t.call_is_selector_form(node)
	if is_recv_param {
		if selector_form {
			return t.generic_call_receiver_id(node)
		}
		if int(node.children_count) <= 1 {
			return none
		}
		return t.a.child(&node, 1)
	}
	arg_idx := if selector_form { param_idx } else { param_idx + 1 }
	if arg_idx >= int(node.children_count) {
		return none
	}
	return t.a.child(&node, arg_idx)
}

fn (mut t Transformer) infer_generic_call_args(decl GenericFnDecl, _id flat.NodeId, node flat.Node, call_module string) ?[]string {
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return none
	}
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	mut inferred := map[string]string{}
	mut param_idx := 0
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		is_recv_param := is_receiver && param_idx == 0
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		arg_type := generic_arg_type_for_param(child.typ,
			t.generic_call_arg_type_for_inference(arg_id))
		if arg_type.len > 0 {
			infer_generic_type_args(child.typ, arg_type, mut inferred)
			if is_recv_param {
				t.infer_generic_receiver_suffix_args(child.typ, arg_type, mut inferred)
				t.infer_generic_embedded_receiver_args(child.typ, arg_type, mut inferred)
			}
		}
		t.infer_generic_struct_init_args(child.typ, arg_id, mut inferred)
		param_idx++
	}
	ret := t.node_type(_id)
	if ret.len > 0 {
		infer_generic_type_args(decl.node.typ, ret, mut inferred)
	}
	mut args := []string{cap: param_names.len}
	for name in param_names {
		arg := inferred[name] or { return none }
		args << t.generic_arg_for_call_and_decl_module(arg, call_module, decl.module)
	}
	return args
}

fn (mut t Transformer) infer_generic_struct_init_args(param_type string, arg_id flat.NodeId, mut inferred map[string]string) {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .struct_init {
		return
	}
	param_base, param_args, is_generic_struct := generic_app_parts(param_type.trim_space())
	if !is_generic_struct || param_args.len == 0 {
		return
	}
	info := t.lookup_struct_info(param_base) or { return }
	for i in 0 .. arg.children_count {
		field := t.a.child_node(&arg, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		mut field_type := ''
		for struct_field in info.fields {
			if struct_field.name == field_name {
				field_type = struct_field.typ
				break
			}
		}
		if field_type.len == 0 {
			continue
		}
		value_id := t.a.child(field, 0)
		value_type := t.generic_call_arg_type_for_inference(value_id)
		if value_type.len > 0 {
			infer_generic_type_args(field_type, value_type, mut inferred)
		}
	}
}

fn (t &Transformer) generic_arg_for_call_and_decl_module(arg string, call_module string, decl_module string) string {
	if t.substituted_type_belongs_to_main_generic(arg)
		&& (call_module in ['', 'main'] || t.current_specialization_has_generic_arg(arg)) {
		return arg
	}
	if t.generic_type_text_contains_alias(arg, call_module) {
		if t.generic_type_text_contains_fixed_array_alias(arg, call_module) {
			call_normalized := t.normalize_type_in_module(arg, call_module)
			if call_normalized != arg {
				return t.generic_arg_for_decl_module(call_normalized, decl_module)
			}
		}
		call_qualified := t.qualify_generic_arg_for_decl_module(arg, call_module)
		return t.generic_arg_for_decl_module(call_qualified, decl_module)
	}
	call_normalized := t.normalize_type_in_module(arg, call_module)
	return t.generic_arg_for_decl_module(call_normalized, decl_module)
}

fn (t &Transformer) current_specialization_has_generic_arg(arg string) bool {
	receiver := t.current_fn_receiver_type()
	if receiver.len == 0 {
		return false
	}
	_, parsed_args, is_generic := generic_app_parts(receiver)
	args := if is_generic {
		parsed_args
	} else {
		t.recorded_generic_specialization_args(receiver) or { return false }
	}
	clean := arg.trim_space()
	for current_arg in args {
		if current_arg.trim_space() == clean {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_arg_for_decl_module(arg string, module_name string) string {
	if t.generic_type_text_contains_alias(arg, module_name) {
		return t.qualify_generic_arg_for_decl_module(arg, module_name)
	}
	normalized := t.normalize_type_in_module(arg, module_name)
	qualified := t.qualify_generic_arg_for_decl_module(normalized, module_name)
	if qualified != normalized {
		return qualified
	}
	return normalized
}

fn strip_decl_module_from_generic_arg(arg string, module_name string) string {
	clean := arg.trim_space()
	if clean.len == 0 || module_name.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + strip_decl_module_from_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + strip_decl_module_from_generic_arg(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + strip_decl_module_from_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + strip_decl_module_from_generic_arg(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + strip_decl_module_from_generic_arg(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := strip_decl_module_from_generic_arg(clean[4..bracket_end], module_name)
			val := strip_decl_module_from_generic_arg(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + strip_decl_module_from_generic_arg(clean[bracket_end +
				1..], module_name)
		}
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		stripped_base := strip_decl_module_from_generic_base(base, module_name)
		mut stripped_args := []string{cap: args.len}
		for generic_arg in args {
			stripped_args << strip_decl_module_from_generic_arg(generic_arg, module_name)
		}
		return '${stripped_base}[${stripped_args.join(', ')}]'
	}
	return strip_decl_module_from_generic_base(clean, module_name)
}

fn strip_decl_module_from_generic_base(name string, module_name string) string {
	prefix := '${module_name}.'
	if !name.starts_with(prefix) {
		return name
	}
	short := name[prefix.len..]
	if short.starts_with('Array_') {
		return name
	}
	return short
}

fn (t &Transformer) generic_arg_is_alias_name(arg string, module_name string) bool {
	clean := arg.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return false
	}
	if clean in t.tc.type_aliases {
		return true
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		if '${module_name}.${clean}' in t.tc.type_aliases {
			return true
		}
	}
	for alias, _ in t.tc.type_aliases {
		if alias.all_after_last('.') == clean {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_args_contain_alias(args []string, module_name string) bool {
	for arg in args {
		if t.generic_type_text_contains_alias(arg, module_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_type_text_contains_alias(typ string, module_name string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if t.generic_arg_is_alias_name(clean, module_name) {
		return true
	}
	if clean.starts_with('&') {
		return t.generic_type_text_contains_alias(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return t.generic_type_text_contains_alias(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.generic_type_text_contains_alias(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return t.generic_type_text_contains_alias(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return t.generic_type_text_contains_alias(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_alias(clean[4..bracket_end], module_name)
				|| t.generic_type_text_contains_alias(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_alias(clean[bracket_end + 1..], module_name)
		}
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if t.generic_type_text_contains_alias(arg, module_name) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) generic_type_text_contains_fixed_array_alias(typ string, module_name string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if t.generic_arg_is_alias_name(clean, module_name) {
		return t.is_fixed_array_type(t.normalize_type_in_module(clean, module_name))
	}
	if clean.starts_with('&') {
		return t.generic_type_text_contains_fixed_array_alias(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return t.generic_type_text_contains_fixed_array_alias(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.generic_type_text_contains_fixed_array_alias(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return t.generic_type_text_contains_fixed_array_alias(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return t.generic_type_text_contains_fixed_array_alias(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return
				t.generic_type_text_contains_fixed_array_alias(clean[4..bracket_end], module_name)
				|| t.generic_type_text_contains_fixed_array_alias(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_fixed_array_alias(clean[bracket_end + 1..],
				module_name)
		}
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if t.generic_type_text_contains_fixed_array_alias(arg, module_name) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) qualify_generic_arg_for_decl_module(arg string, module_name string) string {
	clean := arg.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.qualify_generic_arg_for_decl_module(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + t.qualify_generic_arg_for_decl_module(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + t.qualify_generic_arg_for_decl_module(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + t.qualify_generic_arg_for_decl_module(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + t.qualify_generic_arg_for_decl_module(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := t.qualify_generic_arg_for_decl_module(clean[4..bracket_end], module_name)
			val := t.qualify_generic_arg_for_decl_module(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] +
				t.qualify_generic_arg_for_decl_module(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.contains('.') || types.is_builtin_type_name(clean) {
		return clean
	}
	if t.generic_arg_module_owns_type(clean, module_name) {
		return '${module_name}.${clean}'
	}
	if qualified := t.qualified_types[clean] {
		return qualified
	}
	return clean
}

fn (t &Transformer) generic_arg_module_owns_type(name string, module_name string) bool {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return false
	}
	qname := '${module_name}.${name}'
	if qname in t.structs || qname in t.sum_types || qname in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		return qname in t.tc.type_aliases || qname in t.tc.structs || qname in t.tc.sum_types
			|| qname in t.tc.enum_names || qname in t.tc.interface_names
	}
	return false
}

fn (t &Transformer) generic_arg_expr_type(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			typ := t.var_type(node.value)
			if typ.len > 0 {
				raw_typ := t.raw_var_type(node.value)
				return if raw_typ.len > 0 { raw_typ } else { typ }
			}
			if decl_type := t.local_decl_type_before(node.value, id) {
				return decl_type
			}
		}
		.array_literal {
			module_name := t.node_module_or(int(id), t.cur_module)
			if alias_type := t.generic_array_literal_alias_type_for_inference(node, module_name) {
				return alias_type
			}
			if checker_alias_type := t.array_literal_checker_alias_type(id) {
				return checker_alias_type
			}
			if alias_type := t.array_literal_alias_type(node) {
				return alias_type
			}
			if node.children_count > 0 {
				child_id := t.a.child(&node, 0)
				mut elem_type := t.generic_arg_expr_type(child_id)
				if elem_type.len == 0 {
					elem_type = t.node_type(child_id)
				}
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
		}
		.fn_literal, .lambda_expr {
			if fn_type := t.fn_value_type_name(id) {
				return fn_type
			}
		}
		.cast_expr, .as_expr {
			if node.value.len > 0 {
				return node.value
			}
		}
		.infix {
			if node.children_count >= 2 {
				left_type := t.node_type(t.a.child(&node, 0))
				if left_type.len > 0 {
					return left_type
				}
				right_type := t.node_type(t.a.child(&node, 1))
				if right_type.len > 0 {
					return right_type
				}
			}
		}
		.prefix, .paren {
			if node.children_count > 0 {
				return t.node_type(t.a.child(&node, 0))
			}
		}
		.index {
			typ := t.node_type(id)
			if typ.len > 0 && !t.generic_arg_is_unresolved(typ) {
				return typ
			}
		}
		else {}
	}

	return t.node_type(id)
}

fn (t &Transformer) generic_array_literal_alias_type_for_inference(node flat.Node, module_name string) ?string {
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	alias_name := t.generic_array_literal_alias_expr_name(t.a.child(&node, 0)) or { return none }
	if !t.generic_arg_is_alias_name(alias_name, module_name) {
		return none
	}
	return '[]${t.generic_qualified_alias_name(alias_name, module_name)}'
}

fn (t &Transformer) generic_array_literal_alias_expr_name(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.generic_array_literal_alias_expr_name(t.a.child(&node, 0))
	}
	raw_alias := t.raw_alias_type_for_expr(id)
	if raw_alias.len > 0 {
		return raw_alias
	}
	if node.kind in [.cast_expr, .as_expr] && node.value.len > 0 {
		return node.value
	}
	if node.kind == .call && node.children_count > 0 {
		name := t.generic_call_type_arg_name(t.a.child(&node, 0))
		if name.len > 0 {
			return name
		}
	}
	for candidate in [node.typ, node.value] {
		if candidate.len > 0 {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) generic_qualified_alias_name(name string, module_name string) string {
	clean := name.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return clean
	}
	if clean in t.tc.type_aliases {
		return clean
	}
	if !clean.contains('.') && module_name.len > 0 && module_name != 'main'
		&& module_name != 'builtin' {
		qname := '${module_name}.${clean}'
		if qname in t.tc.type_aliases {
			return qname
		}
	}
	if clean.contains('.') {
		return clean
	}
	mut found := ''
	for alias, _ in t.tc.type_aliases {
		if alias.all_after_last('.') != clean {
			continue
		}
		if found.len > 0 && found != alias {
			return clean
		}
		found = alias
	}
	if found.len > 0 {
		return found
	}
	return clean
}

fn (mut t Transformer) generic_call_arg_type_for_inference(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		// `mut val T` is represented internally as `&T`, but taking its address
		// passes `&T`, not `&&T`. Infer from the value type of the child so nested
		// generic calls do not specialize their payload as a pointer.
		inner := t.generic_call_arg_type_for_inference(t.a.child(&node, 0))
		if inner.len > 0 {
			return '&${inner}'
		}
	}
	if node.kind == .call {
		concrete_ret := t.concrete_generic_call_return_type(id, node)
		if concrete_ret.len > 0 && !t.generic_arg_is_unresolved(concrete_ret) {
			return concrete_ret
		}
		call_ret := t.get_call_return_type(id, node)
		if call_ret.len > 0 && !t.generic_arg_is_unresolved(call_ret) {
			return call_ret
		}
	}
	if node.kind == .array_literal {
		module_name := t.node_module_or(int(id), t.cur_module)
		if alias_type := t.generic_array_literal_alias_type_for_inference(node, module_name) {
			return alias_type
		}
	}
	if node.kind in [.array_literal, .fn_literal, .lambda_expr] {
		typ := t.generic_arg_expr_type(id)
		if typ.len > 0 {
			return typ
		}
	}
	if node.kind == .ident {
		if t.mut_value_ident_nodes[int(id)] && node.typ.starts_with('&') {
			return node.typ[1..]
		}
		// During the ordinary monomorphize node scan there is no live function
		// scope: var_types holds bindings from whatever function was transformed
		// last, so an unrelated local with the same name (`result`, `val`, ...)
		// would hijack inference. While a specialization is actively cloning,
		// emit_generic_fn_specialization has seeded a live parameter scope instead.
		if t.in_monomorphize_scan && t.cloning_generic_fn_depth == 0 && node.typ.len > 0
			&& node.typ != 'unknown'
			&& !t.generic_arg_is_unresolved(t.normalize_type_alias(node.typ)) {
			return node.typ
		}
		var_typ := t.var_type(node.value)
		if var_typ.len > 0 {
			raw_typ := t.raw_var_type(node.value)
			mut resolved := if raw_typ.len > 0 { raw_typ } else { var_typ }
			// A `mut val T` param is internally `&T`, but a by-value use of it
			// has the declared value type — inferring `&Concrete` here would
			// disagree with the specialization the monomorphize scan emits.
			if resolved.starts_with('&') && t.mut_param_values[node.value] {
				resolved = resolved[1..]
			}
			return resolved
		}
		if decl_type := t.local_decl_type_before(node.value, id) {
			return decl_type
		}
	}
	if node.typ.len > 0 {
		if t.generic_type_text_contains_alias(node.typ, t.cur_module) {
			return node.typ
		}
		typ := t.normalize_type_alias(node.typ)
		if typ.len > 0 && !t.generic_arg_is_unresolved(typ) {
			return typ
		}
	}
	mut typ := t.node_type(id)
	if typ.len == 0 {
		typ = t.generic_arg_expr_type(id)
	}
	return typ
}

fn (t &Transformer) local_decl_type_before(name string, before flat.NodeId) ?string {
	if name.len == 0 || int(before) <= 0 {
		return none
	}
	mut best := ''
	limit := int(before)
	mut start := 0
	for i in 0 .. limit {
		node := t.a.nodes[i]
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			start = i
		}
	}
	for i in start .. limit {
		node := t.a.nodes[i]
		if node.kind != .decl_assign || node.children_count < 2 {
			continue
		}
		lhs_id := t.a.child(&node, 0)
		if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
			continue
		}
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value != name {
			continue
		}
		for candidate in [lhs.typ, node.typ] {
			typ := if t.generic_type_text_contains_alias(candidate, t.cur_module) {
				candidate
			} else {
				t.normalize_type_alias(candidate)
			}
			if typ.len > 0 && !t.generic_arg_is_unresolved(typ) && decl_type_is_usable(typ) {
				best = typ
			}
		}
		if best.len == 0 {
			rhs_id := t.a.child(&node, 1)
			rhs_typ := t.node_type(rhs_id)
			if rhs_typ.len > 0 && !t.generic_arg_is_unresolved(rhs_typ)
				&& decl_type_is_usable(rhs_typ) {
				best = rhs_typ
			}
		}
	}
	if best.len == 0 {
		return none
	}
	return best
}

fn (t &Transformer) infer_generic_receiver_suffix_args(param_type string, arg_type string, mut inferred map[string]string) {
	param := param_type.trim_space().trim_left('&')
	arg := arg_type.trim_space().trim_left('&')
	base, param_args, ok := generic_app_parts(param)
	if !ok || param_args.len == 0 || arg.len == 0 {
		return
	}
	if args := t.recorded_generic_specialization_args(arg) {
		t.infer_generic_receiver_recorded_args(param_args, args, mut inferred)
		return
	}
	mut suffix := ''
	for prefix in [base, c_name(base)] {
		if arg.starts_with('${prefix}_') {
			suffix = arg[prefix.len + 1..]
			break
		}
	}
	if suffix.len == 0 {
		short := base.all_after_last('.')
		arg_short := if arg.contains('.') { arg.all_after_last('.') } else { arg }
		for candidate in [arg, arg_short, c_name(arg), c_name(arg_short)] {
			for prefix in [short, c_name(short)] {
				if candidate.starts_with('${prefix}_') {
					suffix = candidate[prefix.len + 1..]
					break
				}
			}
			if suffix.len > 0 {
				break
			}
		}
	}
	if suffix.len == 0 {
		return
	}
	mut assigned := false
	for i, param_arg in param_args {
		if !is_generic_fn_placeholder_name(param_arg) || param_arg in inferred {
			continue
		}
		decoded := generic_type_arg_from_receiver_suffix(suffix)
		if decoded.len == 0 {
			continue
		}
		if i == 0 || !assigned {
			inferred[param_arg] = decoded
			assigned = true
		}
	}
}

fn (t &Transformer) infer_generic_receiver_recorded_args(param_args []string, args []string, mut inferred map[string]string) {
	mut arg_idx := 0
	for param_arg in param_args {
		if !is_generic_fn_placeholder_name(param_arg) || param_arg in inferred {
			continue
		}
		if arg_idx >= args.len {
			return
		}
		inferred[param_arg] = args[arg_idx]
		arg_idx++
	}
}

fn (t &Transformer) infer_generic_embedded_receiver_args(param_type string, arg_type string, mut inferred map[string]string) {
	param := param_type.trim_space().trim_left('&')
	base, _, ok := generic_app_parts(param)
	if !ok || base.len == 0 {
		return
	}
	mut receiver_type := arg_type.trim_space().trim_left('&')
	receiver_base, _, receiver_is_generic := generic_app_parts(receiver_type)
	if receiver_is_generic {
		receiver_type = receiver_base
	}
	info := t.structs[receiver_type] or {
		short := if receiver_type.contains('.') {
			receiver_type.all_after_last('.')
		} else {
			receiver_type
		}
		t.structs[short] or { return }
	}
	for field in info.fields {
		field_type := field.typ.trim_space().trim_left('&')
		field_base, _, field_is_generic := generic_app_parts(field_type)
		if !field_is_generic {
			continue
		}
		if field_base == base || field_base.all_after_last('.') == base.all_after_last('.') {
			infer_generic_type_args(param, field_type, mut inferred)
			return
		}
	}
}

fn (t &Transformer) generic_call_receiver_id(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	mut callee_id := t.a.child(&node, 0)
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 {
		callee_id = t.a.child(&callee, 0)
		callee = t.a.nodes[int(callee_id)]
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	return t.a.child(&callee, 0)
}

fn infer_generic_type_args(param_type string, arg_type string, mut inferred map[string]string) {
	param := param_type.trim_space()
	arg := arg_type.trim_space()
	if param.len == 0 || arg.len == 0 {
		return
	}
	if arg == 'unknown' || arg == 'generic' {
		return
	}
	if is_generic_fn_placeholder_name(param) {
		if param !in inferred {
			inferred[param] = arg
		}
		return
	}
	if param.starts_with('&') {
		// A `&T` / `mut T` parameter binds to a by-value argument too (the arg type
		// carries no `&`), so strip the reference from the parameter regardless of
		// whether the argument is itself a reference.
		arg_inner := if arg.starts_with('&') { arg[1..] } else { arg }
		infer_generic_type_args(param[1..], arg_inner, mut inferred)
		return
	}
	if param.starts_with('mut ') {
		infer_generic_type_args(param[4..], arg.trim_left('&'), mut inferred)
		return
	}
	if param.starts_with('...') {
		infer_generic_type_args(param[3..], arg.trim_left('[]'), mut inferred)
		return
	}
	if param.starts_with('[]') && arg.starts_with('[]') {
		infer_generic_type_args(param[2..], arg[2..], mut inferred)
		return
	}
	if param.starts_with('[') && arg.starts_with('[') {
		p_end := generic_matching_bracket(param, 0)
		a_end := generic_matching_bracket(arg, 0)
		if p_end < param.len && a_end < arg.len {
			infer_generic_type_args(param[p_end + 1..], arg[a_end + 1..], mut inferred)
		}
		return
	}
	if param.starts_with('[') && arg.ends_with(']') && arg.contains('[') {
		p_len := fixed_array_len_text(param)
		a_len := fixed_array_len_text(arg)
		if p_len == a_len {
			infer_generic_type_args(fixed_array_elem_type(param), fixed_array_elem_type(arg), mut
				inferred)
		}
		return
	}
	if param.starts_with('?') && arg.starts_with('?') {
		infer_generic_type_args(param[1..], arg[1..], mut inferred)
		return
	}
	if param.starts_with('!') && arg.starts_with('!') {
		infer_generic_type_args(param[1..], arg[1..], mut inferred)
		return
	}
	if param.starts_with('fn') && arg.starts_with('fn') {
		infer_generic_fn_type_args(param, arg, mut inferred)
		return
	}
	if param.starts_with('map[') && arg.starts_with('map[') {
		p_end := generic_matching_bracket(param, 3)
		a_end := generic_matching_bracket(arg, 3)
		if p_end < param.len && a_end < arg.len {
			infer_generic_type_args(param[4..p_end], arg[4..a_end], mut inferred)
			infer_generic_type_args(param[p_end + 1..], arg[a_end + 1..], mut inferred)
		}
		return
	}
	p_base, p_args, p_ok := generic_app_parts(param)
	if p_ok {
		a_base, a_args, a_ok := generic_app_parts(arg)
		if p_ok && a_ok && p_base.all_after_last('.') == a_base.all_after_last('.') {
			for i, p_arg in p_args {
				if i < a_args.len {
					infer_generic_type_args(p_arg, a_args[i], mut inferred)
				}
			}
		}
	}
}

fn infer_generic_fn_type_args(param string, arg string, mut inferred map[string]string) {
	p_params, p_ret := fn_type_text_parts(param) or { return }
	a_params, a_ret := fn_type_text_parts(arg) or { return }
	for i, p_param in p_params {
		if i >= a_params.len {
			break
		}
		infer_generic_type_args(generic_fn_type_param_payload(p_param),
			generic_fn_type_param_payload(a_params[i]), mut inferred)
	}
	if p_ret.len > 0 && a_ret.len > 0 {
		infer_generic_type_args(p_ret, a_ret, mut inferred)
	}
}

fn fn_type_text_parts(typ string) ?([]string, string) {
	clean := typ.trim_space()
	open := clean.index_u8(`(`)
	if open < 0 {
		return none
	}
	mut depth := 1
	mut close := open + 1
	for close < clean.len {
		if clean[close] == `(` {
			depth++
		} else if clean[close] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		close++
	}
	if close >= clean.len {
		return none
	}
	params_text := clean[open + 1..close]
	params := split_fn_type_params(params_text)
	ret := clean[close + 1..].trim_space()
	return params, ret
}

fn split_fn_type_params(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `(` || c == `[` {
			depth++
		} else if c == `)` || c == `]` {
			depth--
		} else if c == `,` && depth == 0 {
			part := s[start..i].trim_space()
			if part.len > 0 {
				parts << part
			}
			start = i + 1
		}
	}
	tail := s[start..].trim_space()
	if tail.len > 0 {
		parts << tail
	}
	return parts
}

fn generic_fn_type_param_payload(param string) string {
	mut text := param.trim_space()
	if text.starts_with('mut ') {
		text = text[4..].trim_space()
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if generic_fn_type_param_head_is_name(head, tail) {
			return tail
		}
	}
	return text
}

// generic_const_string_cond evaluates an if condition made purely of
// comptime-known string facts (`T.name in ['x.json2.Any', ...]`,
// `T.name == 'X'`, combined with &&/||/!) once the generic args are bound.
fn (mut t Transformer) generic_const_string_cond(id flat.NodeId, args []string) ?bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.generic_const_string_cond(t.a.child(&node, 0), args)
	}
	if node.kind == .prefix && node.op == .not && node.children_count > 0 {
		v := t.generic_const_string_cond(t.a.child(&node, 0), args) or { return none }
		return !v
	}
	if node.kind == .infix && node.children_count == 2 {
		if node.op == .logical_and {
			l := t.generic_const_string_cond(t.a.child(&node, 0), args) or { return none }
			if !l {
				return false
			}
			return t.generic_const_string_cond(t.a.child(&node, 1), args)
		}
		if node.op == .logical_or {
			l := t.generic_const_string_cond(t.a.child(&node, 0), args) or { return none }
			if l {
				return true
			}
			return t.generic_const_string_cond(t.a.child(&node, 1), args)
		}
		if node.op in [.eq, .ne] {
			l := t.generic_const_string_value(t.a.child(&node, 0), args) or { return none }
			r := t.generic_const_string_value(t.a.child(&node, 1), args) or { return none }
			return if node.op == .eq { l == r } else { l != r }
		}
		return none
	}
	if node.kind == .in_expr && node.children_count == 2 {
		l := t.generic_const_string_value(t.a.child(&node, 0), args) or { return none }
		rhs := t.a.nodes[int(t.a.child(&node, 1))]
		if rhs.kind != .array_literal {
			return none
		}
		for i in 0 .. rhs.children_count {
			item := t.generic_const_string_value(t.a.child(&rhs, i), args) or { return none }
			if item == l {
				return true
			}
		}
		return false
	}
	return none
}

fn (mut t Transformer) generic_const_string_value(id flat.NodeId, args []string) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal {
		return node.value
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		base := t.a.nodes[int(base_id)]
		if base.kind == .typeof_expr {
			if reflected := t.generic_comptime_typeof_target(base, args) {
				return generic_type_name_display(reflected)
			}
		}
		if reflected := t.generic_comptime_base_type(base_id, args) {
			return generic_type_name_display(reflected)
		}
	}
	return none
}

fn (mut t Transformer) clone_generic_fn_node(node flat.Node, args []string) flat.NodeId {
	return t.clone_generic_node_from(node, args, true)
}

fn (mut t Transformer) clone_generic_node(id flat.NodeId, args []string) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	clone_id := t.clone_generic_node_from(node, args, false)
	t.copy_cloned_resolution(id, clone_id)
	return clone_id
}

fn (mut t Transformer) clone_generic_node_from(node flat.Node, args []string, is_root bool) flat.NodeId {
	if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		if node.value == 'name' {
			base := t.a.nodes[int(base_id)]
			if base.kind == .typeof_expr {
				if reflected := t.generic_comptime_typeof_target(base, args) {
					return t.make_string_literal(generic_type_name_display(reflected))
				}
			}
			if reflected := t.generic_comptime_base_type(base_id, args) {
				return t.make_string_literal(generic_type_name_display(reflected))
			}
		}
		if node.value in ['idx', 'key_type', 'value_type', 'element_type'] {
			if concrete := t.generic_comptime_base_type(base_id, args) {
				target := if node.value == 'idx' {
					concrete
				} else {
					t.generic_comptime_type_member(concrete, node.value) or { '' }
				}
				if target.len > 0 {
					return t.make_int_literal(t.comptime_field_type_id(target, t.cur_module))
				}
			}
		}
	}
	// Evaluate `$if` guards that become decidable once the generic args are
	// substituted, cloning only the taken branch. Cloning dead branches would
	// leave their nodes in the flat arena where the monomorphize call-site scan
	// still visits them, emitting bogus specializations for calls that only
	// typecheck in the pruned branch (e.g. `decode_string[SomeStruct]`).
	// A runtime `if` whose condition is entirely comptime-known strings
	// (`if T.name in [...] { } else { <only valid for arrays> }`) keeps only
	// its taken branch, like the reference compiler; the dead branch may not
	// even typecheck for this specialization.
	if node.kind == .if_expr && t.cloning_comptime_for_depth == 0 && node.children_count >= 2 {
		if take_then := t.generic_const_string_cond(t.a.child(&node, 0), args) {
			branch_index := if take_then { 1 } else { 2 }
			if branch_index >= int(node.children_count) {
				return t.make_empty()
			}
			return t.clone_generic_node(t.a.child(&node, branch_index), args)
		}
	}
	if node.kind == .comptime_if && t.cloning_comptime_for_depth == 0 {
		cond := t.subst_comptime_type_condition(node.value, args)
		if take_then := t.comptime_type_condition_value(cond) {
			branch_index := if take_then { 0 } else { 1 }
			if branch_index >= int(node.children_count) {
				return t.make_empty()
			}
			return t.clone_generic_node(t.a.child(&node, branch_index), args)
		}
	}
	if node.kind == .typeof_expr {
		if reflected := t.generic_comptime_typeof_target(node, args) {
			return t.make_string_literal(generic_type_name_display(reflected))
		}
	}
	// Calls nested inside a `$for` body must not be specialized during the clone: the loop
	// variable's members are not resolved yet, so mono would infer a garbage type argument and
	// emit an uncalled, uncompilable specialization. The comptime unroll (or its skip) handles
	// them later against the concrete field types.
	is_comptime_for := node.kind == .comptime_for
	if is_comptime_for {
		t.cloning_comptime_for_depth++
		loop_var, _ := comptime_for_parts(node.value)
		t.cloning_comptime_for_vars << loop_var
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.clone_generic_node(t.a.child(&node, i), args)
	}
	if is_comptime_for {
		t.cloning_comptime_for_vars = t.cloning_comptime_for_vars[..t.cloning_comptime_for_vars.len - 1]
		t.cloning_comptime_for_depth--
	}
	// A string interpolation format is stored in `directive.typ`, but it is not a
	// type name. Qualifying it in an imported generic specialization turns `04X`
	// into e.g. `json2.04X`, which silently drops the requested base and width.
	substituted_node_type := if node.kind == .directive && node.value == 'string_interp_format' {
		node.typ
	} else {
		t.subst_type(node.typ, args)
	}
	mut cloned_typ := if t.generic_type_text_contains_alias(substituted_node_type, t.cur_module) {
		substituted_node_type
	} else if node.kind == .directive && node.value == 'string_interp_format' {
		substituted_node_type
	} else {
		t.resolve_substituted_type_text(substituted_node_type)
	}
	if node.kind == .ident && t.cloning_generic_fn_depth > 0 {
		scoped_type := t.raw_var_type(node.value)
		if scoped_type.len > 0 && !t.generic_arg_is_unresolved(scoped_type) {
			cloned_typ = scoped_type
		}
	}
	if node.kind == .ident && t.mut_param_values[node.value] && cloned_typ.starts_with('&') {
		// A use of a `mut T` parameter is a T value even though the parameter's
		// storage annotation is `&T`. Keeping the pointer annotation on cloned
		// idents makes the later generic-call scan emit duplicate `&T`
		// specializations instead of reusing the value-type specialization.
		cloned_typ = cloned_typ[1..]
	}
	if node.kind == .struct_init && node.value == 'Optional'
		&& t.is_optional_type_name(t.cur_fn_ret_type) {
		cloned_typ = t.cur_fn_ret_type
	}
	if node.kind == .array_init && node.value.len > 0 {
		array_value := t.resolve_substituted_type_text(t.subst_type(node.value, args))
		cloned_typ = if t.is_fixed_array_type(array_value) || array_value.starts_with('[') {
			array_value
		} else {
			'[]${array_value}'
		}
	} else if node.kind == .struct_init && node.value.len > 0 {
		// The checker can annotate `T{}` with its surrounding optional/result
		// context. For a concrete clone the literal itself is authoritative.
		struct_value0 := t.resolve_substituted_type_text(t.subst_type(node.value, args))
		struct_value := t.normalize_type_alias(struct_value0)
		parsed := t.tc.parse_resolution_type(struct_value)
		cloned_typ = if t.is_fixed_array_type(struct_value0) {
			t.resolved_fixed_array_canonical_type(struct_value0)
		} else if t.generic_arg_is_alias_name(struct_value0, t.cur_module) {
			struct_value0
		} else if parsed is types.Unknown {
			struct_value
		} else {
			specialized_signature_storage_type_name(parsed)
		}
	}
	if node.kind == .or_expr && children.len > 0 {
		source_type := t.node_type(children[0])
		if t.is_optional_type_name(source_type) {
			cloned_typ = t.optional_base_type(t.qualify_optional_type(source_type))
		}
	}
	mut cloned_op := node.op
	if node.kind == .selector && children.len > 0 {
		base_type := t.node_type(children[0])
		if field_type := t.lookup_struct_field_type(base_type, node.value) {
			cloned_typ = field_type
		}
		if base_type.starts_with('&') {
			cloned_op = .arrow
		}
	}
	// Record the local's substituted type as the body is cloned: later sibling
	// clones (nested generic calls) infer their type args from these locals, and
	// without a binding the lookup falls back to an arena scan that can land in
	// an unrelated, previously cloned function with a same-named local.
	if node.kind == .decl_assign && node.children_count == 2 && t.cloning_comptime_for_depth == 0 {
		lhs_clone := t.a.nodes[int(children[0])]
		if lhs_clone.kind == .ident && lhs_clone.value.len > 0 {
			rhs_node := t.a.nodes[int(children[1])]
			rhs_raw_typ := if t.generic_type_text_contains_alias(rhs_node.typ, t.cur_module) {
				rhs_node.typ
			} else {
				''
			}
			mut rhs_typ := if rhs_raw_typ.len > 0 {
				rhs_raw_typ
			} else {
				t.concrete_node_type_name(rhs_node)
			}
			if rhs_typ.len == 0 {
				rhs_typ = t.node_type(children[1])
			}
			if rhs_typ.len > 0 && !t.generic_arg_is_unresolved(rhs_typ) {
				cloned_typ = rhs_typ
				t.set_node_typ(int(children[0]), rhs_typ)
			}
			lhs_typ := if rhs_typ.len > 0 { rhs_typ } else { cloned_typ }
			if lhs_typ.len > 0 && !t.generic_arg_is_unresolved(lhs_typ) {
				t.set_var_type_with_raw(lhs_clone.value, t.normalize_type_alias(lhs_typ), lhs_typ)
			}
		}
	}
	t.substitute_cloned_generic_call_type_args(node, mut children, args)
	if t.cloning_comptime_for_depth > 0 {
		// Inside a `$for` body: clone verbatim, no generic-call retargeting.
		start2 := t.a.children.len
		for child in children {
			t.a.children << child
		}
		clone_id := t.a.add_node(flat.Node{
			kind:           node.kind
			kind_id:        node.kind_id
			op:             cloned_op
			pos:            node.pos
			children_start: start2
			children_count: flat.child_count(children.len)
			typ:            cloned_typ
			value:          t.subst_node_value(node, args)
			is_mut:         node.is_mut
		})
		if node.kind == .ident && t.mut_param_values[node.value] {
			t.mut_value_ident_nodes[int(clone_id)] = true
		}
		return clone_id
	}
	cloned_typ = t.retarget_cloned_map_key_storage_type(node, mut children, cloned_typ)
	t.retarget_cloned_new_map_call(node, mut children, cloned_typ)
	retargeted_typ := t.retarget_cloned_generic_call(node, mut children, args)
	final_typ := if retargeted_typ.len > 0 { retargeted_typ } else { cloned_typ }
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	if node.kind == .call && children.len > 0
		&& (node.typ.contains('[') || node.value.contains('[')) {
		type_text := if node.typ.contains('[') { node.typ } else { node.value }
		base, _, ok := generic_app_parts(type_text)
		if ok {
			callee_id := children[0]
			mut callee := t.a.nodes[int(callee_id)]
			if callee.kind in [.ident, .selector] && callee.value == base.all_after_last('.') {
				t.set_node_value(int(callee_id), t.subst_type(type_text, args))
			}
		}
	}
	cloned_value := if node.kind == .struct_init && cloned_typ.len > 0 {
		cloned_typ
	} else if is_root {
		specialized_generic_fn_value(node.value, args)
	} else {
		t.subst_node_value(node, args)
	}
	if is_root && t.cur_module.len > 0 {
		t.a.add_node(flat.Node{
			kind:  .module_decl
			value: t.cur_module
		})
	}
	clone_id := t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             cloned_op
		pos:            node.pos
		children_start: start
		children_count: flat.child_count(children.len)
		typ:            final_typ
		value:          cloned_value
		is_mut:         node.is_mut
	})
	if node.kind == .param && cloned_value.len > 0 && cloned_typ.len > 0 {
		t.set_var_type(cloned_value, cloned_typ)
	}
	if node.kind == .ident && t.mut_param_values[node.value] {
		t.mut_value_ident_nodes[int(clone_id)] = true
	}
	if node.kind == .call && (t.call_has_source_generic_args(node)
		|| t.generic_args_contain_alias(args, t.cur_module)) {
		// Explicit method type arguments belong to the surrounding specialization.
		// Retarget them while that argument context is still available; implicit
		// calls normally stay for the reachability-aware scan below. An alias-bearing
		// specialization must also retarget implicit nested calls now, before type
		// annotation erases the alias identity from their arguments.
		t.retarget_cloned_implicit_generic_call(clone_id, node, args)
	}
	return clone_id
}

fn (t &Transformer) generic_comptime_base_type(id flat.NodeId, args []string) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && is_generic_fn_placeholder_name(node.value) {
		idx := t.active_generic_param_index(node.value)
		if idx < args.len {
			return args[idx]
		}
	}
	if node.kind == .typeof_expr {
		if typ := generic_type_name_from_marker(node.value) {
			idx := t.active_generic_param_index(typ)
			if idx < args.len {
				return args[idx]
			}
		}
	}
	return none
}

fn (mut t Transformer) generic_comptime_typeof_target(node flat.Node, args []string) ?string {
	if typ := generic_type_name_from_marker(node.value) {
		idx := t.active_generic_param_index(typ)
		if idx < args.len {
			return args[idx]
		}
	}
	if node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .selector && child.children_count > 0
		&& child.value in ['idx', 'key_type', 'value_type', 'element_type'] {
		base_id := t.a.child(&child, 0)
		if concrete := t.generic_comptime_base_type(base_id, args) {
			if child.value == 'idx' {
				return 'int'
			}
			return t.generic_comptime_type_member(concrete, child.value)
		}
	}
	return t.generic_comptime_base_type(child_id, args)
}

fn (mut t Transformer) generic_comptime_type_member(raw string, member string) ?string {
	clean := t.comptime_normalize_type_alias_chain(raw)
	match member {
		'key_type', 'value_type' {
			if !clean.starts_with('map[') {
				return none
			}
			key_type, value_type := t.map_type_parts(clean)
			return if member == 'key_type' { key_type } else { value_type }
		}
		'element_type' {
			if clean.starts_with('[]') {
				return clean[2..]
			}
			if t.is_fixed_array_type(clean) {
				return fixed_array_elem_type(clean)
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) substitute_cloned_generic_call_type_args(node flat.Node, mut children []flat.NodeId, args []string) {
	if node.kind != .index || node.children_count < 2 || node.value == 'range'
		|| children.len < int(node.children_count) || t.index_callee_is_value_index(node) {
		return
	}
	for i in 1 .. int(node.children_count) {
		arg := t.generic_call_type_arg_name(t.a.child(&node, i))
		if arg.len == 0 {
			continue
		}
		// Only rewrite args that actually contain a generic placeholder. A plain
		// value index like `attr[start]` matches the `callee[arg]` shape too, and
		// resolve_substituted_type_text would "qualify" the local `start` into a
		// phantom type/fn name (`json2.start`), corrupting the index expression.
		if !t.generic_args_have_placeholders([arg]) {
			continue
		}
		substituted := t.resolve_substituted_type_text(t.subst_type(arg, args))
		if substituted.len == 0 || substituted == arg
			|| t.generic_args_have_placeholders([substituted]) {
			continue
		}
		children[i] = t.make_ident(substituted)
	}
}

const generic_type_name_marker_prefix = '__v3_generic_type_name:'

fn generic_type_name_marker(typ string) string {
	return generic_type_name_marker_prefix + typ
}

fn generic_type_name_from_marker(value string) ?string {
	if value.starts_with(generic_type_name_marker_prefix) {
		return value[generic_type_name_marker_prefix.len..]
	}
	return none
}

fn generic_type_name_display(typ string) string {
	if !typ.starts_with('fn(') {
		return typ
	}
	close := generic_fn_type_params_close(typ) or { return 'fn ' + typ[2..] }
	params := split_type_display_params(typ[3..close])
	mut displayed_params := []string{cap: params.len}
	for param in params {
		displayed_params << generic_fn_param_type_display(param)
	}
	return 'fn (' + displayed_params.join(', ') + ')' + typ[close + 1..]
}

fn generic_fn_param_type_display(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('&') {
		return 'mut ' + generic_type_name_display(clean[1..])
	}
	return generic_type_name_display(clean)
}

fn generic_fn_type_params_close(typ string) ?int {
	mut depth := 0
	for i in 2 .. typ.len {
		if typ[i] == `(` {
			depth++
		} else if typ[i] == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn split_type_display_params(s string) []string {
	mut parts := []string{}
	mut paren_depth := 0
	mut bracket_depth := 0
	mut start := 0
	for i in 0 .. s.len {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
			}
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			`,` {
				if paren_depth == 0 && bracket_depth == 0 {
					parts << s[start..i].trim_space()
					start = i + 1
				}
			}
			else {}
		}
	}
	tail := s[start..].trim_space()
	if tail.len > 0 {
		parts << tail
	}
	return parts
}

fn (mut t Transformer) retarget_cloned_new_map_call(node flat.Node, mut children []flat.NodeId, map_type string) {
	if node.kind != .call || children.len < 7 || !map_type.starts_with('map[') {
		return
	}
	callee := t.a.nodes[int(children[0])]
	if callee.kind != .ident || callee.value != 'new_map' {
		return
	}
	key_type, _ := t.map_type_parts(map_type)
	if key_type.len == 0 {
		return
	}
	key_storage_type := t.map_key_storage_type(key_type)
	children[1] = t.make_sizeof_type(key_storage_type)
	hash_fn, eq_fn, clone_fn, free_fn := map_callback_names(key_storage_type)
	children[3] = t.make_ident(hash_fn)
	children[4] = t.make_ident(eq_fn)
	children[5] = t.make_ident(clone_fn)
	children[6] = t.make_ident(free_fn)
}

fn (mut t Transformer) retarget_cloned_map_key_storage_type(node flat.Node, mut children []flat.NodeId, cloned_typ string) string {
	if cloned_typ.len == 0 {
		return cloned_typ
	}
	mut name := ''
	if node.kind == .ident {
		name = node.value
	} else if node.kind == .decl_assign && children.len > 0 {
		lhs := t.a.nodes[int(children[0])]
		if lhs.kind == .ident {
			name = lhs.value
		}
	}
	if !is_lowered_map_key_temp_name(name) {
		return cloned_typ
	}
	key_storage_type := t.map_key_storage_type(cloned_typ)
	if key_storage_type.len == 0 || key_storage_type == cloned_typ {
		return cloned_typ
	}
	if node.kind == .decl_assign && children.len > 0 {
		t.set_node_typ(int(children[0]), key_storage_type)
	}
	t.set_var_type(name, key_storage_type)
	return key_storage_type
}

fn is_lowered_map_key_temp_name(name string) bool {
	return name.starts_with('__map_key_') || name.starts_with('__map_eq_key_')
}

fn (mut t Transformer) retarget_cloned_generic_call(node flat.Node, mut children []flat.NodeId, args []string) string {
	if node.kind != .call || children.len == 0 || t.skip_generics {
		return ''
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return ''
	}
	decl_key := t.generic_call_decl_key(flat.empty_node, node, t.cur_module, decls) or { return '' }
	decl := decls[decl_key] or { return '' }
	if t.should_skip_generic_call_specialization(decl_key)
		|| (t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)) {
		return ''
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return ''
	}
	mut call_args := []string{}
	if explicit := t.explicit_generic_call_args(node, t.cur_module) {
		for arg in explicit {
			call_args << t.subst_type(arg, args)
		}
	} else {
		callee_id := children[0]
		if int(callee_id) < 0 || int(callee_id) >= t.a.nodes.len {
			return ''
		}
		callee := t.a.nodes[int(callee_id)]
		if callee.kind != .ident || callee.value.contains('[') {
			return ''
		}
		if t.plain_concrete_callee_shadows_decl(callee.value, decl) {
			return ''
		}
		mut inferred := map[string]string{}
		mut param_idx := 0
		for i in 0 .. decl.node.children_count {
			child := t.a.child_node(&decl.node, i)
			if child.kind != .param {
				continue
			}
			arg_pos := param_idx + 1
			if arg_pos >= children.len {
				param_idx++
				continue
			}
			arg_type := generic_arg_type_for_param(child.typ,
				t.generic_call_arg_type_for_inference(children[arg_pos]))
			if arg_type.len > 0 {
				infer_generic_type_args(child.typ, arg_type, mut inferred)
			}
			param_idx++
		}
		for name in param_names {
			arg := inferred[name] or {
				idx := t.active_generic_param_index(name)
				if idx < 0 || idx >= args.len {
					return ''
				}
				args[idx]
			}
			call_args << t.generic_arg_for_decl_module(arg, decl.module)
		}
	}
	if call_args.len == 0 || t.generic_args_have_placeholders(call_args) {
		return ''
	}
	spec_value := specialized_generic_fn_value(decl.node.value, call_args)
	qualified_spec := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([
		spec_value,
		qualified_spec,
		c_name(spec_value),
		c_name(qualified_spec),
	], call_args)
	children[0] = t.make_ident(qualified_spec)
	return t.specialized_fn_return_type_text(decl, call_args)
}

fn (mut t Transformer) retarget_cloned_implicit_generic_call(clone_id flat.NodeId, source flat.Node, active_args []string) {
	if source.kind != .call || t.skip_generics || int(clone_id) < 0
		|| int(clone_id) >= t.a.nodes.len {
		return
	}
	clone := t.a.nodes[int(clone_id)]
	if clone.children_count == 0 {
		return
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return
	}
	decl_key := t.generic_call_decl_key(flat.empty_node, source, t.cur_module, decls) or { return }
	decl := decls[decl_key] or { return }
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(source, decl)
	source_has_explicit_args := t.call_has_source_generic_args(source)
	if t.should_skip_generic_call_specialization(decl_key)
		|| (source_has_explicit_args && !is_receiver) {
		return
	}
	mut callee_id := t.a.child(&clone, 0)
	if int(callee_id) < 0 || int(callee_id) >= t.a.nodes.len {
		return
	}
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee_id = t.a.child(&callee, 0)
		callee = t.a.nodes[int(callee_id)]
	}
	if callee.kind == .ident && callee.value.contains('[') {
		return
	}
	if callee.kind !in [.ident, .selector] {
		return
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return
	}
	mut call_args := []string{cap: param_names.len}
	if source_has_explicit_args {
		explicit := t.explicit_generic_call_args(source, t.cur_module) or { return }
		for arg in explicit {
			substituted := t.resolve_substituted_type_text(t.subst_type(arg, active_args))
			call_args << t.generic_arg_for_decl_module(substituted, decl.module)
		}
	} else {
		mut inferred := map[string]string{}
		mut param_idx := 0
		for i in 0 .. decl.node.children_count {
			child := t.a.child_node(&decl.node, i)
			if child.kind != .param {
				continue
			}
			clone_arg_id := t.generic_call_arg_id_for_param(clone, param_idx, is_receiver) or {
				param_idx++
				continue
			}
			source_arg_id := t.generic_call_arg_id_for_param(source, param_idx, is_receiver) or {
				param_idx++
				continue
			}
			mut arg_type := t.generic_call_arg_type_for_inference(clone_arg_id)
			source_substituted := t.subst_type(t.generic_call_arg_type_for_inference(source_arg_id),
				active_args)
			source_arg_type := if t.generic_type_text_contains_alias(source_substituted,
				t.cur_module)
			{
				source_substituted
			} else {
				t.resolve_substituted_type_text(source_substituted)
			}
			if active_args.len > 0 && decl_type_is_usable(source_arg_type)
				&& !t.generic_arg_is_unresolved(source_arg_type) {
				arg_type = source_arg_type
			}
			if arg_type.starts_with('&') && !child.typ.starts_with('&')
				&& !child.typ.starts_with('mut ') {
				arg_type = arg_type[1..]
			}
			arg_type = generic_arg_type_for_param(child.typ, arg_type)
			if arg_type.len > 0 {
				infer_generic_type_args(child.typ, arg_type, mut inferred)
			}
			param_idx++
		}
		for name in param_names {
			arg := inferred[name] or { return }
			call_args << t.generic_arg_for_decl_module(arg, decl.module)
		}
	}
	if call_args.len == 0 || t.generic_args_have_placeholders(call_args) {
		return
	}
	if t.plain_concrete_callee_shadows_decl(callee.value, decl) {
		return
	}
	concrete_call_args := t.canonical_generic_specialization_args(call_args)
	if !t.generic_specialization_registered(decl, concrete_call_args)
		&& !t.generic_specialization_in_progress(decl, concrete_call_args) {
		t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl, concrete_call_args),
			concrete_call_args)
	}
	if is_receiver {
		t.rewrite_generic_method_call(clone_id, clone, decl, concrete_call_args)
	} else {
		t.rewrite_generic_plain_call(clone_id, clone, decl, concrete_call_args)
	}
}

fn (t &Transformer) generic_specialization_registered(decl GenericFnDecl, args []string) bool {
	spec_value := specialized_generic_fn_value(decl.node.value, args)
	qname := transform_qualified_fn_name(decl.module, spec_value)
	for name in [spec_value, qname, c_name(spec_value), c_name(qname)] {
		if name in t.fn_ret_types {
			return true
		}
		if !isnil(t.tc) && (name in t.tc.fn_ret_types || name in t.tc.fn_param_types) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_specialization_in_progress(decl GenericFnDecl, args []string) bool {
	return t.generic_fn_specs_in_progress[t.generic_specialization_progress_key(decl, args)]
}

fn (t &Transformer) generic_specialization_progress_key(decl GenericFnDecl, args []string) string {
	base := generic_fn_decl_base_value(decl.node.value)
	qbase := transform_qualified_fn_name(decl.module, base)
	return generic_fn_spec_key(qbase, args)
}

fn (mut t Transformer) copy_cloned_resolution(src_id flat.NodeId, dst_id flat.NodeId) {
	if isnil(t.tc) {
		return
	}
	src_idx := int(src_id)
	dst_idx := int(dst_id)
	if src_idx < 0 || dst_idx < 0 {
		return
	}
	if !isnil(t.tc.fork_overlay) {
		// Parallel-transform worker: never write the shared node-indexed arrays
		// (other threads read them, and appending would realloc them). Record the
		// resolution in the fork's private overlay; reads check it first and
		// merge_worker replays it into the master under the shifted ids.
		t.copy_cloned_resolution_forked(src_idx, dst_idx)
		return
	}
	if src_idx < t.tc.resolved_call_set.len && t.tc.resolved_call_set[src_idx]
		&& !t.resolved_call_is_generic_fn(t.tc.resolved_call_names[src_idx]) {
		for t.tc.resolved_call_names.len <= dst_idx {
			t.tc.resolved_call_names << ''
			t.tc.resolved_call_set << false
		}
		t.tc.resolved_call_names[dst_idx] = t.tc.resolved_call_names[src_idx]
		t.tc.resolved_call_set[dst_idx] = true
	}
	if src_idx < t.tc.resolved_fn_value_set.len && t.tc.resolved_fn_value_set[src_idx] {
		for t.tc.resolved_fn_value_names.len <= dst_idx {
			t.tc.resolved_fn_value_names << ''
			t.tc.resolved_fn_value_set << false
		}
		t.tc.resolved_fn_value_names[dst_idx] = t.tc.resolved_fn_value_names[src_idx]
		t.tc.resolved_fn_value_set[dst_idx] = true
	}
}

fn (mut t Transformer) copy_cloned_resolution_forked(src_idx int, dst_idx int) {
	mut overlay := t.tc.fork_overlay
	mut call_name := overlay.resolved_call_names[src_idx] or { '' }
	if call_name.len == 0 && src_idx < t.tc.resolved_call_set.len && t.tc.resolved_call_set[src_idx] {
		call_name = t.tc.resolved_call_names[src_idx]
	}
	if call_name.len > 0 && !t.resolved_call_is_generic_fn(call_name) {
		overlay.resolved_call_names[dst_idx] = call_name
	}
	mut fn_value := overlay.resolved_fn_values[src_idx] or { '' }
	if fn_value.len == 0 && src_idx < t.tc.resolved_fn_value_set.len
		&& t.tc.resolved_fn_value_set[src_idx] {
		fn_value = t.tc.resolved_fn_value_names[src_idx]
	}
	if fn_value.len > 0 {
		overlay.resolved_fn_values[dst_idx] = fn_value
	}
}

fn (t &Transformer) resolved_call_is_generic_fn(name string) bool {
	if t.skip_generics || name.len == 0 || isnil(t.tc) {
		return false
	}
	if name.contains('[') && name.contains(']') {
		return true
	}
	if name in t.tc.fn_generic_params {
		return true
	}
	base := generic_fn_decl_base_value(name)
	if base in t.tc.fn_generic_params {
		return true
	}
	if name.contains('.') {
		short := name.all_after_last('.')
		if short in t.tc.fn_generic_params {
			return true
		}
	}
	return false
}

fn substitute_generic_node_value(node flat.Node, args []string) string {
	match node.kind {
		.call, .array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .sizeof_expr,
		.typeof_expr, .is_expr, .type_decl, .field_decl, .param {
			return substitute_generic_type_text(node.value, args)
		}
		else {
			return node.value
		}
	}
}

fn (mut t Transformer) fn_decl_has_unresolved_generics(node flat.Node, module_name string) bool {
	if generic_params_have_runtime_type_param(node.generic_params) {
		return true
	}
	if t.skip_generics {
		return false
	}
	if t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.type_text_has_generic_placeholder(node.value, module_name) {
		return true
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .param && t.type_text_has_generic_placeholder(child.typ, module_name) {
			return true
		}
	}
	return false
}

fn generic_params_have_runtime_type_param(params []string) bool {
	for param in params {
		if !generic_param_is_lifetime(param) {
			return true
		}
	}
	return false
}

fn generic_param_is_lifetime(param string) bool {
	return param.trim_space().starts_with('^')
}

fn (mut t Transformer) node_subtree_has_generic_placeholder(id flat.NodeId, module_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.type_text_has_generic_placeholder(node.value, module_name) {
		return true
	}
	for i in 0 .. node.children_count {
		if t.node_subtree_has_generic_placeholder(t.a.child(&node, i), module_name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) type_text_has_generic_placeholder(typ string, module_name string) bool {
	if t.skip_generics {
		return false
	}
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if clean == 'generic' {
		return true
	}
	if is_generic_fn_placeholder_name(clean) && !t.concrete_type_name_known(clean, module_name) {
		return true
	}
	if clean.starts_with('&') {
		return t.type_text_has_generic_placeholder(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return t.type_text_has_generic_placeholder(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.type_text_has_generic_placeholder(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return t.type_text_has_generic_placeholder(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return t.type_text_has_generic_placeholder(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.type_text_has_generic_placeholder(clean[4..bracket_end], module_name)
				|| t.type_text_has_generic_placeholder(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.type_text_has_generic_placeholder(clean[bracket_end + 1..], module_name)
		}
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if t.type_text_has_generic_placeholder(arg, module_name) {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) concrete_type_name_known(name string, module_name string) bool {
	if types.is_builtin_type_name(name) || name in ['C', 'JS'] {
		return true
	}
	qname := if name.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' {
		name
	} else {
		'${module_name}.${name}'
	}
	return name in t.structs || qname in t.structs || name in t.sum_types
		|| qname in t.sum_types || name in t.enum_types || qname in t.enum_types
		|| (!isnil(t.tc) && (name in t.tc.type_aliases || qname in t.tc.type_aliases
		|| name in t.tc.structs || qname in t.tc.structs || name in t.tc.sum_types
		|| qname in t.tc.sum_types || name in t.tc.enum_names
		|| qname in t.tc.enum_names || name in t.tc.interface_names
		|| qname in t.tc.interface_names))
}

fn (mut t Transformer) generic_fn_decl_key(node flat.Node, module_name string) string {
	base_value := generic_fn_decl_base_value(node.value)
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${base_value}'
	}
	return base_value
}

fn generic_fn_decl_base_value(value string) string {
	if !value.contains('.') {
		return value
	}
	receiver := value.all_before_last('.')
	method := value.all_after_last('.')
	base, _, ok := generic_app_parts(receiver)
	if ok {
		return '${base}.${method}'
	}
	return value
}

fn (t &Transformer) generic_decl_is_receiver_method(node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := t.a.child_node(&node, 0)
	if first.kind != .param || first.typ.len == 0 {
		return false
	}
	mut first_type := first.typ.trim_space()
	if first_type.starts_with('mut ') {
		first_type = first_type[4..].trim_space()
	}
	if first_type.starts_with('&') {
		first_type = first_type[1..].trim_space()
	}
	method_name := generic_fn_decl_base_value(node.value)
	clean_first := t.normalize_type_alias(first_type)
	return receiver_param_matches_method_name(clean_first, method_name)
		|| receiver_param_matches_method_name(first_type, method_name)
}

fn (mut t Transformer) generic_decl_has_method_level_params(decl GenericFnDecl) bool {
	all_params := t.generic_fn_param_names(decl.node, decl.module)
	if all_params.len == 0 {
		return false
	}
	mut receiver_params := []string{}
	if decl.node.value.contains('.') {
		receiver := decl.node.value.all_before_last('.')
		t.collect_generic_param_names_from_type(receiver, decl.module, mut receiver_params)
	}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type(child.typ, decl.module, mut receiver_params)
			break
		}
	}
	for param in all_params {
		if param !in receiver_params {
			return true
		}
	}
	return false
}

fn (mut t Transformer) generic_fn_param_names(node flat.Node, module_name string) []string {
	mut names := []string{}
	for param in node.generic_params {
		if param !in names {
			names << param
		}
	}
	t.collect_generic_param_names_from_type(node.value, module_name, mut names)
	t.collect_generic_param_names_from_type(node.typ, module_name, mut names)
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type(child.typ, module_name, mut names)
		}
	}
	return names
}

fn (mut t Transformer) collect_generic_param_names_from_node(id flat.NodeId, module_name string, mut names []string) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	t.collect_generic_param_names_from_type(node.typ, module_name, mut names)
	t.collect_generic_param_names_from_type(node.value, module_name, mut names)
	for i in 0 .. node.children_count {
		t.collect_generic_param_names_from_node(t.a.child(&node, i), module_name, mut names)
	}
}

fn (mut t Transformer) collect_generic_param_names_from_type(typ string, module_name string, mut names []string) {
	clean := typ.trim_space()
	if clean.len == 0 {
		return
	}
	if is_generic_fn_placeholder_name(clean) && !t.concrete_type_name_known(clean, module_name) {
		if clean !in names {
			names << clean
		}
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_param_names_from_type(clean[1..], module_name, mut names)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_param_names_from_type(clean[4..], module_name, mut names)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_param_names_from_type(clean[1..], module_name, mut names)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_param_names_from_type(clean[3..], module_name, mut names)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_param_names_from_type(clean[2..], module_name, mut names)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_param_names_from_type(clean[4..bracket_end], module_name, mut names)
			t.collect_generic_param_names_from_type(clean[bracket_end + 1..], module_name, mut
				names)
		}
		return
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			t.collect_generic_param_names_from_type(arg, module_name, mut names)
		}
		return
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := generic_matching_bracket(clean, bracket)
		if bracket_end > bracket && bracket_end < clean.len {
			for arg in split_generic_args(clean[bracket + 1..bracket_end]) {
				t.collect_generic_param_names_from_type(arg, module_name, mut names)
			}
		}
	}
}

fn generic_app_parts(typ string) (string, []string, bool) {
	bracket := typ.index_u8(`[`)
	if bracket <= 0 {
		return '', []string{}, false
	}
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return '', []string{}, false
	}
	bracket_end := generic_matching_bracket(typ, bracket)
	if bracket_end <= bracket || bracket_end >= typ.len {
		return '', []string{}, false
	}
	return typ[..bracket], split_generic_args(typ[bracket + 1..bracket_end]), true
}

fn generic_matching_bracket(s string, start int) int {
	mut depth := 0
	for i in start .. s.len {
		if s[i] == `[` {
			depth++
		} else if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

fn split_generic_args(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i in 0 .. s.len {
		match s[i] {
			`[` {
				depth++
			}
			`]` {
				depth--
			}
			`,` {
				if depth == 0 {
					parts << s[start..i].trim_space()
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << s[start..].trim_space()
	return parts
}

fn normalize_generic_args(args []string, module_name string) []string {
	mut result := []string{cap: args.len}
	for arg in args {
		result << normalize_generic_arg(arg, module_name)
	}
	return result
}

fn normalize_generic_arg(arg string, module_name string) string {
	clean := arg.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + normalize_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + normalize_generic_arg(clean[2..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + normalize_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := normalize_generic_arg(clean[4..bracket_end], module_name)
			val := normalize_generic_arg(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${val}'
		}
	}
	if clean.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' || types.is_builtin_type_name(clean) {
		return clean
	}
	return clean
}

fn substitute_generic_type_text(typ string, args []string) string {
	clean := typ.trim_space()
	if clean.len == 0 || args.len == 0 {
		return typ
	}
	if is_generic_fn_placeholder_name(clean) {
		idx := generic_param_index(clean)
		if idx < args.len {
			return args[idx]
		}
		return clean
	}
	if clean.starts_with('&') {
		return '&' + substitute_generic_type_text(clean[1..], args)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + substitute_generic_type_text(clean[4..], args)
	}
	if clean.starts_with('?') {
		return '?' + substitute_generic_type_text(clean[1..], args)
	}
	if clean.starts_with('!') {
		return '!' + substitute_generic_type_text(clean[1..], args)
	}
	if clean.starts_with('...') {
		return '...' + substitute_generic_type_text(clean[3..], args)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + substitute_generic_type_text(clean[7..], args)
	}
	if clean.starts_with('[]') {
		return '[]' + substitute_generic_type_text(clean[2..], args)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := substitute_generic_type_text(clean[4..bracket_end], args)
			val := substitute_generic_type_text(clean[bracket_end + 1..], args)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + substitute_generic_type_text(clean[bracket_end +
				1..], args)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			parts << substitute_generic_type_text(part, args)
		}
		return '(' + parts.join(', ') + ')'
	}
	base, nested_args, ok := generic_app_parts(clean)
	if ok {
		mut resolved_args := []string{}
		for arg in nested_args {
			resolved_args << substitute_generic_type_text(arg, args)
		}
		return '${base}[${resolved_args.join(', ')}]'
	}
	return clean
}

fn substitute_generic_type_text_with_params(typ string, args []string, params []string) string {
	clean := typ.trim_space()
	if clean.len == 0 || args.len == 0 {
		return typ
	}
	for i, param in params {
		if clean == param {
			if i < args.len {
				return args[i]
			}
			return clean
		}
	}
	if params.len == 0 && is_generic_fn_placeholder_name(clean) {
		idx := generic_param_index(clean)
		if idx < args.len {
			return args[idx]
		}
		return clean
	}
	if clean.starts_with('&') {
		return '&' + substitute_generic_type_text_with_params(clean[1..], args, params)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + substitute_generic_type_text_with_params(clean[4..], args, params)
	}
	if clean.starts_with('?') {
		return '?' + substitute_generic_type_text_with_params(clean[1..], args, params)
	}
	if clean.starts_with('!') {
		return '!' + substitute_generic_type_text_with_params(clean[1..], args, params)
	}
	if clean.starts_with('...') {
		return '...' + substitute_generic_type_text_with_params(clean[3..], args, params)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + substitute_generic_type_text_with_params(clean[7..], args, params)
	}
	if clean.starts_with('[]') {
		return '[]' + substitute_generic_type_text_with_params(clean[2..], args, params)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := substitute_generic_type_text_with_params(clean[4..bracket_end], args, params)
			val := substitute_generic_type_text_with_params(clean[bracket_end + 1..], args, params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] +
				substitute_generic_type_text_with_params(clean[bracket_end + 1..], args, params)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			parts << substitute_generic_type_text_with_params(part, args, params)
		}
		return '(' + parts.join(', ') + ')'
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		// Substitute the generic params inside a function-type parameter so a specialized
		// method body emits e.g. `fn (string) int`, not the placeholder `fn (T) int` (which
		// cgen would otherwise render as `fn (int) int`).
		if sub := subst_generic_fn_type_text(clean, args, params) {
			return sub
		}
	}
	base, nested_args, ok := generic_app_parts(clean)
	if ok {
		mut resolved_args := []string{}
		for arg in nested_args {
			resolved_args << substitute_generic_type_text_with_params(arg, args, params)
		}
		return '${base}[${resolved_args.join(', ')}]'
	}
	return clean
}

// subst_generic_fn_type_text substitutes generic params inside a `fn (...) ...` type text,
// recursing into each parameter type and the return type. Returns none when the signature
// is malformed (unbalanced parens).
fn subst_generic_fn_type_text(clean string, args []string, params []string) ?string {
	params_start := clean.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < clean.len {
		if clean[params_end] == `(` {
			depth++
		} else if clean[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= clean.len {
		return none
	}
	params_str := clean[params_start..params_end]
	mut fn_parts := []string{}
	if params_str.trim_space().len > 0 {
		mut pdepth := 0
		mut start := 0
		for i := 0; i < params_str.len; i++ {
			c := params_str[i]
			if c == `(` || c == `[` {
				pdepth++
			} else if c == `)` || c == `]` {
				pdepth--
			} else if c == `,` && pdepth == 0 {
				fn_parts << subst_generic_fn_type_param_text(params_str[start..i], args, params)
				start = i + 1
			}
		}
		fn_parts << subst_generic_fn_type_param_text(params_str[start..], args, params)
	}
	ret_str := clean[params_end + 1..].trim_space()
	if ret_str.len > 0 {
		return 'fn(${fn_parts.join(', ')}) ${substitute_generic_type_text_with_params(ret_str,
			args, params)}'
	}
	return 'fn(${fn_parts.join(', ')})'
}

fn subst_generic_fn_type_param_text(param string, args []string, params []string) string {
	mut text := param.trim_space()
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if generic_fn_type_param_head_is_name(head, tail) {
			sub := substitute_generic_type_text_with_params(tail, args, params)
			if is_mut && sub.len > 0 && !sub.starts_with('&') {
				return '${head} &${sub}'
			}
			return '${head} ${sub}'
		}
	}
	sub := substitute_generic_type_text_with_params(text, args, params)
	if is_mut && sub.len > 0 && !sub.starts_with('&') {
		return '&${sub}'
	}
	return sub
}

fn generic_top_level_space_index(s string) int {
	mut depth := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			` ` {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return -1
}

fn generic_fn_type_param_head_is_name(head string, tail string) bool {
	if head.len == 0 || tail.len == 0 {
		return false
	}
	if head.starts_with('fn') || head.starts_with('&') || head.starts_with('[') {
		return false
	}
	if head in ['shared', 'atomic', 'chan', 'thread', 'map'] || head.contains('.') {
		return false
	}
	if types.is_builtin_type_name(head) {
		return false
	}
	return (head[0] >= `a` && head[0] <= `z`) || head[0] == `_`
}

// subst_type substitutes generic placeholders in a type-text using the currently
// active generic parameter names (so non-canonical params resolve by name). Falls
// back to positional substitution when no params are active.
fn (t &Transformer) subst_type(typ string, args []string) string {
	return substitute_generic_type_text_with_params(typ, args, t.active_generic_params)
}

// subst_node_value is the param-aware counterpart of substitute_generic_node_value.
fn (t &Transformer) subst_node_value(node flat.Node, args []string) string {
	match node.kind {
		.ident {
			if node.value.contains('[') {
				return t.subst_type(node.value, args)
			}
			if node.typ.contains('[') {
				base, _, ok := generic_app_parts(node.typ)
				if ok && node.value == base.all_after_last('.') {
					return t.subst_type(node.typ, args)
				}
			}
			return node.value
		}
		.selector {
			if node.value.contains('[') {
				return t.subst_type(node.value, args)
			}
			if node.typ.contains('[') {
				old_field_type := t.trim_pointer_type(node.typ)
				new_field_type := t.trim_pointer_type(t.subst_type(node.typ, args))
				if old_field_type != new_field_type
					&& t.sum_field_name(old_field_type) == node.value {
					return t.sum_field_name(new_field_type)
				}
			}
			return node.value
		}
		.is_expr {
			if node.value in t.cloning_comptime_for_vars {
				return node.value
			}
			return t.resolve_substituted_type_text(t.subst_type(node.value, args))
		}
		.array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .sizeof_expr,
		.typeof_expr {
			return t.resolve_substituted_type_text(t.subst_type(node.value, args))
		}
		.call, .type_decl, .field_decl, .param {
			return t.subst_type(node.value, args)
		}
		.comptime_if {
			return t.subst_comptime_type_condition(node.value, args)
		}
		else {
			return node.value
		}
	}
}

fn (t &Transformer) resolve_substituted_type_text(typ string) string {
	clean := typ.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return typ
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.resolve_substituted_type_text(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + t.resolve_substituted_type_text(clean[7..])
	}
	if t.substituted_type_belongs_to_main_generic(clean) {
		return clean
	}
	parsed := t.tc.parse_resolution_type(clean)
	if parsed is types.Unknown {
		return typ
	}
	resolved := parsed.name()
	base, args, is_generic_app := generic_app_parts(clean)
	if is_generic_app && args.len > 0 {
		resolved_base := if resolved.contains('[') { resolved.all_before('[') } else { resolved }
		if resolved_base.len > 0 && resolved_base != base {
			mut resolved_args := []string{cap: args.len}
			for arg in args {
				resolved_args << t.resolve_substituted_type_text(arg)
			}
			return '${resolved_base}[${resolved_args.join(', ')}]'
		}
	}
	return resolved
}

fn (t &Transformer) substituted_type_belongs_to_main_generic(typ string) bool {
	clean := typ.trim_space()
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
		if clean.starts_with(prefix) {
			return t.substituted_type_belongs_to_main_generic(clean[prefix.len..])
		}
	}
	if !clean.contains('.') && (clean in t.structs || clean in t.sum_types || clean in t.enum_types) {
		return true
	}
	base, _, ok := generic_app_parts(clean)
	if !ok || base.contains('.') || isnil(t.tc) {
		return false
	}
	if base !in t.tc.struct_generic_params {
		return false
	}
	decl_module := t.tc.struct_modules[base] or { '' }
	return decl_module.len == 0 || decl_module == 'main'
}

fn (t &Transformer) subst_comptime_type_condition(cond string, args []string) string {
	mut clean := cond.trim_space()
	for i, param in t.active_generic_params {
		if i >= args.len {
			break
		}
		clean = clean.replace('${param}.indirections', generic_type_indirections(args[i]).str())
	}
	if clean.starts_with('(') {
		end := comptime_condition_matching_paren(clean, 0)
		if end == clean.len - 1 {
			inner := t.subst_comptime_type_condition(clean[1..clean.len - 1], args)
			return '(${inner})'
		}
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := t.subst_comptime_type_condition(clean[..or_idx], args)
		right := t.subst_comptime_type_condition(clean[or_idx + 2..], args)
		return '${left} || ${right}'
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := t.subst_comptime_type_condition(clean[..and_idx], args)
		right := t.subst_comptime_type_condition(clean[and_idx + 2..], args)
		return '${left} && ${right}'
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			return '${t.subst_comptime_type_operand(left, args)}${op}${t.subst_comptime_type_operand(right,
				args)}'
		}
	}
	for op in [' !in', ' in'] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			after := op_idx + op.len
			if after >= clean.len || (clean[after] != `[` && clean[after] != ` `) {
				continue
			}
			left := t.subst_comptime_type_operand(clean[..op_idx], args)
			list := clean[after..].trim_space()
			if list.starts_with('[') && list.ends_with(']') {
				mut items := []string{}
				for item in split_generic_args(list[1..list.len - 1]) {
					items << t.subst_comptime_type_operand(item, args)
				}
				return '${left}${op}[${items.join(',')}]'
			}
		}
	}
	if clean.starts_with('!') {
		inner_raw := clean[1..].trim_space()
		inner := t.subst_comptime_type_condition(inner_raw, args)
		if inner_raw.starts_with('(') {
			return '!(${inner})'
		}
		return '!${inner}'
	}
	return clean
}

fn generic_arg_type_for_param(param_type string, arg_type string) string {
	mut actual := arg_type.trim_space()
	param := param_type.trim_space()
	for wrapper in ['shared ', 'atomic '] {
		if actual.starts_with(wrapper) && !param.starts_with(wrapper) {
			actual = actual[wrapper.len..].trim_space()
		}
	}
	return actual
}

fn generic_type_indirections(typ string) int {
	mut clean := typ.trim_space()
	mut count := 0
	if clean.starts_with('shared ') {
		count++
		clean = clean[7..].trim_space()
	}
	for clean.starts_with('&') {
		count++
		clean = clean[1..].trim_space()
	}
	return count
}

fn (t &Transformer) subst_comptime_type_operand(raw string, args []string) string {
	clean := raw.trim_space()
	if clean.starts_with('$') {
		// `$int`, `$struct`, ... are metatype keywords, not type names; they must
		// not be substituted or module-qualified (`mymod.$int` breaks matching).
		return clean
	}
	if t.cloning_comptime_for_depth > 0 && !clean.contains('.') && t.raw_var_type(clean).len > 0 {
		// A runtime value can be smartcast by the current comptime loop variant.
		// Keep its name until the loop body is cloned so nested `$if value is
		// $type_group` conditions see the variant type, not the unspecialized
		// outer sum type.
		return clean
	}
	if !clean.contains('.') {
		var_type := t.raw_var_type(clean)
		if var_type.len > 0 {
			value_type := if t.mut_param_values[clean] {
				var_type.trim_string_left('&')
			} else {
				var_type
			}
			return t.resolve_substituted_type_text(t.subst_type(value_type, args))
		}
		if comptime_condition_is_unresolved_value_ident(clean) {
			return clean
		}
	}
	if clean.ends_with('.unaliased_typ') {
		base := clean[..clean.len - '.unaliased_typ'.len]
		substituted := t.resolve_substituted_type_text(t.subst_type(base, args))
		return t.comptime_normalize_type_alias_chain(substituted)
	}
	if clean.ends_with('.typ') {
		base := clean[..clean.len - '.typ'.len]
		if base.len > 0 {
			// Substitute the base but keep the `.typ` selector: subst_type does
			// not recognize the selector form, and evaluation uses the suffix
			// to preserve alias identity (`MyAlias.typ is string` is false).
			return t.subst_type(base, args) + '.typ'
		}
	}
	return t.resolve_substituted_type_text(t.subst_type(clean, args))
}

// active_generic_param_index returns the position of a placeholder name within the
// active generic params, or the positional fallback when none are active.
fn (t &Transformer) active_generic_param_index(name string) int {
	for i, p in t.active_generic_params {
		if p == name {
			return i
		}
	}
	return generic_param_index(name)
}

fn specialized_generic_fn_value(value string, args []string) string {
	if value.contains('.') {
		receiver := value.all_before_last('.')
		method := value.all_after_last('.')
		base, _, ok := generic_app_parts(receiver)
		if ok {
			return '${base}[${generic_type_args_short(args)}].${method}'
		}
		return '${receiver}[${generic_type_args_short(args)}].${method}'
	}
	return '${value}_T_${generic_type_suffixes(args)}'
}

fn generic_type_args_short(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << generic_type_arg_short(arg)
	}
	return parts.join(', ')
}

fn generic_type_arg_short(type_arg string) string {
	clean := type_arg.trim_space()
	if clean.starts_with('[]') {
		return 'Array_${generic_type_arg_short(clean[2..])}'
	}
	if clean.starts_with('&') {
		return 'ptr_${generic_type_arg_short(clean[1..])}'
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := generic_type_arg_short(clean[4..bracket_end])
			value := generic_type_arg_short(clean[bracket_end + 1..])
			return 'Map_${key}_${value}'
		}
	}
	if clean.starts_with('?') {
		return 'Option_${generic_type_arg_short(clean[1..])}'
	}
	if clean.starts_with('!') {
		return 'Result_${generic_type_arg_short(clean[1..])}'
	}
	if fixed := generic_fixed_array_type_arg_short(clean) {
		return fixed
	}
	// Function-type args (`fn (A, B) R`) and other compound types cannot appear
	// verbatim in a C identifier and must not be naively shortened by the last
	// `.` (which would truncate `fn(...) mod.R` to `R)`). Reduce them to a
	// deterministic sanitized fragment instead.
	if clean.contains('(') || clean.contains(' ') {
		return sanitize_type_name_fragment(clean)
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		mut parts := [generic_type_arg_short(base)]
		for arg in args {
			parts << generic_type_arg_short(arg)
		}
		return parts.join('_')
	}
	if clean.contains('.') {
		return clean
	}
	return clean
}

fn generic_fixed_array_type_arg_short(type_arg string) ?string {
	clean := type_arg.trim_space()
	if !clean.starts_with('[') {
		return none
	}
	close_idx := clean.index_u8(`]`)
	if close_idx <= 1 || close_idx + 1 >= clean.len {
		return none
	}
	len_text := clean[1..close_idx].trim_space()
	elem_text := clean[close_idx + 1..].trim_space()
	if len_text.len == 0 || elem_text.len == 0 {
		return none
	}
	elem := generic_type_arg_short(elem_text)
	return '${elem}_${len_text}'
}

// sanitize_type_name_fragment reduces an arbitrary type text (notably a function
// type) to a deterministic fragment that is a valid C identifier piece: module
// qualifiers are dropped, `[]`/`&` become readable prefixes, and every other run
// of non-identifier characters collapses to a single underscore.
fn sanitize_type_name_fragment(typ string) string {
	mut out := []u8{}
	mut prev_us := false
	mut i := 0
	for i < typ.len {
		c := typ[i]
		if (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) {
			out << c
			prev_us = false
			i++
		} else if c == `[` && i + 1 < typ.len && typ[i + 1] == `]` {
			for ch in 'Array_'.bytes() {
				out << ch
			}
			prev_us = false
			i += 2
		} else if c == `&` {
			for ch in 'ptr_'.bytes() {
				out << ch
			}
			prev_us = false
			i++
		} else if c == `.` {
			// Drop the preceding module qualifier (everything emitted since the
			// last separator), keeping only the unqualified name.
			for out.len > 0 {
				last := out[out.len - 1]
				if (last >= `A` && last <= `Z`) || (last >= `a` && last <= `z`)
					|| (last >= `0` && last <= `9`) {
					out.delete_last()
				} else {
					break
				}
			}
			i++
		} else {
			if !prev_us {
				out << `_`
				prev_us = true
			}
			i++
		}
	}
	mut s := out.bytestr()
	for s.starts_with('_') {
		s = s[1..]
	}
	for s.ends_with('_') {
		s = s[..s.len - 1]
	}
	return s
}

fn generic_type_suffixes(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << c_name(generic_type_arg_short(arg).replace('[]', 'Array_').replace('&', 'ptr_'))
	}
	return parts.join('_')
}

fn generic_type_full_suffixes(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << c_name(arg.trim_space().replace('[]', 'Array_').replace('&', 'ptr_'))
	}
	return parts.join('_')
}

fn (mut t Transformer) record_generic_specialization_args_in_module(base string, module_name string, args []string) {
	clean_base := base.trim_space()
	if clean_base.len == 0 || args.len == 0 {
		return
	}
	recorded_args := t.canonical_generic_specialization_args(args)
	if t.generic_args_have_placeholders(recorded_args) {
		return
	}
	suffix := generic_type_suffixes(recorded_args)
	qualified_base := if clean_base.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' {
		clean_base
	} else {
		'${module_name}.${clean_base}'
	}
	mut keys := []string{}
	if clean_base.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' {
		keys << '${clean_base}[${recorded_args.join(', ')}]'
		keys << c_name('${clean_base}[${recorded_args.join(', ')}]')
		keys << '${clean_base}_${suffix}'
		keys << c_name('${clean_base}_${suffix}')
	}
	keys << '${qualified_base}[${recorded_args.join(', ')}]'
	keys << c_name('${qualified_base}[${recorded_args.join(', ')}]')
	keys << '${qualified_base}_${suffix}'
	keys << c_name('${qualified_base}_${suffix}')
	if qualified_base.contains('.') {
		base_module := qualified_base.all_before_last('.')
		short_base := qualified_base.all_after_last('.')
		keys << '${base_module}.${short_base}_${suffix}'
		keys << c_name('${base_module}.${short_base}_${suffix}')
	}
	for key in keys {
		if key.len > 0 && key !in t.generic_specialization_args {
			t.generic_specialization_args[key] = recorded_args.clone()
		}
	}
}

fn (mut t Transformer) record_generic_specialization_args_for_names(names []string, args []string) {
	if args.len == 0 || t.generic_args_have_placeholders(args) {
		return
	}
	recorded_args := t.canonical_generic_specialization_args(args)
	for name in names {
		if name.len > 0 && name !in t.generic_specialization_args {
			t.generic_specialization_args[name] = recorded_args.clone()
		}
	}
}

fn (t &Transformer) canonical_generic_specialization_args(args []string) []string {
	mut out := []string{cap: args.len}
	for arg in args {
		out << t.canonical_generic_specialization_arg(arg)
	}
	return out
}

fn (t &Transformer) canonical_generic_specialization_arg(arg string) string {
	clean := arg.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.canonical_generic_specialization_arg(clean[1..])
	}
	if clean.starts_with('mut ') {
		return 'mut ' + t.canonical_generic_specialization_arg(clean[4..])
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.canonical_generic_specialization_arg(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + t.canonical_generic_specialization_arg(clean[7..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + t.canonical_generic_specialization_arg(clean[1..])
	}
	if clean.starts_with('...') {
		return '...' + t.canonical_generic_specialization_arg(clean[3..])
	}
	if clean.starts_with('[]') {
		return '[]' + t.canonical_generic_specialization_arg(clean[2..])
	}
	if fixed := t.canonical_suffix_fixed_array_arg(clean) {
		return fixed
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := t.canonical_generic_specialization_arg(clean[4..bracket_end])
			value := t.canonical_generic_specialization_arg(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	base, nested_args, is_generic_app := generic_app_parts(clean)
	if is_generic_app {
		mut canonical_args := []string{cap: nested_args.len}
		for nested_arg in nested_args {
			canonical_args << t.canonical_generic_specialization_arg(nested_arg)
		}
		return '${base}[${canonical_args.join(', ')}]'
	}
	if t.generic_arg_is_alias_name(clean, '') {
		return clean
	}
	// Resolve an import-alias module prefix (`import x.json2 as json` makes
	// `json.Any` mean `json2.Any`); the alias text must not leak into
	// specialization keys/typedef names parsed later without file context.
	if clean.contains('.') {
		alias := clean.all_before('.')
		resolved_mod := t.import_alias_module(alias)
		if resolved_mod != alias && resolved_mod.len > 0 {
			resolved := '${resolved_mod}.${clean.all_after('.')}'
			normalized_resolved := t.normalize_type_alias(resolved)
			if normalized_resolved != resolved {
				return t.canonical_generic_specialization_arg(normalized_resolved)
			}
			return resolved
		}
		if !t.type_name_is_declared(clean) {
			short := clean.all_after_last('.')
			if qualified := t.qualified_types[short] {
				return qualified
			}
		}
	}
	normalized := t.normalize_type_alias(clean)
	if normalized != clean {
		return t.canonical_generic_specialization_arg(normalized)
	}
	if decoded_array := generic_array_type_arg_from_suffix(clean) {
		return decoded_array
	}
	return clean
}

fn (t &Transformer) canonical_suffix_fixed_array_arg(arg string) ?string {
	clean := arg.trim_space()
	if !clean.ends_with(']') {
		return none
	}
	open_idx := clean.last_index('[') or { return none }
	if open_idx <= 0 || open_idx + 1 >= clean.len - 1 {
		return none
	}
	elem := clean[..open_idx].trim_space()
	len_text := clean[open_idx + 1..clean.len - 1].trim_space()
	if elem.len == 0 || len_text.len == 0 {
		return none
	}
	if types.is_builtin_type_name(len_text) || len_text.contains(',') {
		return none
	}
	_, _, is_generic_app := generic_app_parts(clean)
	if is_generic_app && !types.is_builtin_type_name(elem) {
		return none
	}
	clean_len := if len_text.contains('.') { len_text.all_after_last('.') } else { len_text }
	return '[${clean_len}]${t.canonical_generic_specialization_arg(elem)}'
}

fn (t &Transformer) recorded_generic_specialization_args(typ string) ?[]string {
	clean := typ.trim_space().trim_left('&')
	if clean.len == 0 {
		return none
	}
	candidates := generic_specialization_lookup_candidates(clean)
	for candidate in candidates {
		if args := t.generic_specialization_args[candidate] {
			return args
		}
	}
	return none
}

fn generic_specialization_lookup_candidates(typ string) []string {
	mut candidates := []string{}
	add_generic_specialization_lookup_candidate(mut candidates, typ)
	add_generic_specialization_lookup_candidate(mut candidates, c_name(typ))
	if typ.contains('__') {
		dotted := typ.replace('__', '.')
		add_generic_specialization_lookup_candidate(mut candidates, dotted)
		add_generic_specialization_lookup_candidate(mut candidates, c_name(dotted))
	}
	return candidates
}

fn add_generic_specialization_lookup_candidate(mut candidates []string, candidate string) {
	clean := candidate.trim_space()
	if clean.len == 0 || clean in candidates {
		return
	}
	candidates << clean
}

fn generic_param_index(name string) int {
	return match name {
		'T', 'A', 'K', 'X' { 0 }
		'U', 'B', 'V', 'Y' { 1 }
		'C', 'W', 'Z' { 2 }
		else { 0 }
	}
}

fn is_generic_fn_placeholder_name(typ string) bool {
	clean := typ.trim_space()
	if clean.contains('.') {
		return is_generic_fn_placeholder_name(clean.all_after_last('.'))
	}
	return clean.len == 1 && clean[0] >= `A` && clean[0] <= `Z`
}

fn (t &Transformer) generic_args_have_placeholders(args []string) bool {
	if t.skip_generics {
		return false
	}
	for arg in args {
		if t.generic_arg_is_unresolved(arg) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_arg_is_unresolved(arg string) bool {
	if t.skip_generics {
		return false
	}
	// Hot: called for every call/assign type text during body transforms, with
	// heavy repetition (a few thousand distinct texts per build). The result is
	// a pure function of the text plus the declared-type tables and cur_module
	// (both fixed during body transforms), memoized per (module, text).
	if !isnil(t.generic_unresolved_cache) {
		mut cache := t.generic_unresolved_cache
		if cache.module != t.cur_module {
			cache.module = t.cur_module
			cache.entries.clear()
		}
		if cached := cache.entries[arg] {
			return cached > 0
		}
		result := t.generic_arg_is_unresolved_uncached(arg)
		cache.entries[arg] = if result { i8(1) } else { i8(-1) }
		return result
	}
	return t.generic_arg_is_unresolved_uncached(arg)
}

fn (t &Transformer) generic_arg_is_unresolved_uncached(arg string) bool {
	clean := arg.trim_space()
	if clean.len == 0 || clean.all_after_last('.') in ['unknown', 'void', 'generic'] {
		return true
	}
	if is_generic_fn_placeholder_name(clean) {
		// A one-letter uppercase name is only an unresolved generic placeholder
		// when it does not name a real declared concrete type. `Box[A]` where
		// `struct A` exists is a concrete instantiation that must be materialized.
		return !t.is_known_concrete_type_name(clean)
	}
	if clean.starts_with('&') {
		return t.generic_arg_is_unresolved(clean[1..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.generic_arg_is_unresolved(clean[1..])
	}
	if clean.starts_with('...') {
		return t.generic_arg_is_unresolved(clean[3..])
	}
	if clean.starts_with('[]') {
		return t.generic_arg_is_unresolved(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.generic_arg_is_unresolved(clean[4..bracket_end])
				|| t.generic_arg_is_unresolved(clean[bracket_end + 1..])
		}
	}
	_, nested_args, ok := generic_app_parts(clean)
	if ok {
		for nested in nested_args {
			if t.generic_arg_is_unresolved(nested) {
				return true
			}
		}
	}
	return false
}

// is_known_concrete_type_name reports whether `name` refers to an already
// declared concrete type (struct, sum type, enum, interface or alias), so that
// one-letter type names like `A` are not mistaken for generic placeholders.
fn (t &Transformer) is_known_concrete_type_name(name string) bool {
	if t.type_name_is_declared(name) {
		return true
	}
	if !name.contains('.') {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& t.type_name_is_declared('${t.cur_module}.${name}') {
			return true
		}
		if t.type_name_is_declared('main.${name}') {
			return true
		}
	}
	return false
}

fn (t &Transformer) type_name_is_declared(name string) bool {
	if name in t.structs || name in t.sum_types || name in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		if name in t.tc.structs || name in t.tc.sum_types || name in t.tc.enum_names
			|| name in t.tc.interface_names || name in t.tc.type_aliases {
			return true
		}
	}
	return false
}

fn generic_type_arg_from_receiver_suffix(suffix string) string {
	clean := suffix.trim_space()
	if clean.len == 0 || (clean.starts_with('Array_') && !clean.starts_with('Array_fixed_')) {
		return ''
	}
	return generic_type_arg_from_suffix(clean)
}

fn generic_array_type_arg_from_suffix(suffix string) ?string {
	clean := suffix.trim_space()
	if !clean.starts_with('Array_') || clean.starts_with('Array_fixed_') {
		return none
	}
	elem_suffix := clean['Array_'.len..]
	if elem_suffix.len == 0 {
		return none
	}
	split := elem_suffix.last_index_u8(`_`)
	if split > 0 {
		len_text := elem_suffix[split + 1..]
		if len_text.len > 0 && is_decimal_text(len_text) {
			elem := generic_type_arg_from_suffix(elem_suffix[..split])
			if elem.len > 0 {
				return '[${len_text}]${elem}'
			}
		}
	}
	elem := if nested := generic_array_type_arg_from_suffix(elem_suffix) {
		nested
	} else {
		generic_type_arg_from_suffix(elem_suffix)
	}
	if elem.len == 0 {
		return none
	}
	return '[]${elem}'
}

fn (t &Transformer) generic_type_arg_from_suffix(suffix string) string {
	clean := suffix.trim_space()
	if clean.len == 0 {
		return ''
	}
	return generic_type_arg_from_suffix(clean)
}

fn generic_type_arg_from_suffix(suffix string) string {
	clean := suffix.trim_space()
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('ptr_') {
		inner := generic_type_arg_from_suffix(clean['ptr_'.len..])
		if inner.len > 0 {
			return '&${inner}'
		}
	}
	return match clean {
		'v_int' { 'int' }
		'v_u8' { 'u8' }
		'v_u16' { 'u16' }
		'v_u32' { 'u32' }
		'v_u64' { 'u64' }
		'v_i8' { 'i8' }
		'v_i16' { 'i16' }
		'v_i32' { 'i32' }
		'v_i64' { 'i64' }
		'v_f32' { 'f32' }
		'v_f64' { 'f64' }
		'Array_u8' { '[]u8' }
		else { clean.replace('__', '.') }
	}
}

fn generic_fn_spec_key(decl_key string, args []string) string {
	return '${decl_key}[${args.join(', ')}]'
}

fn (t &Transformer) ident_is_import_alias(name string) bool {
	if isnil(t.tc) {
		return false
	}
	return name in t.tc.imports || file_import_key(t.tc.cur_file, name) in t.tc.file_imports
}

fn (t &Transformer) import_alias_module(name string) string {
	if isnil(t.tc) {
		return name
	}
	if mod := t.tc.file_imports[file_import_key(t.tc.cur_file, name)] {
		return mod
	}
	return t.tc.imports[name] or { name }
}

fn file_import_key(file string, alias string) string {
	return '${file}\n${alias}'
}
