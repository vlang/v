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
	template_nodes := t.generic_decl_template_nodes(decls)
	ignored_nodes := t.unused_fn_subtree_nodes()
	mut emitted := map[string]bool{}
	mut generated := []string{}
	mut generic_call_sites := []GenericCallSite{}
	mut recorded_call_sites := map[int]bool{}
	mut changed := true
	mut scan_start := 0
	for changed {
		changed = false
		t.ensure_node_module_map()
		node_count := t.a.nodes.len
		for i in scan_start .. node_count {
			if (i < template_nodes.len && template_nodes[i])
				|| (i < ignored_nodes.len && ignored_nodes[i]) {
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
						t.a.nodes[i].value = args.join(', ')
					}
					if i !in recorded_call_sites {
						generic_call_sites << GenericCallSite{
							id:     flat.NodeId(i)
							module: call_module
						}
						recorded_call_sites[i] = true
					}
					spec_key := generic_fn_spec_key(decl_key, args)
					if emitted[spec_key] {
						continue
					}
					generated << t.generated_fn_used_names(decl,
						t.emit_generic_fn_specialization(decl, args), args)
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
		extra_call_sites := t.collect_generic_call_sites_after_type_refresh(decls, template_nodes,
			ignored_nodes, mut emitted, mut generated, mut recorded_call_sites)
		if extra_call_sites.len > 0 {
			t.rewrite_generic_call_sites(decls, extra_call_sites)
			t.refresh_decl_assign_types_after_generic_rewrite()
		}
	}
	t.erase_generic_fn_decls(decls)
	return generated
}

fn (mut t Transformer) collect_generic_call_sites_after_type_refresh(decls map[string]GenericFnDecl, template_nodes []bool, ignored_nodes []bool, mut emitted map[string]bool, mut generated []string, mut recorded_call_sites map[int]bool) []GenericCallSite {
	mut sites := []GenericCallSite{}
	t.ensure_node_module_map()
	node_count := t.a.nodes.len
	for i in 0 .. node_count {
		if (i < template_nodes.len && template_nodes[i])
			|| (i < ignored_nodes.len && ignored_nodes[i]) {
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
			t.a.nodes[i].value = args.join(', ')
		}
		if i !in recorded_call_sites {
			sites << GenericCallSite{
				id:     flat.NodeId(i)
				module: call_module
			}
			recorded_call_sites[i] = true
		}
		spec_key := generic_fn_spec_key(decl_key, args)
		if emitted[spec_key] {
			continue
		}
		generated << t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl, args), args)
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
				if !t.generic_struct_method_used_for_spec(spec, decl, args, method)
					&& !generic_struct_spec_has_emitted_method(base, args, decls, emitted) {
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
			spec_key := generic_fn_spec_key(decl_key, args)
			if emitted[spec_key] {
				continue
			}
			generated << t.generated_fn_used_names(decl,
				t.emit_generic_fn_specialization(decl, args), args)
			emitted[spec_key] = true
			any = true
		}
	}
	return any
}

fn generic_struct_spec_has_emitted_method(base string, args []string, decls map[string]GenericFnDecl, emitted map[string]bool) bool {
	for decl_key, _ in decls {
		if !decl_key.contains('.') || decl_key.all_before_last('.') != base {
			continue
		}
		if emitted[generic_fn_spec_key(decl_key, args)] {
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
	if receiver.len > 0 && t.generic_struct_method_used_for_base(receiver, decl.module, method) {
		return true
	}
	return false
}

fn (t &Transformer) generic_struct_method_used_for_base(receiver string, module_name string, method string) bool {
	mut bases := [receiver]
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !receiver.contains('.') {
		bases << '${module_name}.${receiver}'
	}
	if receiver.contains('.') {
		bases << receiver.all_after_last('.')
	}
	mut prefixes := []string{}
	for base in bases {
		if base.len == 0 {
			continue
		}
		prefixes << '${base}_'
		prefixes << c_name('${base}_')
	}
	for used, _ in t.used_fns {
		if !(used.ends_with('.${method}') || used.ends_with('__${c_name(method)}')) {
			continue
		}
		for prefix in prefixes {
			if used.starts_with(prefix) {
				return true
			}
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
	return t.interface_boxed_types[interface_boxed_type_key(iface_name, concrete_type)]
		|| t.interface_boxed_types[interface_boxed_type_key(iface_name, c_name(concrete_type))]
}

fn (mut t Transformer) collect_interface_boxed_types() {
	if t.interface_boxed_types_done || isnil(t.tc) {
		return
	}
	t.interface_boxed_types_done = true
	for node in t.a.nodes {
		if node.kind != .struct_init || node.children_count == 0 {
			continue
		}
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
	t.interface_boxed_types[interface_boxed_type_key(iface_name, concrete_type)] = true
	t.interface_boxed_types[interface_boxed_type_key(iface_name, c_name(concrete_type))] = true
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
		parsed := t.tc.parse_type(arg)
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
		parsed := t.tc.parse_type(arg)
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
		parsed := t.tc.parse_type(variant_type)
		variants << if parsed is types.Unknown { variant_type } else { parsed.name() }
	}
	t.tc.sum_types[spec_name] = variants
	t.sum_types[spec_name] = variants
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
			typ:  t.tc.parse_type(field_type)
		}
	}
	t.tc.structs[spec_name] = fields
	if decl.key in t.tc.unions {
		t.tc.unions[spec_name] = true
	}
	if decl.key in t.tc.params_structs {
		t.tc.params_structs[spec_name] = true
	}
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

fn (mut t Transformer) generic_decl_template_nodes(decls map[string]GenericFnDecl) []bool {
	mut nodes := []bool{len: t.a.nodes.len}
	for _, decl in decls {
		t.collect_node_subtree_flags(decl.id, mut nodes)
	}
	return nodes
}

fn (mut t Transformer) unused_fn_subtree_nodes() []bool {
	mut nodes := []bool{len: t.a.nodes.len}
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
				if !t.should_transform_fn(node) {
					t.collect_node_subtree_flags(flat.NodeId(i), mut nodes)
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

fn (mut t Transformer) collect_node_subtree_flags(id flat.NodeId, mut nodes []bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len || int(id) >= nodes.len || nodes[int(id)] {
		return
	}
	nodes[int(id)] = true
	node := t.a.nodes[int(id)]
	for i in 0 .. node.children_count {
		t.collect_node_subtree_flags(t.a.child(&node, i), mut nodes)
	}
}

fn (mut t Transformer) emit_generic_fn_specialization(decl GenericFnDecl, args []string) flat.NodeId {
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
			t.record_generic_specialization_args_in_module(receiver, decl.module, args)
		}
	}
	clone_id := t.clone_generic_fn_node(decl.node, args)
	t.specialize_cloned_fn_signature(clone_id, decl, args)
	clone := t.a.nodes[int(clone_id)]
	t.register_specialized_fn_signature(decl, clone, args)
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
	t.transform_fn_body(int(clone_id))
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
			t.set_var_type_with_raw(child.value, typ, raw_typ)
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
	t.a.nodes[int(clone_id)].typ = t.specialized_signature_type_text(decl, decl.node.typ, args,
		params)
	t.a.nodes[int(clone_id)].generic_params = []string{}
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
		t.a.nodes[int(dst_id)].typ = t.specialized_signature_type_text(decl, src.typ, args, params)
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
	ret := if !isnil(t.tc) { t.tc.parse_type(ret_name) } else { types.Type(types.void_) }
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
			params << t.tc.parse_type(param_type)
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
	substituted := substitute_generic_type_text_with_params(typ, args, params)
	qualified := t.qualify_specialized_signature_type_text(substituted, decl)
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
	parsed := t.tc.parse_type(qualified)
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
	if parsed is types.Unknown {
		return qualified
	}
	return parsed.name()
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
			t.a.nodes[i].value = args.join(', ')
		}
		if t.generic_decl_is_receiver_method(decl.node) {
			t.rewrite_generic_method_call(site.id, node, decl, args)
		} else {
			t.rewrite_generic_plain_call(site.id, node, decl, args)
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
		if node.typ.len > 0 && !t.generic_arg_is_unresolved(node.typ) {
			continue
		}
		t.a.nodes[i].typ = rhs.typ
		changed = true
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident {
			t.a.nodes[int(lhs_id)].typ = rhs.typ
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
		arg_type := t.generic_call_arg_type_for_inference(arg_id)
		if arg_type.len > 0 {
			infer_generic_type_args(child.typ, arg_type, mut inferred)
			if is_recv_param {
				t.infer_generic_receiver_suffix_args(child.typ, arg_type, mut inferred)
				t.infer_generic_embedded_receiver_args(child.typ, arg_type, mut inferred)
			}
		}
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
	t.a.nodes[int(id)] = flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          ''
		typ:            ret_typ
	}
	t.clear_resolved_call(id)
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
	substituted := substitute_generic_type_text_with_params(typ, args, params)
	return t.qualify_specialized_signature_type_text(substituted, decl)
}

fn (mut t Transformer) retype_generic_call_literal_arg(arg_id flat.NodeId, param_type string) flat.NodeId {
	if int(arg_id) < 0 || param_type.len == 0 {
		return arg_id
	}
	node := t.a.nodes[int(arg_id)]
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
			t.a.nodes[int(arg_id)].typ = param_type
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
				t.a.nodes[i].typ = array_type
				t.a.nodes[int(lhs_id)].typ = array_type
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
			t.a.nodes[int(sizeof_id)].value = elem_type
		}
	}
	t.a.nodes[int(call_id)].typ = array_type
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
		t.a.nodes[i].typ = typ
		t.a.nodes[int(lhs_id)].typ = typ
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

	mut children := []flat.NodeId{cap: int(node.children_count) + 1}
	children << t.make_ident(spec_name)
	if t.call_is_selector_form(node) {
		// Receiver is embedded in the selector callee; explicit args are children 1..n.
		if recv_id := t.generic_call_receiver_id(node) {
			children << recv_id
		}
		for i in 1 .. node.children_count {
			children << t.a.child(&node, i)
		}
	} else {
		// Ident-lowered form: receiver and args are already explicit children 1..n.
		for i in 1 .. node.children_count {
			children << t.a.child(&node, i)
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	t.a.nodes[int(id)] = flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          ''
		typ:            ret_typ
	}
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
		t.a.nodes[int(decl.id)] = flat.Node{
			kind: .empty
			pos:  decl.node.pos
		}
	}
}

fn (mut t Transformer) unregister_generic_fn_signature(decl GenericFnDecl) {
	if isnil(t.tc) {
		return
	}
	mut names := [decl.node.value, decl.key, c_name(decl.node.value),
		c_name(decl.key)]
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
	if args := t.specialized_plain_generic_call_args(node, decl, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	if t.call_has_source_generic_args(node) {
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
	if args := t.explicit_generic_call_args(node, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	return none
}

fn (mut t Transformer) infer_generic_call_args_from_receiver_record(decl GenericFnDecl, node flat.Node, call_module string) ?[]string {
	if !t.generic_decl_is_receiver_method(decl.node) || node.children_count == 0 {
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
	return decl_key in ['json.decode', 'json2.decode', 'x.json2.decode', 'json.encode',
		'json2.encode', 'x.json2.encode', 'veb.run_at', 'veb.run_new']
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
			key := t.generic_resolved_call_decl_key(resolved, callee, module_name, decls) or {
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
		if t.local_concrete_fn_shadows_generic(callee.value, module_name, decls) {
			return none
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
		mut base_type := t.node_type(base_id)
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
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					continue
				}
				return key
			}
		}
	}
	return none
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
	if callee.kind != .ident || !callee.value.contains('_T_') {
		return none
	}
	suffix := callee.value.all_after('_T_')
	if suffix.len == 0 {
		return none
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
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

fn (t &Transformer) generic_resolved_call_decl_key(resolved string, callee flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	key := generic_fn_decl_base_value(resolved)
	if key in decls {
		return key
	}
	if flat_key := t.generic_flat_receiver_call_decl_key(key, module_name, decls) {
		return flat_key
	}
	if flat_key := t.generic_flat_receiver_call_decl_key(resolved, module_name, decls) {
		return flat_key
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
	is_receiver := decl.node.value.contains('.')
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
	if name in decls || qname in decls {
		return false
	}
	if qname in t.fn_ret_types {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return qname in t.tc.fn_param_types || qname in t.tc.fn_ret_types
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
		arg_type := t.generic_call_arg_type_for_inference(arg_id)
		if arg_type.len > 0 {
			infer_generic_type_args(child.typ, arg_type, mut inferred)
			if is_recv_param {
				t.infer_generic_receiver_suffix_args(child.typ, arg_type, mut inferred)
				t.infer_generic_embedded_receiver_args(child.typ, arg_type, mut inferred)
			}
		}
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

fn (t &Transformer) generic_arg_for_call_and_decl_module(arg string, call_module string, decl_module string) string {
	call_normalized := t.normalize_type_in_module(arg, call_module)
	return t.generic_arg_for_decl_module(call_normalized, decl_module)
}

fn (t &Transformer) generic_arg_for_decl_module(arg string, module_name string) string {
	if t.generic_arg_is_alias_name(arg, module_name) {
		return t.qualify_generic_arg_for_decl_module(arg, module_name)
	}
	normalized := t.normalize_type_in_module(arg, module_name)
	qualified := t.qualify_generic_arg_for_decl_module(normalized, module_name)
	if qualified != normalized {
		return qualified
	}
	if module_name.len > 0 {
		stripped := strip_decl_module_from_generic_arg(normalized, module_name)
		if stripped != normalized {
			return stripped
		}
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
		if clean.starts_with('Array_') {
			return '${module_name}.${clean}'
		}
		return clean
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
				return typ
			}
		}
		.array_literal {
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
			if node.children_count > 0 {
				base_type := t.normalize_type_alias(t.node_type(t.a.child(&node, 0)))
				if base_type.starts_with('[]') {
					return base_type[2..]
				}
				if base_type.starts_with('map[') {
					bracket_end := generic_matching_bracket(base_type, 3)
					if bracket_end < base_type.len {
						return base_type[bracket_end + 1..]
					}
				}
				if base_type.starts_with('[') {
					bracket_end := generic_matching_bracket(base_type, 0)
					if bracket_end < base_type.len {
						return base_type[bracket_end + 1..]
					}
				}
			}
		}
		else {}
	}

	return t.node_type(id)
}

fn (mut t Transformer) generic_call_arg_type_for_inference(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
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
	if node.kind in [.array_literal, .fn_literal, .lambda_expr] {
		typ := t.generic_arg_expr_type(id)
		if typ.len > 0 {
			return typ
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
		infer_generic_type_args(param[1..], arg.trim_left('&'), mut inferred)
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
	if node.kind == .typeof_expr {
		if typ := generic_type_name_from_marker(node.value) {
			idx := t.active_generic_param_index(typ)
			if idx < args.len {
				return t.make_string_literal(generic_type_name_display(args[idx]))
			}
		}
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && is_generic_fn_placeholder_name(base.value) {
			idx := t.active_generic_param_index(base.value)
			if idx < args.len {
				return t.make_string_literal(generic_type_name_display(args[idx]))
			}
		}
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.clone_generic_node(t.a.child(&node, i), args)
	}
	cloned_typ := t.resolve_substituted_type_text(t.subst_type(node.typ, args))
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
				t.a.nodes[int(callee_id)].value = t.subst_type(type_text, args)
			}
		}
	}
	cloned_value := if is_root {
		specialized_generic_fn_value(node.value, args)
	} else {
		t.subst_node_value(node, args)
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		children_start: start
		children_count: flat.child_count(children.len)
		typ:            final_typ
		value:          cloned_value
		is_mut:         node.is_mut
	})
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
	if typ.starts_with('fn(') {
		return 'fn ' + typ[2..]
	}
	return typ
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
	hash_fn, eq_fn, clone_fn, free_fn := map_callback_names(key_type)
	children[3] = t.make_ident(hash_fn)
	children[4] = t.make_ident(eq_fn)
	children[5] = t.make_ident(clone_fn)
	children[6] = t.make_ident(free_fn)
}

fn (mut t Transformer) retarget_cloned_generic_call(node flat.Node, mut children []flat.NodeId, args []string) {
	if node.kind != .call || children.len == 0 || t.skip_generics {
		return
	}
	if node.children_count > 0 {
		c0 := t.a.child_node(&node, 0)
		if c0.kind == .ident && c0.value.contains('captures_iter_at') {
			eprintln('DBG retarget start node=${c0.value} typ=${node.typ} value=${node.value} active=${t.active_generic_params} args=${args}')
		}
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return
	}
	decl_key := t.generic_call_decl_key(flat.empty_node, node, t.cur_module, decls) or {
		if node.children_count > 0 {
			c0 := t.a.child_node(&node, 0)
			if c0.kind == .ident && c0.value.contains('captures_iter_at') {
				eprintln('DBG retarget no decl')
			}
		}
		return
	}
	decl := decls[decl_key] or { return }
	if node.children_count > 0 {
		c0 := t.a.child_node(&node, 0)
		if c0.kind == .ident && c0.value.contains('captures_iter_at') {
			eprintln('DBG retarget decl=${decl_key} params=${t.generic_fn_param_names(decl.node,
				decl.module)}')
		}
	}
	if t.should_skip_generic_call_specialization(decl_key)
		|| t.generic_decl_is_receiver_method(decl.node) {
		return
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return
	}
	mut call_args := []string{}
	if explicit := t.explicit_generic_call_args(node, t.cur_module) {
		for arg in explicit {
			call_args << t.subst_type(arg, args)
		}
	} else {
		callee_id := children[0]
		if int(callee_id) < 0 || int(callee_id) >= t.a.nodes.len {
			return
		}
		callee := t.a.nodes[int(callee_id)]
		if callee.kind != .ident || callee.value.contains('[') {
			return
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
			arg_type := t.generic_call_arg_type_for_inference(children[arg_pos])
			if arg_type.len > 0 {
				infer_generic_type_args(child.typ, arg_type, mut inferred)
			}
			param_idx++
		}
		for name in param_names {
			arg := inferred[name] or {
				idx := t.active_generic_param_index(name)
				if idx < 0 || idx >= args.len {
					if node.children_count > 0 {
						c0 := t.a.child_node(&node, 0)
						if c0.kind == .ident && c0.value.contains('captures_iter_at') {
							eprintln('DBG retarget missing ${name} idx=${idx}')
						}
					}
					return
				}
				args[idx]
			}
			call_args << t.generic_arg_for_decl_module(arg, decl.module)
		}
	}
	if call_args.len == 0 || t.generic_args_have_placeholders(call_args) {
		return
	}
	spec_value := specialized_generic_fn_value(decl.node.value, call_args)
	if node.children_count > 0 {
		c0 := t.a.child_node(&node, 0)
		if c0.kind == .ident && c0.value.contains('captures_iter_at') {
			eprintln('DBG retarget spec ${c0.value} -> ${spec_value} call_args=${call_args}')
		}
	}
	children[0] = t.make_ident(transform_qualified_fn_name(decl.module, spec_value))
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
	if src_idx < t.tc.resolved_call_set.len && t.tc.resolved_call_set[src_idx] {
		src_name := t.tc.resolved_call_names[src_idx]
		if src_name.contains('captures_iter_at') {
			eprintln('DBG copy resolution src=${src_name} is_gen=${t.resolved_call_is_generic_fn(src_name)}')
		}
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

fn (t &Transformer) resolved_call_is_generic_fn(name string) bool {
	if name.len == 0 || isnil(t.tc) {
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
	return node.value.contains('.')
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
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return '', []string{}, false
	}
	bracket := typ.index_u8(`[`)
	if bracket <= 0 {
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
		.array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .sizeof_expr,
		.typeof_expr, .is_expr {
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
	parsed := t.tc.parse_type(clean)
	if parsed is types.Unknown {
		return typ
	}
	return parsed.name()
}

fn (t &Transformer) subst_comptime_type_condition(cond string, args []string) string {
	clean := cond.trim_space()
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
			return '${t.subst_type(left, args)}${op}${t.subst_type(right, args)}'
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
	// Function-type args (`fn (A, B) R`) and other compound types cannot appear
	// verbatim in a C identifier and must not be naively shortened by the last
	// `.` (which would truncate `fn(...) mod.R` to `R)`). Reduce them to a
	// deterministic sanitized fragment instead.
	if clean.contains('(') || clean.contains(' ') {
		return sanitize_type_name_fragment(clean)
	}
	if clean.contains('.') {
		short := clean.all_after_last('.')
		if short.starts_with('Array_') {
			return clean
		}
		return short
	}
	return clean
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
	recorded_args := canonical_generic_specialization_args(args)
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

fn canonical_generic_specialization_args(args []string) []string {
	mut out := []string{cap: args.len}
	for arg in args {
		out << canonical_generic_specialization_arg(arg)
	}
	return out
}

fn canonical_generic_specialization_arg(arg string) string {
	clean := arg.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + canonical_generic_specialization_arg(clean[1..])
	}
	if clean.starts_with('mut ') {
		return 'mut ' + canonical_generic_specialization_arg(clean[4..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + canonical_generic_specialization_arg(clean[1..])
	}
	if clean.starts_with('...') {
		return '...' + canonical_generic_specialization_arg(clean[3..])
	}
	if clean.starts_with('[]') {
		return '[]' + canonical_generic_specialization_arg(clean[2..])
	}
	if decoded_array := generic_array_type_arg_from_suffix(clean) {
		return decoded_array
	}
	return clean
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
	for arg in args {
		if t.generic_arg_is_unresolved(arg) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_arg_is_unresolved(arg string) bool {
	clean := arg.trim_space()
	if clean.len == 0 || clean in ['unknown', 'void', 'generic'] {
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
