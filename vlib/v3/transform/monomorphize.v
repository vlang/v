module transform

import v3.flat
import v3.types

struct GenericStructDecl {
	id     flat.NodeId
	node   flat.Node
	module string
	key    string
}

fn (mut t Transformer) is_generic_fn(name string) bool {
	decls := t.collect_generic_fn_decls()
	return name in decls || transform_qualified_fn_name(t.cur_module, name) in decls
}

fn (mut t Transformer) is_generic_struct(name string) bool {
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
	decls := t.collect_generic_fn_decls()
	if decls.len == 0 {
		return []string{}
	}
	struct_decls := t.collect_generic_struct_decls()
	template_nodes := t.generic_decl_template_nodes(decls)
	mut emitted := map[string]bool{}
	mut generated := []string{}
	mut changed := true
	for changed {
		changed = false
		node_modules := t.node_module_map()
		mut cur_module := ''
		node_count := t.a.nodes.len
		for i in 0 .. node_count {
			if template_nodes[i] {
				continue
			}
			node := t.a.nodes[i]
			match node.kind {
				.module_decl {
					cur_module = node.value
				}
				.call {
					// An infix operator on a generic instance was already lowered to a
					// direct call (`Vec_int__plus(a, b)`) before this pass. Record the
					// callee so `specialize_generic_struct_methods` emits an operator
					// overload only for instances whose operator is actually called.
					t.record_called_fn_name(node)
					call_module := node_modules[i] or { cur_module }
					decl_key, args := t.generic_call_specialization(flat.NodeId(i), node,
						call_module, decls) or { continue }
					spec_key := generic_fn_spec_key(decl_key, args)
					if emitted[spec_key] {
						continue
					}
					decl := decls[decl_key] or { continue }
					generated << t.generated_fn_used_names(decl,
						t.emit_generic_fn_specialization(decl, args), args)
					emitted[spec_key] = true
					changed = true
				}
				else {}
			}
		}
		// Specialize methods of instantiated generic structs that are never reached
		// through an explicit call node — notably operator overloads (`a + b`), which
		// are infix expressions lowered to method calls only later in the pipeline.
		if t.specialize_generic_struct_methods(struct_decls, decls, mut emitted, mut generated) {
			changed = true
		}
	}
	t.rewrite_generic_calls(decls, template_nodes)
	t.erase_generic_fn_decls(decls)
	return generated
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
			} else {
				mvkey := '${spec}.${method}'
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
			if decl.node.generic_params.len != 0 {
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

fn (mut t Transformer) materialize_generic_structs() {
	if isnil(t.tc) {
		return
	}
	decls := t.collect_generic_struct_decls()
	if decls.len == 0 {
		return
	}
	mut specs := map[string]string{}
	t.collect_generic_struct_specs(decls, mut specs)
	for _ in 0 .. 20 {
		before := specs.len
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
				t.collect_generic_struct_spec_from_type(field_type, decl.module, decls, mut specs)
			}
		}
		if specs.len == before {
			break
		}
	}
	for spec, base in specs {
		decl := decls[base] or { continue }
		t.materialize_generic_struct_spec(spec, decl)
	}
	for _, decl in decls {
		t.tc.structs.delete(decl.key)
		t.tc.unions.delete(decl.key)
		t.tc.params_structs.delete(decl.key)
	}
}

fn (mut t Transformer) collect_generic_struct_decls() map[string]GenericStructDecl {
	mut decls := map[string]GenericStructDecl{}
	node_modules := t.node_module_map()
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				if node.generic_params.len == 0 && !node.typ.contains('generic') {
					continue
				}
				module_name := node_modules[i] or { cur_module }
				key := generic_struct_decl_key(node.value, module_name)
				decls[key] = GenericStructDecl{
					id:     flat.NodeId(i)
					node:   node
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

fn (mut t Transformer) collect_generic_struct_specs(decls map[string]GenericStructDecl, mut specs map[string]string) {
	for _, target in t.tc.type_aliases {
		t.collect_generic_struct_spec_from_type(target, '', decls, mut specs)
	}
	mut module_name := ''
	for node in t.a.nodes {
		match node.kind {
			.file {
				module_name = ''
			}
			.module_decl {
				module_name = node.value
			}
			else {}
		}

		if node.typ.len > 0 {
			t.collect_generic_struct_spec_from_type(node.typ, module_name, decls, mut specs)
		}
		match node.kind {
			.struct_init, .array_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr, .is_expr {
				t.collect_generic_struct_spec_from_type(node.value, module_name, decls, mut specs)
			}
			else {}
		}
	}
}

fn (mut t Transformer) collect_generic_struct_spec_from_type(typ string, module_name string, decls map[string]GenericStructDecl, mut specs map[string]string) {
	clean := typ.trim_space()
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_struct_spec_from_type(clean[1..], module_name, decls, mut specs)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_struct_spec_from_type(clean[4..], module_name, decls, mut specs)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_struct_spec_from_type(clean[1..], module_name, decls, mut specs)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_struct_spec_from_type(clean[3..], module_name, decls, mut specs)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_struct_spec_from_type(clean[2..], module_name, decls, mut specs)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_struct_spec_from_type(clean[4..bracket_end], module_name, decls, mut
				specs)
			t.collect_generic_struct_spec_from_type(clean[bracket_end + 1..], module_name, decls, mut
				specs)
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			t.collect_generic_struct_spec_from_type(clean[bracket_end + 1..], module_name, decls, mut
				specs)
		}
		return
	}
	base, args, ok := generic_app_parts(clean)
	if !ok {
		return
	}
	for arg in args {
		t.collect_generic_struct_spec_from_type(arg, module_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(args) {
		return
	}
	spec_base := generic_struct_spec_base_name(base, module_name, decls) or { return }
	spec_name := '${spec_base}[${args.join(', ')}]'
	specs[spec_name] = spec_base
}

fn generic_struct_spec_base_name(base string, module_name string, decls map[string]GenericStructDecl) ?string {
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

fn (mut t Transformer) materialize_generic_struct_spec(spec_name string, decl GenericStructDecl) {
	_, args, ok := generic_app_parts(spec_name)
	if !ok || args.len == 0 {
		return
	}
	old_module := t.cur_module
	old_tc_module := t.tc.cur_module
	t.cur_module = decl.module
	t.tc.cur_module = decl.module
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
	t.tc.cur_module = old_tc_module
}

fn (mut t Transformer) collect_generic_fn_decls() map[string]GenericFnDecl {
	mut decls := map[string]GenericFnDecl{}
	node_modules := t.node_module_map()
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				fn_module := node_modules[i] or { cur_module }
				if !t.fn_decl_has_unresolved_generics(node, fn_module) {
					continue
				}
				key := t.generic_fn_decl_key(node, fn_module)
				decls[key] = GenericFnDecl{
					id:     flat.NodeId(i)
					node:   node
					module: fn_module
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn (mut t Transformer) generic_decl_template_nodes(decls map[string]GenericFnDecl) map[int]bool {
	mut nodes := map[int]bool{}
	for _, decl in decls {
		t.collect_node_subtree_ids(decl.id, mut nodes)
	}
	return nodes
}

fn (mut t Transformer) node_module_map() map[int]string {
	mut modules := map[int]string{}
	mut cur_module := ''
	for i, node in t.a.nodes {
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.fn_decl, .const_decl, .global_decl, .struct_decl, .type_decl, .enum_decl,
			.interface_decl {
				t.mark_node_module(flat.NodeId(i), cur_module, mut modules)
			}
			else {}
		}
	}
	return modules
}

fn (mut t Transformer) mark_node_module(id flat.NodeId, module_name string, mut modules map[int]string) {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len || idx in modules {
		return
	}
	modules[idx] = module_name
	node := t.a.nodes[idx]
	for i in 0 .. node.children_count {
		t.mark_node_module(t.a.child(&node, i), module_name, mut modules)
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

fn (mut t Transformer) emit_generic_fn_specialization(decl GenericFnDecl, args []string) flat.NodeId {
	if decl.module.len > 0 {
		t.a.add_node(flat.Node{
			kind:  .module_decl
			value: decl.module
		})
	}
	old_params := t.active_generic_params
	t.active_generic_params = t.generic_fn_param_names(decl.node, decl.module)
	clone_id := t.clone_generic_fn_node(decl.node, args)
	clone := t.a.nodes[int(clone_id)]
	t.register_specialized_fn_signature(decl, clone, args)
	t.active_generic_params = old_params
	t.transform_specialized_fn_body(clone_id, decl.module)
	return clone_id
}

fn (mut t Transformer) transform_specialized_fn_body(clone_id flat.NodeId, module_name string) {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return
	}
	old_module := t.cur_module
	old_fn_name := t.cur_fn_name
	old_ret_type := t.cur_fn_ret_type
	old_var_types := t.var_types.clone()
	t.cur_module = module_name
	t.transform_fn_body(int(clone_id))
	t.cur_module = old_module
	t.cur_fn_name = old_fn_name
	t.cur_fn_ret_type = old_ret_type
	t.var_types = old_var_types
}

fn (mut t Transformer) generated_fn_used_names(decl GenericFnDecl, clone_id flat.NodeId, args []string) []string {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return []string{}
	}
	clone := t.a.nodes[int(clone_id)]
	qname := transform_qualified_fn_name(decl.module, clone.value)
	mut names := [clone.value, qname, c_name(clone.value), c_name(qname)]
	names << specialized_generic_fn_signature_aliases(decl, args)
	return names
}

fn (mut t Transformer) register_specialized_fn_signature(decl GenericFnDecl, clone flat.Node, args []string) {
	old_module := t.cur_module
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	t.cur_module = decl.module
	if !isnil(t.tc) {
		t.tc.cur_module = decl.module
	}
	ret_name := t.subst_type(decl.node.typ, args)
	ret := if !isnil(t.tc) { t.tc.parse_type(ret_name) } else { types.Type(types.void_) }
	mut params := []types.Type{}
	mut variadic := false
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		param_type := t.subst_type(child.typ, args)
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
	if !isnil(t.tc) {
		mut names := [clone.value, qname, c_name(clone.value),
			c_name(qname)]
		names << specialized_generic_fn_signature_aliases(decl, args)
		for name in names {
			t.tc.fn_ret_types[name] = ret
			t.tc.fn_param_types[name] = params.clone()
			t.tc.fn_variadic[name] = variadic
		}
		t.tc.cur_module = old_tc_module
	}
	t.cur_module = old_module
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
	]
}

fn (mut t Transformer) rewrite_generic_calls(decls map[string]GenericFnDecl, template_nodes map[int]bool) {
	node_modules := t.node_module_map()
	mut cur_module := ''
	for i in 0 .. t.a.nodes.len {
		if template_nodes[i] {
			continue
		}
		node := t.a.nodes[i]
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.call {
				call_module := node_modules[i] or { cur_module }
				decl_key, args := t.generic_call_specialization(flat.NodeId(i), node, call_module,
					decls) or { continue }
				decl := decls[decl_key] or { continue }
				if t.generic_decl_is_receiver_method(decl.node) {
					t.rewrite_generic_method_call(flat.NodeId(i), node, decl, args)
				} else {
					t.rewrite_generic_plain_call(flat.NodeId(i), node, decl, args)
				}
			}
			else {}
		}
	}
}

fn (mut t Transformer) rewrite_generic_plain_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args []string) {
	spec_value := specialized_generic_fn_value(decl.node.value, args)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << t.make_ident(spec_name)
	for i in 1 .. node.children_count {
		children << t.a.child(&node, i)
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
		typ:            substitute_generic_type_text(decl.node.typ, args)
	}
	t.clear_resolved_call(id)
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
	// Method-level generics (`method[U](...)`) cannot be resolved from the receiver
	// type alone — the receiver carries no `_U` suffix — so the call must name the
	// specialized function explicitly, passing the receiver as the first argument
	// (exactly like a plain generic call). Pure struct-generic methods keep the
	// selector callee, which cgen resolves via the already-monomorphized receiver.
	if decl.node.generic_params.len > 0 {
		t.rewrite_method_level_generic_call(id, node, decl, args)
		return
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .index && fn_node.kind != .selector {
		return
	}
	mut callee_id := fn_id
	if fn_node.kind == .index && fn_node.children_count > 0 {
		callee_id = t.a.child(&fn_node, 0)
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << callee_id
	for i in 1 .. node.children_count {
		children << t.a.child(&node, i)
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
		typ:            substitute_generic_type_text(decl.node.typ, args)
	}
	t.clear_resolved_call(id)
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
	ret_typ := t.subst_type(decl.node.typ, args)
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

fn (mut t Transformer) generic_call_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []string) {
	if node.children_count == 0 {
		return none
	}
	decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
	decl := decls[decl_key] or { return none }
	if t.should_skip_generic_call_specialization(decl_key) {
		return none
	}
	if args := t.explicit_generic_call_args(node, module_name) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	if args := t.infer_generic_call_args(decl, id, node) {
		if args.len > 0 && !t.generic_args_have_placeholders(args) {
			return decl_key, args
		}
	}
	return none
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
		for key, _ in decls {
			if key.all_after_last('.') == callee.value {
				decl := decls[key]
				if t.generic_decl_is_receiver_method(decl.node) {
					if !t.generic_call_arg_count_matches_decl(node, decl) {
						continue
					}
					return key
				}
			}
		}
	}
	return none
}

fn (t &Transformer) generic_resolved_call_decl_key(resolved string, callee flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	key := generic_fn_decl_base_value(resolved)
	if key in decls {
		return key
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
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		param_count++
		if child.typ.starts_with('...') {
			is_variadic = true
		}
	}
	// param_count includes the receiver for methods. The call's child count
	// depends on form: the selector form (`recv.method(args)`) keeps the receiver
	// inside the callee child, so children = [callee, args...] and the callee child
	// offsets the receiver param (actual == children_count). The ident-lowered form
	// (`Type.method(recv, args)`) carries the receiver as a real child, so
	// children = [callee, recv, args...] and actual == children_count - 1.
	is_receiver := decl.node.value.contains('.')
	actual := if is_receiver && t.call_is_selector_form(node) {
		int(node.children_count)
	} else {
		int(node.children_count) - 1
	}
	if is_variadic {
		return actual >= param_count - 1
	}
	return actual == param_count
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
	type_arg := t.generic_call_type_arg_name(t.a.child(fn_node, 1))
	if type_arg.len == 0 {
		return none
	}
	return normalize_generic_args(split_generic_args(type_arg), module_name)
}

fn (mut t Transformer) infer_generic_call_args(decl GenericFnDecl, _id flat.NodeId, node flat.Node) ?[]string {
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return none
	}
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
	// Determine the call form. The selector form (`recv.method(args)`) embeds the
	// receiver in the callee, so explicit args begin at child index 1. The
	// ident-lowered form (`Type__method(recv, args)`) passes the receiver as child
	// 1, so explicit args begin at child index 2. Using the wrong offset misaligns
	// every explicit param and makes method-level generic inference silently fail.
	mut selector_form := false
	if is_receiver {
		mut callee := t.a.nodes[int(t.a.child(&node, 0))]
		if callee.kind == .index && callee.children_count > 0 {
			callee = t.a.nodes[int(t.a.child(&callee, 0))]
		}
		selector_form = callee.kind == .selector
	}
	mut inferred := map[string]string{}
	mut param_idx := 0
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			continue
		}
		is_recv_param := is_receiver && param_idx == 0
		arg_id := if is_recv_param {
			if selector_form {
				t.generic_call_receiver_id(node) or {
					param_idx++
					continue
				}
			} else {
				if int(node.children_count) <= 1 {
					param_idx++
					continue
				}
				t.a.child(&node, 1)
			}
		} else {
			arg_idx := if selector_form { param_idx } else { param_idx + 1 }
			if arg_idx >= int(node.children_count) {
				param_idx++
				continue
			}
			t.a.child(&node, arg_idx)
		}
		mut arg_type := t.node_type(arg_id)
		if arg_type.len == 0 {
			arg_type = t.generic_arg_expr_type(arg_id)
		}
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
		args << t.generic_arg_for_decl_module(arg, decl.module)
	}
	return args
}

fn (t &Transformer) generic_arg_for_decl_module(arg string, module_name string) string {
	normalized := t.normalize_type_in_module(arg, module_name)
	qualified := t.qualify_generic_arg_for_decl_module(normalized, module_name)
	if qualified != normalized {
		return qualified
	}
	if module_name.len > 0 && normalized.starts_with('${module_name}.') {
		return normalized.all_after_last('.')
	}
	return normalized
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

	return ''
}

fn (t &Transformer) infer_generic_receiver_suffix_args(param_type string, arg_type string, mut inferred map[string]string) {
	param := param_type.trim_space().trim_left('&')
	arg := arg_type.trim_space().trim_left('&')
	base, param_args, ok := generic_app_parts(param)
	if !ok || param_args.len == 0 || arg.len == 0 {
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
	for i, param_arg in param_args {
		if !is_generic_fn_placeholder_name(param_arg) || param_arg in inferred {
			continue
		}
		decoded := generic_type_arg_from_suffix(suffix)
		if decoded.len == 0 {
			continue
		}
		if i == 0 {
			inferred[param_arg] = decoded
		}
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

fn (mut t Transformer) clone_generic_fn_node(node flat.Node, args []string) flat.NodeId {
	return t.clone_generic_node_from(node, args, true)
}

fn (mut t Transformer) clone_generic_node(id flat.NodeId, args []string) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	return t.clone_generic_node_from(node, args, false)
}

fn (mut t Transformer) clone_generic_node_from(node flat.Node, args []string, is_root bool) flat.NodeId {
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && is_generic_fn_placeholder_name(base.value) {
			idx := t.active_generic_param_index(base.value)
			if idx < args.len {
				return t.make_string_literal(args[idx])
			}
		}
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.clone_generic_node(t.a.child(&node, i), args)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
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
		typ:            t.subst_type(node.typ, args)
		value:          cloned_value
	})
}

fn substitute_generic_node_value(node flat.Node, args []string) string {
	match node.kind {
		.call, .array_init, .struct_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr,
		.is_expr, .type_decl, .field_decl, .param {
			return substitute_generic_type_text(node.value, args)
		}
		else {
			return node.value
		}
	}
}

fn (mut t Transformer) fn_decl_has_unresolved_generics(node flat.Node, module_name string) bool {
	if node.generic_params.len > 0 {
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
				fn_parts << substitute_generic_type_text_with_params(params_str[start..i], args,
					params)
				start = i + 1
			}
		}
		fn_parts << substitute_generic_type_text_with_params(params_str[start..], args, params)
	}
	ret_str := clean[params_end + 1..].trim_space()
	if ret_str.len > 0 {
		return 'fn(${fn_parts.join(', ')}) ${substitute_generic_type_text_with_params(ret_str,
			args, params)}'
	}
	return 'fn(${fn_parts.join(', ')})'
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
		.call, .array_init, .struct_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr,
		.is_expr, .type_decl, .field_decl, .param {
			return t.subst_type(node.value, args)
		}
		else {
			return node.value
		}
	}
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
		return clean.all_after_last('.')
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

fn generic_type_arg_from_suffix(suffix string) string {
	if suffix.len == 0 {
		return ''
	}
	return match suffix {
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
		else { suffix.replace('__', '.') }
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
