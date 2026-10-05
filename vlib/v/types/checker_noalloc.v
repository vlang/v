module types

import v.flat

struct NoAllocFunction {
	id     flat.NodeId
	module string
	name   string
}

struct NoAllocScan {
mut:
	tc        &TypeChecker = unsafe { nil }
	functions map[string]NoAllocFunction
	root      NoAllocFunction
	strict    bool
	lowered   bool
	visited   map[int]bool
	reported  map[string]bool
}

// has_noalloc_contracts reports whether allocation contracts need validation.
// Foreign declarations alone do not require walking the program.
pub fn (tc &TypeChecker) has_noalloc_contracts() bool {
	for index, attributes in tc.declaration_attributes {
		if !tc.declaration_has_attribute(flat.NodeId(index), 'noalloc') {
			continue
		}
		node := tc.a.nodes[index]
		if node.kind !in [.fn_decl, .c_fn_decl] || !noalloc_foreign(node) {
			return true
		}
		for attribute in attributes {
			if attribute.all_before(':').trim_space().starts_with('noalloc_') {
				return true
			}
			if attribute.all_before(':').trim_space() == 'noalloc' && attribute.contains(':')
				&& attribute.all_after(':').trim_space().trim('\'"') != 'strict' {
				return true
			}
		}
	}
	return false
}

// check_noalloc_contracts checks every annotated V function and its reachable
// callees. Source checks cover operations that lowering erases; the final pass
// also sees synthesized allocation sites. Unknown operations fail conservatively.
pub fn (mut tc TypeChecker) check_noalloc_contracts(lowered bool, unsupported_modes string) {
	if !lowered {
		tc.noalloc_terminal_functions.clear()
	}
	if !tc.has_noalloc_contracts() {
		return
	}
	mut roots := map[string]bool{}
	mut strict_roots := map[string]bool{}
	for index, attributes in tc.declaration_attributes {
		if !tc.declaration_has_attribute(flat.NodeId(index), 'noalloc') {
			continue
		}
		node := tc.a.nodes[index]
		if node.kind !in [.fn_decl, .c_fn_decl] && !(lowered && node.kind == .empty) {
			tc.record_transform_error(flat.NodeId(index), node.pos,
				'@[noalloc] is only supported on V and C function declarations')
			continue
		}
		for attribute in attributes {
			attribute_name := attribute.all_before(':').trim_space()
			if attribute_name.starts_with('noalloc_') {
				tc.record_transform_error(flat.NodeId(index), node.pos,
					'@[noalloc] accepts at most one positional argument, `strict`; named arguments are unsupported')
				continue
			}
			if attribute_name != 'noalloc' {
				continue
			}
			mode := attribute.all_after(':').trim_space().trim('\'"')
			if attribute.contains(':') && mode != 'strict' {
				tc.record_transform_error(flat.NodeId(index), node.pos,
					'@[noalloc] accepts only the `strict` argument')
			}
			if mode == 'strict' {
				strict_roots[noalloc_position_key(node)] = true
			}
		}
		if !noalloc_foreign(node) {
			roots[noalloc_position_key(node)] = true
		}
	}
	mut scan := NoAllocScan{
		tc:      &tc
		lowered: lowered
	}
	mut declarations := []NoAllocFunction{}
	for index, node in tc.a.nodes {
		if node.kind !in [.fn_decl, .c_fn_decl] {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		raw_module := tc.a.specialized_fn_modules[index] or {
			tc.file_modules[file.name] or { 'main' }
		}
		module_name := if raw_module == '' { 'main' } else { raw_module }
		name := if noalloc_foreign(node) {
			if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
		} else {
			checker_qualified_fn_name(module_name, node.value)
		}
		decl := NoAllocFunction{
			id:     flat.NodeId(index)
			module: module_name
			name:   name
		}
		declarations << decl
		scan.functions[name] = decl
		scan.functions['${module_name}\x01${node.value}'] = decl
		if module_name == 'builtin' || noalloc_foreign(node) {
			scan.functions[node.value] = decl
		}
	}
	for decl in declarations {
		node := tc.a.node(decl.id)
		if !roots[noalloc_position_key(*node)] || node.generic_params().len > 0 {
			continue
		}
		scan.root = decl
		scan.strict = strict_roots[noalloc_position_key(*node)]
		scan.visited.clear()
		if unsupported_modes != '' {
			scan.report(decl.id, [decl.name], flat.empty_node,
				'compiler-generated ${unsupported_modes} operations cannot be proven nonallocating')
			continue
		}
		scan.walk_function(decl, [decl.name], flat.empty_node)
	}
}

fn noalloc_position_key(node flat.Node) string {
	return '${node.pos.id}:${node.pos.offset}:${node.pos.end}'
}

fn noalloc_foreign(node flat.Node) bool {
	return node.kind == .c_fn_decl || node.value.starts_with('C.')
}

fn (mut s NoAllocScan) report(id flat.NodeId, chain []string, anchor flat.NodeId, reason string) {
	if !s.tc.valid_node_id(id) {
		return
	}
	key := '${s.root.id}:${id}:${reason}'
	if s.reported[key] {
		return
	}
	s.reported[key] = true
	position := s.tc.node_position_string(id)
	message := '@[noalloc] `${s.root.name}` may allocate: ${chain.join(' -> ')}: ${reason} (${position})'
	at := if s.tc.valid_node_id(anchor) { anchor } else { id }
	s.tc.record_transform_error(at, s.tc.a.node(at).pos, message)
}

fn (s &NoAllocScan) function(name string, module_name string) ?NoAllocFunction {
	if found := s.functions[name] {
		return found
	}
	if found := s.functions['${module_name}\x01${name}'] {
		return found
	}
	if !s.lowered && name.contains('[') {
		base := name.all_before('[')
		if found := s.functions[base] {
			if s.tc.a.node(found.id).generic_params().len > 0 {
				return found
			}
		}
		if found := s.functions['${module_name}\x01${base}'] {
			if s.tc.a.node(found.id).generic_params().len > 0 {
				return found
			}
		}
	}
	return none
}

fn (mut s NoAllocScan) walk_function(decl NoAllocFunction, chain []string, anchor flat.NodeId) {
	if s.visited[int(decl.id)] {
		return
	}
	s.visited[int(decl.id)] = true
	node := s.tc.a.node(decl.id)
	if node.generic_params().len > 0 {
		if s.lowered {
			s.report(decl.id, chain, anchor, 'generic body has not been specialized')
		}
		return
	}
	old_module := s.tc.cur_module
	old_file := s.tc.cur_file
	s.tc.cur_module = decl.module
	if file := s.tc.a.source_files[node.pos.id] {
		s.tc.cur_file = file.name
	}
	defer {
		s.tc.cur_module = old_module
		s.tc.cur_file = old_file
	}
	if noalloc_foreign(*node) {
		if !s.tc.declaration_has_attribute(decl.id, 'noalloc') {
			s.report(decl.id, chain, anchor, 'C function has no @[noalloc] contract')
		}
		return
	}
	if node.is_mut || s.tc.cur_file.ends_with('.vh') {
		s.report(decl.id, chain, anchor, 'function body is unavailable')
		return
	}
	mut parameters := map[string]string{}
	mut mutable_arrays := map[string]bool{}
	for i in 0 .. node.children_count {
		child := s.tc.a.child_node(node, i)
		if child.kind == .param {
			parameters[child.value] = child.typ
			if child.is_mut && child.typ.trim_left('&').starts_with('[]') {
				mutable_arrays[child.value] = true
			}
		}
	}
	// Function bodies are direct children after the parameters, not block nodes.
	for i in 0 .. node.children_count {
		if s.tc.a.child_node(node, i).kind != .param {
			s.collect_locals(s.tc.a.child(node, i), mut parameters)
		}
	}
	if s.lowered && s.tc.noalloc_terminal_functions[noalloc_position_key(*node)] {
		// The source body contained only this terminating call. Every preceding
		// lowered statement therefore evaluates its exempt argument expression.
		// Generic bodies and functions with preceding statements have no proof.
		if node.children_count > 0
			&& s.terminal(s.tc.a.child(node, node.children_count - 1), decl)
			&& !s.has_control_exit(decl.id) {
			return
		}
	}
	previous_errors := s.tc.errors.len
	for i in 0 .. node.children_count {
		if s.tc.a.child_node(node, i).kind != .param {
			s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, mutable_arrays, false,
				false)
		}
	}
	if !s.lowered && s.tc.errors.len == previous_errors
		&& s.sole_terminal_body(*node, decl, parameters) {
		s.tc.noalloc_terminal_functions[noalloc_position_key(*node)] = true
	}
}

fn (s &NoAllocScan) has_control_exit(id flat.NodeId) bool {
	node := s.tc.a.node(id)
	if node.kind in [.return_stmt, .break_stmt, .continue_stmt, .goto_stmt] {
		return true
	}
	if node.kind in [.fn_literal, .lambda_expr] {
		return false
	}
	for i in 0 .. node.children_count {
		if s.has_control_exit(s.tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (s &NoAllocScan) sole_terminal_body(node flat.Node, decl NoAllocFunction,
	parameters map[string]string) bool {
	mut body := flat.empty_node
	for i in 0 .. node.children_count {
		if s.tc.a.child_node(&node, i).kind == .param {
			continue
		}
		if s.tc.valid_node_id(body) {
			return false
		}
		body = s.tc.a.child(&node, i)
	}
	if !s.tc.valid_node_id(body) || !s.terminal(body, decl) || s.has_control_exit(body) {
		return false
	}
	mut call := s.tc.a.node(body)
	for call.kind in [.expr_stmt, .block] && call.children_count == 1 {
		call = s.tc.a.child_node(call, 0)
	}
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	callee := s.tc.a.child_node(call, 0)
	if callee.kind == .ident && callee.value in parameters {
		return false
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := s.tc.a.child_node(callee, 0)
		if base.kind == .ident && base.value in parameters {
			return false
		}
	}
	return true
}

fn (s &NoAllocScan) collect_locals(id flat.NodeId, mut bindings map[string]string) {
	if !s.tc.valid_node_id(id) {
		return
	}
	node := s.tc.a.node(id)
	if node.kind == .decl_assign {
		for i := 0; i + 1 < node.children_count; i += 2 {
			lhs := s.tc.a.child_node(node, i)
			if lhs.kind == .ident {
				bindings[lhs.value] = s.type_name(s.tc.a.child(node, i + 1), bindings)
			}
		}
	}
	if node.kind == .for_in_stmt && node.children_count >= 2 {
		for i in 0 .. 2 {
			binding := s.tc.a.child_node(node, i)
			if binding.kind == .ident {
				bindings[binding.value] = s.type_name(s.tc.a.child(node, i), bindings)
			}
		}
	}
	if node.kind in [.fn_literal, .lambda_expr] {
		return
	}
	for i in 0 .. node.children_count {
		s.collect_locals(s.tc.a.child(node, i), mut bindings)
	}
}

fn (s &NoAllocScan) type_name(id flat.NodeId, parameters map[string]string) string {
	if typ := s.tc.expr_type(id) {
		return typ.name()
	}
	node := s.tc.a.node(id)
	if node.typ != '' {
		return node.typ
	}
	if node.kind == .ident {
		name := parameters[node.value] or { 'unknown' }
		return if name == '' { 'unknown' } else { name }
	}
	if node.kind == .selector && node.value in ['len', 'cap'] {
		return 'int'
	}
	if node.kind == .struct_init && node.value != '' {
		return node.value
	}
	if node.kind == .cast_expr && node.value != '' {
		return node.value
	}
	if node.kind in [.paren, .prefix, .postfix] && node.children_count > 0 {
		child_id := s.tc.a.child(node, 0)
		if node.kind == .postfix && node.op == .not
			&& s.tc.a.node(child_id).kind == .array_literal {
			literal := s.tc.a.node(child_id)
			return '[${literal.children_count}]${s.tc.array_literal_elem_type(*literal).name()}'
		}
		child_type := s.type_name(child_id, parameters)
		if node.kind == .prefix && node.op == .amp {
			return '&${child_type}'
		}
		if node.kind == .prefix && node.op == .mul {
			return child_type.trim_string_left('&')
		}
		return child_type
	}
	return match node.kind {
		.int_literal { 'int' }
		.float_literal { 'f64' }
		.bool_literal { 'bool' }
		.string_literal { 'string' }
		.char_literal { 'char' }
		else { 'unknown' }
	}
}

fn (s &NoAllocScan) terminal(id flat.NodeId, decl NoAllocFunction) bool {
	node := s.tc.a.node(id)
	if node.kind in [.block, .expr_stmt] && node.children_count > 0 {
		return s.terminal(s.tc.a.child(node, node.children_count - 1), decl)
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	name := s.tc.resolved_call_name(id) or { s.tc.call_display_name(*node) }
	if name in ['builtin.panic', 'os.exit', 'C.exit', 'C.abort'] {
		return true
	}
	if name in ['panic', 'exit', 'abort'] {
		if callee := s.function(name, decl.module) {
			return callee.module in ['builtin', 'os'] || callee.name.starts_with('C.')
		}
	}
	return false
}

fn (s &NoAllocScan) array_parameter(id flat.NodeId, mutable_arrays map[string]bool) bool {
	node := s.tc.a.node(id)
	if node.kind in [.prefix, .paren] && node.children_count > 0 {
		return s.array_parameter(s.tc.a.child(node, 0), mutable_arrays)
	}
	return node.kind == .ident && mutable_arrays[node.value]
}

fn (s &NoAllocScan) boxed_storage(typ Type) bool {
	clean := unalias_type(typ)
	return match clean {
		Interface, SumType { true }
		OptionType, ResultType { s.boxed_storage(clean.base_type) }
		Array, ArrayFixed { s.boxed_storage(clean.elem_type) }
		else { false }
	}
}

fn (s &NoAllocScan) custom_operator_storage(typ Type) bool {
	clean := unalias_type(unwrap_pointer(typ))
	return match clean {
		Unknown, Struct, Interface, SumType, Map, Channel { true }
		Array, ArrayFixed { s.custom_operator_storage(clean.elem_type) }
		OptionType, ResultType { s.custom_operator_storage(clean.base_type) }
		else { false }
	}
}

fn (s &NoAllocScan) owning_storage(typ Type, mut seen map[string]bool) bool {
	clean := unalias_type(typ)
	return match clean {
		String, Array, Map, Channel, Interface, SumType { true }
		OptionType, ResultType { s.owning_storage(clean.base_type, mut seen) }
		ArrayFixed { s.owning_storage(clean.elem_type, mut seen) }
		MultiReturn {
			mut owns := false
			for item in clean.types {
				if s.owning_storage(item, mut seen) {
					owns = true
				}
			}
			owns
		}
		Struct {
			if seen[clean.name] {
				false
			} else {
				seen[clean.name] = true
				mut owns := false
				for field in s.tc.struct_fields_for_type(clean.name) {
					if s.owning_storage(field.typ, mut seen) {
						owns = true
						break
					}
				}
				owns
			}
		}
		else { false }
	}
}

fn (s &NoAllocScan) inline_fixed_storage(typ Type, mut seen map[string]bool) bool {
	clean := unalias_type(unwrap_pointer(typ))
	return match clean {
		ArrayFixed { true }
		Struct {
			if seen[clean.name] {
				false
			} else {
				seen[clean.name] = true
				mut fixed := false
				for field in s.tc.struct_fields_for_type(clean.name) {
					if s.inline_fixed_storage(field.typ, mut seen) {
						fixed = true
						break
					}
				}
				fixed
			}
		}
		else { false }
	}
}

fn (s &NoAllocScan) executable_defaults(typ Type, mut seen map[string]bool) bool {
	clean := unalias_type(typ)
	if clean is ArrayFixed {
		return s.executable_defaults(clean.elem_type, mut seen)
	}
	if clean is Struct {
		return s.struct_executable_defaults(clean, mut seen)
	}
	return false
}

fn (s &NoAllocScan) struct_executable_defaults(typ Struct, mut seen map[string]bool) bool {
	if seen[typ.name] {
		return false
	}
	seen[typ.name] = true
	if declaration := s.tc.source_struct_decl_for_name(typ.name) {
		for i in 0 .. declaration.children_count {
			field := s.tc.a.child_node(&declaration, i)
			if field.kind == .field_decl && field.children_count > 0 {
				initial := s.tc.a.child_node(field, 0)
				if initial.kind !in [.int_literal, .float_literal, .bool_literal, .char_literal,
					.string_literal, .nil_literal, .none_expr, .enum_val] {
					return true
				}
			}
		}
	}
	for field in s.tc.struct_fields_for_type(typ.name) {
		if s.executable_defaults(field.typ, mut seen) {
			return true
		}
	}
	return false
}

fn (mut s NoAllocScan) check_argument_storage(call flat.Node, target NoAllocFunction,
	chain []string, anchor flat.NodeId, parameters map[string]string) {
	fn_node := s.tc.a.node(target.id)
	if !noalloc_foreign(*fn_node) && s.tc.fn_variadic[target.name] {
		s.report(s.tc.a.child(&call, 0), chain, anchor, 'variadic arguments may allocate an array')
	}
	callee := s.tc.a.child_node(&call, 0)
	mut parameter_index := 0
	for i in 0 .. fn_node.children_count {
		param := s.tc.a.child_node(fn_node, i)
		if param.kind != .param {
			continue
		}
		// Source method selectors carry the receiver separately from the argument list.
		if parameter_index == 0 && callee.kind == .selector
			&& !target.name.starts_with('C.') && fn_node.value.contains('.')
			&& !fn_node.is_static_type_method() {
			parameter_index++
			continue
		}
		offset := if callee.kind == .selector && !target.name.starts_with('C.')
			&& fn_node.value.contains('.') && !fn_node.is_static_type_method() {
			0
		} else {
			1
		}
		argument_index := parameter_index + offset
		parameter_index++
		if argument_index >= call.children_count {
			break
		}
		argument_id := s.tc.a.child(&call, argument_index)
		old_module := s.tc.cur_module
		old_file := s.tc.cur_file
		s.tc.cur_module = target.module
		if file := s.tc.a.source_files[fn_node.pos.id] {
			s.tc.cur_file = file.name
		}
		expected := s.tc.parse_type(param.typ)
		s.tc.cur_module = old_module
		s.tc.cur_file = old_file
		if s.boxed_storage(expected) {
			s.report(argument_id, chain, anchor, 'argument conversion may allocate interface or sum-type storage')
		}
		if param.is_mut || expected is Pointer {
			actual := s.tc.parse_type(s.type_name(argument_id, parameters))
			if expected is Pointer && actual !is Pointer && unalias_type(actual) is Struct {
				s.report(argument_id, chain, anchor, 'implicit reference to local aggregate may move it to the heap')
			}
			mut seen := map[string]bool{}
			if s.inline_fixed_storage(actual, mut seen) {
				s.report(argument_id, chain, anchor, 'passing inline fixed-array storage by reference may move it to the heap')
			}
		}
	}
}

fn (mut s NoAllocScan) walk(id flat.NodeId, decl NoAllocFunction, chain []string, anchor flat.NodeId,
	parameters map[string]string, mutable_arrays map[string]bool, callee_position bool,
	borrowed_address bool) {
	if !s.tc.valid_node_id(id) {
		return
	}
	node := s.tc.a.node(id)
	mut reason := ''
	mut buffer_growth := false
	match node.kind {
		.string_interp { reason = 'string interpolation allocates' }
		.map_init { reason = 'map initialization allocates' }
		.array_literal, .array_init {
			typ := s.type_name(id, parameters)
			inferred_fixed := node.kind == .array_literal && s.tc.array_literal_is_inferred_fixed(id)
			mut array_type := s.tc.parse_type(typ)
			if inferred_fixed {
				array_type = ArrayFixed{
					elem_type: s.tc.array_literal_elem_type(*node)
					len:       int(node.children_count)
				}
			}
			if unalias_type(array_type) !is ArrayFixed {
				reason = 'dynamic array initialization allocates'
			} else {
				mut seen := map[string]bool{}
				if s.owning_storage(array_type, mut seen) {
					reason = 'fixed array element initialization may allocate owning storage'
				}
				seen.clear()
				if s.executable_defaults(array_type, mut seen) {
					reason = 'fixed array element default cannot be proven nonallocating'
				}
			}
		}
		.fn_literal, .lambda_expr {
			reason = 'function literal or lambda may allocate a closure'
		}
		.spawn_expr, .sql_expr, .lock_expr, .asm_stmt, .select_stmt, .dump_expr, .typeof_expr,
		.debugger_stmt, .directive, .defer_stmt, .assert_stmt {
			reason = '${node.kind} cannot be proven nonallocating'
		}
		.comptime_if, .comptime_for {
			if !s.lowered {
				reason = 'unexpanded compile-time code cannot be proven nonallocating'
			}
		}
		.selector {
			if !callee_position && s.tc.expr_is_method_value(id) {
				reason = 'bound method value allocates a closure'
			}
		}
		.prefix {
			if node.op == .arrow {
				reason = 'channel receive cannot be proven nonallocating'
			}
			if node.op == .amp && node.children_count > 0 && !borrowed_address {
				child := s.tc.a.child_node(node, 0)
				if child.kind != .ident || !mutable_arrays[child.value] {
					reason = 'taking a local address may move storage to the heap'
				}
			}
		}
		.cast_expr, .as_expr {
			target := s.tc.parse_type(if node.value != '' { node.value } else { node.typ })
			mut seen := map[string]bool{}
			if s.boxed_storage(target) {
				reason = 'conversion to an interface or sum type may allocate a box'
			} else if s.owning_storage(target, mut seen) {
				reason = 'conversion to owning storage may allocate or clone its contents'
			}
		}
		.struct_init, .assoc {
			typ := s.tc.parse_type(s.type_name(id, parameters))
			mut seen := map[string]bool{}
			if s.owning_storage(typ, mut seen) {
				reason = 'initializing aggregate owning storage may allocate or clone defaults'
			}
			if s.tc.type_has_declaration_attribute(typ, 'heap') {
				reason = 'heap struct initialization allocates'
			}
			seen.clear()
			if s.executable_defaults(typ, mut seen) {
				reason = 'aggregate field default cannot be proven nonallocating'
			}
			if typ is Unknown {
				reason = 'aggregate type cannot be proven nonallocating'
			}
		}
		.field_init {
			if node.typ != '' && node.children_count > 0 {
				expected := unalias_type(s.tc.parse_type(node.typ))
				actual := unalias_type(s.tc.parse_type(s.type_name(s.tc.a.child(node, 0), parameters)))
				if s.boxed_storage(expected) && actual.name() != expected.name() {
					reason = 'field conversion may allocate interface or sum-type storage'
				}
			}
		}
		.index {
			// In callee position an index can select generic arguments. The call
			// itself resolves the direct declaration or rejects an indirect target.
			if !callee_position {
				for i in 1 .. node.children_count {
					if s.tc.a.child_node(node, i).kind == .range {
						reason = 'slicing may allocate storage'
					}
				}
				if node.children_count > 0 {
					base := s.tc.parse_type(s.type_name(s.tc.a.child(node, 0), parameters))
					clean := unalias_type(unwrap_pointer(base))
					if clean is Unknown || clean is Map || clean is Struct || clean is Interface || clean is SumType {
						reason = 'indexing this type cannot be proven nonallocating'
					}
				}
			}
		}
		.infix {
			if node.op == .arrow {
				reason = 'channel send cannot be proven nonallocating'
			}
			if node.children_count >= 2 {
				lhs := s.tc.a.child(node, 0)
				typ := s.tc.parse_type(s.type_name(lhs, parameters))
				clean := unalias_type(unwrap_pointer(typ))
				if node.op == .left_shift && clean is Array {
					rhs := unalias_type(s.tc.parse_type(s.type_name(s.tc.a.child(node, 1), parameters)))
					mut seen := map[string]bool{}
					if s.boxed_storage(clean) {
						reason = 'array element conversion may allocate interface or sum-type storage'
					} else if s.owning_storage(clean.elem_type, mut seen) {
						reason = 'appending owning elements may allocate or clone their contents'
					} else if (rhs is Array || rhs is ArrayFixed) && rhs.name() != clean.elem_type.name() {
						reason = 'bulk append may allocate an aliasing-source copy'
					} else if s.strict || !s.array_parameter(lhs, mutable_arrays) {
						reason = 'array append may grow its buffer'
					}
				} else if s.tc.comparison_calls_operator_method(typ, node.op)
					|| s.custom_operator_storage(typ) {
					reason = 'operator on this type cannot be proven nonallocating'
				} else if node.op in [.plus, .plus_assign] && (clean is String || clean is Array) {
					reason = 'string or array concatenation allocates'
				}
			}
		}
		.assign, .decl_assign, .selector_assign, .index_assign {
			for i := 0; i + 1 < node.children_count; i += 2 {
				lhs := s.tc.a.child(node, i)
				rhs := s.tc.a.child(node, i + 1)
				typ := s.tc.parse_type(s.type_name(lhs, parameters))
				mut seen := map[string]bool{}
				if s.boxed_storage(typ) {
					reason = 'assignment conversion may allocate interface or sum-type storage'
				} else if s.owning_storage(typ, mut seen)
					&& s.tc.a.node(rhs).kind != .string_literal {
					reason = 'copying owning storage may allocate or clone its contents'
				} else if node.kind != .decl_assign && node.op !in [.assign, .none] {
					clean := unalias_type(unwrap_pointer(typ))
					operator := compound_assignment_infix_op(node.op) or { node.op }
					if s.tc.comparison_calls_operator_method(typ, operator) || s.custom_operator_storage(typ)
						|| (node.op == .plus_assign && clean is String) {
						reason = 'compound assignment cannot be proven nonallocating'
					}
				}
			}
		}
		.postfix {
			if node.children_count > 0 {
				typ := s.tc.parse_type(s.type_name(s.tc.a.child(node, 0), parameters))
				operator := match node.op {
					.inc { flat.Op.plus }
					.dec { flat.Op.minus }
					else { node.op }
				}
				if s.tc.comparison_calls_operator_method(typ, operator) || s.custom_operator_storage(typ) {
					reason = 'postfix operation cannot be proven nonallocating'
				}
			}
		}
		.in_expr {
			for i in 0 .. node.children_count {
				typ := s.tc.parse_type(s.type_name(s.tc.a.child(node, i), parameters))
				if s.tc.comparison_calls_operator_method(typ, .eq) || s.custom_operator_storage(typ) {
					reason = 'membership operation cannot be proven nonallocating'
				}
			}
		}
		.return_stmt {
			fn_node := s.tc.a.node(decl.id)
			result_type := s.tc.parse_type(fn_node.typ)
			mut seen := map[string]bool{}
			if s.owning_storage(result_type, mut seen) {
				if node.children_count != 1 || s.tc.a.child_node(node, 0).kind != .string_literal
					|| fn_node.typ != 'string' {
					reason = 'returning owning storage may allocate or clone its contents'
				}
			}
			if result_type is Pointer {
				for i in 0 .. node.children_count {
					actual := s.tc.parse_type(s.type_name(s.tc.a.child(node, i), parameters))
					if actual !is Pointer {
						reason = 'returning an implicit reference may move storage to the heap'
					}
				}
			}
			if result_type is ResultType {
				base := unalias_type(result_type.base_type)
				for i in 0 .. node.children_count {
					actual := unalias_type(s.tc.parse_type(s.type_name(s.tc.a.child(node, i), parameters)))
					concrete := unalias_type(unwrap_pointer(actual))
					if actual.name() != base.name()
						&& s.tc.named_type_compatible_with_ierror(concrete.name()) {
						reason = 'returning a concrete error may allocate IError storage'
					}
				}
			}
		}
		.for_in_stmt {
			// The header is key, value, iterable, followed by direct body statements.
			if node.children_count >= 3 && node.value != '4'
				&& s.tc.a.child_node(node, 2).kind != .range {
				typ := unalias_type(s.tc.parse_type(s.type_name(s.tc.a.child(node, 2), parameters)))
				if typ is Unknown || typ is Map || typ is Struct || typ is Interface || typ is SumType || typ is Channel {
					reason = 'iteration may allocate a snapshot or call an unknown iterator'
				}
			}
		}
		.call {
			name := s.tc.resolved_call_name(id) or { s.tc.call_display_name(*node) }
			callee := s.tc.a.child_node(node, 0)
			mut indirect := false
			if callee.kind == .ident && callee.value in parameters {
				// Every called local binding is an indirect function value, even when
				// semantic caches no longer retain its type after lowering.
				indirect = true
			}
			if callee.kind in [.index, .paren, .prefix, .call] {
				mut base := callee
				for base.children_count > 0 && base.kind in [.index, .paren, .prefix, .call] {
					base = s.tc.a.child_node(base, 0)
				}
				if base.kind == .ident && base.value in parameters {
					indirect = true
				}
			}
			if callee.kind == .selector && callee.children_count > 0 {
				receiver := unalias_type(unwrap_pointer(s.tc.parse_type(s.type_name(s.tc.a.child(callee,
					0), parameters))))
				if receiver is Interface {
					indirect = true
				} else if receiver is Struct {
					for field in s.tc.struct_fields_for_type(receiver.name) {
						if field.name == callee.value && unalias_type(field.typ) is FnType {
							indirect = true
						}
					}
				}
			}
			growth := name in ['array_push', 'builtin.array_push']
			mut growth_target := s.function(name, decl.module) or { NoAllocFunction{} }
			if growth && s.lowered && growth_target.name == '' {
				// Cgen's runtime macro is an alias for the actual builtin method.
				growth_target = s.function('array.push', 'builtin') or { NoAllocFunction{} }
			}
			if indirect {
				reason = 'indirect or interface call cannot be proven nonallocating'
			} else if s.terminal(id, decl) && !s.has_control_exit(id) {
				return
			} else if growth && growth_target.module == 'builtin' && !s.strict && node.children_count > 1
				&& s.array_parameter(s.tc.a.child(node, 1), mutable_arrays) {
				// Only growth of an existing mutable parameter is the default policy exception.
				buffer_growth = true
				buffer_type := unalias_type(unwrap_pointer(s.tc.parse_type(s.type_name(s.tc.a.child(node, 1), parameters))))
				if s.boxed_storage(buffer_type) {
					reason = 'array element conversion may allocate interface or sum-type storage'
				} else if buffer_type is Array {
					mut seen := map[string]bool{}
					if s.owning_storage(buffer_type.elem_type, mut seen) {
						reason = 'appending owning elements may allocate or clone their contents'
					}
				}
			} else if target := s.function(name, decl.module) {
				s.check_argument_storage(*node, target, chain, anchor, parameters)
				mut next_chain := chain.clone()
				next_chain << target.name
				first_call := if s.tc.valid_node_id(anchor) { anchor } else { id }
				s.walk_function(target, next_chain, first_call)
			} else {
				reason = 'call to `${name}` has no analyzable function body or C contract'
			}
		}
		else {}
	}
	if reason != '' {
		s.report(id, chain, anchor, reason)
	}
	for i in 0 .. node.children_count {
		if node.kind == .call && i == 0 {
			s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, mutable_arrays, true,
				false)
		} else {
			// array_push copies its item before returning. Its synthesized address
			// borrows the item; the original item expression is still traversed.
			s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, mutable_arrays, false,
				s.lowered && buffer_growth && i > 1)
		}
	}
}
