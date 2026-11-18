module types

import v.flat

struct NoSuspendFunction {
	id     flat.NodeId
	module string
	name   string
}

struct NoSuspendScan {
mut:
	tc        &TypeChecker = unsafe { nil }
	functions map[string]NoSuspendFunction
	root      NoSuspendFunction
	lowered   bool
	visited   map[int]bool
	reported  map[string]bool
}

// has_nosuspend_contracts reports whether suspension contracts need validation.
// Foreign declarations alone do not require walking the program.
pub fn (tc &TypeChecker) has_nosuspend_contracts() bool {
	for index, attributes in tc.declaration_attributes {
		if !tc.declaration_has_attribute(flat.NodeId(index), 'nosuspend') {
			continue
		}
		node := tc.a.nodes[index]
		if node.kind !in [.fn_decl, .c_fn_decl] || !nosuspend_foreign(node) {
			return true
		}
		for attribute in attributes {
			if attribute.all_before(':').trim_space() == 'nosuspend' && attribute.contains(':') {
				return true
			}
		}
	}
	return false
}

// check_nosuspend_contracts checks every annotated V function and its reachable
// callees. Suspension points cover `spawn`, channel operations, `select`,
// `lock`, and known blocking calls. Unknown operations fail conservatively.
pub fn (mut tc TypeChecker) check_nosuspend_contracts(lowered bool, unsupported_modes string) {
	if !tc.has_nosuspend_contracts() {
		return
	}
	mut roots := map[string]bool{}
	for index, attributes in tc.declaration_attributes {
		if !tc.declaration_has_attribute(flat.NodeId(index), 'nosuspend') {
			continue
		}
		node := tc.a.nodes[index]
		if node.kind !in [.fn_decl, .c_fn_decl] && !(lowered && node.kind == .empty) {
			tc.record_transform_error(flat.NodeId(index), node.pos,
				'@[nosuspend] is only supported on V and C function declarations')
			continue
		}
		for attribute in attributes {
			if attribute.all_before(':').trim_space() != 'nosuspend' {
				continue
			}
			if attribute.contains(':') {
				tc.record_transform_error(flat.NodeId(index), node.pos,
					'@[nosuspend] accepts no arguments')
			}
		}
		if !nosuspend_foreign(node) {
			roots[nosuspend_position_key(node)] = true
		}
	}
	mut scan := NoSuspendScan{
		tc:      &tc
		lowered: lowered
	}
	mut declarations := []NoSuspendFunction{}
	for index, node in tc.a.nodes {
		if node.kind !in [.fn_decl, .c_fn_decl] {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		raw_module := tc.a.specialized_fn_modules[index] or { tc.file_modules[file.name] or { 'main' } }
		module_name := if raw_module == '' { 'main' } else { raw_module }
		name := if nosuspend_foreign(node) {
			if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
		} else {
			checker_qualified_fn_name(module_name, node.value)
		}
		decl := NoSuspendFunction{
			id:     flat.NodeId(index)
			module: module_name
			name:   name
		}
		declarations << decl
		scan.functions[name] = decl
		scan.functions['${module_name}\x01${node.value}'] = decl
		if module_name == 'builtin' || nosuspend_foreign(node) {
			scan.functions[node.value] = decl
		}
	}
	for decl in declarations {
		node := tc.a.node(decl.id)
		if !roots[nosuspend_position_key(*node)] || node.generic_params().len > 0 {
			continue
		}
		scan.root = decl
		scan.visited.clear()
		if unsupported_modes != '' {
			scan.report(decl.id, [decl.name], flat.empty_node,
				'compiler-generated ${unsupported_modes} operations cannot be proven non-suspending')
			continue
		}
		scan.walk_function(decl, [decl.name], flat.empty_node)
	}
}

fn nosuspend_position_key(node flat.Node) string {
	return '${node.pos.id}:${node.pos.offset}:${node.pos.end}'
}

fn nosuspend_foreign(node flat.Node) bool {
	return node.kind == .c_fn_decl || node.value.starts_with('C.')
}

fn (mut s NoSuspendScan) report(id flat.NodeId, chain []string, anchor flat.NodeId, reason string) {
	if !s.tc.valid_node_id(id) {
		return
	}
	key := '${s.root.id}:${id}:${reason}'
	if s.reported[key] {
		return
	}
	s.reported[key] = true
	position := s.tc.node_position_string(id)
	message := '@[nosuspend] `${s.root.name}` may suspend: ${chain.join(' -> ')}: ${reason} (${position})'
	at := if s.tc.valid_node_id(anchor) { anchor } else { id }
	s.tc.record_transform_error(at, s.tc.a.node(at).pos, message)
}

fn (s &NoSuspendScan) function(name string, module_name string) ?NoSuspendFunction {
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

// nosuspend_blocking_call reports whether a resolved call is a known blocking
// primitive. Unlisted V calls are still checked transitively; unannotated C
// calls fail closed elsewhere. Trusted `@[nosuspend]` on a C declaration is
// an explicit programmer assumption, so C names are never blocked here.
fn nosuspend_blocking_call(name string, display string) bool {
	clean := name.all_before('[')
	base := clean.all_after_last('.')
	if clean in ['time.sleep', 'os.wait', 'os.Process.wait', 'sync.channel_select',
		'sync.WaitGroup.wait', 'sync.Barrier.wait', 'sync.Cond.wait', 'sync.Latch.wait',
		'sync.Phaser.arrive_and_wait', 'sync.Semaphore.wait', 'sync.Semaphore.timed_wait',
		'sync.WaitGroup.go'] {
		return true
	}
	if display in ['time.sleep', 'os.wait', 'Process.wait', 'channel_select', 'WaitGroup.wait',
		'Barrier.wait', 'Cond.wait', 'Latch.wait', 'Semaphore.wait', 'Semaphore.timed_wait'] {
		return true
	}
	// Methods on sync primitives suspend even when reached through a short name.
	if clean.starts_with('sync.') {
		if base in ['sleep', 'wait', 'timed_wait', 'arrive_and_wait', 'lock', 'rlock', 'read_lock',
			'write_lock', 'push', 'pop', 'go'] {
			if base in ['push', 'pop'] {
				return clean.contains('Channel')
			}
			return true
		}
	}
	if base in ['sleep'] && (clean.starts_with('time.') || display.starts_with('time.')) {
		return true
	}
	return false
}

fn (mut s NoSuspendScan) walk_function(decl NoSuspendFunction, chain []string, anchor flat.NodeId) {
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
	if nosuspend_foreign(*node) {
		if !s.tc.declaration_has_attribute(decl.id, 'nosuspend') {
			s.report(decl.id, chain, anchor, 'C function has no @[nosuspend] contract')
		}
		return
	}
	if node.is_mut || s.tc.cur_file.ends_with('.vh') {
		s.report(decl.id, chain, anchor, 'function body is unavailable')
		return
	}
	mut parameters := map[string]u32{}
	for i in 0 .. node.children_count {
		child := s.tc.a.child_node(node, i)
		if child.kind == .param {
			parameters[child.value] = child.typ
		}
	}
	// Function bodies are direct children after the parameters, not block nodes.
	for i in 0 .. node.children_count {
		if s.tc.a.child_node(node, i).kind != .param {
			s.collect_locals(s.tc.a.child(node, i), mut parameters)
		}
	}
	for i in 0 .. node.children_count {
		if s.tc.a.child_node(node, i).kind != .param {
			s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, false)
		}
	}
}

fn (s &NoSuspendScan) collect_locals(id flat.NodeId, mut bindings map[string]u32) {
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

fn (s &NoSuspendScan) type_name(id flat.NodeId, parameters map[string]u32) u32 {
	if typ := s.tc.expr_type(id) { return typ.source_type_id() }
	node := s.tc.a.node(id)
	if node.typ != 0 { return node.typ }
	if node.kind == .ident {
		typ := parameters[node.value] or { flat.source_type_unknown }
		return if typ == 0 { flat.source_type_unknown } else { typ }
	}
	if node.kind == .selector && node.value in ['len', 'cap'] { return flat.source_type_int }
	if node.kind in [.struct_init, .cast_expr] && node.source_value_id() != 0 {
		return node.source_value_id()
	}
	if node.kind in [.paren, .prefix, .postfix] && node.children_count > 0 {
		child_id := s.tc.a.child(node, 0)
		if node.kind == .postfix && node.op == .not && s.tc.a.node(child_id).kind == .array_literal {
			literal := s.tc.a.node(child_id)
			return flat.source_type_fixed_array(literal.children_count, '', s.tc.array_literal_elem_type(*literal).source_type_id(), false)
		}
		child_type := s.type_name(child_id, parameters)
		if node.kind == .prefix && node.op == .amp {
			return flat.source_type_composition_one(.pointer, child_type)
		}
		if node.kind == .prefix && node.op == .mul {
			if !flat.source_type_shape(child_type).has(.pointer) {
				return child_type
			}
			return flat.source_type_syntax(child_type).raw_base()
		}
		return child_type
	}
	return match node.kind {
		.int_literal { flat.source_type_int }
		.float_literal { flat.source_type_f64 }
		.bool_literal { flat.source_type_bool }
		.string_literal { flat.source_type_string }
		.char_literal { flat.source_type_char }
		else { flat.source_type_unknown }
	}
}

fn (s &NoSuspendScan) has_control_exit(id flat.NodeId) bool {
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

fn (s &NoSuspendScan) terminal(id flat.NodeId, decl NoSuspendFunction, bindings map[string]u32) bool {
	node := s.tc.a.node(id)
	if node.kind in [.block, .expr_stmt] && node.children_count > 0 {
		return s.terminal(s.tc.a.child(node, node.children_count - 1), decl, bindings)
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := s.tc.a.child_node(node, 0)
	// A local binding spelling a terminal name is a function value, not the
	// terminating declaration. This covers both bare `exit()` and shadowed
	// module spellings such as `os.exit()` on a local `os`.
	if callee.kind == .ident && callee.value in bindings {
		return false
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := s.tc.a.child_node(callee, 0)
		if base.kind == .ident && base.value in bindings {
			return false
		}
	}
	name := s.tc.resolved_call_name(id) or { s.tc.call_display_name(*node) }
	if name in ['builtin.panic', 'os.exit', 'C.exit', 'C.abort'] {
		// Display names alone prove nothing: a local struct field can spell
		// `os.exit`. Require the resolved declaration from the intended module.
		if target := s.function(name, decl.module) {
			return target.module in ['builtin', 'os'] || target.name.starts_with('C.')
		}
		return false
	}
	if name in ['panic', 'exit', 'abort'] {
		if target := s.function(name, decl.module) {
			return target.module in ['builtin', 'os'] || target.name.starts_with('C.')
		}
	}
	return false
}

fn (mut s NoSuspendScan) walk(id flat.NodeId, decl NoSuspendFunction, chain []string, anchor flat.NodeId,
	parameters map[string]u32, callee_position bool) {
	if !s.tc.valid_node_id(id) {
		return
	}
	node := s.tc.a.node(id)
	mut reason := ''
	match node.kind {
		.spawn_expr { reason = '`spawn` may suspend' }
		.lock_expr { reason = '`lock` may suspend' }
		.select_stmt { reason = '`select` may suspend' }
		.sql_expr { reason = 'SQL query may suspend on IO' }
		.comptime_if, .comptime_for {
			if !s.lowered {
				reason = 'unexpanded compile-time code cannot be proven non-suspending'
			}
		}
		.selector {
			if !callee_position && s.tc.expr_is_method_value(id) {
				reason = 'bound method value may suspend'
			}
		}
		.prefix {
			if node.op == .arrow {
				reason = 'channel receive may suspend'
			}
		}
		.infix {
			if node.op == .arrow {
				reason = 'channel send may suspend'
			}
		}
		.call {
			name := s.tc.resolved_call_name(id) or { s.tc.call_display_name(*node) }
			display := s.tc.call_display_name(*node)
			callee := s.tc.a.child_node(node, 0)
			mut indirect := false
			if callee.kind == .ident && callee.value in parameters {
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
					for field in s.tc.struct_fields_for_type(receiver.source_type_id()) {
						if field.name == callee.value && unalias_type(field.typ) is FnType {
							indirect = true
						}
					}
				} else if receiver is Unknown {
					// A local binding can shadow a module name (`os := ...; os.exit()`).
					// Without a proven receiver type the selector may be a
					// function field, so fail closed before any terminal exemption.
					base := s.tc.a.child_node(callee, 0)
					if base.kind == .ident && base.value in parameters {
						indirect = true
					}
				}
			}
			if indirect {
				reason = 'indirect or interface call may suspend'
			} else if s.terminal(id, decl, parameters) && !s.has_control_exit(id) {
				// Terminating calls never resume, but their arguments are still
				// evaluated. Walk arguments below without following the callee.
				for i in 1 .. node.children_count {
					s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, false)
				}
				return
			} else if nosuspend_blocking_call(name, display) {
				reason = 'call to `${name}` may suspend'
			} else if target := s.function(name, decl.module) {
				target_node := s.tc.a.node(target.id)
				if nosuspend_foreign(*target_node) {
					if !s.tc.declaration_has_attribute(target.id, 'nosuspend') {
						reason = 'C function `${target.name}` has no @[nosuspend] contract'
					}
				} else {
					// Interface declarations have no body to verify.
					if target_node.kind == .empty {
						reason = 'call to `${name}` has no analyzable function body'
					} else {
						mut next_chain := chain.clone()
						next_chain << target.name
						first_call := if s.tc.valid_node_id(anchor) { anchor } else { id }
						s.walk_function(target, next_chain, first_call)
					}
				}
			} else {
				// Unresolvable targets include interface dispatch and missing bodies.
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
			s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, true)
		} else {
			s.walk(s.tc.a.child(node, i), decl, chain, anchor, parameters, false)
		}
	}
}
