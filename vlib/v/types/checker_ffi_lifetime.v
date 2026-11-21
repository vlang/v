module types

import v.flat

struct FfiLifetimeFunction {
	id     flat.NodeId
	module string
	name   string
}

struct FfiLifetimeScan {
mut:
	tc        &TypeChecker = unsafe { nil }
	functions map[string]FfiLifetimeFunction
	reported  map[string]bool
	lowered   bool
}

// ffi_lifetime_contract_names lists the declarative FFI memory/lifetime contract attributes.
const ffi_lifetime_contract_names = ['nonnull', 'noescape', 'len_of', 'returns_owned',
	'returns_borrowed']

// has_ffi_lifetime_contracts reports whether any declaration carries an FFI
// lifetime contract attribute.
pub fn (tc &TypeChecker) has_ffi_lifetime_contracts() bool {
	for _, attributes in tc.declaration_attributes {
		for attribute in attributes {
			name := attribute.all_before(':').trim_space()
			if name in ffi_lifetime_contract_names {
				return true
			}
			for contract in ffi_lifetime_contract_names {
				if name.starts_with(contract + '_') {
					return true
				}
			}
		}
	}
	return false
}

// check_ffi_lifetime_contracts validates FFI memory/lifetime contract schemas on
// every annotated declaration and checks call sites against statically known
// `@[nonnull]`/`@[noescape]` arguments. Annotations on C declarations are
// trusted claims; unannotated C declarations keep their current behavior.
// Unresolvable callees are silently skipped (fail open).
// These checks are independent of the noalloc mode restriction: compiler-generated
// allocation operations do not invalidate pointer/lifetime schemas.
pub fn (mut tc TypeChecker) check_ffi_lifetime_contracts(lowered bool) {
	if !tc.has_ffi_lifetime_contracts() {
		return
	}
	for index, attributes in tc.declaration_attributes {
		mut family := []string{}
		for attribute in attributes {
			name := attribute.all_before(':').trim_space()
			if name in ffi_lifetime_contract_names {
				family << attribute
				continue
			}
			for contract in ffi_lifetime_contract_names {
				if name.starts_with(contract + '_') {
					family << attribute
				}
			}
		}
		if family.len == 0 {
			continue
		}
		node := tc.a.nodes[index]
		if node.kind !in [.fn_decl, .c_fn_decl] && !(lowered && node.kind == .empty) {
			tc.record_transform_error(flat.NodeId(index), node.pos,
				'@[ffi] contracts are only supported on V and C function declarations')
			continue
		}
		if lowered && node.kind == .empty {
			continue
		}
		mut params := map[string]int{}
		mut order := 0
		for i in 0 .. node.children_count {
			param := tc.a.child_node(&node, i)
			if param.kind != .param {
				continue
			}
			if param.value !in params {
				params[param.value] = order
			}
			order++
		}
		for attribute in family {
			name := attribute.all_before(':').trim_space()
			if name !in ffi_lifetime_contract_names {
				tc.record_transform_error(flat.NodeId(index), node.pos,
					'@[ffi] unknown contract capability `@[${name}]`')
				continue
			}
			raw := if attribute.contains(':') {
				attribute_string_unquote(attribute.all_after(':').trim_space())
			} else {
				''
			}
			match name {
				'nonnull', 'noescape', 'returns_borrowed' {
					names := ffi_contract_param_list(raw)
					if names.len == 0 {
						tc.record_transform_error(flat.NodeId(index), node.pos,
							'@[ffi] `@[${name}]` expects a parameter list naming parameters of this declaration')
						continue
					}
					for entry in names {
						if entry !in params {
							tc.record_transform_error(flat.NodeId(index), node.pos,
								'@[ffi] `@[${name}]` names unknown parameter `${entry}`')
						}
					}
				}
				'len_of' {
					pairs := ffi_contract_param_list(raw)
					if pairs.len == 0 {
						tc.record_transform_error(flat.NodeId(index), node.pos,
							'@[ffi] `@[len_of]` expects `buf=len` pairs naming parameters of this ffi declaration')
						continue
					}
					for pair in pairs {
						parts := pair.split('=')
						if parts.len != 2 || parts[0].trim_space() !in params
							|| parts[1].trim_space() !in params {
							tc.record_transform_error(flat.NodeId(index), node.pos,
								'@[ffi] `@[len_of]` expects `buf=len` pairs naming parameters of this ffi declaration: `${pair}`')
						}
					}
				}
				'returns_owned' {
					releaser := raw.trim_space()
					if releaser.len == 0 || releaser.contains(',') {
						tc.record_transform_error(flat.NodeId(index), node.pos,
							'@[ffi] `@[returns_owned]` expects a single releaser function name')
					}
				}
				else {}
			}
		}
	}
	mut scan := FfiLifetimeScan{
		tc:      &tc
		lowered: lowered
	}
	for index, node in tc.a.nodes {
		if node.kind !in [.fn_decl, .c_fn_decl] {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		raw_module := tc.a.specialized_fn_modules[index] or {
			tc.file_modules[file.name] or { 'main' }
		}
		module_name := if raw_module == '' { 'main' } else { raw_module }
		name := if ffi_foreign(node) {
			if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
		} else {
			checker_qualified_fn_name(module_name, node.value)
		}
		decl := FfiLifetimeFunction{
			id:     flat.NodeId(index)
			module: module_name
			name:   name
		}
		scan.functions[name] = decl
		scan.functions['${module_name}\x01${node.value}'] = decl
		if module_name == 'builtin' || ffi_foreign(node) {
			scan.functions[node.value] = decl
		}
	}
	for index, attributes in tc.declaration_attributes {
		for attribute in attributes {
			if attribute.all_before(':').trim_space() != 'returns_owned' {
				continue
			}
			node := tc.a.nodes[index]
			if node.kind !in [.fn_decl, .c_fn_decl] {
				continue
			}
			raw := if attribute.contains(':') {
				attribute_string_unquote(attribute.all_after(':').trim_space())
			} else {
				''
			}
			releaser := raw.trim_space()
			if releaser.len == 0 || releaser.contains(',') {
				continue
			}
			releaser_module := scan.module_of(index, node)
			if scan.contract_function(releaser, releaser_module) == none {
				tc.record_transform_error(flat.NodeId(index), node.pos,
					'@[ffi] `@[returns_owned]` names unknown releaser `${releaser}`')
			}
		}
	}
	scan.check_call_sites()
}

// FfiLifetimeContract holds the parsed parameter contracts of one declaration.
struct FfiLifetimeContract {
	nonnull  map[int]bool
	noescape map[int]bool
}

fn ffi_foreign(node flat.Node) bool {
	return node.kind == .c_fn_decl || node.value.starts_with('C.')
}

fn ffi_contract_param_list(raw string) []string {
	mut names := []string{}
	for part in raw.split(',') {
		entry := attribute_string_unquote(part.trim_space())
		if entry.len > 0 {
			names << entry
		}
	}
	return names
}

fn (s &FfiLifetimeScan) contract_function(name string, module_name string) ?FfiLifetimeFunction {
	if found := s.functions[name] {
		return found
	}
	if module_name.len > 0 {
		if found := s.functions['${module_name}\x01${name}'] {
			return found
		}
	}
	if !s.lowered && name.contains('[') {
		base := name.all_before('[')
		if found := s.functions[base] {
			if s.tc.a.node(found.id).generic_params().len > 0 {
				return found
			}
		}
		if module_name.len > 0 {
			if found := s.functions['${module_name}\x01${base}'] {
				if s.tc.a.node(found.id).generic_params().len > 0 {
					return found
				}
			}
		}
	}
	return none
}

// check_call_sites visits every V function body and checks statically known
// arguments of calls resolving to declarations with nonnull/noescape contracts.
fn (mut s FfiLifetimeScan) check_call_sites() {
	for index, node in s.tc.a.nodes {
		if node.kind != .fn_decl || ffi_foreign(node) {
			continue
		}
		if node.is_mut {
			continue
		}
		if file := s.tc.a.source_files[node.pos.id] {
			if file.name.ends_with('.vh') {
				continue
			}
		}
		decl := FfiLifetimeFunction{
			id:     flat.NodeId(index)
			module: s.module_of(index, node)
			name:   node.value
		}
		s.check_body_calls(decl, node)
	}
}

fn (s &FfiLifetimeScan) module_of(index int, node flat.Node) string {
	if file := s.tc.a.source_files[node.pos.id] {
		raw := s.tc.a.specialized_fn_modules[index] or {
			s.tc.file_modules[file.name] or { 'main' }
		}
		return if raw == '' { 'main' } else { raw }
	}
	return 'main'
}

fn (mut s FfiLifetimeScan) check_body_calls(decl FfiLifetimeFunction, node flat.Node) {
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
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		if s.tc.a.child_node(&node, i).kind == .param {
			continue
		}
		stack << s.tc.a.child(&node, i)
	}
	for stack.len > 0 {
		id := stack.pop()
		if !s.tc.valid_node_id(id) {
			continue
		}
		current := s.tc.a.node(id)
		if current.kind == .call {
			s.check_call(decl, id, *current)
		}
		for i in 0 .. current.children_count {
			stack << s.tc.a.child(current, i)
		}
	}
}

fn (mut s FfiLifetimeScan) check_call(caller FfiLifetimeFunction, id flat.NodeId, call flat.Node) {
	if call.children_count == 0 {
		return
	}
	name := s.tc.resolved_call_name(id) or { s.tc.call_display_name(call) }
	target := s.contract_function(name, caller.module) or { return }
	contract := s.contract_of(target.id)
	if contract.nonnull.len == 0 && contract.noescape.len == 0 {
		return
	}
	fn_node := s.tc.a.node(target.id)
	callee := s.tc.a.child_node(&call, 0)
	mut parameter_index := 0
	for i in 0 .. fn_node.children_count {
		param := s.tc.a.child_node(fn_node, i)
		if param.kind != .param {
			continue
		}
		// Source method selectors carry the receiver separately from the argument list.
		if parameter_index == 0 && callee.kind == .selector && !target.name.starts_with('C.')
			&& fn_node.value.contains('.') && !fn_node.is_static_type_method() {
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
		current_param := parameter_index
		parameter_index++
		if argument_index >= call.children_count {
			break
		}
		if current_param !in contract.nonnull && current_param !in contract.noescape {
			continue
		}
		argument_id := s.tc.a.child(&call, argument_index)
		value_id := s.unwrap_ffi_argument(argument_id)
		if !s.tc.valid_node_id(value_id) {
			continue
		}
		value := s.tc.a.node(value_id)
		param_name := param.value
		if value.kind in [.nil_literal, .none_expr] {
			if current_param in contract.nonnull {
				s.report_call(id, argument_id, 'nonnull', param_name, target.name)
			} else {
				s.report_call(id, argument_id, 'noescape', param_name, target.name)
			}
			continue
		}
		if current_param in contract.nonnull {
			if value.kind == .or_expr {
				continue
			}
			if typ := s.tc.expr_type(argument_id) {
				parsed := s.tc.parse_type(typ.source_type_id())
				if unalias_type(parsed) is OptionType {
					s.report_call(id, argument_id, 'unchecked optional', param_name,
						target.name)
				}
			}
		}
	}
}

fn (s &FfiLifetimeScan) contract_of(id flat.NodeId) FfiLifetimeContract {
	mut nonnull := map[int]bool{}
	mut noescape := map[int]bool{}
	node := s.tc.a.node(id)
	mut params := map[string]int{}
	mut order := 0
	for i in 0 .. node.children_count {
		param := s.tc.a.child_node(node, i)
		if param.kind != .param {
			continue
		}
		if param.value !in params {
			params[param.value] = order
		}
		order++
	}
	for raw in s.tc.declaration_attributes[int(id)] {
		name := raw.all_before(':').trim_space()
		if name != 'nonnull' && name != 'noescape' {
			continue
		}
		value := if raw.contains(':') {
			attribute_string_unquote(raw.all_after(':').trim_space())
		} else {
			''
		}
		for entry in ffi_contract_param_list(value) {
			if position := params[entry] {
				if name == 'nonnull' {
					nonnull[position] = true
				} else {
					noescape[position] = true
				}
			}
		}
	}
	return FfiLifetimeContract{
		nonnull:  nonnull
		noescape: noescape
	}
}

// unwrap_ffi_argument unwraps parentheses and unsafe blocks around a call argument.
fn (s &FfiLifetimeScan) unwrap_ffi_argument(id flat.NodeId) flat.NodeId {
	mut current := id
	for s.tc.valid_node_id(current) {
		node := s.tc.a.node(current)
		if node.kind == .paren && node.children_count > 0 {
			current = s.tc.a.child(node, 0)
			continue
		}
		if node.kind == .block && node.value == 'unsafe' {
			tail := s.tc.branch_tail_expr_id(current)
			if !s.tc.valid_node_id(tail) {
				return current
			}
			current = tail
			continue
		}
		return current
	}
	return current
}

fn (mut s FfiLifetimeScan) report_call(call_id flat.NodeId, argument_id flat.NodeId, kind string, param string, target string) {
	key := '${int(call_id)}:${param}:${kind}'
	if s.reported[key] {
		return
	}
	s.reported[key] = true
	position := s.tc.node_position_string(argument_id)
	message := match kind {
		'unchecked optional' {
			'@[ffi] `@[nonnull]` unchecked optional argument `${param}` of `${target}` (${position})'
		}
		else {
			'@[ffi] `@[${kind}]` argument `${param}` of `${target}` must not be nil or none (${position})'
		}
	}
	at := if s.tc.valid_node_id(argument_id) { argument_id } else { call_id }
	s.tc.record_transform_error(at, s.tc.a.node(at).pos, message)
}
