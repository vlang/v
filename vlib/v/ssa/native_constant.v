module ssa

import v.flat
import v.types

struct NativeInitializer {
	module_name string
	field       flat.NodeId
	is_const    bool
mut:
	function     int = -1
	dependencies []string
}

fn (b &Builder) native_const_key(name string, module_name string) ?string {
	if !name.contains('.') {
		key := module_name + '.' + name
		if key in b.const_exprs {
			return key
		}
	} else {
		alias := name.all_before('.')
		if imported := b.global_imports[module_name + '#' + alias] {
			key := imported + '.' + name.all_after('.')
			if key in b.const_exprs {
				return key
			}
		}
	}
	if name in b.const_exprs {
		return name
	}
	return none
}

fn (b &Builder) native_const_requires_storage(id flat.NodeId, module_name string, mut cache map[int]bool, mut visiting map[int]bool) bool {
	if cached := cache[int(id)] {
		return cached
	}
	if visiting[int(id)] {
		return false
	}
	visiting[int(id)] = true
	node := b.a.node(id)
	mut required := node.kind in [.array_literal, .array_init, .map_init, .struct_init, .string_interp]
	if node.kind == .call {
		callee := b.qualified_expr_name(b.a.child(node, 0))
		required = callee !in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64',
			'usize', 'rune', 'f32', 'f64', 'bool', 'voidptr', 'char']
	}
	if node.kind == .infix && b.checked_expr_type_name(id) == 'string' {
		required = true
	}
	if node.kind in [.ident, .selector] {
		name := if node.kind == .ident { node.value } else { b.selector_qualified_name(*node) }
		if key := b.native_const_key(name, module_name) {
			expr := b.const_exprs[key]
			declaring_module := if key.contains('.') { key.all_before_last('.') } else { 'main' }
			required = b.native_const_requires_storage(expr, declaring_module, mut cache,
				mut visiting)
		}
	}
	if !required && node.kind !in [.sizeof_expr, .offsetof_expr] {
		for index in 0 .. node.children_count {
			if b.native_const_requires_storage(b.a.child(node, index), module_name, mut cache,
				mut visiting) {
				required = true
				break
			}
		}
	}
	visiting[int(id)] = false
	cache[int(id)] = required
	return required
}

fn (mut b Builder) register_native_initializers() {
	mut module_name := 'main'
	mut cache := map[int]bool{}
	mut visiting := map[int]bool{}
	for node in b.a.nodes {
		if node.kind == .file {
			module_name = 'main'
		} else if node.kind == .module_decl {
			module_name = node.value
		}
		if node.kind !in [.const_decl, .global_decl] {
			continue
		}
		for index in 0 .. node.children_count {
			field_id := b.a.child(&node, index)
			field := b.a.node(field_id)
			if field.children_count == 0 || field.value.starts_with('C.') {
				continue
			}
			name := native_global_name(module_name, field.value)
			if name in ['g_main_argc', 'g_main_argv'] {
				continue
			}
			expr_id := b.a.child(field, 0)
			is_const := node.kind == .const_decl
			if is_const {
				if !b.native_const_requires_storage(expr_id, module_name, mut cache, mut visiting) {
					continue
				}
				b.cur_module = module_name
				type_name := b.native_const_storage_type_name(*field, expr_id, module_name)
				typ := b.resolve_struct_storage_type(type_name, module_name)
				addr := b.m.add_global('__const_' + ssa_c_name(name), typ)
				b.global_vars[name] = addr
				b.global_type_names[name] = type_name
				b.global_aliases[module_name + '.' + field.value] = name
			}
			b.native_initializers[name] = NativeInitializer{
				module_name: module_name
				field:       field_id
				is_const:    is_const
				function:    -1
			}
			b.native_initializer_order << name
		}
	}
}

fn (b &Builder) native_const_storage_type_name(field flat.Node, expr_id flat.NodeId, module_name string) string {
	if b.tc != unsafe { nil } {
		if typ := b.tc.const_types[module_name + '.' + field.value] {
			if typ !is types.Unknown && typ !is types.Void {
				return types.unalias_type(typ).name()
			}
		}
	}
	if field.typ.len > 0 && field.typ != 'unknown' {
		return field.typ
	}
	expr := b.a.node(expr_id)
	if expr.kind == .array_literal {
		if b.is_fixed_array_type_name(expr.typ) {
			return expr.typ
		}
		element := if expr.children_count > 0 {
			b.native_const_storage_type_name(flat.Node{}, b.a.child(expr, 0), module_name)
		} else {
			'int'
		}
		return '[]' + element
	}
	if expr.kind in [.array_init, .struct_init] {
		return expr.value
	}
	checked := b.checked_expr_type_name(expr_id)
	if checked.len > 0 {
		return checked
	}
	if expr.kind == .call && expr.children_count > 0 {
		callee := b.qualified_expr_name(b.a.child(expr, 0))
		for node in b.a.nodes {
			if node.kind == .fn_decl && node.value == callee && node.typ.len > 0 {
				return qualify_type_ref_name(node.typ, module_name)
			}
		}
	}
	return b.infer_v_type(expr_id)
}

fn (b &Builder) native_initializer_references(function int, names map[ValueID]string, mut visited map[int]bool, mut references map[string]bool) {
	if function < 0 || function >= b.m.funcs.len || visited[function] {
		return
	}
	visited[function] = true
	for block in b.m.funcs[function].blocks {
		for value_id in b.m.blocks[block].instrs {
			instruction := b.m.instrs[b.m.values[value_id].index]
			if instruction.op in [.br, .jmp, .switch_] {
				continue
			}
			for index, operand in instruction.operands {
				if instruction.op == .phi && index % 2 == 1 {
					continue
				}
				if name := names[operand] {
					references[name] = true
				}
			}
			if instruction.op in [.call, .call_sret] && instruction.operands.len > 0 {
				callee := b.m.values[instruction.operands[0]]
				if callee.kind == .func_ref {
					b.native_initializer_references(callee.index, names, mut visited, mut references)
				}
			}
		}
	}
}

fn (mut b Builder) build_native_initializer_functions() {
	mut names := map[ValueID]string{}
	for name, _ in b.native_initializers {
		if addr := b.global_vars[name] {
			names[addr] = name
		}
	}
	mut required := map[string]bool{}
	mut visited := map[int]bool{}
	for function in 0 .. b.m.funcs.len {
		b.native_initializer_references(function, names, mut visited, mut required)
	}
	mut pending := []string{}
	for name in b.native_initializer_order {
		if !b.native_initializers[name].is_const || required[name] {
			pending << name
		}
	}
	mut index := 0
	for index < pending.len {
		name := pending[index]
		index++
		mut initializer := b.native_initializers[name]
		if initializer.function >= 0 {
			continue
		}
		initializer.function = b.register_synthetic_function('__ssa_init_' + ssa_c_name(name),
			b.void_type, [])
		b.cur_func = initializer.function
		b.cur_func_ret_type = ''
		b.cur_module = initializer.module_name
		b.reset_function_state()
		b.cur_block = b.m.add_block(initializer.function, 'entry')
		b.build_native_global_initializer(*b.a.node(initializer.field))
		b.emit0(.ret, b.void_type)
		mut dependencies := map[string]bool{}
		visited.clear()
		b.native_initializer_references(initializer.function, names, mut visited, mut dependencies)
		for dependency in dependencies.keys() {
			if dependency != name {
				initializer.dependencies << dependency
				pending << dependency
			}
		}
		b.native_initializers[name] = initializer
	}
}

fn (mut b Builder) emit_native_initializer(name string, mut initialized map[string]bool) {
	if initialized[name] {
		return
	}
	initialized[name] = true
	initializer := b.native_initializers[name] or { return }
	if initializer.function < 0 {
		return
	}
	for dependency in initializer.dependencies {
		b.emit_native_initializer(dependency, mut initialized)
	}
	function := b.m.funcs[initializer.function]
	callee := b.m.add_value(.func_ref, b.void_type, function.name, initializer.function)
	b.emit1(.call, b.void_type, callee)
}
