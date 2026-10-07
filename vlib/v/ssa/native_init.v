module ssa

import v.flat

// build_native_initialization initializes globals and module state before main.
fn (mut b Builder) build_native_initialization() {
	if b.skip_fn_bodies || b.hot_fn.len > 0 {
		return
	}
	main_id := b.fn_ids['main'] or { return }
	if b.m.funcs[main_id].blocks.len == 0 {
		return
	}
	b.build_native_initializer_functions()
	init_id := b.register_synthetic_function('__ssa_init', b.void_type, [])
	b.cur_func = init_id
	b.cur_func_ret_type = ''
	b.cur_module = 'main'
	b.reset_function_state()
	b.cur_block = b.m.add_block(init_id, 'entry')
	mut module_imports := map[string][]string{}
	mut init_functions := map[string]int{}
	mut modules := ['main']
	for node in b.a.nodes {
		match node.kind {
			.file { b.cur_module = 'main' }
			.module_decl {
				b.cur_module = node.value
				if node.value !in modules {
					modules << node.value
				}
			}
			.import_decl { module_imports[b.cur_module] << node.value }
			.fn_decl {
				if node.value == 'init' || (b.cur_module == 'builtin' && node.value == 'builtin_init') {
					name := ssa_fn_name_in_module(b.cur_module, node.value)
					if fn_id := b.fn_ids[name] {
						init_functions[b.cur_module] = fn_id
					}
				}
			}
			else {}
		}
	}
	mut visited := map[string]bool{}
	mut order := []string{}
	if 'builtin' in modules {
		native_module_init_order('builtin', module_imports, mut visited, mut order)
	}
	for name in modules {
		native_module_init_order(name, module_imports, mut visited, mut order)
	}
	mut initialized := map[string]bool{}
	for name in order {
		b.cur_module = name
		b.reset_function_state()
		for initializer_name in b.native_initializer_order {
			if b.native_initializers[initializer_name].module_name == name {
				b.emit_native_initializer(initializer_name, mut initialized)
			}
		}
		if fn_id := init_functions[name] {
			function := b.m.funcs[fn_id]
			callee := b.m.add_value(.func_ref, b.void_type, function.name, fn_id)
			b.emit1(.call, b.void_type, callee)
		}
	}
	b.emit0(.ret, b.void_type)
	entry := b.m.funcs[main_id].blocks[0]
	callee := b.m.add_value(.func_ref, b.void_type, '__ssa_init', init_id)
	call := b.m.add_instr(.call, entry, b.void_type, [callee])
	b.m.blocks[entry].instrs.delete_last()
	b.m.blocks[entry].instrs.prepend(call)
}

fn native_module_init_order(name string, imports map[string][]string, mut visited map[string]bool, mut order []string) {
	if visited[name] {
		return
	}
	visited[name] = true
	for dependency in imports[name] {
		native_module_init_order(dependency, imports, mut visited, mut order)
	}
	order << name
}

fn native_global_name(module_name string, name string) string {
	if name.contains('.') {
		return name
	}
	return ssa_fn_name_in_module(module_name, name)
}

fn (b &Builder) native_global_selector_addr(node flat.Node) ?ValueID {
	mut root := node
	for root.kind == .selector && root.children_count > 0 {
		root = b.a.nodes[int(b.a.child(&root, 0))]
	}
	if root.kind == .ident && root.value in b.vars {
		return none
	}
	name := b.selector_qualified_name(node)
	if name.starts_with('C.') {
		if addr := b.global_vars[name] {
			value := b.m.values[addr]
			if value.kind == .global && b.m.globals[value.index].linkage == .external {
				return addr
			}
		}
		return none
	}
	if addr := b.global_vars[name] {
		return addr
	}
	if canonical := b.global_aliases[name] {
		if addr := b.global_vars[canonical] {
			return addr
		}
	}
	alias := name.all_before('.')
	if module_name := b.global_imports[b.cur_module + '#' + alias] {
		if addr := b.global_vars[module_name + '.' + name.all_after('.')] {
			return addr
		}
	}
	return none
}

fn (mut b Builder) load_native_global(addr ValueID) ValueID {
	typ := b.deref_type(addr)
	if b.m.type_store.types[typ].kind == .array_t {
		return b.emit1(.bitcast, b.m.type_store.get_ptr(b.m.type_store.types[typ].elem_type), addr)
	}
	return b.emit1(.load, typ, addr)
}

fn (mut b Builder) build_native_global_initializers(node flat.Node) {
	for i in 0 .. node.children_count {
		field := b.a.child_node(&node, i)
		b.build_native_global_initializer(*field)
	}
}

fn (mut b Builder) build_native_global_initializer(field flat.Node) {
	if field.children_count == 0 {
		return
	}
	name := native_global_name(b.cur_module, field.value)
	if name in ['g_main_argc', 'g_main_argv'] {
		// main's native entry already saved the process arguments here.
		return
	}
	addr := b.global_vars[name] or { return }
	expr_id := b.a.child(&field, 0)
	expr := b.a.node(expr_id)
	if expr.kind == .selector && b.selector_qualified_name(*expr) == 'C.PTHREAD_MUTEX_INITIALIZER' {
		// Darwin's static mutex initializer is an opaque struct, not zero.
		ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
		mutex := b.emit1(.bitcast, ptr_i8, addr)
		nil_attr := b.m.get_or_add_const(ptr_i8, '0')
		callee := b.m.add_value(.func_ref, b.void_type, 'pthread_mutex_init', b.fn_ids['pthread_mutex_init'])
		b.emit3(.call, b.i32_type, callee, mutex, nil_attr)
		return
	}
	typ := b.deref_type(addr)
	if b.m.type_store.types[typ].kind == .array_t {
		mut fixed_expr := *expr
		fixed_expr.value = b.global_type_names[name]
		value := if expr.kind in [.array_init, .array_literal] {
			b.build_fixed_array_init(fixed_expr)
		} else {
			b.build_expr(expr_id)
		}
		ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
		dest := b.emit1(.bitcast, ptr_i8, addr)
		source := b.emit1(.bitcast, ptr_i8, value)
		length := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(typ)}')
		callee := b.m.add_value(.func_ref, ptr_i8, 'memcpy', b.fn_ids['memcpy'])
		b.emit4(.call, ptr_i8, callee, dest, source, length)
		return
	}
	mut value := b.build_expr(expr_id)
	if b.is_int_type(typ) && b.is_int_type(b.value_type(value)) {
		value = b.coerce_int_value(value, typ)
	}
	b.emit2(.store, b.void_type, value, addr)
}
