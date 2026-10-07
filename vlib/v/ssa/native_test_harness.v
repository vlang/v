module ssa

import v.flat

// Native test builds use the driver's selected files and function-name globs.
// Assertions already terminate a native executable when they fail.
fn (mut b Builder) build_native_test_main(test_files []string, run_only []string) {
	if test_files.len == 0 || b.skip_fn_bodies || b.hot_fn.len > 0 {
		return
	}
	mut tests := []string{}
	mut hooks := map[string]string{}
	for file_idx, file in b.a.nodes {
		if file_idx < b.a.user_code_start || file.kind != .file || file.value !in test_files {
			continue
		}
		mut module_name := 'main'
		for i in 0 .. file.children_count {
			child := b.a.child_node(&file, i)
			if child.kind == .module_decl {
				module_name = child.value
				break
			}
		}
		mut declarations := []flat.NodeId{}
		b.collect_native_test_declarations(file, mut declarations)
		for id in declarations {
			node := b.a.nodes[int(id)]
			name := ssa_fn_name_in_module(module_name, node.value)
			if node.value in ['testsuite_begin', 'testsuite_end', 'before_each', 'after_each'] {
				if node.value !in hooks {
					hooks[node.value] = name
				}
			} else if node.value.starts_with('test_')
				&& native_test_matches_run_only(module_name, node.value, run_only) {
				tests << name
			}
		}
	}
	main_id := if existing := b.fn_ids['main'] {
		// A test harness owns the entrypoint, including when a file declares main.
		b.m.funcs[existing].blocks = []BlockID{}
		b.m.funcs[existing].typ = b.i32_type
		existing
	} else {
		b.register_synthetic_function('main', b.i32_type, [])
	}
	b.cur_func = main_id
	b.cur_func_ret_type = 'int'
	b.cur_module = 'main'
	b.reset_function_state()
	b.cur_block = b.m.add_block(main_id, 'entry')
	if name := hooks['testsuite_begin'] {
		b.emit_native_test_call(name)
	}
	for name in tests {
		if hook := hooks['before_each'] {
			b.emit_native_test_call(hook)
		}
		b.emit_native_test_call(name)
		if hook := hooks['after_each'] {
			b.emit_native_test_call(hook)
		}
	}
	if name := hooks['testsuite_end'] {
		b.emit_native_test_call(name)
	}
	zero := b.m.get_or_add_const(b.i32_type, '0')
	b.emit1(.ret, b.void_type, zero)
}

fn (b &Builder) collect_native_test_declarations(node flat.Node, mut ids []flat.NodeId) {
	for i in 0 .. node.children_count {
		id := b.a.child(&node, i)
		if int(id) < b.a.user_code_start {
			continue
		}
		child := b.a.nodes[int(id)]
		if child.kind == .fn_decl {
			ids << id
		} else if child.kind == .block {
			b.collect_native_test_declarations(child, mut ids)
		}
	}
}

fn native_test_matches_run_only(module_name string, name string, patterns []string) bool {
	if patterns.len == 0 {
		return true
	}
	for pattern in patterns {
		if name.match_glob(pattern) || '${module_name}.${name}'.match_glob(pattern) {
			return true
		}
	}
	return false
}

fn (mut b Builder) emit_native_test_call(name string) {
	function_id := b.fn_ids[name] or { panic('native test function was not registered: ${name}') }
	function := b.m.funcs[function_id]
	if function.blocks.len == 0 || function.params.len != 0 {
		panic('native test function has no runnable zero-argument body: ${name}')
	}
	callee := b.m.add_value(.func_ref, b.void_type, name, function_id)
	result := b.emit1(.call, function.typ, callee)
	if b.is_option_type(function.typ) {
		slot := b.emit0(.alloca, b.m.type_store.get_ptr(function.typ))
		b.emit2(.store, b.void_type, result, slot)
		ok_ptr := b.get_field_ptr(slot, 'ok')
		ok := b.emit1(.load, b.i1_type, ok_ptr)
		passed := b.m.add_block(b.cur_func, 'test_passed')
		failed := b.m.add_block(b.cur_func, 'test_propagation_failed')
		b.emit3(.br, b.void_type, ok, ValueID(passed), ValueID(failed))
		b.cur_block = failed
		message := b.m.add_value(.string_literal, b.str_type, 'native test ${name} failed propagation', 0)
		b.emit_runtime_call('eprintln', b.void_type, [message])
		one := b.m.get_or_add_const(b.i64_type, '1')
		b.emit_runtime_call('exit', b.void_type, [one])
		b.emit0(.unreachable, b.void_type)
		b.cur_block = passed
	} else if function.typ != b.void_type {
		panic('native test function has an unsupported return type: ${name}')
	}
}
