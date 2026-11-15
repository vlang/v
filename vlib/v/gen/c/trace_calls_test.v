module c

import v.flat
import v.types

fn test_trace_function_selectors() {
	assert trace_fn_matches([], 'main', 'main.main')
	assert trace_fn_matches(['main.main'], 'main', 'main.main')
	assert trace_fn_matches(['println'], 'builtin', 'println')
	assert trace_fn_matches(['builtin'], 'builtin', 'println')
	assert trace_fn_matches(['os'], 'os', 'os.join_path')
	assert trace_fn_matches(['main.*'], 'main', 'main.Worker.run')
	assert trace_fn_matches(['run'], 'main', 'main.Worker.run')
	assert trace_fn_matches(['missing', '_vinit'], '', '_vinit')
	assert !trace_fn_matches(['main'], '', 'C.main')
	assert trace_fn_matches(['C.main'], '', 'C.main')
	assert !trace_fn_matches(['other'], 'main', 'main.main')
}

fn test_trace_function_labels() {
	assert trace_qualified_fn_name('main', 'main') == 'main.main'
	assert trace_qualified_fn_name('', 'run') == 'main.run'
	assert trace_qualified_fn_name('main', 'Worker.run') == 'main.Worker.run'
	assert trace_qualified_fn_name('os', 'os.join_path') == 'os.join_path'
	assert trace_qualified_fn_name('os', 'join_path') == 'os.join_path'
	assert trace_qualified_fn_name('builtin', 'println') == 'println'
	assert trace_qualified_fn_name('builtin', 'builtin.println') == 'println'
}

fn test_entry_tracing_is_independent_of_custom_call_site_hooks() {
	mut g := FlatGen.new()
	g.set_compile_values({
		'trace': '1'
	})
	g.set_trace_calls(true, ['main.*'])
	assert g.trace_calls
	assert g.is_trace_calls
	g.set_compile_values({})
	assert !g.trace_calls
	assert g.is_trace_calls
	g.set_trace_calls(false, [])
	g.gen_trace_call('main main.main/0', 'main', 'main.main')
	g.gen_trace_startup()
	assert g.sb.len == 0
}

fn test_trace_runtime_is_not_instrumented_or_profiled() {
	mut g := FlatGen.new()
	g.set_trace_calls(true, [])
	g.set_profile('-', false, [])
	g.gen_trace_fn_begin(flat.Node{}, 'trace_calls')
	g.gen_profile_fn_begin('trace_calls__on_call', 'trace_calls', 'on_call', false, false)
	assert g.sb.len == 0
	assert !g.profile_fn_active
}

fn test_parallel_codegen_inherits_entry_tracing() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.set_trace_calls(true, ['main.*', 'println'])
	worker := g.new_parallel_worker(1)
	assert worker.is_trace_calls
	assert worker.trace_fns == ['main.*', 'println']
	assert !worker.trace_calls
}

fn test_call_site_tracing_keeps_v_functions_next_to_builtin_c_declarations() {
	for hook_module in ['debug', 'v.debug'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.trace_calls = true
		void_type := tc.intern_type_reference(types.Type(types.void_))
		g.fn_decl_ret_types[fn_decl_module_key(hook_module, 'before_call_hook')] = void_type
		g.fn_decl_ret_types[fn_decl_module_key(hook_module, 'after_call_hook')] = void_type
		assert g.trace_hook_module_name() == hook_module
		// C.abs is declared by builtin, while this program has its own V abs.
		g.fn_decl_ret_types[fn_decl_module_key('builtin', 'abs')] = tc.intern_type_reference(types.Type(types.int_))
		g.fn_decl_ret_types[fn_decl_module_key('main', 'abs')] = tc.intern_type_reference(types.Type(types.string_))
		g.non_generic_fn_names_by_module[fn_decl_module_key('main', 'abs')] = true
		abs := flat.Node{ kind: .ident, value: 'abs' }
		assert g.trace_call_name(&abs, 'abs', 'abs', 'abs') or { '' } == 'abs'
		assert g.trace_call_name(&abs, 'abs', 'C.abs', 'C.abs') == none
		// Actual V builtin functions still do not invoke the custom hooks.
		g.fn_decl_ret_types[fn_decl_module_key('builtin', 'println')] = void_type
		g.non_generic_fn_names_by_module[fn_decl_module_key('builtin', 'println')] = true
		println_node := flat.Node{ kind: .ident, value: 'println' }
		assert g.trace_call_name(&println_node, 'println', 'println', 'println') == none
		g.fn_decl_ret_types[fn_decl_module_key('builtin', 'int.str')] = tc.intern_type_reference(types.Type(types.string_))
		g.non_generic_fn_names_by_module[fn_decl_module_key('builtin', 'int.str')] = true
		method := flat.Node{ kind: .ident, value: 'int.str' }
		assert g.trace_call_name(&method, 'int.str', 'int.str', 'int.str') == none
		tc.cur_module = hook_module
		assert g.trace_call_name(&abs, 'abs', 'abs', 'abs') == none
	}
}
