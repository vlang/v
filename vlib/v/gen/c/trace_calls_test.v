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
	g.gen_profile_fn_begin('trace_calls__on_call', 'trace_calls', 'on_call', false)
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
