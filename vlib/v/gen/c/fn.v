	return false
}

fn (mut g FlatGen) gen_executable_cleanup_registration() {
	if g.module_cleanup_fns.len > 0 || g.is_trace_calls {
		g.writeln('atexit(_vcleanup);')
	}
}

	if g.has_builtins {
		g.writeln('\tg_main_argc = 0;')
		g.writeln('\tg_main_argv = NULL;')
	}
	g.gen_trace_startup()
	g.gen_profile_startup_enable()
	if g.runtime_init_is_needed() {
		g.writeln('\t_vinit();')
	}
			g.writeln('\tg_main_argv = argv;')
		}
		g.gen_compiler_vexe_env_setup()
		g.gen_coverage_registration()
		g.gen_trace_startup()
		g.gen_profile_startup_enable()
		if g.runtime_init_is_needed() {
			g.writeln('\t_vinit();')
		}
	if is_direct_no_main_export {
		g.writeln('_vno_main_init_caller();')
	}
	g.gen_function_defer_prelude()
	g.gen_trace_fn_begin(node, module_name)
	g.gen_profile_fn_begin(generated_fn_name, module_name, node.value, g.tc.declaration_has_attribute(node_id, 'inline'))

	for i in 0 .. node.children_count {
		id := g.a.child(&node, i)
		g.writeln('\tg_main_argv = argv;')
	}
	g.gen_compiler_vexe_env_setup()
	g.gen_coverage_registration()
	g.gen_trace_startup()
	g.gen_profile_startup_enable()
	needs_no_main_runtime_init_caller := g.needs_no_main_runtime_init_caller()
	if needs_no_main_runtime_init_caller {
		// Exported callbacks can be invoked without main. Share their guarded
		g.gen_profile_registration()
	}
	g.indent++
	g.gen_function_defer_prelude()
	g.gen_trace_call('main main.main/0', 'main', 'main.main')
	g.gen_profile_fn_begin('main', 'main', 'main', false)
	for stmt in stmts {
		g.tc.cur_file = stmt.file
		g.tc.cur_module = stmt.module
		g.writeln('\tg_main_argv = argv;')
	}
	g.gen_compiler_vexe_env_setup()
	g.gen_coverage_registration()
	g.gen_trace_startup()
	g.gen_profile_startup_enable()
	if g.runtime_init_is_needed() {
		g.writeln('\t_vinit();')
	}
