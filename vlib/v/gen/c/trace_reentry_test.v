module c

import os
import time
import v.pref
import v.types

fn test_trace_hook_tls_does_not_match_unrelated_globals() {
	for owner in ['trace_calls', 'v.trace_calls'] {
		mut g := FlatGen.new()
		g.target = pref.target_from('linux', 'amd64') or { panic(err) }
		if owner == 'v.trace_calls' {
			g.global_types['v.trace_calls.g_stack_base'] = types.Type(types.bool_)
		}
		name := '${owner}.g_trace_in_hook'
		g.global_modules[name] = owner
		g.global_modules['main.g_trace_in_hook'] = 'main'
		g.global_modules['${owner}.unrelated'] = owner
		assert g.global_is_thread_local(name)
		assert !g.global_is_thread_local('main.g_trace_in_hook')
		assert !g.global_is_thread_local('${owner}.unrelated')
		if owner == 'v.trace_calls' {
			g.global_modules['trace_calls.g_trace_in_hook'] = 'trace_calls'
			assert !g.global_is_thread_local('trace_calls.g_trace_in_hook')
		}
	}
}

fn test_trace_output_can_reenter_v_and_preserves_thread_errno() ! {
	$if !linux {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'trace_reentry_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	binary := os.join_path(root, 'probe')
	fixture := os.join_path(@VEXEROOT, 'vlib/v/gen/c/testdata/trace_reentry/main.c.v')
	built := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-no-retry-compilation', '-trace-calls',
		'-cflags', '-fno-builtin-fprintf', '-o', binary, fixture])
	assert built.exit_code == 0, built.output
	mut process := os.new_process(binary)
	process.set_redirect_stdio()
	process.run()
	watch := time.new_stopwatch()
	mut chunks := []string{}
	for process.is_alive() && watch.elapsed() < 10 * time.second {
		if data := process.pipe_read(.stdout) { chunks << data }
		if data := process.pipe_read(.stderr) { chunks << data }
		time.sleep(time.millisecond)
	}
	expired := process.is_alive()
	if expired { process.signal_kill() }
	process.wait()
	chunks << process.stdout_slurp()
	chunks << process.stderr_slurp()
	code := process.code
	process.close()
	output := chunks.join('')
	assert !expired && code == 0, output
	assert output.contains('trace guard ok'), output
	assert !output.contains('main.reentrant_callback/'), output
	assert output.split_into_lines().filter(it.contains('main.work/')).len == 2, output
}
