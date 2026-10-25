module driver

import os
import v.cmdexec

fn test_cached_pthread_types_keep_the_cold_build_layout() ! {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_pthread_abi_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'layout_test.v')
	program := os.join_path(root, 'program')
	os.write_file(source, 'import os
import sync
fn test_layout() {
	assert os.getpid() > 0
	println(sizeof(sync.Mutex))
	println(sizeof(sync.RwMutex))
	println(sizeof(sync.Semaphore))
}
')!
	mut environment := os.environ()
	environment['VTMP'] = os.join_path(root, 'tmp')
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	environment.delete('VFLAGS')
	environment['V3CACHE'] = os.join_path(root, 'cache')
	mut layouts := []string{}
	for _ in 0 .. 2 {
		mut compiler := os.new_process(@VEXE)
		compiler.set_environment(environment)
		compiler.set_args(['-cc', 'clang', '-skip-running', '-o', program, source])
		compiler.set_redirect_stdio()
		compiler.wait()
		output := compiler.stdout_slurp() + compiler.stderr_slurp()
		code := compiler.code
		compiler.close()
		assert code == 0, output
		result := cmdexec.run_with_timeout(program, []string{}, 10_000)
		assert result.exit_code == 0, result.output
		layouts << result.output.trim_space()
	}
	// Loading os.vh introduces system headers. It must not change the ABI of
	// pthread-backed types shared with the already compiled sync module.
	assert layouts[0] == layouts[1], layouts.str()
}
