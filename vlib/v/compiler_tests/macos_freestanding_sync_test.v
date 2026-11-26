import os
import time

fn test_macos_amd64_raw_sync_contention_and_deadlines() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_raw_sync_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VMODROOT, 'vlib/builtin/testdata/macos_sync/main.v')
	for prod in [false, true] {
		binary := os.join_path(root, 'sync_${prod}')
		mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc',
			'clang', '-o', binary]
		if prod { command << '-prod' }
		command << fixture
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		mut process := os.new_process(binary)
		process.set_redirect_stdio()
		process.run()
		watch := time.new_stopwatch()
		for process.is_alive() && watch.elapsed() < 15 * time.second {
			time.sleep(time.millisecond)
		}
		expired := process.is_alive()
		if expired { process.signal_kill() }
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert !expired && code == 0 && output.trim_space() == 'raw macOS synchronization ok', output
		loads := os.exec(['otool', '-l', binary])
		assert loads.exit_code == 0 && !loads.output.contains('LC_LOAD_DYLIB')
			&& !loads.output.contains('LC_LOAD_DYLINKER'), loads.output
		undefined := os.exec(['nm', '-u', binary])
		assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
	}
}
