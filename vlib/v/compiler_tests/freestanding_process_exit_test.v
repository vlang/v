import os
import time

fn test_freestanding_process_return_exit_and_worker_panic_terminate_all_threads() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'freestanding_exit_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_process_exit', 'main.v')
	for role in ['debug', 'prod'] {
		program := os.join_path(work, role)
		mut command := [@VEXE, '-no-retry-compilation', '-cc', 'clang', '-freestanding',
			'-enable-globals', '-d', 'vlibc', '-o', program]
		if role == 'prod' { command << '-prod' }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		for mode, expected in {
			'return':       0
			'main-exit':    7
			'worker-exit':  8
			'worker-panic': 1
		} {
			mut child := os.new_process(program)
			child.set_args([mode])
			child.set_redirect_stdio_merged()
			child.run()
			deadline := time.sys_mono_now() + u64(5 * time.second)
			for child.is_alive() && time.sys_mono_now() < deadline { time.sleep(time.millisecond) }
			timed_out := child.is_alive()
			if timed_out { child.signal_kill() }
			child.wait()
			text := child.stdout_slurp()
			child.close()
			assert !timed_out, '${role}/${mode}: surviving worker after process termination'
			assert child.code == expected, '${role}/${mode}: ${child.code}: ${text}'
			if mode == 'worker-panic' {
				assert text.contains('worker process panic probe'), text
			}
		}
	}
}
