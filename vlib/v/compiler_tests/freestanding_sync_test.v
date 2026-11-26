import os
import time

fn test_freestanding_sync_waits_and_io_pipe() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_sync_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixtures := os.join_path(@VMODROOT, 'vlib/sync/testdata/freestanding')
	for production in [false, true] {
		for with_vlibc in [false, true] {
			output := os.join_path(root, 'sync_${production}_${with_vlibc}')
			mut command := [@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
				'-gc', 'none', '-cstrict', '-o', output]
			if production { command << '-prod' }
			if with_vlibc { command << ['-d', 'vlibc'] }
			command << os.join_path(fixtures, 'main.c.v')
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			ran := run_freestanding_sync_binary(output)
			assert ran.exit_code == 0, ran.output
			assert ran.output.trim_space() == 'freestanding sync ok', ran.output

			command[command.len - 1] = os.join_path(fixtures, 'overflow', 'main.v')
			overflow := os.exec(command)
			assert overflow.exit_code == 0, overflow.output
			failed := run_freestanding_sync_binary(output)
			assert failed.exit_code != 0 && failed.output.contains('semaphore count overflow'), failed.output
		}
	}
}

fn run_freestanding_sync_binary(binary string) os.Result {
	mut process := os.new_process(binary)
	process.set_redirect_stdio()
	process.run()
	defer { process.close() }
	elapsed := time.new_stopwatch()
	mut timed_out := false
	for process.is_alive() {
		if elapsed.elapsed() > 30 * time.second {
			timed_out = true
			process.signal_kill()
			break
		}
		time.sleep(time.millisecond)
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	return os.Result{
		exit_code: if timed_out { -1 } else { process.code }
		output:    if timed_out { 'timed out after 30 seconds\n' + output } else { output }
	}
}
