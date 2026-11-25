import os
import time

fn test_subprocess_capture_in_syscall_runtimes() ! {
	$if !((linux && (amd64 || arm64)) || (macos && amd64)) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_capture_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/os/testdata/freestanding_process/capture.c.v')
	input := os.join_path(root, 'input')
	os.write_file(input, 'inherited stdin')!
	for flags in [[]string{}, ['-autofree'], ['-syscalls'], ['-vlibc'], ['-vlibc', '-prod'],
		['-syscalls', '-autofree'], ['-vlibc', '-autofree']] {
		binary := os.join_path(root, 'capture')
		mut command := [@VEXE, '-no-retry-compilation', '-cc', 'gcc', '-gc', 'none']
		command << flags
		// Native GUI dependencies export executable symbols. Hosted-only helper
		// objects must be absent even when the linker retains every export.
		if '-vlibc' in flags { command << ['-ldflags', '-Wl,--export-dynamic'] }
		if '-prod' in flags { command << ['-d', 'capture_production'] }
		command << ['-o', binary, source]
		built := os.exec(command)
		assert built.exit_code == 0, '${flags}: ${built.output}'
		mut process := os.new_process(binary)
		process.set_stdin_path(input)
		process.set_work_folder(root)
		process.set_redirect_stdio_merged()
		process.run()
		deadline := time.sys_mono_now() + 60 * i64(time.second)
		for process.is_alive() && time.sys_mono_now() < deadline {
			time.sleep(10 * time.millisecond)
		}
		if process.is_alive() { process.signal_kill() }
		process.wait()
		output := process.stdout_slurp()
		assert process.code == 0, '${flags}: ${process.code}: ${output}'
		assert output.contains('subprocess capture arguments, environment, pipes, inherited stdin, failures and concurrency passed'), output
		process.close()
	}
}
