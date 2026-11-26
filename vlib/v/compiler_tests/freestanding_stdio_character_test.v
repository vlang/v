import os
import time

fn test_freestanding_stdio_character_state_and_thread_ownership() ! {
	$if linux && (amd64 || arm64) {
		root := os.join_path(os.vtmp_dir(), 'freestanding_stdio_character_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/freestanding_compat/stdio_character.c.v')
		for compiler in ['gcc', 'clang'] {
			for production in [false, true] {
				binary := os.join_path(root, '${compiler}_${production}')
				mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-syscalls', '-vlibc',
					'-no-system-libc', '-cc', compiler, '-o', binary]
				if production { command << '-prod' }
				command << source
				built := os.exec(command)
				assert built.exit_code == 0, built.output
				mut process := os.new_process(binary)
				process.set_args([os.join_path(root, 'characters')])
				process.set_redirect_stdio()
				process.run()
				deadline := time.sys_mono_now() + 15 * i64(time.second)
				for process.is_alive() && time.sys_mono_now() < deadline {
					time.sleep(time.millisecond)
				}
				if process.is_alive() { process.signal_kill() }
				process.wait()
				output := process.stdout_slurp() + process.stderr_slurp()
				code := process.code
				process.close()
				assert code == 0, '${compiler}/${production}: ${output}'
				assert output.trim_space() == 'stdio character ownership passed', output
			}
		}
	}
}
