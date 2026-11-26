module context

import os
import time

fn test_linux_freestanding_context_repetition_releases_worker_stacks() ! {
	$if (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_context_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		for role in ['debug', 'prod', 'prealloc'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none']
			if role == 'prod' { command << '-prod' }
			if role == 'prealloc' { command << '-prealloc' }
			command << ['-o', program,
				os.join_path(@VEXEROOT, 'vlib', 'context', 'testdata', 'lifecycle')]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			mut process := os.new_process(program)
			process.set_redirect_stdio()
			process.run()
			deadline := time.sys_mono_now() + u64(15 * time.second)
			for process.is_alive() && time.sys_mono_now() < deadline {
				time.sleep(10 * time.millisecond)
			}
			if process.is_alive() { process.signal_kill() }
			process.wait()
			output := process.stdout_slurp() + process.stderr_slurp()
			code := process.code
			process.close()
			assert code == 0, '${role}: ${output}'
			assert output.contains('freestanding context lifecycle passed')
		}
	}
}
