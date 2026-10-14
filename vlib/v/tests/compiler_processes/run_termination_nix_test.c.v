module main

import os
import time

fn test_run_and_inline_forward_termination_and_clean_files() ! {
	for signal_number in [1, 15] {
		for inline in [false, true] {
			root := os.join_path(os.vtmp_dir(), 'run_signal_${os.getpid()}_${time.now().unix_nano()}')
			os.mkdir_all(root)!
			defer { os.rmdir_all(root) or {} }
			ready := os.join_path(root, 'ready')
			program := "import os\nimport time\nfn main() { os.write_file('${ready}', os.getpid().str()) or { panic(err) }; time.sleep(30 * time.second) }\n"
			mut args := ['-nocache', '-gc', 'none']
			if inline {
				args << ['-e', program]
			} else {
				source := os.join_path(root, 'main.v')
				os.write_file(source, program)!
				args << ['run', source]
			}
			mut process := os.new_process(@VEXE)
			process.set_args(args)
			process.set_work_folder(root)
			mut environment := os.environ()
			environment.delete('VEXE')
			environment.delete('VFLAGS')
			environment['V_MACOS_V3_NO_FALLBACK'] = '1'
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			defer {
				if process.is_alive() { process.signal_kill() }
				process.wait()
				process.close()
			}
			deadline := time.now().add(45 * time.second)
			for !os.exists(ready) && process.is_alive() && time.now() < deadline {
				time.sleep(10 * time.millisecond)
			}
			assert os.exists(ready), 'child did not become ready'
			child_pid := os.read_file(ready)!.int()
			mut child_completed := false
			defer {
				if !child_completed { C.kill(child_pid, 9) }
			}
			assert C.kill(process.pid, signal_number) == 0
			stop_deadline := time.now().add(10 * time.second)
			for process.is_alive() && time.now() < stop_deadline {
				time.sleep(10 * time.millisecond)
			}
			assert !process.is_alive(), 'compiler did not stop after signal ${signal_number}'
			process.wait()
			assert process.code == 128 + signal_number, process.stderr_slurp()
			assert C.kill(child_pid, 0) == -1, 'run child still exists'
			child_completed = true
			for entry in os.ls(root)! {
				assert !entry.starts_with('.v3_inline_'), entry
				assert entry != 'main', 'implicit executable survived termination'
			}
		}
	}
}
