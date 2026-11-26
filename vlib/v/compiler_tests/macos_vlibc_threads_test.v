import os
import time

fn test_macos_vlibc_threads_select_raw_or_host_runtime() ! {
	work := os.join_path(os.vtmp_dir(), 'macos_vlibc_selection_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_vlibc_threads/main.v')
	for role in ['raw', 'arm64', 'explicit_host', 'shared', 'object'] {
		output := os.join_path(work, '${role}.c')
		mut command := [@VEXE, '-no-retry-compilation', '-nocache', '-gc', 'none', '-os', 'macos',
			'-arch', if role == 'arm64' { 'arm64' } else { 'amd64' }, '-freestanding', '-enable-globals',
			'-d', 'vlibc']
		match role {
			'explicit_host' { command << ['-d', 'v_bare_no_startup'] }
			'shared' { command << '-shared' }
			'object' { command << '-is_o' }
			else {}
		}
		command << ['-o', output, source]
		built := os.exec(command)
		assert built.exit_code == 0, '${role}: ${built.output}'
		code := os.read_file(output)!
		if role == 'raw' {
			assert code.contains('t.thread = bare_thread_create('), role
			assert !code.contains('pthread_create(&t.thread,'), role
			assert !code.contains('pthread_join(t->thread,'), role
		} else {
			assert code.contains('pthread_create(&t.thread,'), role
			assert code.contains('pthread_join(t->thread,'), role
			assert !code.contains('t.thread = bare_thread_create('), role
		}
	}
}

fn test_macos_vlibc_thread_ownership_and_raw_imports() ! {
	$if !macos {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'macos_vlibc_threads_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_vlibc_threads/main.v')
	for optimized in [false, true] {
		for autofree in [false, true] {
			program := os.join_path(work, 'threads-${optimized}-${autofree}')
			mut command := [@VEXE, '-no-retry-compilation', '-cc', 'clang', '-gc', 'none',
				'-freestanding', '-d', 'vlibc', '-enable-globals']
			if optimized { command << '-prod' }
			if autofree { command << '-autofree' }
			command << ['-o', program, source]
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			mut process := os.new_process(program)
			process.set_redirect_stdio()
			process.run()
			deadline := time.sys_mono_now() + 10 * i64(time.second)
			for process.is_alive() && time.sys_mono_now() < deadline {
				time.sleep(time.millisecond)
			}
			if process.is_alive() { process.signal_kill() }
			process.wait()
			output := process.stdout_slurp() + process.stderr_slurp()
			code := process.code
			process.close()
			assert code == 0 && output == 'vlibc thread ownership passed\n', output
			$if amd64 {
				undefined := os.exec(['nm', '-u', program])
				assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
				loads := os.exec(['otool', '-l', program])
				assert loads.exit_code == 0 && !loads.output.contains('LC_LOAD_DYLIB'), loads.output
			}
		}
	}
}
