import os
import time

fn test_freestanding_v_spawn_wait_results_detach_and_context_deadline() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_v_threads_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_threads', 'main.v')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${role}: ${compiled.output}'
			code, output := run_freestanding_thread_probe(program, [])
			assert code == 0, '${role}: ${output}'
			assert output.trim_space() == 'freestanding V threads passed'
		}
	}
}

fn test_freestanding_raw_thread_failure_join_and_late_detach() ! {
	$if (linux && (amd64 || arm64)) || (macos && amd64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_raw_threads_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_threads', 'raw.c.v')
		program := os.join_path(work, 'raw')
		compiled := os.exec([@VEXE, '-freestanding', '-gc', 'none', '-enable-globals', '-o', program,
			source])
		assert compiled.exit_code == 0, compiled.output
		code, output := run_freestanding_thread_probe(program, [])
		assert code == 0, output
		assert output.trim_space() == 'freestanding raw thread ownership passed'
	}
}

fn run_freestanding_thread_probe(program string, args []string) (int, string) {
	mut process := os.new_process(program)
	process.set_args(args)
	process.set_redirect_stdio()
	process.run()
	deadline := time.sys_mono_now() + 10 * i64(time.second)
	for process.is_alive() && time.sys_mono_now() < deadline { time.sleep(10 * time.millisecond) }
	if process.is_alive() { process.signal_kill() }
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	code := process.code
	process.close()
	return code, output
}

fn test_freestanding_library_v_spawn_uses_foreign_host_pthreads() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_v_threads_host_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_threads')
		mut library := os.join_path(work, 'libthreads.so')
		$if macos {
			library = os.join_path(work, 'libthreads.dylib')
		}
		compiled := os.exec([@VEXE, '-freestanding', '-gc', 'none', '-shared', '-o', library,
			os.join_path(fixtures, 'library.v')])
		assert compiled.exit_code == 0, compiled.output
		host := os.join_path(work, 'host')
		mut command := ['cc', '-pthread', '-o', host, os.join_path(fixtures, 'host.c')]
		$if linux {
			command << '-ldl'
		}
		linked := os.exec(command)
		assert linked.exit_code == 0, linked.output
		code, output := run_freestanding_thread_probe(host, [library])
		assert code == 0, output
		assert output.trim_space() == 'freestanding library V threads passed'
	}
}

fn test_freestanding_detached_teardown_blocks_signal_handlers() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_thread_signals_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_threads', 'signals.c.v')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			code, output := run_freestanding_thread_probe(program, [])
			assert code == 0, '${role}: ${output}'
			assert output.trim_space() == 'freestanding thread signal teardown passed'
		}
	}
}

fn test_freestanding_basic_v_spawn_wait_results_and_detach() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_basic_threads_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_threads', 'basic.v')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			code, output := run_freestanding_thread_probe(program, [])
			assert code == 0, '${role}: ${output}'
			assert output.trim_space() == 'freestanding basic V threads passed'
		}
	}
}

fn test_freestanding_sync_native_thread_helper_and_waitgroup() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_sync_threads_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_threads', 'sync_helper.c.v')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			code, output := run_freestanding_thread_probe(program, [])
			assert code == 0, '${role}: ${output}'
			assert output.trim_space() == 'freestanding sync thread helper passed'
		}
	}
}

fn test_freestanding_macos_v_thread_signal_tls_and_teardown() ! {
	$if macos && amd64 {
		work := os.join_path(os.vtmp_dir(), 'macos_v_thread_signals_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_v_threads/signals.c.v')
		for prod in [false, true] {
			program := os.join_path(work, 'signals_${prod}')
			mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none',
				'-enable-globals', '-cc', 'clang']
			if prod { command << '-prod' }
			command << ['-o', program, source]
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			check_freestanding_macos_thread_imports(program)
			code, output := run_freestanding_thread_probe(program, [])
			assert code == 0 && output.trim_space() == 'macOS V thread signal teardown passed', output
		}
	}
}

fn check_freestanding_macos_thread_imports(program string) {
	$if macos && amd64 {
		undefined := os.exec(['xcrun', 'nm', '-u', program])
		assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
		commands := os.exec(['xcrun', 'otool', '-l', program])
		assert commands.exit_code == 0 && commands.output.contains('LC_UNIXTHREAD'), commands.output
		assert !commands.output.contains('LC_LOAD_DYLIB')
		assert !commands.output.contains('LC_LOAD_DYLINKER')
	}
}
