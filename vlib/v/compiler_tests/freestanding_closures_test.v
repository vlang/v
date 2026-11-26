import os
import time

fn test_freestanding_captured_functions_pool_lifetimes_and_context_cancel() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_closures_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_closures')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none',
				'-enable-globals']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${role}: ${compiled.output}'
			code, output := run_freestanding_closure_probe(program, [])
			assert code == 0, '${role}: ${output}'
			assert output.trim_space() == 'freestanding captured closures passed', role
		}
	}
}

fn test_macos_amd64_freestanding_closure_pages_enforce_write_and_execute_permissions() ! {
	$if macos && amd64 {
		work := os.join_path(os.vtmp_dir(), 'freestanding_closure_protection_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_closures')
		for probe in ['closure_write_code', 'closure_execute_metadata'] {
			program := os.join_path(work, probe)
			compiled := os.exec([@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none',
				'-enable-globals', '-d', probe, '-o', program, source])
			assert compiled.exit_code == 0, compiled.output
			code, output := run_freestanding_closure_probe(program, [])
			assert output.contains(if probe == 'closure_write_code' {
				'closure code write probe reached'
			} else {
				'closure metadata execute probe reached'
			}), output
			// Darwin reports denied memory access as SIGBUS or SIGSEGV. Exit93
			// means the forbidden access succeeded and is explicitly rejected.
			assert code in [-10, -11, 138, 139], '${probe}: ${code}: ${output}'
		}
	}
}

fn test_macos_immutable_closure_templates_link_from_multiple_c_translation_units() ! {
	$if macos && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_closure_tables_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_closures')
		for optimization in ['-O0', '-O2'] {
			program := os.join_path(work, 'tables')
			compiled := os.exec(['cc', '-std=gnu11', '-Wall', '-Wextra', '-Werror', optimization,
				'-I', os.join_path(@VEXEROOT, 'vlib', 'builtin', 'closure'),
				os.join_path(fixtures, 'table_a.c'), os.join_path(fixtures, 'table_b.c'),
				os.join_path(fixtures, 'tables_main.c'), '-o', program])
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([program])
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == 'Mach closure tables from multiple translation units passed'
		}
	}
}

fn test_macos_amd64_raw_signed_closure_tables_without_dynamic_libraries() ! {
	$if macos && amd64 {
		work := os.join_path(os.vtmp_dir(), 'freestanding_closure_raw_tables_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata')
		for optimization in ['-O0', '-O2'] {
			for probe in ['', 'PROBE_WRITE_CODE', 'PROBE_EXEC_METADATA'] {
				program := os.join_path(work, 'tables')
				mut command := ['cc', '-arch', 'x86_64', optimization, '-std=c11', '-ffreestanding',
					'-fno-builtin', '-fno-stack-protector', '-nostdlib', '-static', '-Wl,-e,_start',
					'-Wl,-no_pie', '-Wall', '-Wextra', '-Werror', '-I',
					os.join_path(@VEXEROOT, 'vlib', 'builtin', 'closure')]
				if probe != '' { command << '-D${probe}' }
				command << [
					os.join_path(fixtures, 'macos_kernel_services', 'kernel_services_oracle.c'),
					os.join_path(fixtures, 'freestanding_closures', 'table_a.c'),
					os.join_path(fixtures, 'freestanding_closures', 'table_b.c'),
					os.join_path(fixtures, 'freestanding_closures', 'raw_tables.c'),
					os.join_path(fixtures, 'macos_kernel_services', 'standalone.c'),
					os.join_path(fixtures, 'macos_kernel_services', 'standalone.s'),
					'-o',
					program,
				]
				compiled := os.exec(command)
				assert compiled.exit_code == 0, compiled.output
				loads := os.exec(['otool', '-l', program])
				assert loads.exit_code == 0, loads.output
				for dependency in ['LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB',
					'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB'] {
					assert !loads.output.contains('cmd ${dependency}'), loads.output
				}
				undefined := os.exec(['nm', '-u', program])
				assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
				code, output := run_freestanding_closure_probe(program, [])
				if probe == '' {
					assert code == 0, '${optimization}: ${code}: ${output}'
				} else {
					assert output.contains(if probe == 'PROBE_WRITE_CODE' {
						'closure code write probe reached'
					} else {
						'closure metadata execute probe reached'
					}), output
					assert code in [-10, -11, 138, 139], '${probe}: ${code}: ${output}'
				}
			}
		}
	}
}

fn test_freestanding_library_captured_functions_from_foreign_host_threads() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_closure_host_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_closures', 'library')
		mut library := os.join_path(work, 'libclosures.so')
		$if macos {
			library = os.join_path(work, 'libclosures.dylib')
		}
		compiled := os.exec([@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-shared',
			'-o', library, os.join_path(fixtures, 'library.v')])
		assert compiled.exit_code == 0, compiled.output
		host := os.join_path(work, 'host')
		mut command := ['cc', '-pthread', '-o', host, os.join_path(fixtures, 'host.c')]
		$if linux {
			command << '-ldl'
		}
		linked := os.exec(command)
		assert linked.exit_code == 0, linked.output
		code, output := run_freestanding_closure_probe(host, [library])
		assert code == 0, output
		assert output.trim_space() == 'freestanding library captured closures passed'
	}
}

fn run_freestanding_closure_probe(program string, args []string) (int, string) {
	mut process := os.new_process(program)
	process.set_args(args)
	process.set_redirect_stdio()
	process.run()
	deadline := time.sys_mono_now() + 20 * i64(time.second)
	for process.is_alive() && time.sys_mono_now() < deadline { time.sleep(10 * time.millisecond) }
	if process.is_alive() { process.signal_kill() }
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	code := process.code
	process.close()
	return code, output
}
