module driver

import os
import time
import v.cmdexec
import v.pref

fn literal_output_gc_test_root() !string {
	root := os.join_path(os.vtmp_dir(), 'v literal output gc ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	return root
}

fn test_minimal_literal_output_supports_boehm_and_no_gc_runtime() {
	root := literal_output_gc_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'literal_output.v')
	os.write_file(source, "fn main() { print('\\n') }\n")!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('macos', 'arm64')!
	for mode in ['', 'boehm', 'boehm_full', 'boehm_incr', 'boehm_full_opt', 'boehm_incr_opt',
		'boehm_leak', 'vgc', 'none'] {
		mut defines := []string{}
		mut values := map[string]string{}
		apply_v3_gc_mode(mode, false, mut defines, mut values)!
		prefs.user_defines = defines
		prefs.compile_values = values
		assert input_uses_minimal_literal_output_builtin(source, prefs, false, false) == (mode != 'vgc'), mode
	}
	// Cross/self builds can request a collector while effectively disabling it.
	mut defines := []string{}
	mut values := map[string]string{}
	apply_v3_gc_mode('boehm_full_opt', true, mut defines, mut values)!
	prefs.user_defines = defines
	prefs.compile_values = values
	assert input_uses_minimal_literal_output_builtin(source, prefs, false, false)
}

fn test_minimal_literal_output_builtin_keeps_collector_support_files() {
	for name in ['builtin_d_gcboehm.c.v', 'builtin_d_gcboehm_d_musl.c.v', 'gc_startup_d_gcboehm.c.v',
		'gc_startup_d_v3_backend.v', 'array_d_gcboehm_opt.v', 'map_d_gcboehm_opt.v',
		'builtin_notd_gcboehm.c.v', 'array_notd_gcboehm_opt.v', 'map_notd_gcboehm_opt.v',
		'allocation.c.v', 'builtin_nix.c.v', 'segfault_handler_nix.c.v'] {
		assert is_minimal_literal_output_builtin_file(os.join_path('builtin', name)), name
	}
	for name in ['float.c.v', 'vgc_d_vgc.c.v', 'gc_startup_d_vgc.c.v'] {
		assert !is_minimal_literal_output_builtin_file(os.join_path('builtin', name)), name
	}
}

fn test_minimal_literal_output_gc_keeps_existing_eligibility_gates() {
	root := literal_output_gc_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'literal_output.v')
	os.write_file(source, "fn main() { print('hello') }\n")!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('macos', 'arm64')!
	prefs.user_defines = ['gcboehm', 'gcboehm_opt']
	assert input_uses_minimal_literal_output_builtin(source, prefs, false, false)
	assert !input_uses_minimal_literal_output_builtin(source, prefs, true, false)
	assert !input_uses_minimal_literal_output_builtin(source, prefs, false, true)
	prefs.target = pref.target_from('linux', 'arm64')!
	assert !input_uses_minimal_literal_output_builtin(source, prefs, false, false)
	prefs.target = pref.target_from('macos', 'arm64')!
	prefs.backend = 'arm64'
	assert !input_uses_minimal_literal_output_builtin(source, prefs, false, false)
	prefs.backend = 'c'
	os.write_file(source, 'fn main() { println(3) }\n')!
	assert !input_uses_minimal_literal_output_builtin(source, prefs, false, false)
}

fn test_literal_output_keeps_linux_backtrace_array_iteration_runtime() {
	root := literal_output_gc_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	keys := ['VFLAGS', 'VOSARGS', 'V_C_ERROR_BUG_REPORT_DISABLED']
	mut old_values := map[string]string{}
	for key in keys {
		if value := os.getenv_opt(key) {
			old_values[key] = value
		}
	}
	defer {
		for key in keys {
			if value := old_values[key] {
				os.setenv(key, value, true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	source := os.join_path(root, 'hello.v')
	os.write_file(source, "fn main() { println('Hello, World!') }\n")!
	flags := ['-new-compiler', '-no-retry-compilation', '-nocache', '-show-timings']
	for arch in ['amd64', 'arm64'] {
		c_path := os.join_path(root, 'hello_${arch}.c')
		mut c_args := flags.clone()
		c_args << ['-os', 'linux', '-arch', arch, '-d', 'glibc', '-o', c_path, source]
		generated := cmdexec.run_with_timeout(vexe, c_args, 120_000)
		assert generated.exit_code == 0, generated.output
		output := os.read_file(c_path)!
		assert output.contains('Array_string__join('), c_path
		assert output.contains('array_get(a, '), c_path
		assert output.count('array__get(array ') >= 2, c_path
	}
	bin_path := os.join_path(root, 'hello' + $if windows { '.exe' } $else { '' })
	mut compile_args := flags.clone()
	compile_args << ['-o', bin_path, source]
	compiled := cmdexec.run_with_timeout(vexe, compile_args, 120_000)
	assert compiled.exit_code == 0, compiled.output
	assert !compiled.output.contains('retrying with'), compiled.output
	run := cmdexec.run_with_timeout(bin_path, [], 15_000)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'Hello, World!', run.output
}

fn test_macos_literal_output_with_gc_has_no_compiler_diagnostics() {
	$if !macos {
		return
	}
	root := literal_output_gc_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	// Keep the subprocess independent of the test runner's flags, and make a
	// V3 failure fail the test instead of accepting output from a V1 fallback.
	keys := ['VFLAGS', 'VOSARGS', 'V_MACOS_V3_NO_FALLBACK', 'V_C_ERROR_BUG_REPORT_DISABLED']
	mut old_values := map[string]string{}
	for key in keys {
		if value := os.getenv_opt(key) {
			old_values[key] = value
		}
	}
	defer {
		for key in keys {
			if value := old_values[key] {
				os.setenv(key, value, true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v')
	} else {
		@VEXE
	}
	for mode in ['', 'boehm_full', 'boehm_incr', 'boehm_full_opt', 'boehm_incr_opt', 'boehm_leak',
		'none'] {
		for index, snippet in [r"print('\n')", r"print('\r\n')"] {
			source := os.join_path(root, 'literal_${mode}_${index}.v')
			os.write_file(source, 'fn main() { ${snippet} }\n')!
			mut args := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', 'clang']
			if mode.len > 0 {
				args << ['-gc', mode]
			}
			args << ['run', source]
			result := cmdexec.run_with_timeout(vexe, args, 120_000)
			assert result.exit_code == 0, result.output
			expected := if index == 0 { [u8(10)] } else { [u8(13), 10] }
			// cmdexec captures both streams without trimming newline bytes.
			assert result.output.bytes() == expected, result.output
			if index == 0 {
				c_path := os.join_path(root, 'literal_${mode}.c')
				mut c_args := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', 'clang']
				if mode.len > 0 {
					c_args << ['-gc', mode]
				}
				c_args << ['-o', c_path, source]
				generated := cmdexec.run_with_timeout(vexe, c_args, 120_000)
				assert generated.exit_code == 0, generated.output
				output := os.read_file(c_path)!
				if mode == 'none' {
					assert !output.contains('GC_INIT();'), mode
				} else {
					assert output.contains('GC_INIT();'), mode
					assert output.contains('GC_MALLOC'), mode
					assert output.contains('GC_REGISTER_DISPLACEMENT'), mode
					assert output.contains('gc_runtime_init();'), mode
				}
			}
		}
	}
}
