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

fn test_minimal_literal_output_requires_no_gc_runtime() {
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
		assert input_uses_minimal_literal_output_builtin(source, prefs, false, false) == (mode == 'none'), mode
	}
	// Cross/self builds can request a collector while effectively disabling it.
	mut defines := []string{}
	mut values := map[string]string{}
	apply_v3_gc_mode('boehm_full_opt', true, mut defines, mut values)!
	prefs.user_defines = defines
	prefs.compile_values = values
	assert input_uses_minimal_literal_output_builtin(source, prefs, false, false)
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
	for mode in ['', 'boehm_full_opt', 'none'] {
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
		}
	}
}
