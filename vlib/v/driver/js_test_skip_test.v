module driver

import os
import time
import v.cmdexec
import v.pref

fn js_test_skip_root() !string {
	root := os.join_path(os.vtmp_dir(), 'v js test skip ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	return root
}

fn test_js_test_selection_is_disabled_on_every_target() {
	root := js_test_skip_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	js_test := os.join_path(root, 'int_test.js.v')
	native_test := os.join_path(root, 'native_test.v')
	js_source := os.join_path(root, 'helper.js.v')
	os.write_file(js_test, 'this is deliberately invalid V source\n')!
	os.write_file(native_test, 'fn test_native() { assert true }\n')!
	os.write_file(js_source, 'fn main() {}\n')!
	for target_os in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
		'android', 'termux', 'ios', 'solaris', 'qnx', 'haiku', 'serenity', 'vinix',
		'wasm32_emscripten'] {
		arch := if target_os == 'wasm32_emscripten' { 'wasm32' } else { 'amd64' }
		target := pref.target_from(target_os, arch)!
		for backend in ['c', 'fastc', 'arm64', 'wasm', 'eval', 'js'] {
			assert !pref.is_test_file_for_backend(js_test, backend)
			assert !pref.is_test_file_for_platform(js_test, backend, target)
			assert pref.get_test_v_files_from_dir_for_target(root, [], backend, target) == [native_test]
			for is_test_command in [false, true] {
				assert v3_direct_test_input_is_incompatible(is_test_command, js_test, backend,
					target, 'clang', false, [])
			}
			assert !v3_direct_test_input_is_incompatible(false, native_test, backend, target,
				'clang', false, [])
			assert !v3_direct_test_input_is_incompatible(false, js_source, backend, target,
				'clang', false, [])
		}
	}
	// Other backend-qualified tests keep their existing selection rules.
	assert pref.is_test_file_for_backend('native_test.c.v', 'c')
	assert pref.is_test_file_for_backend('native_test.wasm.v', 'wasm')
	assert !v3_direct_test_input_is_incompatible(false, os.join_path(root, 'missing_test.js.v'),
		'c', pref.host_target(), 'clang', false, [])
}

fn test_direct_js_test_commands_skip_before_parsing() {
	root := js_test_skip_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'int_test.js.v')
	os.write_file(source, 'this is deliberately invalid V source\n')!
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
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	cc := $if windows { 'gcc' } $else { 'cc' }
	for command in ['', 'run', 'test'] {
		mut args := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', cc]
		if command.len > 0 {
			args << command
		}
		args << source
		result := cmdexec.run_with_timeout(vexe, args, 120_000)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'SKIP ${source}', result.output
	}
	quiet := cmdexec.run_with_timeout(vexe, ['-new-compiler', '-no-retry-compilation',
		'-silent', '-cc', cc, source], 120_000)
	assert quiet.exit_code == 0, quiet.output
	assert quiet.output == '', quiet.output
	assert !os.exists(source[..source.len - 2])
}
