module builder

import os

fn test_is_tcc_compilation_failure_detects_tcc_kind() {
	assert is_tcc_compilation_failure('cc', .tcc, '')
}

fn test_is_tcc_compilation_failure_detects_tcc_compiler_name() {
	assert is_tcc_compilation_failure('tcc', .unknown, '')
	assert is_tcc_compilation_failure('/opt/v/thirdparty/tcc/tcc.exe', .unknown, '')
	assert is_tcc_compilation_failure('/usr/local/bin/tcc-0.9.27', .unknown, '')
	assert !is_tcc_compilation_failure('/usr/bin/clang', .unknown, '')
}

fn test_is_tcc_compilation_failure_detects_tcc_output() {
	assert is_tcc_compilation_failure('cc', .unknown, 'tcc: error: bad architecture')
	assert is_tcc_compilation_failure('cc', .unknown, 'line 1\nline 2\ntcc: error: lib not found')
	assert !is_tcc_compilation_failure('cc', .unknown, 'clang: error: unsupported option')
}

fn test_is_tcc_compilation_failure_detects_tcc_alias_compiler() {
	if os.user_os() == 'windows' {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_cc_tcc_retry_test_${os.getpid()}')
	cc_path := os.join_path(test_root, 'cc')
	old_path := os.getenv('PATH')
	os.mkdir_all(test_root) or { panic(err) }
	os.write_file(cc_path, '#!/bin/sh\necho "Tiny C Compiler"\n') or { panic(err) }
	os.chmod(cc_path, 0o700) or { panic(err) }
	os.setenv('PATH', '${test_root}${os.path_delimiter}${old_path}', true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert is_tcc_compilation_failure('cc', .unknown, '')
}

fn fake_windows_short_path(path string) string {
	return path.replace(r'C:\Users\Léo', r'C:\Users\LEO~1')
}

fn test_rewrite_windows_path_arg_rewrites_quoted_object_paths() {
	arg := r'"C:\Users\Léo\.vmodules\.cache\bc\artifact.o"'
	expected := r'"C:\Users\LEO~1\.vmodules\.cache\bc\artifact.o"'
	assert rewrite_windows_path_arg(arg, fake_windows_short_path) == expected
}

fn test_rewrite_windows_path_arg_rewrites_prefixed_paths() {
	assert rewrite_windows_path_arg(r'-I"C:\Users\Léo\include"', fake_windows_short_path) == r'-I"C:\Users\LEO~1\include"'
	assert rewrite_windows_path_arg(r'-L"C:\Users\Léo\lib"', fake_windows_short_path) == r'-L"C:\Users\LEO~1\lib"'
	assert rewrite_windows_path_arg(r'-o "C:\Users\Léo\bin\tool.exe"', fake_windows_short_path) == r'-o "C:\Users\LEO~1\bin\tool.exe"'
}

fn test_rewrite_windows_path_arg_leaves_non_paths_alone() {
	for arg in ['-bt25', '-std=c99', '-D_DEFAULT_SOURCE'] {
		assert rewrite_windows_path_arg(arg, fake_windows_short_path) == arg
	}
}
