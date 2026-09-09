module fixturetest

import os

fn test_is_diagnostic_fixture_dir_restricts_dispatch_to_diagnostic_suites() {
	original_autofix := os.getenv('VAUTOFIX')
	os.unsetenv('VAUTOFIX')
	root := os.join_path(os.temp_dir(), 'v3_fixturetest_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		if original_autofix.len > 0 {
			os.setenv('VAUTOFIX', original_autofix, true)
		} else {
			os.unsetenv('VAUTOFIX')
		}
		os.rmdir_all(root) or {}
	}

	checker_dir := os.join_path(root, 'vlib', 'v', 'checker', 'tests')
	os.mkdir_all(checker_dir) or { panic(err) }
	os.write_file(os.join_path(checker_dir, 'sample_test.vv'), 'fn main() {}\n') or { panic(err) }
	os.write_file(os.join_path(checker_dir, 'sample_test.out'), '') or { panic(err) }
	assert is_diagnostic_fixture_dir(checker_dir)
	os.write_file(os.join_path(checker_dir, 'var_duplicate_const.vv'), 'fn main() {}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(checker_dir, 'var_duplicate_const.out'), 'unstable output\n') or {
		panic(err)
	}
	assert is_comparable_fixture(checker_dir, 'sample_test.vv')
	$if noskip ? {
		assert is_comparable_fixture(checker_dir, 'var_duplicate_const.vv')
	} $else {
		assert !is_comparable_fixture(checker_dir, 'var_duplicate_const.vv')
	}
	os.write_file(os.join_path(checker_dir, 'missing_output.vv'), 'fn main() {}\n') or {
		panic(err)
	}
	assert !is_comparable_fixture(checker_dir, 'missing_output.vv')
	os.setenv('VAUTOFIX', '1', true)
	assert is_comparable_fixture(checker_dir, 'missing_output.vv')
	for name in ['index_expr_implicit_int_downcast_err.vv', 'js_number_requires_explicit_cast.vv',
		'disable_explicit_mutability.vv'] {
		os.write_file(os.join_path(checker_dir, name), 'fn main() {}\n') or { panic(err) }
		assert !is_comparable_fixture(checker_dir, name)
		assert !os.exists(os.join_path(checker_dir, name.all_before_last('.vv') + '.out'))
	}
	os.unsetenv('VAUTOFIX')

	runtime_dir := os.join_path(root, 'vlib', 'v', 'gen', 'c', 'testdata')
	os.mkdir_all(runtime_dir) or { panic(err) }
	os.write_file(os.join_path(runtime_dir, 'sample.vv'), "fn main() { println('runtime') }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(runtime_dir, 'sample.out'), 'runtime\n') or { panic(err) }
	assert !is_diagnostic_fixture_dir(runtime_dir)

	os.write_file(os.join_path(checker_dir, 'compiler_test.v'), 'module main\n') or { panic(err) }
	assert !is_diagnostic_fixture_dir(checker_dir)
}

fn test_environment_specific_fixture_exclusions_match_reference_runner() {
	original_ubuntu_musl := os.getenv('V_CI_UBUNTU_MUSL')
	original_musl := os.getenv('V_CI_MUSL')
	original_cstrict := os.getenv('V_CI_CSTRICT')
	os.unsetenv('V_CI_UBUNTU_MUSL')
	os.unsetenv('V_CI_MUSL')
	os.unsetenv('V_CI_CSTRICT')
	defer {
		for name, value in {
			'V_CI_UBUNTU_MUSL': original_ubuntu_musl
			'V_CI_MUSL':        original_musl
			'V_CI_CSTRICT':     original_cstrict
		} {
			if value.len > 0 {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}

	os.setenv('V_CI_UBUNTU_MUSL', '1', true)
	mut excluded := excluded_diagnostic_fixture_paths()
	$if noskip ? {
		assert 'vlib/v/checker/tests/orm_unused_var.vv' !in excluded
	} $else {
		assert 'vlib/v/checker/tests/orm_unused_var.vv' in excluded
	}
	os.unsetenv('V_CI_UBUNTU_MUSL')

	os.setenv('V_CI_CSTRICT', '1', true)
	excluded = excluded_diagnostic_fixture_paths()
	$if noskip ? {
	} $else {
		assert 'vlib/v/checker/tests/missing_c_lib_header_1.vv' in excluded
	}
	$if tinyc {
		assert 'vlib/v/checker/tests/missing_shader_header_1.vv' in excluded
	}
	$if msvc {
		assert 'vlib/v/checker/tests/asm_alias_does_not_exist.vv' in excluded
	}
	$if windows {
		assert 'vlib/v/checker/tests/invalid_utf8_string.vv' in excluded
	}
}

fn test_forwarded_compiler_options_preserve_configuration_flags() {
	args := ['-silent', '-cc', 'clang', '-d', 'test', '-dfoo', '-o', 'ignored-output', 'test',
		'/checkout/vlib/v/checker/tests', '-os', 'windows', '-thread-stack-size', '4194304',
		'-no-parallel', '-keepc']
	assert forwarded_compiler_options(args) == ['-cc', 'clang', '-d', 'test', '-dfoo', '-os',
		'windows', '-thread-stack-size', '4194304', '-keepc']
}

fn test_run_autofixes_missing_and_mismatched_output() {
	$if windows {
		return
	}
	original_autofix := os.getenv('VAUTOFIX')
	os.setenv('VAUTOFIX', '1', true)
	root := os.join_path(os.temp_dir(), 'v3_fixture_autofix_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		if original_autofix.len > 0 {
			os.setenv('VAUTOFIX', original_autofix, true)
		} else {
			os.unsetenv('VAUTOFIX')
		}
		os.rmdir_all(root) or {}
	}

	checker_dir := os.join_path(root, 'vlib', 'v', 'checker', 'tests')
	os.mkdir_all(checker_dir) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'vlib', 'v3')) or { panic(err) }
	source_path := os.join_path(checker_dir, 'sample.vv')
	expected_path := os.join_path(checker_dir, 'sample.out')
	os.write_file(source_path, 'fn main() {}\n') or { panic(err) }
	fake_vexe := os.join_path(root, 'v3')
	captured_path := os.join_path(root, 'fixture_args.txt')
	expected := 'vlib/v/checker/tests/sample.vv:1:1: error: updated output\n'
	os.write_file(fake_vexe, '#!/bin/sh
printf "%s\n" "\$@" > "${captured_path}"
printf "%s" "${expected}"
exit 1
') or {
		panic(err)
	}
	os.chmod(fake_vexe, 0o700) or { panic(err) }

	invocation_args := ['-d', 'fixture_flag', 'test', checker_dir]
	assert run(fake_vexe, checker_dir, invocation_args) == 1
	actual := os.read_file(expected_path) or { panic(err) }
	assert actual == expected
	assert run(fake_vexe, checker_dir, invocation_args) == 0
	captured := os.read_lines(captured_path) or { panic(err) }
	assert captured[..2] == ['-d', 'fixture_flag']
	assert captured[2..6] == ['-silent', '-no-parallel', '-nocache', '-checker-fixture']
	assert captured.last() == 'vlib/v/checker/tests/sample.vv'
}

fn test_fixture_repo_root_falls_back_to_requested_suite() {
	base := os.join_path(os.temp_dir(), 'v3_fixture_root_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}

	repo_root := os.join_path(base, 'checkout')
	fixture_dir := os.join_path(repo_root, 'vlib', 'v', 'checker', 'tests')
	os.mkdir_all(fixture_dir) or { panic(err) }
	os.mkdir_all(os.join_path(repo_root, 'vlib', 'v3')) or { panic(err) }

	install_dir := os.join_path(base, 'installed', 'bin')
	os.mkdir_all(install_dir) or { panic(err) }
	vexe := os.join_path(install_dir, 'v3')
	os.write_file(vexe, '') or { panic(err) }

	assert fixture_repo_root(vexe, fixture_dir) == repo_root
}

fn test_expected_fixture_exit_code_follows_diagnostic_severity() {
	assert expected_fixture_exit_code('') == 0
	assert expected_fixture_exit_code('sample.vv:1:1: warning: warning text') == 0
	assert expected_fixture_exit_code('sample.vv:1:1: notice: notice text') == 0
	assert expected_fixture_exit_code('sample.vv:1:1: error: error text') == 1
	assert expected_fixture_exit_code('sample.vv:1:1: builder error: missing module') == 1
	assert expected_fixture_exit_code('sample.vv:1:1: cgen error: invalid expression') == 1
	assert expected_fixture_exit_code("    1 | println(': error: source text')") == 0
	assert expected_fixture_exit_code('sample.vv:1:1: error: cannot use `(i8 | i16 | int | i64)` as type `SimpleInt`') == 1
}

fn test_fixture_result_requires_expected_exit_code() {
	error_output := 'sample.vv:1:1: error: error text'
	assert fixture_result_matches('', '', 0, 0)
	assert !fixture_result_matches('', '', 0, 1)
	assert fixture_result_matches(error_output, error_output, 1, 1)
	assert !fixture_result_matches(error_output, error_output, 1, 0)
}
