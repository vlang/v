module main

import os
import v.cmdexec

fn test_implicit_tcc_preflight_selects_platform_before_transform() {
	with_implicit_tcc_environment(check_implicit_tcc_preflight_selects_platform_before_transform)
}

fn test_implicit_tcc_preflight_ignores_inactive_and_deferred_directives() {
	with_implicit_tcc_environment(check_implicit_tcc_preflight_ignores_inactive_and_deferred_directives)
}

fn test_implicit_tcc_preflight_probes_missing_libraries() {
	with_implicit_tcc_environment(check_implicit_tcc_preflight_probes_missing_libraries)
}

fn test_implicit_tcc_preflight_accepts_native_c_and_available_libraries() {
	with_implicit_tcc_environment(check_implicit_tcc_preflight_accepts_native_c_and_available_libraries)
}

fn test_implicit_tcc_preflight_does_not_link_c_or_object_output() {
	with_implicit_tcc_environment(check_implicit_tcc_preflight_does_not_link_c_or_object_output)
}

fn test_implicit_tcc_preflight_preserves_semantic_errors() {
	with_implicit_tcc_environment(check_implicit_tcc_preflight_preserves_semantic_errors)
}

fn check_implicit_tcc_preflight_preserves_semantic_errors() ! {
	vexe := @VEXE
	vroot := os.dir(vexe)
	if !os.is_file(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) {
		return
	}
	fixture := 'vlib/v/parser/tests/register_imported_enum.vv'
	registered := cmdexec.run_in(vexe, ['-new-compiler', '-nocolor', fixture], vroot)
	assert registered.exit_code != 0, registered.output
	expected := os.read_file(os.join_path(vroot, fixture.replace('.vv', '.out')))!
	actual := registered.output.replace('\r\n', '\n').trim_space()
	assert actual == expected.replace('\r\n', '\n').trim_space(), registered.output
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_preflight_semantic_error_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, '#flag -lV3InvalidSourceMustNotLink\nfn main() { println(v3_missing_value) }\n')!
	invalid := cmdexec.run(vexe, ['-new-compiler', '-gc', 'none', '-nocache', source])
	assert invalid.exit_code != 0, invalid.output
	assert invalid.output.contains('v3_missing_value'), invalid.output
	assert !invalid.output.contains('implicit tcc could not be used'), invalid.output
	old_forced := os.getenv_opt('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE')
	os.setenv('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE', 'injected for invalid source', true)
	defer {
		if value := old_forced {
			os.setenv('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE', value, true)
		} else {
			os.unsetenv('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE')
		}
	}
	os.write_file(source, 'fn read_value[T](value T) { println(value.v3_missing_field) }\nfn main() { read_value(1) }\n')!
	forced := cmdexec.run(vexe, ['-new-compiler', '-gc', 'none', '-nocache', source])
	assert forced.exit_code != 0, forced.output
	assert forced.output.contains('v3_missing_field'), forced.output
	assert !forced.output.contains('implicit tcc could not be used'), forced.output
	fallback := if os.user_os() == 'windows' { 'gcc' } else { 'cc' }
	os.find_abs_path_of_executable(fallback) or { return }
	os.write_file(source, 'fn main() { \$if tinyc { println(v3_invalid_tinyc_value) } \$else { answer := 40 + 2; println(answer) } }\n')!
	exe := os.join_path(dir, 'valid.exe')
	valid := cmdexec.run(vexe, ['-new-compiler', '-v', '-gc', 'none', '-nocache', '-o', exe, source])
	assert valid.exit_code == 0, valid.output
	assert valid.output.count('warning: implicit tcc could not be used') == 1, valid.output
	assert valid.output.count('=== V compiler benchmark ===') == 1, valid.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.replace('\r\n', '\n').trim_space() == '42'
}

fn with_implicit_tcc_environment(check fn () !) {
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		}
	}
	check() or { panic(err) }
}

fn pipeline_stage_count(output string, stage string) int {
	return output.split_into_lines().filter(it.trim_space().starts_with('${stage} ')).len
}

fn check_implicit_tcc_preflight_selects_platform_before_transform() ! {
	$if windows {
		return
	}
	vexe := @VEXE
	tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	os.find_abs_path_of_executable('c++') or { return }
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_preflight_cpp_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	native := os.join_path(dir, 'native.cpp')
	os.write_file(native, 'extern "C" int preflight_value(void) { return 42; }\n')!
	source := os.join_path(dir, 'main.v')
	os.write_file(source, '#flag "${native}"\nfn C.preflight_value() int\nfn identity[T](value T) T { return value }\nfn main() {\n\t\$if tinyc { println(identity(0)) } \$else { println(identity(C.preflight_value())) }\n}\n')!
	exe := os.join_path(dir, 'main')
	build := cmdexec.run(vexe, ['-new-compiler', '-v', '-nocache', '-o', exe, source])
	assert build.exit_code == 0, build.output
	assert build.output.contains('implicit tcc could not be used'), build.output
	assert build.output.count('=== V compiler benchmark ===') == 1, build.output
	assert pipeline_stage_count(build.output, 'transform') == 1, build.output
	assert pipeline_stage_count(build.output, 'monomorphize') == 1, build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}

fn check_implicit_tcc_preflight_ignores_inactive_and_deferred_directives() ! {
	vexe := @VEXE
	tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_preflight_conditions_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, '\$if !tinyc {\n #flag -lV3MustNotBeProbed\n}\nfn main() {\n \$if tinyc { println(42) } \$else { println(0) }\n}\n')!
	exe := os.join_path(dir, 'main.exe')
	build := cmdexec.run(vexe, ['-new-compiler', '-v', '-nocache', '-o', exe, source])
	assert build.exit_code == 0, build.output
	assert !build.output.contains('implicit tcc could not be used'), build.output
	assert build.output.count('=== V compiler benchmark ===') == 1, build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}

fn check_implicit_tcc_preflight_probes_missing_libraries() ! {
	vexe := @VEXE
	tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_preflight_library_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, "#flag -lV3PreflightLibraryThatDoesNotExist\nfn identity[T](value T) T { return value }\nfn main() { println(identity('ok')) }\n")!
	build := cmdexec.run(vexe, ['-new-compiler', '-v', '-nocache', '-o', os.join_path(dir, 'main.exe'),
		source])
	assert build.exit_code != 0, build.output
	assert build.output.contains('implicit tcc could not be used'), build.output
	assert build.output.contains('V3PreflightLibraryThatDoesNotExist'), build.output
	assert build.output.count('=== V compiler benchmark ===') == 1, build.output
	assert pipeline_stage_count(build.output, 'monomorphize') == 1, build.output
}

fn check_implicit_tcc_preflight_accepts_native_c_and_available_libraries() ! {
	$if windows {
		return
	}
	vexe := @VEXE
	tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_preflight_native_c_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	native := os.join_path(dir, 'native.c')
	os.write_file(native, 'int preflight_answer(void) { return 42; }\n')!
	source := os.join_path(dir, 'main.v')
	os.write_file(source, '#flag "${native}"\n#flag -lm\nfn C.preflight_answer() int\nfn main() {\n \$if tinyc { println(C.preflight_answer()) } \$else { println(0) }\n}\n')!
	exe := os.join_path(dir, 'main')
	build := cmdexec.run(vexe, ['-new-compiler', '-gc', 'none', '-v', '-nocache', '-o', exe, source])
	assert build.exit_code == 0, build.output
	assert !build.output.contains('implicit tcc could not be used'), build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}

fn check_implicit_tcc_preflight_does_not_link_c_or_object_output() ! {
	vexe := @VEXE
	tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_preflight_output_only_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, "#flag -lV3OutputOnlyMustNotBeLinked\nfn main() { println('ok') }\n")!
	for extension in ['c', 'o'] {
		output := os.join_path(dir, 'main.${extension}')
		build := cmdexec.run(vexe, ['-new-compiler', '-gc', 'none', '-nocache', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('implicit tcc could not be used'), build.output
		assert os.is_file(output)
	}
}
