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
