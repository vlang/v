import os
import rand
import v3.cmdexec

const secure_tests_dir = os.dir(@FILE)
const secure_v3_dir = os.dir(secure_tests_dir)
const secure_vlib_dir = os.dir(secure_v3_dir)
const secure_v3_src = os.join_path(secure_v3_dir, 'v3.v')

fn build_secure_command_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_secure_command_${os.getpid()}_${rand.ulid()}')
	result := cmdexec.run(@VEXE, ['-gc', 'none', '-path', '${secure_vlib_dir}|@vlib|@vmodules',
		'-o', v3_bin, secure_v3_src])
	assert result.exit_code == 0, result.output
	return v3_bin
}

fn secure_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
}

fn test_command_argument_parser_preserves_quoted_values() {
	args := cmdexec.split_args('-I "dir with spaces" -DNAME=\'quoted value\' plain\\ value') or {
		panic(err)
	}
	assert args == ['-I', 'dir with spaces', '-DNAME=quoted value', 'plain value']
	windows_args := cmdexec.split_args(r'-IC:\SDK\include @DIR\include "C:\Program Files\SDK"') or {
		panic(err)
	}
	assert windows_args == [r'-IC:\SDK\include', r'@DIR\include', r'C:\Program Files\SDK']
	invalid := cmdexec.split_args('"unterminated') or { []string{} }
	assert invalid == []string{}
}

fn test_split_linker_path_is_not_passed_to_object_compile() {
	v3_bin := build_secure_command_v3()
	root := secure_temp_path('split_linker_object')
	lib_dir := os.join_path(root, 'link lib')
	os.mkdir_all(lib_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'helper.c'), 'int split_link_answer(void) { return 42; }\n') or {
		panic(err)
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main
#flag -L "link lib"
#flag @DIR/helper.o
fn C.split_link_answer() int
fn main() { println(int_str(C.split_link_answer())) }
') or {
		panic(err)
	}
	output := os.join_path(root, 'program')
	compile := cmdexec.run(v3_bin, [source, '-prod', '-o', output])
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}

// test_source_directives_cannot_inject_shell_commands verifies that #flag and
// #pkgconfig text is passed as literal argv rather than evaluated by a shell.
fn test_source_directives_cannot_inject_shell_commands() {
	v3_bin := build_secure_command_v3()
	marker := secure_temp_path('directive_injection_marker')
	os.rm(marker) or {}
	flag_src := secure_temp_path('malicious_flag') + '.v'
	os.write_file(flag_src, 'module main
#flag ; touch ${marker}
fn main() {}
') or { panic(err) }
	flag_result := cmdexec.run(v3_bin, [flag_src, '-prod', '-o', secure_temp_path('flag_out')])
	assert flag_result.exit_code != 0, flag_result.output
	assert !os.exists(marker)

	pkg_src := secure_temp_path('malicious_pkgconfig') + '.v'
	os.write_file(pkg_src, 'module main
#pkgconfig v3_missing_package; touch ${marker}
fn main() {}
') or {
		panic(err)
	}
	pkg_result := cmdexec.run(v3_bin, [pkg_src, '-prod', '-o', secure_temp_path('pkg_out')])
	assert pkg_result.exit_code == 0, pkg_result.output
	assert !os.exists(marker)
}

fn test_quoted_flag_paths_and_metacharacter_output_paths_are_literal() {
	v3_bin := build_secure_command_v3()
	root := secure_temp_path('flag path with spaces')
	include_dir := os.join_path(root, 'include dir')
	os.mkdir_all(include_dir) or { panic(err) }
	os.write_file(os.join_path(include_dir, 'answer.h'), 'static int answer(void) { return 42; }\n') or {
		panic(err)
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main
#flag -I "./include dir"
#include "answer.h"
fn C.answer() int
fn main() { println(int_str(C.answer())) }
') or {
		panic(err)
	}
	output := os.join_path(root, 'program;not-a-command')
	compile := cmdexec.run(v3_bin, [source, '-prod', '-o', output])
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}

fn test_concurrent_builds_to_same_output_use_private_directories() {
	v3_bin := build_secure_command_v3()
	source := secure_temp_path('concurrent_build') + '.v'
	os.write_file(source, "fn main() { println('ok') }\n") or { panic(err) }
	output := secure_temp_path('concurrent_build_out')
	first := spawn cmdexec.run(v3_bin, [source, '-prod', '-o', output])
	second := spawn cmdexec.run(v3_bin, [source, '-prod', '-o', output])
	first_result := first.wait()
	second_result := second.wait()
	assert first_result.exit_code == 0, first_result.output
	assert second_result.exit_code == 0, second_result.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_object_cache_tracks_headers_and_publishes_atomically() {
	v3_bin := build_secure_command_v3()
	root := secure_temp_path('object_cache')
	os.mkdir_all(root) or { panic(err) }
	header := os.join_path(root, 'value.h')
	c_source := os.join_path(root, 'helper.c')
	v_source := os.join_path(root, 'main.v')
	os.write_file(header, '#define CACHED_VALUE 7\n') or { panic(err) }
	os.write_file(c_source, '#include "value.h"\nint cached_value(void) { return CACHED_VALUE; }\n') or {
		panic(err)
	}
	os.write_file(v_source, 'module main
#flag @DIR/helper.o
fn C.cached_value() int
fn main() { println(int_str(C.cached_value())) }
') or {
		panic(err)
	}
	first_out := os.join_path(root, 'first')
	second_out := os.join_path(root, 'second')
	first := spawn cmdexec.run(v3_bin, [v_source, '-prod', '-o', first_out])
	second := spawn cmdexec.run(v3_bin, [v_source, '-prod', '-o', second_out])
	first_result := first.wait()
	second_result := second.wait()
	assert first_result.exit_code == 0, first_result.output
	assert second_result.exit_code == 0, second_result.output
	assert cmdexec.run(first_out, []string{}).output.trim_space() == '7'
	assert cmdexec.run(second_out, []string{}).output.trim_space() == '7'

	os.write_file(header, '#define CACHED_VALUE 9\n') or { panic(err) }
	third_out := os.join_path(root, 'third')
	third := cmdexec.run(v3_bin, [v_source, '-prod', '-o', third_out])
	assert third.exit_code == 0, third.output
	assert cmdexec.run(third_out, []string{}).output.trim_space() == '9'

	os.setenv('V3_CACHE_TRACE', '1', true)
	defer {
		os.unsetenv('V3_CACHE_TRACE')
	}
	fourth_out := os.join_path(root, 'fourth')
	fourth := cmdexec.run(v3_bin, [v_source, '-prod', '-o', fourth_out])
	assert fourth.exit_code == 0, fourth.output
	assert fourth.output.contains('C object cache hit: key='), fourth.output
	assert fourth.output.contains('reason=compiler, target, argv, and dependency contents matched'), fourth.output

	assert fourth.output.contains('dependencies=2'), fourth.output
	assert fourth.output.contains('C object content-key hits'), fourth.output
	assert cmdexec.run(fourth_out, []string{}).output.trim_space() == '9'
}
