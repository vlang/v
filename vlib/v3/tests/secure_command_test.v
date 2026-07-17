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

fn test_c_object_cache_bypasses_failed_dependency_scan() {
	$if windows {
		return
	}
	v3_bin := build_secure_command_v3()
	root := secure_temp_path('object_cache_dependency_fallback')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.unsetenv('V3_CACHE_TRACE')
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	compiler := os.join_path(root, 'cc_without_dependencies')
	os.write_file(compiler, '#!/bin/sh
for arg in "\$@"; do
	if [ "\$arg" = "-M" ]; then
		exit 1
	fi
done
exec cc "\$@"
') or {
		panic(err)
	}
	os.chmod(compiler, 0o700) or { panic(err) }
	header := os.join_path(root, 'value.h')
	c_source := os.join_path(root, 'helper.c')
	v_source := os.join_path(root, 'main.v')
	os.write_file(header, '#define FALLBACK_VALUE 7\n') or { panic(err) }
	os.write_file(c_source,
		'#include "value.h"\nint fallback_value(void) { return FALLBACK_VALUE; }\n') or {
		panic(err)
	}
	os.write_file(v_source, 'module main
#flag @DIR/helper.o
fn C.fallback_value() int
fn main() { println(int_str(C.fallback_value())) }
') or {
		panic(err)
	}
	os.setenv('V3_CACHE_TRACE', '1', true)
	first_out := os.join_path(root, 'first')
	first := cmdexec.run(v3_bin, [v_source, '-cc', compiler, '-prod', '-o', first_out])
	assert first.exit_code == 0, first.output
	assert cmdexec.run(first_out, []string{}).output.trim_space() == '7'
	assert first.output.contains('C object cache bypass:'), first.output
	os.write_file(header, '#define FALLBACK_VALUE 9\n') or { panic(err) }
	second_out := os.join_path(root, 'second')
	second := cmdexec.run(v3_bin, [v_source, '-cc', compiler, '-prod', '-o', second_out])
	assert second.exit_code == 0, second.output
	assert cmdexec.run(second_out, []string{}).output.trim_space() == '9'
	assert second.output.contains('C object cache bypass:'), second.output
}

// A `cc -M` that succeeds but emits no `-MT` target marker must not be accepted
// as a valid, source-only dependency set: `all_after` would return the whole
// output, tokenize it into bogus paths, and let the header edit go unnoticed.
// The dependency scan must fail closed to a build-local object instead.
fn test_c_object_cache_falls_back_when_dependency_marker_is_missing() {
	$if windows {
		return
	}
	v3_bin := build_secure_command_v3()
	root := secure_temp_path('object_cache_missing_marker')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.unsetenv('V3_CACHE_TRACE')
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	// On `-M` this wrapper succeeds but prints unrelated text with no `v3cache:`
	// marker; every other invocation delegates to the real `cc`.
	compiler := os.join_path(root, 'cc_without_marker')
	os.write_file(compiler, '#!/bin/sh
for arg in "\$@"; do
	if [ "\$arg" = "-M" ]; then
		echo "no dependency marker here"
		exit 0
	fi
done
exec cc "\$@"
') or {
		panic(err)
	}
	os.chmod(compiler, 0o700) or { panic(err) }
	header := os.join_path(root, 'value.h')
	c_source := os.join_path(root, 'helper.c')
	v_source := os.join_path(root, 'main.v')
	os.write_file(header, '#define MARKER_VALUE 7\n') or { panic(err) }
	os.write_file(c_source, '#include "value.h"\nint marker_value(void) { return MARKER_VALUE; }\n') or {
		panic(err)
	}
	os.write_file(v_source, 'module main
#flag @DIR/helper.o
fn C.marker_value() int
fn main() { println(int_str(C.marker_value())) }
') or {
		panic(err)
	}
	os.setenv('V3_CACHE_TRACE', '1', true)
	first_out := os.join_path(root, 'first')
	first := cmdexec.run(v3_bin, [v_source, '-cc', compiler, '-prod', '-o', first_out])
	assert first.exit_code == 0, first.output
	assert cmdexec.run(first_out, []string{}).output.trim_space() == '7'
	assert first.output.contains('C object cache bypass:'), first.output
	// The header change must be reflected because the object was never cached.
	os.write_file(header, '#define MARKER_VALUE 9\n') or { panic(err) }
	second_out := os.join_path(root, 'second')
	second := cmdexec.run(v3_bin, [v_source, '-cc', compiler, '-prod', '-o', second_out])
	assert second.exit_code == 0, second.output
	assert cmdexec.run(second_out, []string{}).output.trim_space() == '9'
	assert second.output.contains('C object cache bypass:'), second.output
}

// If a dependency changes between the moment the cache key is computed and the
// moment compilation finishes, the object no longer matches the key. The cache
// must detect this by re-hashing inputs after compilation and use a build-local
// object rather than certifying the stale content under that key.
fn test_c_object_cache_detects_input_change_during_compile() {
	$if windows {
		return
	}
	v3_bin := build_secure_command_v3()
	root := secure_temp_path('object_cache_input_race')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.unsetenv('V3_CACHE_TRACE')
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	header := os.join_path(root, 'value.h')
	c_source := os.join_path(root, 'helper.c')
	v_source := os.join_path(root, 'main.v')
	// This wrapper delegates to the real `cc`, but on the object compile (`-c`)
	// it mutates the header AFTER building the object — simulating a source edit
	// that lands while the compiler is running.
	compiler := os.join_path(root, 'cc_mutating_header')
	os.write_file(compiler, '#!/bin/sh
is_compile=0
for arg in "\$@"; do
	if [ "\$arg" = "-c" ]; then
		is_compile=1
	fi
done
cc "\$@"
rc=\$?
if [ "\$is_compile" = "1" ]; then
	printf "#define RACE_VALUE 7\\n" > "${header}"
fi
exit \$rc
') or {
		panic(err)
	}
	os.chmod(compiler, 0o700) or { panic(err) }
	os.write_file(header, '#define RACE_VALUE 1\n') or { panic(err) }
	os.write_file(c_source, '#include "value.h"\nint race_value(void) { return RACE_VALUE; }\n') or {
		panic(err)
	}
	os.write_file(v_source, 'module main
#flag @DIR/helper.o
fn C.race_value() int
fn main() { println(int_str(C.race_value())) }
') or {
		panic(err)
	}
	os.setenv('V3_CACHE_TRACE', '1', true)
	first_out := os.join_path(root, 'first')
	first := cmdexec.run(v3_bin, [v_source, '-cc', compiler, '-prod', '-o', first_out])
	assert first.exit_code == 0, first.output
	// The object was built from RACE_VALUE 1; the mid-compile edit to 7 means the
	// object must not be published under the key that hashed the original header.
	assert first.output.contains('inputs changed during compilation'), first.output
	assert cmdexec.run(first_out, []string{}).output.trim_space() == '1'
}
