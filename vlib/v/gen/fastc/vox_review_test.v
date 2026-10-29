module fastc

import os
import v.cmdexec

fn fastc_vox_review_run(name string, source string, native_source string) {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_vox_review_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vox_review' }") or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), source) or { panic(err) }
	if native_source != '' {
		os.write_file(os.join_path(root, 'native.m'), native_source) or { panic(err) }
		os.write_file(os.join_path(root, 'types.h'), 'typedef int NativeNumber;\n') or { panic(err) }
		os.write_file(os.join_path(root, 'types.c.v'), r'module main
#include "@VMODROOT/types.h"
struct NativeBox { value C.NativeNumber }
fn native_box_value() int {
	box := NativeBox{value: 7}
	return int(box.value)
}
') or { panic(err) }
	}
	for mode in ['tinyc', 'clang', 'clang-prod'] {
		if mode != 'tinyc' && os.find_abs_path_of_executable('clang') == none { continue }
		program := os.join_path(root, 'program_${mode}')
		mut args := ['-b', 'fastc', '-d', 'fastc_real_builtin', '-nocache', '-showcc']
		if mode != 'tinyc' { args << ['-cc', 'clang'] }
		if mode == 'clang-prod' { args << '-prod' }
		args << ['-o', program, root]
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, '${name} ${mode}: ${build.output}'
		if mode != 'tinyc' {
			assert build.output.contains('-lm'), build.output
		}
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${name} ${mode}: ${run.output}'
	}
}

fn test_fastc_vox_review_indexed_matches_keep_element_identity() {
	fastc_vox_review_run('indexed_matches', r'module main
struct Left { value int }
struct Right { value int }
type Value = Left | Right
struct Holder { value Value }
fn read(value &Value) int {
	return match value {
		Left { value.value }
		Right { value.value }
	}
}
fn main() {
	values := [Value(Left{11}), Value(Right{22})]
	match values[0] {
		Left { assert values[0].value == 11; assert values[1].value == 22 }
		Right { assert false }
	}
	selected := match values[0] {
		Left { values[1].value }
		Right { 0 }
	}
	assert selected == 22
	if values[0] is Left {
		assert values[0].value == 11
		assert values[1].value == 22
	}
	mapping := {"left": Value(Left{11}), "right": Value(Right{22})}
	match mapping["left"] {
		Left { assert mapping["right"].value == 22 }
		Right { assert false }
	}
	holder := Holder{value: Left{11}}
	if holder.value is Left { assert read(holder.value) == 11 }
}
', '')
}

fn test_fastc_vox_review_clang_preserves_wrapping_and_links_math() {
	fastc_vox_review_run('clang_arithmetic', r'module main
@[noinline]
fn increases(value i32) bool { return value + 1 > value }
@[noinline]
fn power(base f64, exponent f64) f64 { return base ** exponent }
fn main() {
	assert !increases(i32(2147483647))
	assert increases(42)
	assert power(2.0, 3.0) == 8.0
}
', '')
}

fn test_fastc_vox_review_comptime_callbacks_have_distinct_bodies() {
	fastc_vox_review_run('comptime_callbacks', r'module main
struct Fields { x int long_field_name int }
fn main() {
	$for field in Fields.fields {
		name := field.name
		callback := fn () string { return field.name }
		assert callback() == name
		other := fn () string { return field.name + "!" }
		assert other() == name + "!"
	}
}
', '')
}

fn test_fastc_vox_review_optional_elements_keep_missing_index_fallback() {
	fastc_vox_review_run('optional_bounds', r'module main
type Recursive = []Recursive | int
fn entries() ![]Recursive { return [Recursive(42)] }
fn missing_index(mut calls []int) int { calls << 1; return 5 }
fn main() {
	values := [?int(42), ?int(none)]
	if value := values[0] { assert value == 42 } else { assert false }
	for index in [-1, 1, 2, 5] {
		if value := values[index] { assert false, "${value}" }
		assert values[index] or { 73 } == 73
	}
	assert values[0] or { 0 } == 42
	empty := []?int{}
	if value := empty[0] { assert false, "${value}" }
	mut calls := []int{}
	if value := values[missing_index(mut calls)] { assert false, "${value}" }
	assert calls == [1]
	mut items := []Recursive{}
	items << entries()!
	assert items.len == 1
	match items[0] {
		int { assert items[0] == 42 }
		[]Recursive { assert false }
	}
}
', '')
}

fn test_fastc_vox_review_native_includes_keep_macro_context() {
	fastc_vox_review_run('native_macros', r'module main
#define NATIVE_ANSWER 7
#define NATIVE_FUNCTION native_seven
#include "@VMODROOT/native.m"
#undef NATIVE_ANSWER
#undef NATIVE_FUNCTION
#define NATIVE_ANSWER 9
#define NATIVE_FUNCTION native_nine
#include "@VMODROOT/native.m"
#undef NATIVE_ANSWER
#undef NATIVE_FUNCTION
fn C.native_seven(value string) int
fn C.native_nine(value string) int
fn main() {
	assert C.native_seven("abc") == 10
	assert C.native_nine("abcd") == 13
	assert native_box_value() == 7
}
', 'static int NATIVE_FUNCTION(string value) { return NATIVE_ANSWER + value.len; }\n')
}
