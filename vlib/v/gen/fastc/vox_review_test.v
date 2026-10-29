module fastc

import os
import v.cmdexec

fn fastc_vox_review_run(name string, source string, native_source string) {
	mut files := {
		'main.c.v': source
	}
	if native_source != '' {
		files['native.m'] = native_source
		files['types.h'] = 'typedef int NativeNumber;\n'
		files['types.c.v'] = r'module main
#include "@VMODROOT/types.h"
struct NativeBox { value C.NativeNumber }
fn native_box_value() int {
	box := NativeBox{value: 7}
	return int(box.value)
}
'
	}
	fastc_vox_review_run_files(name, files)
}

fn fastc_vox_review_run_files(name string, files map[string]string) {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_vox_review_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vox_review' }") or { panic(err) }
	for path, source in files {
		os.write_file(os.join_path(root, path), source) or { panic(err) }
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

fn test_fastc_vox_review_indexed_matches_end_when_the_key_changes() {
	fastc_vox_review_run('changed_indices', r'module main
struct Left { value int only_left int }
struct Right { value int }
type Value = Left | Right
struct Selector { mut: index int other int }
struct Container { values []Value mut: other int }
fn main() {
	values := [Value(Left{11, 77}), Value(Right{22})]
	mut container := Container{values: values}
	match container.values[0] {
		Left {
			container.other = 9
			assert container.values[0].only_left == 77
		}
		Right { assert false }
	}
	mut selector := Selector{}
	match values[selector.index] {
		Left {
			selector.other = 9
			assert values[selector.index].only_left == 77
			selector.index = 1
			assert values[selector.index].value == 22
		}
		Right { assert false }
	}
	mut index := 0
	match values[index] {
		Left {
			assert values[index].only_left == 77
			index = if values[index].only_left == 77 { 1 } else { 0 }
			assert values[index].value == 22
		}
		Right { assert false }
	}
	index = 0
	match values[index] {
		Left { index++; assert values[index].value == 22 }
		Right { assert false }
	}
	index = 0
	selected := match values[index] {
		Left { index += 1; values[index].value }
		Right { 0 }
	}
	assert selected == 22
	index = 0
	match values[index] {
		Left {
			if values[index] is Left { index = 1 }
			assert values[index].value == 22
		}
		Right { assert false }
	}
	index = 0
	mut other := 0
	match values[index] {
		Left { index, other = 1, 9; assert values[index].value == 22 }
		Right { assert false }
	}
	assert other == 9
	index = 0
	match values[index] {
		Left { index, selector.other = 1, 3; assert values[index].value == 22 }
		Right { assert false }
	}
	index = 0
	match values[index] {
		Left { selector.other, index = 4, 1; assert values[index].value == 22 }
		Right { assert false }
	}
	selector.index = 0
	match values[selector.index] {
		Left {
			other, selector.other = 4, 5
			assert values[selector.index].only_left == 77
			other, selector.index = 4, 1
			assert values[selector.index].value == 22
		}
		Right { assert false }
	}
	selector.index = 0
	match values[selector.index] {
		Left { selector.index, other = 1, 5; assert values[selector.index].value == 22 }
		Right { assert false }
	}
	mapping := {"left": Value(Left{11, 77}), "right": Value(Right{22})}
	mut key := "left"
	match mapping[key] {
		Left { key = "right"; assert mapping[key].value == 22 }
		Right { assert false }
	}
	key = "left"
	selected_map := match mapping[key] {
		Left { key = "right"; mapping[key].value }
		Right { 0 }
	}
	assert selected_map == 22
}
', '')
}

fn test_fastc_vox_review_boolean_map_pointer_reads_keep_compound_keys() {
	fastc_vox_review_run('boolean_map_keys', r'module main
fn matches(cache &map[string]bool, programs &map[string]bool, name string) bool {
	return cache["C." + name] || cache[name] || programs["C." + name] || programs[name]
}
fn main() {
	cache := {"C.left": true, "plain": true}
	programs := {"C.right": true, "ordinary": true}
	assert matches(&cache, &programs, "left")
	assert matches(&cache, &programs, "right")
	assert matches(&cache, &programs, "plain")
	assert matches(&cache, &programs, "ordinary")
	assert !matches(&cache, &programs, "absent")
}
', '')
}

fn test_fastc_vox_review_native_includes_keep_cross_file_macro_context() {
	fastc_vox_review_run_files('cross_file_macros', {
		'a.c.v':  r'module main
#define NATIVE_ANSWER 7
#include "@VMODROOT/a.m"
#undef NATIVE_ANSWER
fn C.answer_a() i32
'
		'b.c.v':  r'module main
#define NATIVE_ANSWER 9
#include "@VMODROOT/b.m"
#undef NATIVE_ANSWER
fn C.answer_b() i32
'
		'a.m':    'static int answer_a(void) { return NATIVE_ANSWER; }\n'
		'b.m':    'static int answer_b(void) { return NATIVE_ANSWER; }\n'
		'main.v': 'module main\nfn main() { assert C.answer_a() == 7; assert C.answer_b() == 9 }\n'
	})
}

fn test_fastc_vox_review_headers_after_native_includes_define_field_types() {
	fastc_vox_review_run_files('following_header', {
		'main.c.v': r'module main
#include "@VMODROOT/native.m"
#include "@VMODROOT/types.h"
struct NativeBox { value C.NativeNumber }
fn C.native_answer() i32
fn main() { assert sizeof(NativeBox) > 0; assert C.native_answer() == 7 }
'
		'native.m': 'static int native_answer(void) { return 7; }\n'
		'types.h':  'typedef struct NativeNumber { int value; } NativeNumber;\n'
	})
}

fn test_fastc_vox_review_header_macro_defaults_survive_relocation() {
	fastc_vox_review_run_files('header_macro_default', {
		'main.c.v': r'module main
#include "@VMODROOT/types.h"
#include "@VMODROOT/native.m"
#undef NATIVE_ANSWER
fn C.native_answer() i32
fn main() { assert C.native_answer() == 7 }
'
		'types.h':  '#define NATIVE_ANSWER 7\n'
		'native.m': 'static int native_answer(void) { return NATIVE_ANSWER; }\n'
	})
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
