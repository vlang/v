import os
import v.tempname

// fixed_array_force_arr_lit_owners returns, for each array literal temporary in
// the generated C, the header of the function that declares it, or the line
// itself, prefixed with `file scope:`, when it is outside any function.
fn fixed_array_force_arr_lit_owners(generated string) []string {
	mut owners := []string{}
	mut owner := ''
	for line in generated.split_into_lines() {
		at_top := line.len > 0 && line[0] != ` ` && line[0] != `\t`
		if at_top && line.ends_with('{') {
			owner = line
		}
		if line.contains('__arr_lit_') {
			owners << if at_top { 'file scope: ${line}' } else { owner }
		}
	}
	return owners
}

fn fixed_array_force_write_project(root string) {
	fixture_dir := os.join_path(root, 'fixture')
	os.mkdir_all(fixture_dir) or { panic(err) }
	os.write_file(os.join_path(fixture_dir, 'pass_action.h'), '#ifndef V3_FIXED_ARRAY_FORCE_PASS_ACTION_H
#define V3_FIXED_ARRAY_FORCE_PASS_ACTION_H
struct pass_action {
	int colors[4];
};
#endif
') or {
		panic(err)
	}
	os.write_file(os.join_path(fixture_dir, 'fixture.v'), 'module fixture

#include "@DIR/pass_action.h"

pub struct Color {
pub:
	r int
}

pub struct DirectPass {
pub:
	colors [4]Color
}

pub struct C.pass_action {
pub:
	colors [4]int
}

pub type Pass = C.pass_action

pub const direct_pass = DirectPass{
	colors: [
		Color{r: 1},
		Color{r: 2},
		Color{r: 3},
		Color{r: 4},
	]!
}

pub const imported_pass = Pass{
	colors: [
		5,
		6,
		7,
		8,
	]!
}

pub fn pass_score() int {
	return direct_pass.colors[0].r + direct_pass.colors[1].r + direct_pass.colors[2].r + direct_pass.colors[3].r +
		imported_pass.colors[0] + imported_pass.colors[1] + imported_pass.colors[2] + imported_pass.colors[3]
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import fixture

fn next_function_marker() int {
	return 6
}

fn main() {
	println(int_str(fixture.pass_score() + next_function_marker()))
}
') or {
		panic(err)
	}
}

fn test_fixed_array_force_array_literal_const_field_does_not_leak_array_temp() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_force_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	fixed_array_force_write_project(root)
	bin := os.join_path(root, 'out')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	generated := os.read_file(bin + '.c') or { panic(err) }
	// Runtime code may build array literals (the Linux backtrace printer does);
	// the fixture's constants, their initialization and main must not.
	for owner in fixed_array_force_arr_lit_owners(generated) {
		assert !owner.starts_with('file scope:') && !owner.contains('fixture')
			&& !owner.contains('main') && !owner.contains('_vinit'), owner
	}
	assert !generated.contains('Array fixture____arr_lit'), generated
	assert generated.contains('fixture__imported_pass'), generated
	assert generated.contains('next_function_marker'), generated
}
