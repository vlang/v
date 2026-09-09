import os

const fixed_array_force_vexe = @VEXE
const fixed_array_force_tests_dir = os.dir(@FILE)
const fixed_array_force_v3_dir = os.dir(fixed_array_force_tests_dir)
const fixed_array_force_vlib_dir = os.dir(fixed_array_force_v3_dir)
const fixed_array_force_v3_src = os.join_path(fixed_array_force_v3_dir, 'v3.v')

fn fixed_array_force_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_force_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fixed_array_force_vexe} -gc none -path "${fixed_array_force_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fixed_array_force_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn fixed_array_force_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_fixed_array_force_${os.getpid()}')
	os.rmdir_all(root) or {}
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
	return root
}

fn test_fixed_array_force_array_literal_const_field_does_not_leak_array_temp() {
	v3_bin := fixed_array_force_build_v3()
	root := fixed_array_force_write_project()
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('__arr_lit_'), generated
	assert !generated.contains('Array fixture____arr_lit'), generated
	assert generated.contains('fixture__imported_pass'), generated
	assert generated.contains('next_function_marker'), generated
}
