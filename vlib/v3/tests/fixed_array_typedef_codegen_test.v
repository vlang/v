import os

const fixed_array_vexe = @VEXE
const fixed_array_tests_dir = os.dir(@FILE)
const fixed_array_v3_dir = os.dir(fixed_array_tests_dir)
const fixed_array_vlib_dir = os.dir(fixed_array_v3_dir)
const fixed_array_v3_src = os.join_path(fixed_array_v3_dir, 'v3.v')

fn fixed_array_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_typedef_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fixed_array_vexe} -gc none -path "${fixed_array_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fixed_array_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn fixed_array_write_project(name string, fixture_src string, main_src string) string {
	root := os.join_path(os.temp_dir(), 'v3_fixed_array_typedef_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	fixture_dir := os.join_path(root, 'fixture')
	os.mkdir_all(fixture_dir) or { panic(err) }
	os.write_file(os.join_path(fixture_dir, 'arrays.c.v'), fixture_src) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), main_src) or { panic(err) }
	return root
}

fn test_fixed_array_typedefs_fold_module_const_lengths() {
	v3_bin := fixed_array_build_v3()

	run_root := fixed_array_write_project('run', 'module fixture

const max_items = 8
const rows = 6
const cols = 16

pub struct Widget {
mut:
	images [max_items]int
}

pub struct Nested {
mut:
	cells [rows][cols]int
}

pub fn score() int {
	mut w := Widget{}
	mut n := Nested{}
	w.images[0] = 3
	n.cells[1][2] = 5
	return w.images[0] + n.cells[1][2]
}
', 'module main

import fixture

fn main() {
	println(int_str(fixture.score()))
}
')
	run_bin := os.join_path(run_root, 'out')
	run_compile := os.execute('${v3_bin} ${run_root} -b c -o ${run_bin}')
	assert run_compile.exit_code == 0, run_compile.output
	run_c := os.read_file(run_bin + '.c') or { panic(err) }
	assert !run_c.contains('[max_items]'), run_c
	assert !run_c.contains('[rows]'), run_c
	assert !run_c.contains('[cols]'), run_c
	assert run_c.contains('images[8]'), run_c
	assert run_c.contains('cells[6][16]'), run_c
	run := os.execute(run_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '8'

	shape_root := fixed_array_write_project('shape', 'module fixture

const max_items = 8
const rows = 6
const cols = 16

pub struct C.Widget {
pub mut:
	images [max_items]int
}

pub struct Nested {
mut:
	cells [rows][cols]int
}

pub fn shape_score() int {
	mut n := Nested{}
	n.cells[1][2] = 5
	return n.cells[1][2]
}
', 'module main

import fixture

fn main() {
	println(int_str(fixture.shape_score()))
}
')
	shape_c_path := os.join_path(shape_root, 'out.c')
	shape_compile := os.execute('${v3_bin} ${shape_root} -b c -o ${shape_c_path}')
	assert shape_compile.exit_code == 0, shape_compile.output
	shape_c := os.read_file(shape_c_path) or { panic(err) }
	assert !shape_c.contains('[max_items]'), shape_c
	assert !shape_c.contains('[rows]'), shape_c
	assert !shape_c.contains('[cols]'), shape_c
	assert shape_c.contains('typedef int Array_fixed_int_max_items[8];'), shape_c
	assert shape_c.contains('typedef int Array_fixed_int_cols[16];'), shape_c
	assert shape_c.contains('typedef Array_fixed_int_cols Array_fixed_Array_fixed_int_cols_rows[6];'), shape_c
}
