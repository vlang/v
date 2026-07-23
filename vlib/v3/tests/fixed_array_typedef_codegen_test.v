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
	assert shape_c.contains('typedef int Array_fixed_int_8[8];'), shape_c
	assert shape_c.contains('typedef int Array_fixed_int_16[16];'), shape_c
	assert shape_c.contains('typedef Array_fixed_int_16 Array_fixed_Array_fixed_int_16_6[6];'), shape_c
}

fn test_fixed_array_typedefs_keep_declaring_module_with_unrelated_math_import() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('module_authority', 'module fixture

pub struct Image {
pub mut:
	id int
}

pub struct TouchCore {
pub mut:
	x int
}

pub type TouchPoint = TouchCore

pub struct Holder {
pub mut:
	images  [12]Image
	touches [8]TouchPoint
}

pub fn score() int {
	mut h := Holder{}
	h.images[0].id = 4
	h.touches[0].x = 5
	return h.images[0].id + h.touches[0].x
}
', 'module main

import math
import fixture

fn main() {
	println(int_str(fixture.score() + int(math.sqrt(4))))
}
')
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '11', run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Array_fixed_fixture__Image_12'), generated
	assert generated.contains('Array_fixed_fixture__TouchCore_8'), generated
	assert !generated.contains('math__Image'), generated
	assert !generated.contains('math__TouchPoint'), generated
	assert !generated.contains('Array_fixed_math__'), generated
}

fn test_sizeof_fixed_array_typedef_precedes_function_pointer() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('sizeof_fn_ptr', 'module fixture

pub type Callback = fn ([sizeof(int)]u8) int

fn first_byte(data [sizeof(int)]u8) int {
	return int(data[0])
}

pub fn call() int {
	callback := Callback(first_byte)
	mut data := [sizeof(int)]u8{}
	data[0] = 7
	return callback(data)
}
', 'module main

import fixture

fn main() {
	println(fixture.call())
}
')
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7', run.output
}

fn test_sizeof_pointer_fixed_array_typedef_precedes_function_pointer() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('sizeof_pointer_fn_ptr', 'module fixture
', 'module main

struct Payload {
	value int
}

type IntPointerCallback = fn ([sizeof(&int)]u8) int
type StructPointerCallback = fn ([sizeof(&Payload)]u8) int

fn int_pointer_size(data [sizeof(&int)]u8) int {
	return data.len
}

fn struct_pointer_size(data [sizeof(&Payload)]u8) int {
	return data.len
}

fn main() {
	int_callback := IntPointerCallback(int_pointer_size)
	struct_callback := StructPointerCallback(struct_pointer_size)
	int_data := [sizeof(&int)]u8{}
	struct_data := [sizeof(&Payload)]u8{}
	println(int_callback(int_data) + struct_callback(struct_data))
}
')
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().int() > 0, run.output
}

fn test_sizeof_struct_fixed_array_typedef_is_emitted_when_target_is_defined() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('sizeof_struct', 'module fixture

pub type Bytes = [sizeof(ZSize)]u8

pub struct ZSize {
	value int
}

pub struct ZZHolder {
pub mut:
	data Bytes
}

pub fn score() int {
	mut holder := ZZHolder{}
	holder.data[0] = 9
	return int(holder.data[0]) + holder.data.len
}
', 'module main

import fixture

fn main() {
	println(fixture.score())
}
')
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '13', run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	size_pos := generated.index('struct fixture__ZSize {') or { -1 }
	typedef_pos := generated.index('typedef u8 Array_fixed_u8_sizeof_fixture__ZSize') or { -1 }
	holder_pos := generated.index('struct fixture__ZZHolder {') or { -1 }
	assert size_pos >= 0, generated
	assert typedef_pos > size_pos, generated
	assert holder_pos > typedef_pos, generated
}
