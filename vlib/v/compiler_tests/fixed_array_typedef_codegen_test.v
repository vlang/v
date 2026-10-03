import os

const fixed_array_vexe = @VEXE
const fixed_array_tests_dir = os.dir(@FILE)
const fixed_array_v3_dir = os.dir(fixed_array_tests_dir)
const fixed_array_vlib_dir = os.dir(fixed_array_v3_dir)
const fixed_array_v3_src = os.join_path(fixed_array_v3_dir, 'v.v')

fn fixed_array_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_typedef_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.exec([fixed_array_vexe, '-gc', 'none', '-path',
			'${fixed_array_vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin, '${fixed_array_v3_src}'])
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
	run_compile := os.exec([v3_bin, run_root, '-b', 'c', '-o', run_bin])
	assert run_compile.exit_code == 0, run_compile.output
	run_c := os.read_file(run_bin + '.c') or { panic(err) }
	assert !run_c.contains('[max_items]'), run_c
	assert !run_c.contains('[rows]'), run_c
	assert !run_c.contains('[cols]'), run_c
	assert run_c.contains('images[8]'), run_c
	assert run_c.contains('cells[6][16]'), run_c
	run := os.exec([run_bin])
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
	shape_compile := os.exec([v3_bin, shape_root, '-b', 'c', '-o', shape_c_path])
	assert shape_compile.exit_code == 0, shape_compile.output
	shape_c := os.read_file(shape_c_path) or { panic(err) }
	assert !shape_c.contains('[max_items]'), shape_c
	assert !shape_c.contains('[rows]'), shape_c
	assert !shape_c.contains('[cols]'), shape_c
	assert shape_c.contains('typedef i64 Array_fixed_i64_8[8];'), shape_c
	assert shape_c.contains('typedef i64 Array_fixed_i64_16[16];'), shape_c
	assert shape_c.contains('typedef Array_fixed_i64_16 Array_fixed_Array_fixed_i64_16_6[6];'), shape_c
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
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
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
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
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
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
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
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '17', run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	size_pos := generated.index('struct fixture__ZSize {') or { -1 }
	typedef_pos := generated.index('typedef u8 Array_fixed_u8_sizeof_fixture__ZSize') or { -1 }
	holder_pos := generated.index('struct fixture__ZZHolder {') or { -1 }
	assert size_pos >= 0, generated
	assert typedef_pos > size_pos, generated
	assert holder_pos > typedef_pos, generated
}

fn test_imported_fn_pointer_fixed_array_of_backed_enum_uses_emitted_typedef() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('fn_pointer_enum', 'module fixture

pub enum Mode as u32 {
	one
	two
}

pub struct Extent {
	width u32
}

pub type Callback = fn (handle voidptr, extent &Extent, modes [2]Mode)
', 'module main

import fixture

fn main() {
	println(fixture.Mode.one)
}
')
	bin := os.join_path(root, 'out')
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'one', run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('typedef fixture__Mode Array_fixed_fixture__Mode_2[2];'), generated
	assert !generated.contains('Array_fixed_int_2'), generated
}

fn test_enum_fixed_array_fn_pointer_uses_emitted_typedef_name() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('enum_fn_pointer', 'module fixture

pub enum Combiner as i32 {
	keep = 0
	replace = 1
}

pub type Callback = fn (command voidptr, ops [2]Combiner)

pub fn callback_size() int {
	return sizeof(Callback)
}
', 'module main

import fixture

fn main() {
	println(fixture.callback_size())
}
')
	bin := os.join_path(root, 'out')
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Array_fixed_fixture__Combiner_2[2]'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().int() > 0, run.output
}

fn test_enum_fixed_array_fn_pointer_pointer_param_uses_emitted_typedef_name() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('enum_fn_pointer_param', 'module fixture

pub enum Mode as u32 {
	one
	two
}

pub type Callback = fn (modes &[2]Mode)

pub fn callback_size() int {
	return sizeof(Callback)
}
', 'module main

import fixture

fn main() {
	println(fixture.callback_size())
}
')
	bin := os.join_path(root, 'out')
	compile := os.exec([v3_bin, '-b', 'c', '-o', bin, root])
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('(Array_fixed_fixture__Mode_2*)'), generated
	assert !generated.contains('Array_fixed_int_2'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().int() > 0, run.output
}

fn test_enum_fixed_array_fn_pointer_return_alias_emits_wrapper() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('enum_fn_pointer_return', 'module fixture

pub enum Mode as u32 {
	one
	two
}

pub type Callback = fn () [2]Mode

pub fn callback_size() int {
	return sizeof(Callback)
}
', 'module main

import fixture

fn main() {
	println(fixture.callback_size())
}
')
	bin := os.join_path(root, 'out')
	compile := os.exec([v3_bin, '-b', 'c', '-o', bin, root])
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('typedef _v_ret_Array_fixed_fixture__Mode_2 (*_fn_ptr_'), generated
	assert !generated.contains('Array_fixed_int_2'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().int() > 0, run.output
}

fn test_backed_enum_callbacks_are_passed_and_invoked() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('enum_callback_calls', 'module fixture
pub enum Mode as i32 {
	one = 7
	two = 19
}
', '')
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'callbacks.h')
	os.write_file(header, '#include <stdint.h>
static int32_t foreign_invoke(int32_t (*cb)(int32_t*)) {
	int32_t modes[2] = {7, 19};
	return cb(modes);
}
static int32_t foreign_first(int32_t* modes) {
	return modes[0] + modes[1];
}
static int32_t foreign_nested(int32_t (*cb)(int32_t (*)(int32_t*))) {
	return cb(foreign_first);
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import fixture
#insert "${header}"

type Callback = fn (modes [2]fixture.Mode) int
type NestedCallback = fn (inner Callback) int

fn first(modes [2]fixture.Mode) int {
	return int(modes[0]) + int(modes[1])
}
fn invoke(cb fn (modes [2]fixture.Mode) int) int {
	return cb([fixture.Mode.one, fixture.Mode.two]!)
}
fn forward(cb Callback) int {
	return invoke(cb) + 1
}
fn nested(cb fn (inner fn (modes [2]fixture.Mode) int) int) int {
	return cb(first)
}
fn first_pointer(modes &[2]fixture.Mode) int {
	return int(modes[0]) + int(modes[1])
}
fn invoke_pointer(cb fn (modes &[2]fixture.Mode) int) int {
	mut modes := [fixture.Mode.one, fixture.Mode.two]!
	return cb(&modes)
}
fn make_modes() [2]fixture.Mode {
	return [fixture.Mode.one, fixture.Mode.two]!
}
fn invoke_factory(cb fn () [2]fixture.Mode) int {
	modes := cb()
	return int(modes[0]) + int(modes[1])
}
fn C.foreign_invoke(cb fn (modes [2]fixture.Mode) i32) i32
fn C.foreign_nested(cb fn (inner fn (modes [2]fixture.Mode) i32) i32) i32
fn c_first(modes [2]fixture.Mode) i32 {
	return i32(modes[0]) + i32(modes[1])
}
fn c_forward(cb fn (modes [2]fixture.Mode) i32) i32 {
	return cb([fixture.Mode.one, fixture.Mode.two]!) + 1
}
fn main() {
	assert invoke(first) == 26
	assert invoke(Callback(first)) == 26
	assert nested(forward) == 27
	assert nested(NestedCallback(forward)) == 27
	assert invoke_pointer(first_pointer) == 26
	assert invoke_factory(make_modes) == 26
	assert C.foreign_invoke(c_first) == 26
	assert C.foreign_nested(c_forward) == 27
}
') or { panic(err) }
	exe_suffix := $if windows { '.exe' } $else { '' }
	bin := os.join_path(root, 'out${exe_suffix}')
	compile := os.exec([v3_bin, '-b', 'c', '-o', bin, root])
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('Array_fixed_int_2'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}

fn test_backed_enum_optional_result_callbacks_keep_payload_types() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('enum_optional_callback_calls', 'module fixture
pub enum Mode as i32 {
 one = 7
 two = 19
}
', "module main
import fixture

type OptionalMaker = fn () ?[2]fixture.Mode
type ResultMaker = fn () ![2]fixture.Mode

fn make_option() ?[2]fixture.Mode {
 return [fixture.Mode.one, fixture.Mode.two]!
}
fn none_option() ?[2]fixture.Mode {
 return none
}
fn make_result() ![2]fixture.Mode {
 return [fixture.Mode.one, fixture.Mode.two]!
}
fn error_result() ![2]fixture.Mode {
 return error('missing modes')
}
fn invoke_option(cb fn () ?[2]fixture.Mode) int {
 modes := cb() or { return -1 }
 return int(modes[0]) + int(modes[1])
}
fn invoke_result(cb fn () ![2]fixture.Mode) int {
 modes := cb() or { return -1 }
 return int(modes[0]) + int(modes[1])
}
fn result_error_message(cb fn () ![2]fixture.Mode) string {
 cb() or { return err.msg() }
 return 'ok'
}
fn main() {
 assert invoke_option(make_option) == 26
 assert invoke_option(OptionalMaker(make_option)) == 26
 assert invoke_option(none_option) == -1
 assert invoke_option(OptionalMaker(none_option)) == -1
 assert invoke_result(make_result) == 26
 assert invoke_result(ResultMaker(make_result)) == 26
 assert invoke_result(error_result) == -1
 assert invoke_result(ResultMaker(error_result)) == -1
 assert result_error_message(error_result) == 'missing modes'
 assert result_error_message(ResultMaker(error_result)) == 'missing modes'
 assert result_error_message(make_result) == 'ok'
}
")
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
	}
	exe_suffix := $if windows { '.exe' } $else { '' }
	bin := os.join_path(root, 'out${exe_suffix}')
	compile := os.exec([v3_bin, '-b', 'c', '-o', bin, root])
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('Array_fixed_int_2'), generated
	assert generated.contains('Array_fixed_fixture__Mode_2 (*_fn_ptr_'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}

fn test_import_alias_const_fixed_array_length_is_folded() {
	v3_bin := fixed_array_build_v3()
	root := fixed_array_write_project('import_alias_const', 'module fixture

pub const max_name_size = u32(256)
', 'module main

import fixture as fx
import fx as otherfx

fn name_size(name [fx /* imported const */ .max_name_size]char) int {
	return name.len
}

fn main() {
	assert otherfx.max_name_size == 16
	println(name_size([fx.max_name_size /* trailing comment */]char{}))
}
')
	// A real module named like the alias must not capture its constant.
	os.mkdir_all(os.join_path(root, 'fx')) or { panic(err) }
	os.write_file(os.join_path(root, 'fx', 'fx.v'), 'module fx
pub const max_name_size = 16
') or { panic(err) }
	bin := os.join_path(root, 'out')
	compile := os.exec([v3_bin, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '256', run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Array_fixed_char_256[256]'), generated
	assert !generated.contains('Array_fixed_char_fx__max_name_size'), generated

	// A second source file gives the alias another meaning. The real fx module
	// must not silently supply its length when the stored type cannot choose one.
	os.mkdir_all(os.join_path(root, 'second')) or { panic(err) }
	os.write_file(os.join_path(root, 'second', 'second.v'), 'module second
pub const max_name_size = 128
') or { panic(err) }
	os.write_file(os.join_path(root, 'second_import.v'), 'module main
import second as fx
fn second_size() int {
	return fx.max_name_size
}
') or { panic(err) }
	ambiguous_bin := os.join_path(root, 'ambiguous')
	ambiguous := os.exec([v3_bin, '-b', 'c', '-o', ambiguous_bin, root])
	assert ambiguous.exit_code != 0, ambiguous.output
	assert ambiguous.output.contains('non-constant array bound `fx.max_name_size`'), ambiguous.output
}
