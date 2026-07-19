import os

const selective_import_vexe = @VEXE
const selective_import_tests_dir = os.dir(@FILE)
const selective_import_v3_dir = os.dir(selective_import_tests_dir)
const selective_import_vlib_dir = os.dir(selective_import_v3_dir)
const selective_import_v3_src = os.join_path(selective_import_v3_dir, 'v3.v')

fn selective_import_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_selective_import_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${selective_import_vexe} -gc none -path "${selective_import_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${selective_import_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn selective_import_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn selective_import_write_project(name string, main_src string) string {
	return selective_import_write_project_with_extra(name, main_src, {})
}

fn selective_import_write_project_with_extra(name string, main_src string, extra_files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_selective_import_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	selective_import_write_file(root, 'v.mod', "Module { name: 'selective_import_test' }\n")
	selective_import_write_file(root, 'mymodules/main_functions.v', 'module mymodules

pub fn add_xy(x int, y int) int {
	return x + y
}

pub fn hidden_xy(x int, y int) int {
	return x * 100 + y
}
')
	selective_import_write_file(root, 'mymodules/submodule/sub_functions.v', 'module submodule

pub fn sub_xy(x int, y int) int {
	return x - y
}
')
	selective_import_write_file(root, 'main.v', main_src)
	for rel, src in extra_files {
		selective_import_write_file(root, rel, src)
	}
	return root
}

fn selective_import_compile_run(v3_bin string, name string, main_src string) (string, string) {
	root := selective_import_write_project(name, main_src)
	return selective_import_compile_run_root(v3_bin, root)
}

fn selective_import_compile_run_with_extra(v3_bin string, name string, main_src string, extra_files map[string]string) (string, string) {
	root := selective_import_write_project_with_extra(name, main_src, extra_files)
	return selective_import_compile_run_root(v3_bin, root)
}

fn selective_import_compile_run_root(v3_bin string, root string) (string, string) {
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	return run.output.trim_space(), generated
}

fn selective_import_compile_bad(v3_bin string, name string, main_src string) string {
	root := selective_import_write_project(name, main_src)
	return selective_import_compile_bad_root(v3_bin, name, root)
}

fn selective_import_compile_bad_with_extra(v3_bin string, name string, main_src string, extra_files map[string]string) string {
	root := selective_import_write_project_with_extra(name, main_src, extra_files)
	return selective_import_compile_bad_root(v3_bin, name, root)
}

fn selective_import_compile_bad_root(v3_bin string, name string, root string) string {
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), compile.output
	return compile.output
}

fn selective_import_ambiguous_modules() map[string]string {
	return {
		'a/a.v': 'module a

pub fn hit() int {
	return 1
}
'
		'b/b.v': 'module b

pub fn hit() int {
	return 2
}
'
	}
}

fn selective_import_type_collision_modules() map[string]string {
	return {
		'geometry/geometry.v': 'module geometry

pub struct Point {
pub:
	x int
}

pub struct Size {
pub:
	w int
}

pub type ItemId = int

pub enum Mode {
	on = 7
	off = 8
}

@[flag]
pub enum Perm {
	a
	b
}

pub struct Box[T] {
pub:
	value T
}
'
		'pixels/pixels.v':     'module pixels

pub struct Point {
pub:
	x int
}

pub struct Size {
pub:
	w int
}

pub type ItemId = int

pub enum Mode {
	on = 70
	off = 80
}

@[flag]
pub enum Perm {
	a
	c
	b
}

pub struct Box[T] {
pub:
	other T
}
'
	}
}

fn test_selective_import_calls_module_and_submodule_functions() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'positive', 'module main

import mymodules { add_xy }
import mymodules.submodule { sub_xy }

fn main() {
	println(int_str(add_xy(2, 3)))
	println(int_str(sub_xy(10, 7)))
}
')
	assert output == '5\n3'
	assert generated.contains('mymodules__add_xy(2, 3)'), generated
	assert generated.contains('submodule__sub_xy(10, 7)'), generated
}

fn test_import_alias_variadic_module_call_without_values_skips_module_receiver() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'module_alias_variadic_zero_args', 'module main

import logger as log

fn main() {
	println(int_str(log.sum()))
	println(int_str(log.sum(4, 5)))
}
', {
		'logger/logger.v': 'module logger

pub fn sum(values ...int) int {
	mut total := 0
	for value in values {
		total += value
	}
	return total
}
'
	})
	assert output == '0\n9'
	assert generated.contains('logger__sum('), generated
	assert !generated.contains('logger__sum(log'), generated
}

fn test_selective_import_json_decode_uses_fast_path() {
	v3_bin := selective_import_build_v3()
	json_output, json_generated := selective_import_compile_run(v3_bin, 'json_decode', 'module main

import json { decode }

struct Config {
	value int
}

fn main() {
	cfg := decode(Config, "{\\"value\\":1}")!
	println(int_str(cfg.value))
}
')
	assert json_output == '1'
	assert json_generated.contains('cJSON_ParseWithLength((char*)'), json_generated
	json2_output, json2_generated := selective_import_compile_run_with_extra(v3_bin,
		'json2_decode', 'module main

import json2

struct Config {
	value int
}

fn main() {
	cfg := json2.decode[Config]("{\\"value\\":2}", json2.DecoderOptions{}) or { Config{value: 8} }
	println(int_str(cfg.value))
}
', {
		'json2/decode.v': 'module json2

pub struct DecoderOptions {}

pub fn decode[T](val string, params DecoderOptions) !T {
	_ = val
	_ = params
	return T{}
}
'
	})
	assert json2_output == '0'
	assert json2_generated.contains('json2__decode'), json2_generated
}

fn test_const_ref_expansion_uses_referenced_const_file_imports() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin, 'const_ref_file_context', 'module main

import y.other as json

const outer = imported_value
const local_marker = json.Any{
	value: 1
}

fn main() {
	println(int_str(outer.value + local_marker.value))
}
', {
		'b.v':             'module main

import json2 as json

const imported_value = json.Any{
	value: 41
}
'
		'json2/json2.v':   'module json2

pub struct Any {
pub:
	value int
}
'
		'y/other/other.v': 'module other

pub struct Any {
pub:
	value int
}
'
	})
	assert output == '42'
	assert generated.contains('json2__Any'), generated
	assert generated.contains('other__Any'), generated
}

fn test_selective_import_json_encode_uses_fast_path() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'json_encode', 'module main

import json { encode }

struct User {
	name string
}

fn main() {
	println(encode(User{name: "x"}))
}
')
	assert output == '{"name":"x"}'
	assert generated.contains('v3_json_encode_string('), generated
}

fn test_json2_encode_pure_v_is_specialized() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'json2_encode_specialized', 'module main

import json2

struct User {
	name string
}

fn main() {
	println(json2.encode(User{name: "x"}, json2.EncoderOptions{}))
}
', {
		'json2/encode.v': 'module json2

pub struct EncoderOptions {}

pub fn encode[T](value T, options EncoderOptions) string {
	_ = value
	_ = options
	return "pure-v"
}
'
	})
	assert output == 'pure-v'
	assert generated.contains('json2__encode'), generated
}

fn test_selective_import_inside_generic_clone_keeps_source_file_symbol() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'generic_clone_selective_import', 'module main

import worker

fn main() {
	println(int_str(worker.use_add[int](0, 2, 3)))
}
', {
		'worker/worker.v': 'module worker

import mymodules { add_xy }
import other

pub fn use_add[T](marker T, x int, y int) int {
	_ = marker
	return add_xy(x, y)
}
'
		'other/other.v':   'module other

pub fn add_xy(x int, y int) int {
	return x * 100 + y
}
'
	})
	assert output == '5'
	assert generated.contains('mymodules__add_xy(x, y)'), generated
	assert !generated.contains('other__add_xy(x, y)'), generated
}

fn test_selective_import_explicit_generic_call_keeps_selected_symbol() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'explicit_generic_selective_import', 'module main

import util { id }
import other

fn main() {
	println(int_str(id[int](1)))
}
', {
		'util/util.v':   'module util

pub fn id[T](x T) T {
	return x
}
'
		'other/other.v': 'module other

pub fn id[T](x T) T {
	return x
}
'
	})
	assert output == '1'
	assert generated.contains('util__id_T_v_int(1)'), generated
	assert !generated.contains('other__id_T_v_int(1)'), generated
}

fn test_selective_import_fn_value_inside_generic_clone_keeps_source_file_symbol() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'generic_clone_selective_import_fn_value', 'module main

import worker

fn main() {
	println(int_str(worker.use_add_cb[int](0, 2, 3)))
}
', {
		'worker/worker.v': 'module worker

import mymodules { add_xy }
import other

fn takes(cb fn (int, int) int, x int, y int) int {
	return cb(x, y)
}

pub fn use_add_cb[T](marker T, x int, y int) int {
	_ = marker
	return takes(add_xy, x, y)
}
'
		'other/other.v':   'module other

pub fn add_xy(x int, y int) int {
	return x * 100 + y
}
'
	})
	assert output == '5'
	assert generated.contains('worker__takes(mymodules__add_xy, x, y)'), generated
	assert !generated.contains('worker__takes(add_xy, x, y)'), generated
	assert !generated.contains('worker__takes(other__add_xy, x, y)'), generated
}

fn test_selective_import_type_inside_generic_clone_signature_keeps_source_file_symbol() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'generic_clone_selective_import_type_signature', 'module main

import worker

fn main() {
	p := worker.make_point[int](7)
	println(int_str(worker.take_point[int](p, 2) + p.x))
}
', {
		'worker/worker.v':     'module worker

import geometry { Point }
import pixels

pub fn make_point[T](x T) Point {
	_ = x
	return Point{
		x: 3
	}
}

pub fn take_point[T](p Point, x T) int {
	_ = x
	return p.x + 4
}
'
		'geometry/geometry.v': 'module geometry

pub struct Point {
pub:
	x int
}
'
		'pixels/pixels.v':     'module pixels

pub struct Point {
pub:
	x int
}
'
	})
	assert output == '10'
	assert generated.contains('geometry__Point worker__make_point_T_v_int(int x)'), generated
	assert generated.contains('int worker__take_point_T_v_int(geometry__Point p, int x)'), generated
	assert !generated.contains('\nPoint worker__make_point_T_v_int(int x)'), generated
	assert !generated.contains('\npixels__Point worker__make_point_T_v_int(int x)'), generated
	assert !generated.contains('\nint worker__take_point_T_v_int(Point p, int x)'), generated
	assert !generated.contains('\nint worker__take_point_T_v_int(pixels__Point p, int x)'), generated
}

fn test_selective_import_symbol_can_be_used_as_function_value() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'fn_value', 'module main

import mymodules { add_xy }

fn main() {
	f := add_xy
	println(int_str(f(2, 3)))
}
')
	assert output == '5'
	assert generated.contains('mymodules__add_xy'), generated
	assert generated.contains('f(2, 3)'), generated
	assert !generated.contains('int f = mymodules__add_xy'), generated
}

fn test_selective_import_function_value_roots_exact_symbol_with_imported_homonym() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'fn_value_imported_homonym', 'module main

import a { choose }
import b

fn main() {
	f := choose
	println(int_str(f()))
}
', {
		'a/a.v': 'module a

pub fn choose() int {
	return 11
}
'
		'b/b.v': 'module b

pub fn choose() int {
	return 99
}
'
	})
	assert output == '11'
	assert generated.contains('a__choose'), generated
	assert !generated.contains('b__choose'), generated
}

fn test_selective_import_does_not_import_other_symbols_by_suffix() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad(v3_bin, 'hidden_symbol', 'module main

import mymodules { add_xy }

fn main() {
	println(int_str(hidden_xy(2, 3)))
}
')
	assert output.contains('unknown function `hidden_xy`'), output
}

fn test_selective_import_scope_is_file_local() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'file_local', 'module main

import mymodules { add_xy }

fn main() {
	println(int_str(add_xy(1, 2)))
}
', {
		'other.v': 'module main

fn from_other_file() int {
	return add_xy(3, 4)
}
'
	})
	assert output.contains('unknown function `add_xy`'), output
}

fn test_selective_import_same_symbol_from_two_modules_is_ambiguous() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_symbol', 'module main

import a { hit }
import b { hit }

fn main() {
	println(int_str(hit()))
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
}

fn test_duplicate_selective_import_fails_even_when_unused() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_unused', 'module main

import a { hit }
import b { hit }

fn main() {
	println("ok")
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
}

fn test_duplicate_selective_import_fails_even_with_local_homonym() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_local_homonym', 'module main

import a { hit }
import b { hit }

fn hit() int {
	return 3
}

fn main() {
	println(int_str(hit()))
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
}

fn test_duplicate_selective_import_function_value_reports_ambiguous() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_fn_value', 'module main

import a { hit }
import b { hit }

fn main() {
	f := hit
	println(int_str(f()))
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
	assert !output.contains('unknown identifier `hit`'), output
}

fn test_local_function_keeps_priority_over_selective_import_homonym() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'local_homonym', 'module main

import mymodules { add_xy }

fn add_xy(x int, y int) int {
	return x * y
}

fn main() {
	println(int_str(add_xy(2, 3)))
}
')
	assert output == '6'
	assert generated.contains('int add_xy(int x, int y)'), generated
	assert generated.contains('int__str(add_xy(2, 3))'), generated
	assert !generated.contains('int__str(mymodules__add_xy(2, 3))'), generated
}

fn test_module_homonym_function_signature_uses_module_key() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'module_homonym_signature', 'module main

import foo

fn value() int {
	return 7
}

fn main() {
	println(int_str(value()))
	println(foo.value())
}
', {
		'foo/foo.v': 'module foo

pub fn value() string {
	return "foo"
}
'
	})
	assert output == '7\nfoo'
	assert generated.contains('int value(void);'), generated
	assert generated.contains('string foo__value(void);'), generated
	assert generated.contains('string foo__value(void) {'), generated
	assert !generated.contains('int foo__value(void);'), generated
}

fn test_module_local_error_method_signature_uses_module_key() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'module_local_error_method_signature', 'module main

import foo

fn main() {
	err := foo.make_error()
	println(err.str())
}
', {
		'foo/foo.v': 'module foo

pub struct Error {
	message string
}

pub fn make_error() Error {
	return Error{
		message: "local"
	}
}

pub fn (err Error) msg() string {
	return err.message
}

pub fn (err Error) str() string {
	return err.msg()
}
'
	})
	assert output == 'local'
	assert generated.contains('string foo__Error__msg(foo__Error err)'), generated
	assert generated.contains('return foo__Error__msg(err);'), generated
	assert !generated.contains('string foo__Error__msg(Error err)'), generated
}

fn test_selective_import_with_module_alias_keeps_symbol_authority() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'alias', 'module main

import mymodules as mm { add_xy }

fn main() {
	println(int_str(add_xy(4, 5)))
}
')
	assert output == '9'
	assert generated.contains('mymodules__add_xy(4, 5)'), generated
	assert !generated.contains('mm__add_xy'), generated
}

fn test_selective_import_resolves_struct_collision() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin, 'struct_collision', 'module main

import geometry { Point }
import pixels

fn main() {
	p := Point{x: 7}
	println(int_str(p.x))
}
',
		selective_import_type_collision_modules())
	assert output == '7'
	assert generated.contains('geometry__Point p ='), generated
	assert !generated.contains('pixels__Point p ='), generated
}

fn test_selective_import_resolves_generic_struct_collision() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'generic_struct_collision', 'module main

import geometry { Box }
import pixels

fn main() {
	b := Box[int]{value: 7}
	p := pixels.Box[int]{other: 8}
	println(int_str(b.value + p.other))
}
',
		selective_import_type_collision_modules())
	assert output == '15'
	assert generated.contains('struct geometry__Box_int'), generated
	assert generated.contains('struct pixels__Box_int'), generated
	assert generated.contains('geometry__Box_int b ='), generated
	assert generated.contains('pixels__Box_int p ='), generated
}

fn test_selective_import_resolves_generic_struct_field_from_decl_file() {
	v3_bin := selective_import_build_v3()
	extra := {
		'types/types.v': 'module types

pub struct Thing {
pub:
	v int
}
'
		'other/other.v': 'module other

pub struct Thing {
pub:
	v int
}
'
		'box/box.v':     'module box

import types { Thing }

pub struct Box[T] {
pub:
	thing Thing
	value T
}
'
	}
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'generic_struct_field_selective_import', 'module main

import box { Box }
import other

fn main() {
	b := Box[int]{value: 7}
	o := other.Thing{v: 8}
	println(int_str(b.value + o.v))
}
',
		extra)
	assert output == '15'
	assert generated.contains('struct box__Box_int'), generated
	assert generated.contains('types__Thing thing;'), generated
	assert !generated.contains('box__Thing thing;'), generated
	assert !generated.contains('other__Thing thing;'), generated
}

fn test_selective_import_resolves_generic_struct_method_signature_from_decl_file() {
	v3_bin := selective_import_build_v3()
	extra := {
		'types/types.v': 'module types

pub struct Thing {
pub:
	v int
}
'
		'other/other.v': 'module other

pub struct Thing {
pub:
	v int
}
'
		'box/box.v':     'module box

import types { Thing }

pub struct Box[T] {
pub:
	thing Thing
	value T
}

pub fn (b Box[T]) combine(thing Thing) int {
	return b.value + thing.v
}
'
	}
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'generic_struct_method_signature_selective_import', 'module main

import box { Box }
import other

fn main() {
	b := Box[int]{value: 7}
	_ := other.Thing{v: 1}
	println(int_str(b.combine(b.thing)))
}
',
		extra)
	assert output == '7'
	assert generated.contains('int box__Box_int__combine(box__Box_int b, types__Thing thing)'), generated
	assert !generated.contains('int box__Box_int__combine(box__Box_int b, box__Thing thing)'), generated
	assert !generated.contains('int box__Box_int__combine(box__Box_int b, other__Thing thing)'), generated
}

fn test_selective_import_resolves_alias_collision() {
	v3_bin := selective_import_build_v3()
	output, _ := selective_import_compile_run_with_extra(v3_bin, 'alias_collision', 'module main

import geometry { ItemId }
import pixels

fn value(id ItemId) int {
	return int(id)
}

fn main() {
	println(int_str(value(ItemId(9))))
}
',
		selective_import_type_collision_modules())
	assert output == '9'
}

fn test_selective_import_resolves_enum_collision() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin, 'enum_collision', 'module main

import geometry { Mode }
import pixels

fn is_on(mode Mode) bool {
	return mode == .on
}

fn main() {
	if is_on(.on) {
		println("on")
	}
}
',
		selective_import_type_collision_modules())
	assert output == 'on'
	assert generated.contains('return mode == 7;'), generated
	assert generated.contains('is_on(7)'), generated
	assert !generated.contains('return mode == 70;'), generated
	assert !generated.contains('is_on(70)'), generated
}

fn test_selective_import_resolves_flag_enum_collision() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin, 'flag_enum_collision', 'module main

import geometry { Perm }
import pixels

fn main() {
	m := Perm.a | .b
	println(int_str(int(m)))
}
',
		selective_import_type_collision_modules())
	assert output == '3'
	assert generated.contains('int m = 1 | 2;'), generated
	assert !generated.contains('int m = 1 | 4;'), generated
	assert !generated.contains('Perm.a'), generated
}

fn test_duplicate_selective_import_type_fails() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_type', 'module main

import geometry { Point }
import pixels { Point }

fn main() {
	p := Point{x: 1}
	println(int_str(p.x))
}
',
		selective_import_type_collision_modules())
	assert output.contains('ambiguous selective import `Point`'), output
}

fn test_duplicate_selective_import_enum_selector_fails() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_enum_selector', 'module main

import geometry { Mode }
import pixels { Mode }

fn main() {
	mode := Mode.on
	println(int_str(int(mode)))
}
',
		selective_import_type_collision_modules())
	assert output.contains('ambiguous selective import `Mode`'), output
}

fn test_selective_import_enum_selector_keeps_selected_authority() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'enum_selector_authority', 'module main

import geometry { Mode }
import pixels

fn takes_pixel(mode pixels.Mode) {}

fn main() {
	takes_pixel(Mode.on)
}
',
		selective_import_type_collision_modules())
	assert output.contains('cannot use `geometry.Mode` as argument 1 to `takes_pixel`; expected `pixels.Mode`'), output
}

fn test_unselected_type_from_selective_import_is_not_resolved_by_suffix() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'unselected_type', 'module main

import geometry { Point }
import pixels

fn use_size(s Size) int {
	return s.w
}

fn main() {
	println("ok")
}
',
		selective_import_type_collision_modules())
	assert output.contains('unknown type `Size`'), output
}
