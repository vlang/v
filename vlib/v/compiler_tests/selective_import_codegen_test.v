import os
import v.cmdexec
import v.tempname

fn selective_import_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn selective_import_write_project(name string, main_src string) string {
	return selective_import_write_project_with_extra(name, main_src, {})
}

fn selective_import_write_project_with_extra(name string, main_src string, extra_files map[string]string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_selective_import_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
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

fn selective_import_compile(root string, bin string) os.Result {
	return cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', root, '-b', 'c', '-o', bin], '', {
		'V3CACHE': os.join_path(root, 'cache')
	})
}

struct SelectiveImportCase {
	name     string
	source   string
	expected string
}

fn selective_import_compile_batch(name string, cases []SelectiveImportCase, extra_files map[string]string) string {
	mut files := extra_files.clone()
	mut calls := []string{cap: cases.len}
	for case in cases {
		entry := 'case_${case.name}'
		files['${case.name}.v'] = case.source.replace_once('\nfn main() {', '\nfn ${entry}() {')
		calls << "\t\t'${case.name}' { ${entry}() }"
	}
	root := selective_import_write_project_with_extra(name, "module main\n\nfn main() {\n\tmatch arguments()[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n", files)
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'out')
	compile := selective_import_compile(root, bin)
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
	return os.read_file(bin + '.c') or { panic(err) }
}

fn selective_import_c_case(generated string, name string) string {
	header := '\nvoid case_${name}(void) {\n'
	start := generated.index(header) or { panic('missing C function for ${name}') }
	body := generated[start + header.len..]
	end := body.index('\n}\n') or { panic('missing end of C function for ${name}') }
	return body[..end]
}

fn selective_import_compile_bad(name string, main_src string) string {
	root := selective_import_write_project(name, main_src)
	return selective_import_compile_bad_root(name, root)
}

fn selective_import_compile_bad_with_extra(name string, main_src string, extra_files map[string]string) string {
	root := selective_import_write_project_with_extra(name, main_src, extra_files)
	return selective_import_compile_bad_root(name, root)
}

fn selective_import_compile_bad_root(name string, root string) string {
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'out')
	compile := selective_import_compile(root, bin)
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

fn test_selective_import_function_calls_and_values() {
	generated := selective_import_compile_batch('function_calls_and_values', [
		SelectiveImportCase{'positive', 'module main

import mymodules { add_xy }
import mymodules.submodule { sub_xy }

fn main() {
	println(int_str(add_xy(2, 3)))
	println(int_str(sub_xy(10, 7)))
}
', '5\n3'},
		SelectiveImportCase{'module_alias_variadic_zero_args', 'module main

import logger as log

fn main() {
	println(int_str(log.sum()))
	println(int_str(log.sum(4, 5)))
}
', '0\n9'},
		SelectiveImportCase{'fn_value', 'module main

import mymodules { add_xy }

fn main() {
	f := add_xy
	println(int_str(f(2, 3)))
}
', '5'},
		SelectiveImportCase{'alias', 'module main

import mymodules as mm { add_xy }

fn main() {
	println(int_str(add_xy(4, 5)))
}
', '9'},
	], {
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
	positive := selective_import_c_case(generated, 'positive')
	assert positive.contains('mymodules__add_xy(2, 3)'), positive
	assert positive.contains('submodule__sub_xy(10, 7)'), positive

	variadic := selective_import_c_case(generated, 'module_alias_variadic_zero_args')
	assert variadic.contains('logger__sum('), variadic
	assert !variadic.contains('logger__sum(log'), variadic

	fn_value := selective_import_c_case(generated, 'fn_value')
	assert fn_value.contains('mymodules__add_xy'), fn_value
	assert fn_value.contains('f(2, 3)'), fn_value
	assert !fn_value.contains('i64 f = mymodules__add_xy'), fn_value

	alias_call := selective_import_c_case(generated, 'alias')
	assert alias_call.contains('mymodules__add_xy(4, 5)'), alias_call
	assert !generated.contains('mm__add_xy'), generated
}

fn test_selective_import_module_homonyms() {
	generated := selective_import_compile_batch('module_homonyms', [
		SelectiveImportCase{'json2_decode', 'module main

import json2

struct Config {
	value int
}

fn main() {
	cfg := json2.decode[Config]("{\\"value\\":2}", json2.DecoderOptions{}) or { Config{value: 8} }
	println(int_str(cfg.value))
}
', '0'},
		SelectiveImportCase{'const_ref_file_context', 'module main

import y.other as json

const outer = imported_value
const local_marker = json.Any{
	value: 1
}

fn main() {
	println(int_str(outer.value + local_marker.value))
}
', '42'},
		SelectiveImportCase{'json2_encode_specialized', 'module main

import json2

struct User {
	name string
}

fn main() {
	println(json2.encode(User{name: "x"}, json2.EncoderOptions{}))
}
', 'pure-v'},
		SelectiveImportCase{'fn_value_imported_homonym', 'module main

import a { choose }
import b

fn main() {
	f := choose
	println(int_str(f()))
}
', '11'},
		SelectiveImportCase{'module_homonym_signature', 'module main

import foo

fn value() int {
	return 7
}

fn main() {
	println(int_str(value()))
	println(foo.value())
}
', '7\nfoo'},
		SelectiveImportCase{'module_local_error_method_signature', 'module main

import foo

fn main() {
	err := foo.make_error()
	println(err.str())
}
', 'local'},
	], {
		'json2/decode.v':  'module json2

pub struct DecoderOptions {}

pub fn decode[T](val string, params DecoderOptions) !T {
	_ = val
	_ = params
	return T{}
}
'
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
		'json2/encode.v':  'module json2

pub struct EncoderOptions {}

pub fn encode[T](value T, options EncoderOptions) string {
	_ = value
	_ = options
	return "pure-v"
}
'
		'a/a.v':           'module a

pub fn choose() int {
	return 11
}
'
		'b/b.v':           'module b

pub fn choose() int {
	return 99
}
'
		'foo/foo.v':       'module foo

pub fn value() string {
	return "foo"
}
'
		'foo/error.v':     'module foo

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

	assert generated.contains('json2__decode'), generated

	assert generated.contains('json2__Any'), generated
	assert generated.contains('other__Any'), generated

	assert generated.contains('json2__encode'), generated

	assert generated.contains('a__choose'), generated
	assert !generated.contains('b__choose'), generated

	assert generated.contains('i64 value(void);'), generated
	assert generated.contains('string foo__value(void);'), generated
	assert generated.contains('string foo__value(void) {'), generated
	assert !generated.contains('i64 foo__value(void);'), generated

	assert generated.contains('string foo__Error__msg(foo__Error err)'), generated
	assert generated.contains('return foo__Error__msg(err);'), generated
	assert !generated.contains('string foo__Error__msg(Error err)'), generated
}

fn test_selective_import_generic_file_scope() {
	generated := selective_import_compile_batch('generic_file_scope', [
		SelectiveImportCase{'generic_clone_selective_import', 'module main

import worker

fn main() {
	println(int_str(worker.use_add[int](0, 2, 3)))
}
', '5'},
		SelectiveImportCase{'explicit_generic_selective_import', 'module main

import util { id }
import other

fn main() {
	println(int_str(id[int](1)))
}
', '1'},
		SelectiveImportCase{'generic_clone_selective_import_fn_value', 'module main

import worker

fn main() {
	println(int_str(worker.use_add_cb[int](0, 2, 3)))
}
', '5'},
		SelectiveImportCase{'generic_clone_selective_import_type_signature', 'module main

import worker

fn main() {
	p := worker.make_point[int](7)
	println(int_str(worker.take_point[int](p, 2) + p.x))
}
', '10'},
		SelectiveImportCase{'generic_struct_field_selective_import', 'module main

import box { Box }
import other

fn main() {
	b := Box[int]{value: 7}
	o := other.Thing{v: 8}
	println(int_str(b.value + o.v))
}
', '15'},
		SelectiveImportCase{'generic_struct_method_signature_selective_import', 'module main

import method_box { Box }
import other

fn main() {
	b := Box[int]{value: 7}
	_ := other.Thing{v: 1}
	println(int_str(b.combine(b.thing)))
}
', '7'},
	], {
		'worker/worker.v':     'module worker

import mymodules { add_xy }
import other

pub fn use_add[T](marker T, x int, y int) int {
	_ = marker
	return add_xy(x, y)
}
'
		'other/other.v':       'module other

pub fn add_xy(x int, y int) int {
	return x * 100 + y
}
'
		'util/util.v':         'module util

pub fn id[T](x T) T {
	return x
}
'
		'other/identity.v':    'module other

pub fn id[T](x T) T {
	return x
}
'
		'worker/callback.v':   'module worker

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
		'worker/point.v':      'module worker

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
		'types/types.v':       'module types

pub struct Thing {
pub:
	v int
}
'
		'other/thing.v':       'module other

pub struct Thing {
pub:
	v int
}
'
		'box/box.v':           'module box

import types { Thing }

pub struct Box[T] {
pub:
	thing Thing
	value T
}
'
		'method_box/box.v':    'module method_box

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
	})

	assert generated.contains('mymodules__add_xy(x, y)'), generated
	assert !generated.contains('other__add_xy(x, y)'), generated

	assert generated.contains('util__id_T_v_int(1)'), generated
	assert !generated.contains('other__id_T_v_int(1)'), generated

	assert generated.contains('worker__takes(mymodules__add_xy, x, y)'), generated
	assert !generated.contains('worker__takes(add_xy, x, y)'), generated
	assert !generated.contains('worker__takes(other__add_xy, x, y)'), generated

	assert generated.contains('geometry__Point worker__make_point_T_v_int(i64 x)'), generated
	assert generated.contains('i64 worker__take_point_T_v_int(geometry__Point p, i64 x)'), generated
	assert !generated.contains('\nPoint worker__make_point_T_v_int(i64 x)'), generated
	assert !generated.contains('\npixels__Point worker__make_point_T_v_int(i64 x)'), generated
	assert !generated.contains('\ni64 worker__take_point_T_v_int(Point p, i64 x)'), generated
	assert !generated.contains('\ni64 worker__take_point_T_v_int(pixels__Point p, i64 x)'), generated

	assert generated.contains('struct box__Box_int'), generated
	assert generated.contains('types__Thing thing;'), generated
	assert !generated.contains('box__Thing thing;'), generated
	assert !generated.contains('other__Thing thing;'), generated

	assert generated.contains('i64 method_box__Box_int__combine(method_box__Box_int b, types__Thing thing)'), generated
	assert !generated.contains('i64 method_box__Box_int__combine(method_box__Box_int b, method_box__Thing thing)'), generated
	assert !generated.contains('i64 method_box__Box_int__combine(method_box__Box_int b, other__Thing thing)'), generated
}

fn test_selective_import_does_not_import_other_symbols_by_suffix() {
	output := selective_import_compile_bad('hidden_symbol', 'module main

import mymodules { add_xy }

fn main() {
	println(int_str(hidden_xy(2, 3)))
}
')
	assert output.contains('unknown function: hidden_xy'), output
}

fn test_selective_import_scope_is_file_local() {
	output := selective_import_compile_bad_with_extra('file_local', 'module main

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
	assert output.contains('unknown function: add_xy'), output
}

fn test_selective_import_same_symbol_from_two_modules_is_ambiguous() {
	output := selective_import_compile_bad_with_extra('ambiguous_symbol', 'module main

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
	output := selective_import_compile_bad_with_extra('ambiguous_unused', 'module main

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
	output := selective_import_compile_bad_with_extra('ambiguous_local_homonym', 'module main

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
	output := selective_import_compile_bad_with_extra('ambiguous_fn_value', 'module main

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

// A local function used to take priority over a selectively imported homonym. Defining
// one is an error now, as in V1 (f39e8541d8), so neither function is silently picked.
fn test_local_function_cannot_redefine_selective_import_homonym() {
	output := selective_import_compile_bad('local_homonym', 'module main

import mymodules { add_xy }

fn add_xy(x int, y int) int {
	return x * y
}

fn main() {
	println(int_str(add_xy(2, 3)))
}
')
	assert output.contains('main.v:5:4: error: cannot redefine imported function `add_xy`'), output
}

fn test_selective_import_type_collisions_and_enum_from() {
	mut extra := selective_import_type_collision_modules()
	extra['colors/colors.v'] = 'module colors

pub enum Color {
	red
	blue
}

pub type Hue = Color
'
	generated := selective_import_compile_batch('type_collisions_and_enum_from', [
		SelectiveImportCase{'struct_collision', 'module main

import geometry { Point }
import pixels

fn main() {
	p := Point{x: 7}
	println(int_str(p.x))
}
', '7'},
		SelectiveImportCase{'generic_struct_collision', 'module main

import geometry { Box }
import pixels

fn main() {
	b := Box[int]{value: 7}
	p := pixels.Box[int]{other: 8}
	println(int_str(b.value + p.other))
}
', '15'},
		SelectiveImportCase{'alias_collision', 'module main

import geometry { ItemId }
import pixels

fn value(id ItemId) int {
	return int(id)
}

fn main() {
	println(int_str(value(ItemId(9))))
}
', '9'},
		SelectiveImportCase{'enum_collision', 'module main

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
', 'on'},
		SelectiveImportCase{'enum_from', 'module main

import geometry { Mode }
import pixels

fn main() {
	mode := Mode.from(8) or { panic(err) }
	println(mode)
	string_mode := Mode.from("on") or { panic(err) }
	println(string_mode)
}
', 'off\non'},
		SelectiveImportCase{'qualified_enum_from', 'module main

import colors

fn main() {
	numeric := colors.Color.from(1) or { panic(err) }
	println(numeric)
	text := colors.Color.from("red") or { panic(err) }
	println(text)
}
', 'blue\nred'},
		SelectiveImportCase{'enum_alias_from_string', 'module main

import colors { Hue }

fn main() {
	value := Hue.from("blue") or { panic(err) }
	println(value)
}
', 'blue'},
		SelectiveImportCase{'flag_enum_collision', 'module main

import geometry { Perm }
import pixels

fn main() {
	m := Perm.a | .b
	println(int_str(int(m)))
}
', '3'},
	], extra)
	point := selective_import_c_case(generated, 'struct_collision')
	assert point.contains('geometry__Point p ='), point
	assert !point.contains('pixels__Point p ='), point

	boxes := selective_import_c_case(generated, 'generic_struct_collision')
	assert generated.contains('struct geometry__Box_int'), generated
	assert generated.contains('struct pixels__Box_int'), generated
	assert boxes.contains('geometry__Box_int b ='), boxes
	assert boxes.contains('pixels__Box_int p ='), boxes

	assert generated.contains('return mode == geometry__Mode__on;'), generated
	assert !generated.contains('return mode == pixels__Mode__on;'), generated
	enum_case := selective_import_c_case(generated, 'enum_collision')
	assert enum_case.contains('is_on(geometry__Mode__on)'), enum_case
	assert !enum_case.contains('is_on(pixels__Mode__on)'), enum_case

	assert !generated.contains('unknown__from'), generated

	flags := selective_import_c_case(generated, 'flag_enum_collision')
	// The flags are both represented as C ints, but .b is 2 in geometry and 4 in pixels.
	assert flags.contains(' m = 1 | 2;'), flags
	assert !flags.contains(' m = 1 | 4;'), flags
	assert !flags.contains('Perm.a'), flags
}

fn test_duplicate_selective_import_type_fails() {
	output := selective_import_compile_bad_with_extra('ambiguous_type', 'module main

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
	output := selective_import_compile_bad_with_extra('ambiguous_enum_selector', 'module main

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
	output := selective_import_compile_bad_with_extra('enum_selector_authority', 'module main

import geometry { Mode }
import pixels

fn takes_pixel(mode pixels.Mode) {}

fn main() {
	takes_pixel(Mode.on)
}
',
		selective_import_type_collision_modules())
	// The V1 wording (238dd3add8). A module of the compiled project directory is named
	// after that directory in diagnostics (9aa2b7350f), so only its tail is checked.
	assert output.contains('.geometry.Mode` as `'), output
	assert output.contains('pixels.Mode` in argument 1 to `takes_pixel`'), output
}

fn test_unselected_type_from_selective_import_is_not_resolved_by_suffix() {
	output := selective_import_compile_bad_with_extra('unselected_type', 'module main

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
