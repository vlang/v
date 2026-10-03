module eval

import os
import v.parser

fn test_eval_match_byte_constant_uses_its_value() {
	mut e := create()
	e.run_text('
const byte = 8

fn classify(value int) string {
	return match value {
		byte { "matched" }
		else { "other" }
	}
}

fn main() {
	value := 9
	match value {
		byte { println("matched") }
		else { println("other") }
	}
	println(classify(8))
	println(classify(9))
}
') or { panic(err) }
	assert e.stdout() == 'other\nmatched\nother\n'
}

fn test_eval_sizeof_byte_constant_uses_its_declared_width() {
	mut e := create()
	e.run_text('
const byte = u8(1)

fn main() {
	println(sizeof(byte))
	println(sizeof(u8))
	println(sizeof(i32))
}
') or { panic(err) }
	assert e.stdout() == '1\n1\n4\n'
}

fn test_eval_sizeof_byte_local_uses_its_declared_width() {
	mut e := create()
	e.run_text('
fn main() {
	byte := i32(0)
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '4\n'
}

fn test_eval_sizeof_byte_constant_ignores_caller_local_types() {
	mut e := create()
	e.run_text('
const narrow = u8(1)
const byte = narrow

fn main() {
	narrow := i32(0)
	println(sizeof(byte))
	println(sizeof(narrow))
}
') or { panic(err) }
	assert e.stdout() == '1\n4\n'
}

fn test_eval_sizeof_byte_alias_uses_its_underlying_width() {
	mut e := create()
	e.run_text('
type Small = u16
type Narrow = Small
const byte = Narrow(1)

fn main() {
	println(sizeof(byte))
	local := Narrow(0)
	println(sizeof(local))
	println(sizeof(Narrow))
}
') or { panic(err) }
	assert e.stdout() == '2\n2\n2\n'
}

fn test_eval_sizeof_byte_call_does_not_evaluate_the_constant() {
	mut e := create()
	e.run_text('
const byte = side_effect()

fn side_effect() u16 {
	println("evaluated")
	return u16(1)
}

fn main() {
	side_effect := fn () u8 { return u8(0) }
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '2\n'
}

fn test_eval_sizeof_byte_local_call_uses_the_return_width() {
	mut e := create()
	e.run_text('
fn make_byte() u8 {
	println("initialized")
	return u8(1)
}

fn main() {
	byte := make_byte()
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == 'initialized\n1\n'
}

fn test_eval_sizeof_byte_fixed_arrays_use_recursive_widths() {
	mut e := create()
	e.run_text('
type Small = u16
type Row = [3]Small
type Matrix = [2]Row
const count = 1 + 2
type Counted = [count]Small
const byte = [3]u8{}

fn main() {
	println(sizeof(byte))
	nested := [2][3]u16{}
	println(sizeof(nested))
	println(sizeof(Matrix))
	println(sizeof([0]u8))
	println(sizeof(Counted))
	println(sizeof([3]&u8))
}
') or { panic(err) }
	assert e.stdout() == '3\n12\n12\n0\n6\n24\n'
}

fn test_eval_sizeof_byte_method_and_static_calls_use_return_widths() {
	mut e := create()
	e.run_text('
struct Maker {}
const byte = Maker.build()

fn (maker Maker) make() u8 {
	println("method")
	return u8(1)
}

fn Maker.build() u16 {
	println("static")
	return u16(1)
}

fn print_constant_width() {
	println(sizeof(byte))
}

fn main() {
	maker := Maker{}
	byte := maker.make()
	println(sizeof(byte))
	local := Maker.build()
	println(sizeof(local))
	print_constant_width()
}
') or { panic(err) }
	assert e.stdout() == 'method\n1\nstatic\n2\n2\n'
}

fn test_eval_sizeof_byte_function_value_call_uses_return_width() {
	mut e := create()
	e.run_text('
fn main() {
	make := fn () u8 {
		println("initialized")
		return u8(1)
	}
	byte := make()
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == 'initialized\n1\n'
}

fn test_eval_sizeof_byte_fixed_array_initializer_is_not_evaluated() {
	mut e := create()
	e.run_text('
const count = 1 + 2
const byte = [count]u8{init: side_effect()}

fn side_effect() u8 {
	println("evaluated")
	return u8(1)
}

fn main() {
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '3\n'
}

fn test_eval_sizeof_byte_method_receiver_is_not_evaluated() {
	mut e := create()
	e.run_text('
struct Maker {}
const byte = make_maker().make()

fn make_maker() Maker {
	println("receiver")
	return Maker{}
}

fn (maker Maker) make() u8 {
	println("method")
	return u8(1)
}

fn main() {
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '1\n'
}

fn test_eval_sizeof_byte_global_receiver_uses_the_method_signature() {
	mut e := create()
	e.run_text('
struct Maker {}
__global maker Maker

fn (maker Maker) make() u8 {
	println("method")
	return u8(1)
}

fn main() {
	byte := maker.make()
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == 'method\n1\n'
}

fn test_eval_sizeof_byte_qualified_calls_preserve_module_types() {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_qualified_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	module_file := os.join_path(dir, 'maker.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(module_file, '
module maker

pub type Octet = u8
const count = 1 + 2
pub type Row = [count]Octet
pub struct Maker {}

pub fn Maker.build() Octet {
	println("static")
	return Octet(1)
}

pub fn make() Octet {
	println("qualified")
	return Octet(1)
}

pub fn rows() [2]Row {
	return [2]Row{}
}

pub fn counted() [count]Octet {
	return [count]Octet{}
}
') or { panic(err) }
	os.write_file(main_file, '
module main

import maker as m

const byte = m.Maker.build()

fn main() {
	println(sizeof(byte))
	local := m.make()
	println(sizeof(local))
	nested := m.rows()
	println(sizeof(nested))
	counted := m.counted()
	println(sizeof(counted))
}
') or { panic(err) }
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([module_file, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '1\nqualified\n1\n6\n3\n'
}

fn test_eval_sizeof_byte_pointer_parameter_preserves_depth() {
	mut e := create()
	e.run_text('
fn width(byte &u8) { println(sizeof(byte)); println(sizeof(*byte)) }
fn wider(byte &i32) { println(sizeof(byte)); println(sizeof(*byte)) }
fn indirect(byte &&u8) { println(sizeof(byte)); println(sizeof(*byte)); println(sizeof(**byte)) }
fn main() {
 value := u8(1)
 wide := i32(1)
 width(&value)
 wider(&wide)
 pointer := &value
 indirect(&pointer)
}
') or { panic(err) }
	assert e.stdout() == '8\n1\n8\n4\n8\n8\n1\n'
}

fn test_eval_sizeof_byte_pointer_alias_parameter() {
	mut e := create()
	e.run_text('
type Pointer = &u16
type Indirect = &Pointer
fn width(byte Pointer) { println(sizeof(byte)); println(sizeof(*byte)) }
fn indirect(byte Indirect) { println(sizeof(byte)) }
fn main() {
 value := u16(1)
 pointer := &value
 width(pointer)
 indirect(&pointer)
 println(sizeof(Pointer))
 println(sizeof(Indirect))
}
') or { panic(err) }
	assert e.stdout() == '8\n2\n8\n8\n8\n'
}

fn test_eval_sizeof_byte_pointer_function_literal_parameter() {
	mut e := create()
	e.run_text('
fn main() {
 width := fn (byte &u16) { println(sizeof(byte)); println(sizeof(*byte)) }
 value := u16(1)
 width(&value)
}
') or { panic(err) }
	assert e.stdout() == '8\n2\n'
}

fn test_eval_sizeof_byte_pointer_method_receiver_and_field() {
	mut e := create()
	e.run_text('
struct Holder { value u8; pointer &u16 }
fn (byte &Holder) width() {
 println(sizeof(byte))
 field := byte.value
 pointer := byte.pointer
 println(sizeof(field))
 println(sizeof(pointer))
}
fn main() { holder := Holder{}; holder.width() }
') or { panic(err) }
	assert e.stdout() == '8\n1\n8\n'
}

fn test_eval_sizeof_byte_local_pointer_survives_assignment() {
	mut e := create()
	e.run_text('
fn main() {
 value := u8(1)
 mut byte := &value
 println(sizeof(byte))
 byte = &value
 println(sizeof(byte))
 copy := byte
 println(sizeof(copy))
}
') or { panic(err) }
	assert e.stdout() == '8\n8\n8\n'
}

fn test_eval_sizeof_byte_pointer_cast_preserves_width() {
	mut e := create()
	e.run_text('
fn main() {
 byte := &u8(0)
 println(sizeof(byte))
 wider := &u16(0)
 println(sizeof(wider))
 indirect := &&u8(0)
 println(sizeof(indirect))
}
') or { panic(err) }
	assert e.stdout() == '8\n8\n8\n'
}

fn test_eval_sizeof_byte_pointer_return_signature_is_not_evaluated() {
	mut e := create()
	e.run_text('
const byte = pointer()
const other = byte
fn pointer() &u8 { println("evaluated"); return &u8(0) }
fn main() { println(sizeof(byte)); println(sizeof(other)) }
') or { panic(err) }
	assert e.stdout() == '8\n8\n'
}

fn test_eval_declared_pointer_metadata_keeps_module_and_depth() {
	mut e := create()
	e.call_stack << CallFrame{ module_name: 'worker' }
	e.declare_var_typed('byte', Value(i64(0)), '&&Holder')
	assert e.lookup_var_type('byte') or { '' } == '&&worker.Holder'
	e.set_var_type('byte', '&u8')
	assert e.lookup_var_type('byte') or { '' } == '&u8'
	assert e.qualify_nested_type_name('worker', '[2]&u16') == '[2]&u16'
}

fn test_eval_sizeof_imported_constants_uses_declaration_context() {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_imported_consts_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	widths := os.join_path(dir, 'widths.v')
	bridge := os.join_path(dir, 'bridge.v')
	main_file := os.join_path(dir, 'main.v')
	nested := os.join_path(dir, 'inner.v')
	os.write_file(nested, 'module inner\npub const narrow = u16(1)\n') or { panic(err) }
	os.write_file(widths, '
module widths
pub type Octet = u8
pub const narrow = Octet(1)
pub const count = 2
pub const row = [count]Octet{}
pub const pointer = make_pointer()
pub fn make_pointer() &u8 { println("evaluated"); return &u8(0) }
') or { panic(err) }
	os.write_file(bridge, '
module bridge
import widths as w
import widths.inner as nested
pub const nested_width = nested.narrow
pub const narrow = w.narrow
pub const row = w.row
pub const count = w.count
pub type Row = [count]w.Octet
pub const pointer = w.pointer
') or { panic(err) }
	os.write_file(main_file, '
module main
import bridge as b
import widths as w
const byte = b.narrow
const nested_width = b.nested_width
const rows = b.row
const pointer = b.pointer
fn main() {
 narrow := i32(0)
 println(sizeof(byte))
 println(sizeof(rows))
 println(sizeof(pointer))
 println(sizeof(b.narrow))
 println(sizeof(w.narrow))
 println(sizeof(b.Row))
 println(sizeof(*pointer))
 println(sizeof(rows[0]))
 println(sizeof(narrow))
 println(sizeof(nested_width))
}
') or { panic(err) }
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([widths, nested, bridge, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '1\n2\n8\n1\n1\n2\n1\n1\n4\n2\n'
}

fn test_eval_sizeof_fixed_array_aggregate_elements_matches_compiler() {
	code := '
struct Tiny { x u8 }
struct Padded { x u8; y u32; z u16 }
struct Outer { lead u8; inner Padded; tail u8 }
union Choice { tiny u8; row [3]u16 }
struct Empty {}
struct Node { value u8; next &Node = unsafe { nil } }
enum Narrow as u8 { first; second }
enum Wide as u64 { first; second }
enum Ordinary { first; second }
type Small = Tiny
type Row = [3]Small
const byte = [2]Tiny{}
fn main() {
 println(sizeof(byte))
 println(sizeof([2]Padded))
 println(sizeof([2]Outer))
 println(sizeof([2]Choice))
 println(sizeof([2]Empty))
 println(sizeof([2]Node))
 println(sizeof([3]Narrow))
 println(sizeof([2]Wide))
 println(sizeof([2]Ordinary))
 println(sizeof([2]Row))
 println(sizeof([2][3]Tiny))
}
'
	assert_eval_sizeof_matches_compiler(code)
}

fn test_eval_sizeof_packed_and_aligned_structs_matches_compiler() {
	code := '
@[_packed]
struct Packed { x u8; y u32 }
@[aligned: 16]
struct Aligned { x u8 }
@[_packed; aligned: 16]
struct Both { x u8; y u32 }
struct Container { first u8; packed Packed; last u16 }
@[_pack: 2]
struct PackedTwo { x u8; y u32 }
@[aligned]
struct MaxAligned { x u8 }
fn main() {
 println(sizeof([2]Packed))
 println(sizeof([2]Aligned))
 println(sizeof([2]Both))
 println(sizeof([2]Container))
 println(sizeof([2]PackedTwo))
 println(sizeof([2]MaxAligned))
}
'
	assert_eval_sizeof_matches_compiler(code)
}

fn assert_eval_sizeof_matches_compiler(code string) {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_layout_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	executable := os.join_path(dir, 'layout')
	os.write_file(source, code) or { panic(err) }
	compiled := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-cc', 'clang', '-o', executable,
		source])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([executable])
	assert result.exit_code == 0, result.output
	mut e := create()
	e.run_text(code) or { panic(err) }
	assert e.stdout() == result.output
}

fn test_eval_sizeof_cycles_and_recursive_pointers_terminate() {
	mut e := create()
	e.run_text('
type First = Second
type Second = First
struct Node { value u8; next &Node }
const byte = other
const other = byte
fn main() {
 println(sizeof(byte))
 println(sizeof(First))
 println(sizeof([2]&Node))
}
') or { panic(err) }
	assert e.stdout() == '8\n8\n16\n'
}

fn test_eval_sizeof_builtin_aggregate_fields_matches_compiler() {
	code := '
struct Fields { head u8; text string; values []u8; lookup map[string]u8 }
type Text = string
type Values = []u8
type Lookup = map[string]u8
struct AliasedFields { head u8; text Text; values Values; lookup Lookup }
struct Wrapped { optional ?u8; pointer &u8 = unsafe { nil } }
fn main() {
 println(sizeof(int))
 println(sizeof(string))
 println(sizeof([]u8))
 println(sizeof(map[string]u8))
 println(sizeof([2]Fields))
 println(sizeof([2]AliasedFields))
 println(sizeof([2]Wrapped))
 println(sizeof(!u16))
 println(sizeof([2]string))
 println(sizeof([2][]u8))
 println(sizeof(IError))
 println(sizeof(?u64))
 println(sizeof([3]?u16))
 println(sizeof(!string))
 println(sizeof(![2]Fields))
}
'
	assert_eval_sizeof_matches_compiler(code)
}

fn test_eval_sizeof_imported_aggregate_fields_preserves_file_aliases() {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_imported_layout_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	os.mkdir_all(os.join_path(dir, 'small')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'holder')) or { panic(err) }
	small := os.join_path(dir, 'small', 'small.v')
	holder := os.join_path(dir, 'holder', 'holder.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(os.join_path(dir, 'v.mod'), 'Module { name: "layout_probe" }') or { panic(err) }
	os.write_file(small, '
module small
pub type Octet = u8
pub const count = 3
pub struct Tiny { pub: x Octet }
pub enum Tag as u16 { first; second }
') or { panic(err) }
	os.write_file(holder, '
module holder
import small as s
pub type Cell = s.Tiny
pub const count = s.count
pub type Row = [count]Cell
pub struct Holder { pub: lead u8; row Row; tag s.Tag; text string; values []u8 }
pub struct Tiny { pub: x u64 }
pub struct Generic[T] { pub: lead u8; value T; row [2]T }
pub type GenericAlias[T] = Generic[T]
pub type Doubled[T] = [2]T
pub struct Wrapped[T] { pub: rows [2]T; optional ?T; aliases Doubled[T] }
pub struct Nested[T] { pub: value Generic[T]; extra T }
pub const byte = [2]Holder{}
pub type Pointer = &s.Octet
pub type Indirect = &Pointer
pub fn pointer() Pointer { return Pointer(unsafe { nil }) }
pub fn indirect() Indirect { return Indirect(unsafe { nil }) }
pub fn make_octet() s.Octet { return s.Octet(1) }
pub fn make_cell() Cell { return Cell{} }
') or { panic(err) }
	os.write_file(main_file, '
module main
import holder as h
struct Tiny { x u8 }
const byte = h.byte
const pointer = h.pointer()
const indirect = h.indirect()
const octet = h.make_octet()
const cell = h.make_cell()
const alias_field = h.Cell{}.x
const generic = [2]h.Generic[Tiny]{}
const generic_rows = [2]h.Generic[[3]Tiny]{}
const nested_generic = [2]h.Nested[Tiny]{}
const aliased_generic = [2]h.GenericAlias[Tiny]{}
const wrapped_generic = [2]h.Wrapped[Tiny]{}
const doubled_generic = [2]h.Doubled[Tiny]{}
fn main() {
 println(sizeof(byte))
 println(sizeof([2]h.Holder))
 println(sizeof(octet))
 println(sizeof(cell))
 println(sizeof(alias_field))
 println(sizeof(*pointer))
 println(sizeof(*indirect))
 println(sizeof(**indirect))
 println(sizeof(generic))
 println(sizeof(generic_rows))
 println(sizeof(nested_generic))
 println(sizeof(aliased_generic))
 println(sizeof(wrapped_generic))
 println(sizeof(doubled_generic))
}
') or { panic(err) }
	original_main := os.read_file(main_file) or { panic(err) }
	mut compiled_main := original_main
	for name in ['generic', 'generic_rows', 'nested_generic', 'aliased_generic', 'wrapped_generic',
		'doubled_generic'] {
		compiled_main = compiled_main.split_into_lines().filter(!it.starts_with('const ${name} =') && !it.contains('sizeof(${name})')).join('\n')
	}
	os.write_file(main_file, compiled_main) or { panic(err) }
	executable := os.join_path(dir, 'layout')
	compiled := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-cc', 'clang', '-o', executable,
		dir])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([executable])
	assert result.exit_code == 0, result.output
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([small, holder, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == result.output
	os.write_file(main_file, original_main) or { panic(err) }
	mut generic_parser := parser.Parser.new(&e.prefs)
	generic_parser.parse_files([small, holder, main_file])
	e.run_files(generic_parser.a) or { panic(err) }
	assert e.stdout() == result.output + '8\n20\n10\n8\n12\n4\n'
}

fn test_eval_sizeof_registered_builtin_layouts_matches_minimal_ast() {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_builtin_layout_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	main_file := os.join_path(dir, 'main.v')
	os.write_file(main_file, '
module main
struct Fields { head u8; text string; values []u8 }
const byte = [2]Fields{}
fn main() { println(sizeof(byte)); println(sizeof(string)); println(sizeof([]u8)) }
') or { panic(err) }
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([os.join_path(os.dir(@VEXE), 'vlib', 'builtin', 'string.v'),
		os.join_path(os.dir(@VEXE), 'vlib', 'builtin', 'array.v'), main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '160\n24\n48\n'
}

fn test_eval_sizeof_aggregate_defaults_are_not_evaluated() {
	mut e := create()
	e.run_text('
struct Tiny { value u8 = side_effect() }
const byte = [2]Tiny{}
fn side_effect() u8 { println("evaluated"); return u8(1) }
fn main() { println(sizeof(byte)) }
') or { panic(err) }
	assert e.stdout() == '2\n'
}

fn test_eval_sizeof_generic_aggregate_elements_matches_compiler() {
	code := '
struct Tiny[T] { x T }
struct Pair[T, U] { first T; second U }
struct Node[T] { x T; next &Node[T] = unsafe { nil } }
type Octet = u8
type Box[T] = Tiny[T]
struct OptionalBox { value ?Tiny[u8] }
const byte = [2]Tiny[u8]{}
fn main() {
 println(sizeof(byte))
 println(sizeof([2]Tiny[Octet]))
 println(sizeof([2]Tiny[[3]u8]))
 println(sizeof([2]Pair[u8, u32]))
 println(sizeof([2]Tiny[Tiny[u8]]))
 println(sizeof([2]Node[u8]))
 println(sizeof([2]Box[u8]))
 println(sizeof([2]OptionalBox))
}
'
	assert_eval_sizeof_matches_compiler(code)
}

fn test_eval_sizeof_global_receiver_uses_declaration_file_imports() {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_global_imports_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	maker := os.join_path(dir, 'maker.v')
	globals := os.join_path(dir, 'globals.v')
	constants := os.join_path(dir, 'constants.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(maker, '
module maker
pub struct Maker {}
pub fn (value Maker) make() u8 { println("evaluated"); return u8(1) }
') or { panic(err) }
	os.write_file(globals, '
module worker
import maker as m
__global maker m.Maker
') or { panic(err) }
	os.write_file(constants, '
module worker
struct Maker {}
fn (value Maker) make() u16 { println("wrong receiver"); return u16(1) }
const byte = maker.make()
pub fn measure() { println(sizeof(byte)) }
') or { panic(err) }
	os.write_file(main_file, '
module main
import worker
fn main() { worker.measure() }
') or { panic(err) }
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([globals, constants, maker, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '1\n'
}

fn test_eval_sizeof_byte_128_bit_values_use_their_full_width() {
	mut e := create()
	e.run_text('
struct Mixed {
	a u128
	b u64
	c u128
}

const byte = i128(1)

fn main() {
	wide := u128(1)
	pair := [2]u128{}
	mixed := Mixed{}
	println(sizeof(byte))
	println(sizeof(wide))
	println(sizeof(pair))
	println(sizeof(mixed))
	println(sizeof(i128))
}
') or { panic(err) }
	assert e.stdout() == '16\n16\n32\n48\n16\n'
}
