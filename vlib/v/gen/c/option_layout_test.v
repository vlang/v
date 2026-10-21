module c

import os
import v.flat
import v.types

fn test_option_and_result_have_distinct_layouts() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.interface_names['IError'] = true
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.interfaces['IError'] = []string{}
	option := types.Type(types.OptionType{ base_type: &types.Type(types.i64_) })
	result := types.Type(types.ResultType{ base_type: &types.Type(types.i64_) })
	option_name := g.optional_type_name(option)
	result_name := g.optional_type_name(result)
	assert option_name != result_name
	assert g.emit_optional_typedef(option_name, 'i64')
	assert g.emit_optional_typedef(result_name, 'i64')
	option_layout := '{ bool ok; i64 value; }'
	result_layout := '{ intptr_t ok; union { IError err; i64 value; }; }'
	expected := [
		'typedef struct __v_option_i64 ${option_layout} __v_option_i64;',
		'typedef struct __v_result_i64 ${result_layout} __v_result_i64;',
	]
	assert g.sb.str().split_into_lines() == expected
}

// A payload that holds pointers gets a pointer sized `ok`, so no padding is left before
// it. A C compiler can copy a payload together with that padding, at an offset that is
// not a multiple of the pointer size, where the conservative GC does not look for the
// pointers in it (the graphify daemon crashed when a decoded graph's arrays were freed
// that way). A payload without pointers keeps the one byte `ok` and its layout.
fn test_option_with_pointer_payload_has_pointer_sized_ok() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.interface_names['IError'] = true
	tc.structs['sample.Node'] = [types.StructField{
		name: 'name'
		typ:  types.string_
	}]
	tc.structs['sample.Pos'] = [types.StructField{
		name: 'line'
		typ:  types.i32_
	}]
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.interfaces['IError'] = []string{}
	for value_type in ['string', 'Array', 'map', 'u8*', 'sample__Node'] {
		assert g.emit_optional_typedef('__v_option_${value_type}', value_type)
	}
	g.needed_optional_types.set_id('__v_option_string', flat.source_type_id('string'))
	assert g.emit_optional_typedef('__v_option___v_option_string', '__v_option_string')
	for value_type in ['u8', 'i32', 'bool', 'sample__Pos'] {
		assert g.emit_optional_typedef('__v_option_${value_type}', value_type)
	}
	assert g.emit_optional_typedef('__v_result_u8', 'u8')
	assert g.sb.str().split_into_lines() == [
		'typedef struct __v_option_string { intptr_t ok; string value; } __v_option_string;',
		'typedef struct __v_option_Array { intptr_t ok; Array value; } __v_option_Array;',
		'typedef struct __v_option_map { intptr_t ok; map value; } __v_option_map;',
		'typedef struct __v_option_u8* { intptr_t ok; u8* value; } __v_option_u8*;',
		'typedef struct __v_option_sample__Node { intptr_t ok; sample__Node value; } __v_option_sample__Node;',
		'typedef struct __v_option___v_option_string { intptr_t ok; __v_option_string value; } __v_option___v_option_string;',
		'typedef struct __v_option_u8 { bool ok; u8 value; } __v_option_u8;',
		'typedef struct __v_option_i32 { bool ok; i32 value; } __v_option_i32;',
		'typedef struct __v_option_bool { bool ok; bool value; } __v_option_bool;',
		'typedef struct __v_option_sample__Pos { bool ok; sample__Pos value; } __v_option_sample__Pos;',
		'typedef struct __v_result_u8 { intptr_t ok; union { IError err; u8 value; }; } __v_result_u8;',
	]
}

// The typedefs of a compiled program, whose payload names are spelled the way the C
// backend spells them (`main__Node`, `main__Box_string`, `chan`, ...). A sum type's tag
// gets the same treatment as `ok`, unless its ownership flag already fills the padding.
fn test_compiled_option_and_sum_headers_fill_padding_before_pointers() {
	path := os.join_path(os.vtmp_dir(), 'option_ok_header_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	source := "struct Node { items []int }
struct Plain { a int b u8 }
struct Box[T] { v T }
@[packed]
struct Packed { b u8 s string }
struct WrapPacked { p Packed }
type Words = int | string
type Num = int | f64
type Ref = &Node | int
struct Leaf { a u32 }
@[packed]
struct PackedPtr { a u8 b voidptr }
type PackedOnly = Leaf | PackedPtr
type HoldsPackedOnly = Leaf | PackedOnly
fn node() ?Node { return Node{} }
fn plain() ?Plain { return Plain{} }
fn box_s() ?Box[string] { return Box[string]{} }
fn box_i() ?Box[int] { return Box[int]{} }
fn ch() ?chan int { return chan int{} }
fn packed() ?Packed { return Packed{} }
fn wrap_packed() ?WrapPacked { return WrapPacked{} }
fn words() ?Words { return Words(1) }
fn num() ?Num { return Num(1) }
fn names() ?[2]string { return ['a', 'b']! }
fn ints() ?[2]int { return [1, 2]! }
fn pair() ?(string, bool) { return 'a', true }
fn ref() Ref { return Ref(&Node{}) }
fn packed_only() ?PackedOnly { return PackedOnly(Leaf{}) }
fn holds_packed_only() HoldsPackedOnly { return HoldsPackedOnly(Leaf{}) }
fn main() {
 _ := node() or { Node{} }
 _ := plain() or { Plain{} }
 _ := box_s() or { Box[string]{} }
 _ := box_i() or { Box[int]{} }
 _ := ch() or { chan int{} }
 _ := packed() or { Packed{} }
 _ := wrap_packed() or { WrapPacked{} }
 _ := words() or { Words(0) }
 _ := num() or { Num(0) }
 _ := names() or { ['', '']! }
 _ := ints() or { [0, 0]! }
 _, _ := pair() or { '', false }
 _ := ref()
 _ := packed_only() or { PackedOnly(Leaf{}) }
 _ := holds_packed_only()
}"
	os.write_file(path, source)!
	command := '${os.quoted_path(@VEXE)} -new-compiler -o ${os.quoted_path(output)} ${os.quoted_path(path)}'
	result := os.execute(command)
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	for payload in ['main__Node', 'main__Box_string', 'chan', 'Words', 'Array_fixed_string_2',
		'multi_return_string_bool'] {
		assert generated.contains('typedef struct __v_option_${payload} { intptr_t ok; ${payload} value; }'), payload
	}
	for payload in ['main__Plain', 'main__Box_int', 'main__Packed', 'main__WrapPacked', 'Num',
		'Array_fixed_i64_2', 'PackedOnly'] {
		assert generated.contains('typedef struct __v_option_${payload} { bool ok; ${payload} value; }'), payload
	}
	assert generated.contains('struct Words {\n\tintptr_t typ;\n\tunion {')
	assert generated.contains('struct Num {\n\tint typ;\n\tunion {')
	assert generated.contains('struct Ref {\n\tint typ;\n\tu32 _pointer_variant_is_owned;')
	// A packed variant's pointers are not aligned, so neither the sum holding it nor a
	// sum or option holding that sum is widened (which would move its payload).
	assert generated.contains('struct PackedOnly {\n\tint typ;\n\tunion {')
	assert generated.contains('struct HoldsPackedOnly {\n\tint typ;\n\tunion {')
}

fn test_ierror_uses_only_interface_metadata() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.interface_names['IError'] = true
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.emit_interface_struct(flat.source_type_id('IError'))
	declaration := g.sb.str()
	assert declaration.contains('void* _object;')
	assert declaration.contains('u32 _typ : 31;')
	assert declaration.contains('u32 _object_is_boxed : 1;')
	assert !declaration.contains('message;')
	assert !declaration.contains('code;')
}
