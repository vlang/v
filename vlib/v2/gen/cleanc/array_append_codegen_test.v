// vtest build: macos
module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_array_append_c_for_test(code string) string {
	return generate_array_append_c_for_test_files([code])
}

fn generate_array_append_c_for_test_files(sources []string) string {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_array_append_codegen_test_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('failed to create temp dir') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{cap: sources.len}
	for i, code in sources {
		tmp_file := os.join_path(tmp_dir, 'file_${i}.v')
		os.write_file(tmp_file, code) or { panic('failed to write temp file') }
		paths << tmp_file
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

fn test_generate_c_uses_push_many_for_array_slice_append() {
	csrc := generate_array_append_c_for_test('
fn main() {
	mut dst := []u8{}
	haystack := [u8(1), 2, 3]
	dst << haystack[1..3]
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){array__slice(haystack, 1, 3)}')
}

fn test_generate_c_uses_push_many_for_array_literal_append() {
	csrc := generate_array_append_c_for_test('
fn main() {
	mut dst := []u8{}
	dst << [u8(`$`)]
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){new_array_from_c_array(')
}

fn test_generate_c_uses_push_many_for_local_array_append() {
	csrc := generate_array_append_c_for_test('
fn source() []u8 {
	return [u8(1), 2]
}

fn main() {
	mut dst := []u8{}
	src := source()
	dst << src
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){src}')
}

fn test_generate_c_uses_push_many_for_fn_pointer_array_return_append() {
	csrc := generate_array_append_c_for_test('
fn source(data []u8) []u8 {
	return data
}

fn consume(hash_func fn ([]u8) []u8) {
	mut dst := []u8{}
	src := hash_func(dst)
	dst << src
}

fn main() {
	consume(source)
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){src}')
}

fn test_generate_c_keeps_bitwise_or_inside_array_append_value() {
	csrc := generate_array_append_c_for_test('
fn main() {
	mut bytes := []u8{}
	n1 := u8(1)
	n0 := u8(2)
	bytes << (n1 << 4) | n0
}
')
	assert csrc.contains('array__push(')
	assert csrc.contains('| n0')
	assert !csrc.contains('}) | n0')
}

fn test_generate_c_indexes_local_array_that_shadows_function_name() {
	csrc := generate_array_append_c_for_test('
fn bytes() []u8 {
	return []u8{}
}

fn main() {
	mut bytes := [u8(1)]
	bytes[0] &= u8(3)
}
')
	assert csrc.contains('((u8*)bytes.data)[((int)(0))] &= ((u8)(3));')
	assert !csrc.contains('bytes &= ((u8)(3));')
}

fn test_generate_c_uses_typed_zero_for_array_tuple_if_declarations() {
	csrc := generate_array_append_c_for_test('
fn pick(operand_a []u64, operand_b []u64) {
	mut a, mut b := if operand_a.len >= operand_b.len {
		operand_a, operand_b
	} else {
		operand_b, operand_a
	}
	_ = a
	_ = b
}
')
	assert csrc.contains('Array_u64 a = ((Array_u64){0});')
	assert csrc.contains('Array_u64 b = ((Array_u64){0});')
	assert !csrc.contains('Array_u64 a = 0;')
	assert !csrc.contains('Array_u64 b = 0;')
}

fn test_generate_c_uses_default_init_for_empty_array_struct_fields() {
	csrc := generate_array_append_c_for_test('
struct Pool {
	items []voidptr
}

fn main() {
	pool := Pool{
		items: []
	}
	_ = pool.items.len
}
	')
	assert csrc.contains('.items = __new_array_with_default_noscan(0, 0, sizeof(void*)')
	assert !csrc.contains('.items = new_array_from_c_array(0, 0, sizeof(void*)')
}

fn test_generate_c_specializes_generic_pointer_array_push_literal() {
	csrc := generate_array_append_c_for_test('
struct Foo {}

fn get_results_ref[T](items []voidptr) []&T {
	mut res := []&T{cap: items.len}
	for i in 0 .. items.len {
		res << unsafe { &T(items[i]) }
	}
	return res
}

fn main() {
	items := []voidptr{}
	res := get_results_ref[Foo](items)
	_ = res.len
}
	')
	assert csrc.contains('(Foo*[1]){((Foo*)')
	assert !csrc.contains('(T*[1])')
	assert !csrc.contains('stdatomic__T')
}

fn test_generate_c_specializes_comptime_pointer_field_generic_call_by_field_type() {
	csrc := generate_array_append_c_for_test('
fn struct_field_should_encode[T](val T) bool {
	_ = val
	return true
}

fn encode_fields[T](val T) bool {
	mut ok := true
	$for field in T.fields {
		ok = struct_field_should_encode(val.$(field.name))
	}
	return ok
}

struct Node {
	next &Node = unsafe { nil }
}

fn main() {
	_ = encode_fields(Node{})
}
')
	assert csrc.contains('struct_field_should_encode_T_Nodeptr(val.next)')
	assert !csrc.contains('struct_field_should_encode_T_Node((*val.next))')
}

fn test_generate_c_specializes_comptime_generic_pointer_field_by_field_type() {
	csrc := generate_array_append_c_for_test('
module json2

struct ValueInfo {}

struct Node[T] {
	value T
	next  &Node[T] = unsafe { nil }
}

struct Decoder {
	current_node &Node[ValueInfo] = unsafe { nil }
}

struct EncoderFieldInfo {}

struct Encoder {}

fn (mut encoder Encoder) encode_value[T](val T) {
	$if T is $pointer {
		encoder.encode_value(*val)
	} $else $if T is $struct {
		_ = encoder.encode_fields[T](val)
	}
}

fn (mut encoder Encoder) cached_field_infos[T]() []EncoderFieldInfo {
	return []EncoderFieldInfo{}
}

fn (mut encoder Encoder) encode_struct_field_value[T](val T) {
	$if T.indirections == 1 {
		encoder.encode_value(*val)
	} $else {
		encoder.encode_value(val)
	}
}

fn struct_field_should_encode[T](field_info EncoderFieldInfo, val T) bool {
	_ = field_info
	_ = val
	return true
}

fn (mut encoder Encoder) encode_fields[T](val T) bool {
	field_infos := encoder.cached_field_infos[T]()
	mut ok := true
	mut i := 0
	$for field in T.fields {
		field_info := field_infos[i]
		ok = struct_field_should_encode(field_info, val.$(field.name))
		encoder.encode_struct_field_value(val.$(field.name))
		i++
	}
	return ok
}

fn use_encoder() {
	mut encoder := Encoder{}
	_ = encoder.encode_fields(Decoder{})
	_ = encoder.encode_fields(Node[ValueInfo]{})
}
')
	assert csrc.contains('json2__struct_field_should_encode_T_json2_Nodeptr(field_info, val.current_node)')
	assert csrc.contains('json2__Encoder__encode_struct_field_value_T_json2_Nodeptr(encoder, val.current_node)')
	assert csrc.contains('json2__Encoder__cached_field_infos_T_json2_Decoder(encoder)')
	assert csrc.contains('Array_json2__EncoderFieldInfo json2__Encoder__cached_field_infos_T_json2_Node(json2__Encoder* encoder)')
	assert !csrc.contains('json2__struct_field_should_encode_T_json2_Node(field_info, (*val.current_node))')
	assert !csrc.contains('json2__Encoder__encode_struct_field_value_T_json2_Node(encoder, (*val.current_node))')
}

fn test_generate_c_extracts_array_sum_variant_for_generic_array_param() {
	csrc := generate_array_append_c_for_test_files([
		'
module orm

pub struct InfixType {
	right int
}

pub type Primitive = InfixType | []InfixType | int

pub fn tenant_filter_array_primitive_type[T](value []T) int {
	return value.len
}

pub fn tenant_filter_primitive_type(value Primitive) int {
	return match value {
		InfixType {
			2
		}
		[]InfixType {
			tenant_filter_array_primitive_type(value)
		}
		int {
			3
		}
	}
}
',
		'
module main

import orm

fn main() {
	_ = orm.tenant_filter_primitive_type([]orm.InfixType{})
}
',
	])
	assert csrc.contains('orm__tenant_filter_array_primitive_type_T_orm_InfixType(((((Array_orm__InfixType*)(value._data._Array_orm__InfixType))')
	assert !csrc.contains('orm__tenant_filter_array_primitive_type_T_orm_InfixType(((((orm__InfixType*)(value._data._InfixType))')
}

fn test_generate_c_lowers_global_interface_generic_method_call() {
	csrc := generate_array_append_c_for_test('
interface Rng {
	intn(max int) !int
}

__global default_rng &Rng

fn (mut rng Rng) element[T](array []T) !T {
	if array.len == 0 {
		return error("empty")
	}
	return array[rng.intn(array.len)!]
}

fn element[T](array []T) !T {
	return default_rng.element[T](array)
}

fn main() {
	_ = element[u8]([u8(1)]) or { u8(0) }
}
')
	assert csrc.contains('Rng__element_T_u8(default_rng, _v_array)')
	assert !csrc.contains('u8 _val = (_v_array)')
}
