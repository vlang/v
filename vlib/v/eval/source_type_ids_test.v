module eval

import v.flat

fn test_eval_type_value_and_pointer_payload_source_ids() {
	mut e := create()
	e.run_text('
type Address = voidptr
type First = Second
type Second = First
fn main() {}
') or { panic(err) }
	value := Value(TypeValue{ name: flat.source_type_int })
	assert e.value_matches_type_name(value, flat.source_type_id('Type'))
	assert e.value_matches_type_name(value, flat.source_type_id('types.Type'))
	assert !e.value_matches_type_name(value, flat.source_type_id('other.Type'))
	assert !e.value_matches_type_name(value, flat.source_type_int)
	for spelling in ['&int', '&&u8', 'voidptr', 'byteptr', 'charptr', 'Address'] {
		assert e.sum_variant_has_pointer_payload(flat.source_type_id(spelling), 'main', []u32{})
	}
	for spelling in ['int', 'string', '[]&int', '?&int', 'First'] {
		assert !e.sum_variant_has_pointer_payload(flat.source_type_id(spelling), 'main', []u32{})
	}
}

fn test_eval_source_operands_remain_independent_of_checked_types() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.int_literal, '42')
	start := a.children.len
	a.children << value
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		source_value:   flat.source_type_u8
		typ:            flat.source_type_string
		children_start: i32(start)
		children_count: 1
	})
	check_start := a.children.len
	a.children << cast
	check := a.add_node(flat.Node{
		kind:           .is_expr
		source_value:   flat.source_type_u8
		typ:            flat.source_type_bool
		children_start: i32(check_start)
		children_count: 1
	})
	size := a.add_node(flat.Node{
		kind:         .sizeof_expr
		source_value: flat.source_type_u8
		typ:          flat.source_type_int
	})
	mut e := create()
	e.reset(&a)
	assert e.infer_expr_type_name(cast) == flat.source_type_u8
	checked := e.eval_expr(check) or { panic(err) }
	sized := e.eval_expr(size) or { panic(err) }
	assert checked == Value(true)
	assert sized == Value(i64(1))
}
