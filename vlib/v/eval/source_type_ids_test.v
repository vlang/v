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
