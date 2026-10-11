module json2

struct Json5BorrowedStrings {
	name        string
	items       []string
	object_text string
	raw         string @[raw]
}

fn test_json5_returned_strings_survive_allocations_and_buffer_reuse() {
	mut buffer := DecodeBuffer{}
	root := decode[string]("'root string'", json5: true)!
	fields := decode_reuse[Json5BorrowedStrings]("{name:'field string',items:['array string'],object_text:{key:'object string'},raw:{key:'raw string'}}",
		mut buffer,
		json5: true
	)!
	values := decode[map[string]string]("{'map key':'map value'}", json5: true)!
	dynamic := decode[Any]("{bareKey:'dynamic string'}", json5: true)!
	dynamic_root := decode[Any]("'dynamic root string'", json5: true)!
	mut allocations := [][]u8{}
	for i in 0 .. 256 {
		// Occupy freed parser allocations of different sizes and repeatedly reuse tokens.
		allocations << []u8{len: 16 + i, init: u8(i)}
		next := decode_reuse[Json5BorrowedStrings]("{name:'replacement',items:['other'],object_text:{key:'other'},raw:{key:'other'}}",
			mut buffer,
			json5: true
		)!
		assert next.name == 'replacement'
	}
	assert allocations.len == 256
	assert root == 'root string'
	assert fields.name == 'field string'
	assert fields.items == ['array string']
	assert fields.object_text == '{"key":"object string"}'
	assert fields.raw == '{"key":"raw string"}'
	assert values.keys() == ['map key']
	assert values['map key'] == 'map value'
	assert dynamic.as_map().keys() == ['bareKey']
	assert (dynamic.as_map()['bareKey'] or { panic('missing key') }).str() == 'dynamic string'
	assert dynamic_root.str() == 'dynamic root string'
}
