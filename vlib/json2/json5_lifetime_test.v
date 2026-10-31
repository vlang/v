module json2

import os

struct Json5BorrowedStrings {
	name        string
	items       []string
	object_text string
	raw         string @[raw]
}

struct Json5RetainedStringHook {
mut:
	text string
}

fn (mut value Json5RetainedStringHook) from_json_string(text string) ! {
	value.text = text
}

struct Json5RetainedNumberHook {
mut:
	text string
}

fn (mut value Json5RetainedNumberHook) from_json_number(text string) ! {
	value.text = text
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
	escaped := decode[string]("'escaped\\nstring'", json5: true)!
	string_hook := decode[Json5RetainedStringHook]("'hook string'", json5: true)!
	number_hook := decode[Json5RetainedNumberHook]('0x1234', json5: true)!
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
	assert escaped == 'escaped\nstring'
	assert string_hook.text == 'hook string'
	assert number_hook.text == '0x1234'
}

@[manualfree]
fn test_file_decoding_returns_owned_strings_in_both_dialects() {
	for json5 in [false, true] {
		path := os.join_path(os.temp_dir(), 'json2_file_lifetime_${os.getpid()}_${json5}.json')
		defer { os.rm(path) or {} }
		os.write_file(path, '"root string"')!
		root := decode_file[string](path, json5: json5)!
		dynamic_root := parse_file(path, json5: json5)!
		string_hook := decode_file[Json5RetainedStringHook](path, json5: json5)!
		os.write_file(path, '{"name":"field string","items":["array string"],"object_text":{"key":"object string"},"raw":{"key":"raw string"}}')!
		fields := decode_file[Json5BorrowedStrings](path, json5: json5)!
		dynamic := parse_file(path, json5: json5)!
		os.write_file(path, '{"map key":"map value"}')!
		values := decode_file[map[string]string](path, json5: json5)!
		os.write_file(path, '4660')!
		number_hook := decode_file[Json5RetainedNumberHook](path, json5: json5)!
		mut allocations := [][]u8{}
		for i in 0 .. 256 {
			allocations << []u8{len: 16 + i, init: u8(i)}
		}
		assert allocations.len == 256
		assert root == 'root string'
		assert dynamic_root.root.str() == 'root string'
		assert string_hook.text == 'root string'
		assert fields.name == 'field string'
		assert fields.items == ['array string']
		assert fields.object_text == '{"key":"object string"}'
		assert fields.raw == '{"key":"raw string"}'
		assert (dynamic.root.as_map()['name'] or { panic('missing name') }).str() == 'field string'
		assert values.keys() == ['map key']
		assert values['map key'] == 'map value'
		assert number_hook.text == '4660'
	}
}

@[manualfree]
fn test_file_decoding_returns_owned_scalar_strings_in_both_dialects() {
	for json5 in [false, true] {
		path := os.join_path(os.temp_dir(), 'json2_file_scalar_lifetime_${os.getpid()}_${json5}.json')
		defer { os.rm(path) or {} }
		os.write_file(path, '"root string"')!
		root := decode_file[string](path, json5: json5)!
		doc := parse_file(path, json5: json5)!
		os.write_file(path, '"replacement"')!
		for _ in 0 .. 256 {
			other := decode_file[string](path, json5: json5)!
			assert other == 'replacement'
			unsafe { other.free() }
		}
		assert root == 'root string'
		assert doc.root.str() == 'root string'
	}
}
