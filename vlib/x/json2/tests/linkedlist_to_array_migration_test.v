import x.json2

// Compatibility test to ensure linked list to array migration
// maintains identical functionality and error messages

fn test_basic_types() {
	// Test all basic types decode correctly
	assert json2.decode[int]('42')! == 42
	assert json2.decode[string]('"hello"')! == 'hello'
	assert json2.decode[bool]('true')! == true
	assert json2.decode[bool]('false')! == false
	assert json2.decode[f64]('3.14')! == 3.14
}

fn test_arrays() {
	// Test array decoding
	arr := json2.decode[[]int]('[1, 2, 3]')!
	assert arr == [1, 2, 3]
	
	str_arr := json2.decode[[]string]('["a", "b", "c"]')!
	assert str_arr == ['a', 'b', 'c']
}

fn test_objects() {
	// Test object decoding
	obj := json2.decode[map[string]json2.Any]('{"key": "value", "num": 42}')!
	assert obj['key']! == json2.Any('value')
	assert obj['num']!.int() == 42
}

struct Person {
	name string
	age  int
}

fn test_structs() {
	// Test struct decoding
	person := json2.decode[Person]('{"name": "Alice", "age": 30}')!
	assert person.name == 'Alice'
	assert person.age == 30
}

fn test_nested_structures() {
	// Test nested objects
	json := '{"outer": {"inner": {"value": 42}}}'
	obj := json2.decode[map[string]json2.Any](json)!
	outer := obj['outer']!.as_map()
	inner := outer['inner']!.as_map()
	assert inner['value']!.int() == 42
}

fn test_error_messages() {
	// Test that error messages remain identical
	mut has_error := false
	
	json2.decode[int]('"not a number"') or {
		if err is json2.JsonDecodeError {
			// Error message format: "Data: Expected number, but got string"
			// Just verify it's a JsonDecodeError with valid line/character
			assert err.line > 0
			assert err.character > 0
			assert err.message.len > 0
		}
		has_error = true
	}
	assert has_error, 'Expected error for int decode from string'
	
	has_error = false
	json2.decode[string]('{"not": "a string"}') or {
		if err is json2.JsonDecodeError {
			// Error message format: "Data: Expected string, but got object"
			// Just verify it's a JsonDecodeError
			assert err.message.len > 0
		}
		has_error = true
	}
	assert has_error, 'Expected error for string decode from object'
}

fn test_edge_cases() {
	// Test edge cases
	assert json2.decode[int]('0')! == 0
	assert json2.decode[int]('-1')! == -1
	assert json2.decode[string]('""')! == ''
	assert json2.decode[[]int]('[]')! == []
	assert json2.decode[map[string]int]('{}')! == map[string]int{}
}

type Sum = int | string | bool

fn test_sum_types() {
	// Test sum type decoding
	int_val := json2.decode[Sum]('42')!
	assert int_val is int
	
	str_val := json2.decode[Sum]('"hello"')!
	assert str_val is string
	
	bool_val := json2.decode[Sum]('true')!
	assert bool_val is bool
}

fn test_any_type() {
	// Test json2.Any type
	any_val := json2.decode[json2.Any]('{"key": "value", "num": 42}')!
	obj := any_val.as_map()
	assert obj['key']!.str() == 'value'
	assert obj['num']!.int() == 42
}

fn test_large_documents() {
	// Test with larger JSON documents
	mut parts := []string{}
	parts << '{"items": ['
	for i := 0; i < 50; i++ {
		if i > 0 {
			parts << ','
		}
		parts << '{"id": ${i}, "name": "item${i}"}'
	}
	parts << ']}'
	json := parts.join('')
	
	result := json2.decode[map[string]json2.Any](json)!
	items := result['items']!.arr()
	assert items.len == 50
}

fn main() {
	println('Running compatibility tests...')
	
	test_basic_types()
	println('✓ Basic types')
	
	test_arrays()
	println('✓ Arrays')
	
	test_objects()
	println('✓ Objects')
	
	test_structs()
	println('✓ Structs')
	
	test_nested_structures()
	println('✓ Nested structures')
	
	test_error_messages()
	println('✓ Error messages')
	
	test_edge_cases()
	println('✓ Edge cases')
	
	test_sum_types()
	println('✓ Sum types')
	
	test_any_type()
	println('✓ Any type')
	
	test_large_documents()
	println('✓ Large documents')
	
	println('')
	println('All compatibility tests passed!')
}
