import x.json2

// Simple struct with option fields
struct MapFromOptionDebugStruct {
	name ?string
	age  ?int
}

// Nested option struct
struct NestedOption {
	inner ?MapFromOptionDebugStruct
}

fn test_simple_option_field() {
	println('=== Testing Simple Option Field ===')
	
	// Test with values
	simple := MapFromOptionDebugStruct{
		name: 'test'
		age: 42
	}
	
	println('Input: MapFromOptionDebugStruct{ name: "test", age: 42 }')
	result := json2.map_from(simple)
	println('Result keys: ${result.keys()}')
	println('Result: ${result.str()}')
	
	// Check what fields were detected
	if 'name' in result {
		println('✓ name field included')
		assert result['name']! == json2.Any('test')
	} else {
		println('✗ name field NOT included')
	}
	
	if 'age' in result {
		println('✓ age field included')
		assert result['age']!.int() == 42
	} else {
		println('✗ age field NOT included')
	}
	
	// Test with none
	simple_none := MapFromOptionDebugStruct{}
	result_none := json2.map_from(simple_none)
	println('Input: MapFromOptionDebugStruct{} (all none)')
	println('Result keys: ${result_none.keys()}')
	println('Result: ${result_none.str()}')
	
	assert 'name' !in result_none
	assert 'age' !in result_none
}

fn test_nested_option_struct() {
	println('=== Testing Nested Option Struct ===')
	
	// Test with values
	nested := NestedOption{
		inner: MapFromOptionDebugStruct{
			name: 'nested'
			age: 25
		}
	}
	
	println('Input: NestedOption{ inner: MapFromOptionDebugStruct{ name: "nested", age: 25 } }')
	result := json2.map_from(nested)
	println('Result keys: ${result.keys()}')
	println('Result: ${result.str()}')
	
	if 'inner' in result {
		println('✓ inner field included')
		inner_map := result['inner']!.as_map()
		println('Inner map keys: ${inner_map.keys()}')
		println('Inner map: ${inner_map.str()}')
		
		if 'name' in inner_map {
			println('✓ inner.name field included')
			assert inner_map['name']!.str() == 'nested'
		} else {
			println('✗ inner.name field NOT included')
		}
		
		if 'age' in inner_map {
			println('✓ inner.age field included')
			assert inner_map['age']!.int() == 25
		} else {
			println('✗ inner.age field NOT included')
		}
	} else {
		println('✗ inner field NOT included')
	}
	
	// Test with none
	nested_none := NestedOption{}
	result_none := json2.map_from(nested_none)
	println('Input: NestedOption{} (inner is none)')
	println('Result keys: ${result_none.keys()}')
	println('Result: ${result_none.str()}')
	
	assert 'inner' !in result_none
}

fn main() {
	test_simple_option_field()
	println('')
	test_nested_option_struct()
	println('')
	println('Diagnostic tests completed.')
}
