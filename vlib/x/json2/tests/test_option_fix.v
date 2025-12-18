import x.json2

// Simple struct with option fields
struct OptionFixTestStruct {
	name ?string
	age  ?int
}

fn test_option_detection() ! {
	// Test with values
	simple := OptionFixTestStruct{
		name: 'test'
		age: 42
	}
	
	result := json2.map_from(simple)
	
	// Verify option fields are now detected and included
	assert 'name' in result
	assert result['name']! == json2.Any('test')
	assert 'age' in result
	assert result['age']!.int() == 42
	
	// Test with none - fields should be omitted
	simple_none := OptionFixTestStruct{}
	result_none := json2.map_from(simple_none)
	assert 'name' !in result_none
	assert 'age' !in result_none
	
	println('✓ Option field detection test passed!')
}

fn main() {
	test_option_detection() or { panic(err) }
}
