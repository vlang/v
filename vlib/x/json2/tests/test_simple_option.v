import x.json2

// Simple test without time dependencies
struct SimpleOptionTestStruct {
	name ?string
	age  ?int
}

fn main() {
	// Test with values
	simple := SimpleOptionTestStruct{
		name: 'test'
		age: 42
	}
	
	result := json2.map_from(simple)
	
	println('Result keys: ${result.keys()}')
	println('Result: ${result}')
	
	// Verify option fields are now detected and included
	if 'name' in result {
		println('✓ name field included')
		assert result['name']! == json2.Any('test')
	} else {
		println('✗ name field NOT included')
		exit(1)
	}
	
	if 'age' in result {
		println('✓ age field included')
		assert result['age']!.int() == 42
	} else {
		println('✗ age field NOT included')
		exit(1)
	}
	
	// Test with none - fields should be omitted
	simple_none := SimpleOptionTestStruct{}
	result_none := json2.map_from(simple_none)
	
	if 'name' in result_none || 'age' in result_none {
		println('✗ Fields with none should be omitted')
		exit(1)
	} else {
		println('✓ Fields with none correctly omitted')
	}
	
	println('✓ All option field tests passed!')
}
