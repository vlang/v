import x.json2

struct NestedStruct {
	name string
}

struct ComplexStruct {
	name     string
	metadata map[string]string
	alias    string
	age      ?int
	nested   ?NestedStruct
}

fn main() {
	complex := ComplexStruct{
		name: 'complex'
		metadata: {
			'key': 'value'
		}
		alias: 'alias'
		age: 42
		nested: NestedStruct{
			name: 'nested'
		}
	}
	
	result := json2.map_from(complex)
	
	println('Result keys: ${result.keys()}')
	println('Result: ${result}')
	
	// Check if option field 'age' is included
	if 'age' in result {
		println('✓ Option field age IS included')
		assert result['age']!.int() == 42
	} else {
		println('✗ Option field age NOT included')
	}
	
	// Check if alias field is included
	if 'alias' in result {
		println('✓ Alias field IS included')
		assert result['alias']! == json2.Any('alias')
	} else {
		println('✗ Alias field NOT included')
	}
	
	// Check if map field is included
	if 'metadata' in result {
		println('✓ Map field IS included')
		metadata_map := result['metadata']!.as_map()
		assert metadata_map['key']!.str() == 'value'
	} else {
		println('✗ Map field NOT included')
	}
	
	// Check nested option struct
	if 'nested' in result {
		println('✓ Nested option struct IS included')
		nested_map := result['nested']!.as_map()
		if 'name' in nested_map {
			println('✓ Nested struct field IS included')
			assert nested_map['name']!.str() == 'nested'
		} else {
			println('✗ Nested struct field NOT included')
		}
	} else {
		println('✗ Nested option struct NOT included')
	}
}
