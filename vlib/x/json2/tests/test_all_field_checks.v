import x.json2

type AllFieldChecksStringAlias = string
type AllFieldChecksIntAlias = int

struct TestAllChecks {
	a ?string
	b ?int
	c string
	d int
	e AllFieldChecksStringAlias
	f AllFieldChecksIntAlias
}

fn main() {
	test_struct := TestAllChecks{
		a: 'option_string'
		b: 42
		c: 'string'
		d: 100
		e: 'alias_string'
		f: 200
	}
	
	result := json2.map_from(test_struct)
	
	println('Result keys: ${result.keys()}')
	println('Result: ${result}')
	
	// Verify option fields are included
	if 'a' in result {
		println('✓ Option field a (?string) included')
		assert result['a']! == json2.Any('option_string')
	} else {
		println('✗ Option field a NOT included')
		exit(1)
	}
	
	if 'b' in result {
		println('✓ Option field b (?int) included')
		assert result['b']!.int() == 42
	} else {
		println('✗ Option field b NOT included')
		exit(1)
	}
	
	// Verify regular fields are included
	if 'c' in result {
		println('✓ Regular field c (string) included')
		assert result['c']! == json2.Any('string')
	} else {
		println('✗ Regular field c NOT included')
		exit(1)
	}
	
	if 'd' in result {
		println('✓ Regular field d (int) included')
		assert result['d']!.int() == 100
	} else {
		println('✗ Regular field d NOT included')
		exit(1)
	}
	
	// Verify alias fields are included (they should fall through to primitive handling)
	if 'e' in result {
		println('✓ Alias field e (StringAlias) included')
		assert result['e']! == json2.Any('alias_string')
	} else {
		println('✗ Alias field e NOT included')
		exit(1)
	}
	
	if 'f' in result {
		println('✓ Alias field f (IntAlias) included')
		assert result['f']!.int() == 200
	} else {
		println('✗ Alias field f NOT included')
		exit(1)
	}
	
	println('')
	println('✓ All field type checks passed!')
}
