import x.json2

struct MapFromFieldDebugStruct {
	name ?string
	age  ?int
}

fn test_field_detection() {
	simple := MapFromFieldDebugStruct{
		name: 'test'
		age: 42
	}
	
	// Try to manually check what fields are detected
	$for field in MapFromFieldDebugStruct.fields {
		println('Field: ${field.name}')
		println('  is_option: ${field.is_option}')
		println('  typ: ${field.typ}')
		println('  unaliased_typ: ${field.unaliased_typ}')
		
		$if field is $option {
			println('  ✓ Detected as option via: field is option')
		} $else $if field.typ is $option {
			println('  ✓ Detected as option via: field.typ is option')
		} $else {
			println('  ✗ NOT detected as option')
		}
		
		value := simple.$(field.name)
		println('  value type: ${typeof(value).name}')
		println('  value == none: ${value == none}')
		if value != none {
			println('  value != none: true')
			unwrapped := value?
			println('  unwrapped type: ${typeof(unwrapped).name}')
		} else {
			println('  value != none: false')
		}
		println('')
	}
}

fn main() {
	test_field_detection()
}
