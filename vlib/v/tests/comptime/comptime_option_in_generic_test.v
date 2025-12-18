// Test that field.is_option works correctly in generic functions
struct TestStruct {
	a ?int
	b ?string
	c int
	d string
}

fn check_option_detection_in_generic[T](val T) bool {
	mut option_fields := []string{}
	mut non_option_fields := []string{}
	
	$if T is $struct {
		$for field in T.fields {
			$if field.is_option {
				option_fields << field.name
			} $else {
				non_option_fields << field.name
			}
		}
	}
	
	// Verify option fields are detected
	assert 'a' in option_fields
	assert 'b' in option_fields
	assert option_fields.len == 2
	
	// Verify non-option fields are detected
	assert 'c' in non_option_fields
	assert 'd' in non_option_fields
	assert non_option_fields.len == 2
	return true
}

fn check_option_typ_check_in_generic[T](val T) bool {
	mut option_fields := []string{}
	
	$if T is $struct {
		$for field in T.fields {
			$if field.typ is $option {
				option_fields << field.name
			}
		}
	}
	
	// Verify option fields are detected via field.typ is $option
	assert 'a' in option_fields
	assert 'b' in option_fields
	assert option_fields.len == 2
	return true
}

fn test_option_detection() {
	test_struct := TestStruct{
		a: 42
		b: 'test'
		c: 100
		d: 'non-option'
	}
	
	result1 := check_option_detection_in_generic(test_struct)
	assert result1
	
	result2 := check_option_typ_check_in_generic(test_struct)
	assert result2
}
