struct EmptyStruct {
	b []string
pub:
	a int
}

fn encode_struct[T](val T) {
	mut result := map[string][]string{}
	$for field in T.fields {
		$if field.is_array == false {
			result[field.name] << '> is not array'
		} $else {
			result[field.name] << '> is array'
		}
		$if !field.is_array {
			result[field.name] << '>> is not array'
		} $else $if field.is_pub {
			result[field.name] << '>> is public'
		} $else {
			result[field.name] << '>> is array'
		}
		$if field.is_array {
			result[field.name] << '>>> is array'
		} $else {
			result[field.name] << '>>> is not array'
		}
		$if field.is_shared {
			result[field.name] << '>>>> is shared'
		} $else $if field.is_pub {
			result[field.name] << '>>>> is pub'
		}
	}
	$if !false {
		result['bool'] << '1'
	}
	$if !true {
		result['bool'] << '2'
	}
	$if true {
		result['bool'] << '3'
	}
	$if false {
		result['bool'] << '4'
	}

	assert result['a'].len == 4
	assert result['b'].len == 3
	assert result['bool'].len == 2

	assert result['a'][0] == '> is not array'
	assert result['a'][1] == '>> is not array'
	assert result['a'][2] == '>>> is not array'
	assert result['a'][3] == '>>>> is pub'

	assert result['b'][0] == '> is array'
	assert result['b'][1] == '>> is array'
	assert result['b'][2] == '>>> is array'

	assert result['bool'][0] == '1'
	assert result['bool'][1] == '3'
}

fn test_main() {
	encode_struct(EmptyStruct{})
}
