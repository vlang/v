struct StructType {
mut:
	val string = 'string from StructType'
}

struct Data {
mut:
	not_map       string
	empty_to_test map[string]string
	users         map[string]StructType
	extra         map[string]map[string]int
}

fn test_comptimeselector_map_different_types() {
	data := Data{
		users: {
			'a': StructType{}
		}
		extra: {
			'b': {
				'c': 10
			}
		}
	}

	mut keys := []string{}
	mut vals := []string{}

	$for field in Data.fields {
		$if field.typ is $map {
			for k, v in data.$(field.name) {
				keys << k.str()
				vals << v.str()
			}
		}
	}
	assert keys == ['a', 'b']
	assert vals[0] == "StructType{
    val: 'string from StructType'
}"
	assert vals[1] == "{'c': 10}"
}

fn test_comptime_var_map_different_types() {
	data := Data{
		users: {
			'a': StructType{}
		}
		extra: {
			'b': {
				'c': 10
			}
		}
	}

	mut keys := []string{}
	mut vals := []string{}

	$for field in Data.fields {
		$if field.typ is $map {
			gg := data.$(field.name)
			for k, v in gg {
				keys << k.str()
				vals << v.str()
			}
		}
	}
	assert keys == ['a', 'b']
	assert vals[0] == "StructType{
    val: 'string from StructType'
}"
	assert vals[1] == "{'c': 10}"
}

fn test_comptime_with_dump() {
	data := Data{
		users: {
			'a': StructType{}
		}
		extra: {
			'b': {
				'c': 10
			}
		}
	}

	$for field in Data.fields {
		$if field.typ is $map {
			for k, v in data.$(field.name) {
				dump(k)
				dump(v)
			}
		}
	}
	assert true
}

fn test_comptime_dump_for_key_and_value() {
	data := Data{
		users: {
			'a': StructType{}
		}
		extra: {
			'b': {
				'c': 10
			}
		}
	}

	mut key_types := []string{}
	mut val_types := []string{}

	$for field in Data.fields {
		$if field.typ is $map {
			for k, v in data.$(field.name) {
				key_types << typeof(k).name
				val_types << typeof(v).name
			}
		}
	}
	assert val_types[0] == 'StructType'
	assert val_types[1] == 'map[string]int'

	assert key_types[0] == 'string'
	assert key_types[1] == 'string'
}

fn test_comptime_key_value_var() {
	data := Data{
		users: {
			'a': StructType{}
		}
		extra: {
			'b': {
				'c': 10
			}
		}
	}

	mut keys := []string{}
	mut vals := []string{}

	$for field in Data.fields {
		$if field.typ is $map {
			for k, v in data.$(field.name) {
				if k == 'a' {
					keys << dump(k)
					vals << dump(v.str())
				}
				if k == 'b' {
					keys << dump(k)
					vals << dump(v.str())
				}
			}
		}
	}
	assert keys[0] == 'a'
	assert keys[1] == 'b'

	assert vals[0] == "StructType{
    val: 'string from StructType'
}"
	assert vals[1] == "{'c': 10}"
}

fn test_comptime_generic_argument() {
	data := Data{
		users: {
			'a': StructType{}
		}
		extra: {
			'b': {
				'c': 10
			}
		}
	}

	$for field in Data.fields {
		$if field.typ is $map {
			for k, v in data.$(field.name) {
				process_value_from_map(v)
				assert k in ['a', 'b']
			}
		}
	}
}

fn process_value_from_map[T](v T) {
	$if T is $map {
		assert typeof(v).name == 'map[string]int'
		assert v.str() == "{'c': 10}"
		assert true
		return
	} $else $if T is $struct {
		assert typeof(v).name == 'StructType'
		assert v.str() == "StructType{
    val: 'string from StructType'
}"
		assert true
		return
	}
	assert false
}
