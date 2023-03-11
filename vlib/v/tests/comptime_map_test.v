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
		$if field.typ is $Map {
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
		$if field.typ is $Map {
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
