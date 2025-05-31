import x.json2

struct User {
pub mut:
	numbers map[string]string
}

fn test_main() {
	user_json := '{"numbers":{"home":"123456","work":"987653"}}'
	res := json2.raw_decode(user_json)!.as_map()

	mut numbers := map[string]string{}
	decode_map(mut numbers, res['numbers']!.as_map())!
	assert numbers == {
		'home': '123456'
		'work': '987653'
	}

	assert decode_struct[User](res)! == User{
		numbers: {
			'home': '123456'
			'work': '987653'
		}
	}
}

fn decode_struct[T](res map[string]json2.Any) !T {
	mut typ := T{}
	$for field in T.fields {
		$if field.is_map {
			decode_map(mut typ.$(field.name), res[field.name]!.as_map())!
		} $else {
			return error("The type of `${field.name}` can't be decoded.")
		}
	}
	return typ
}

fn decode_map[V](mut m map[string]V, res map[string]json2.Any) ! {
	$if V is $string {
		for k, v in res {
			m[k] = v.str()
		}
	} $else {
		return error("The map value can't be decoded.")
	}
}
