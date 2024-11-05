module main

struct Decoder {
	json string
}

pub fn decode[T](val string) !T {
	mut decoder := Decoder{
		json: val
	}

	mut result := T{}
	decoder.decode_value(mut &result)!
	return result
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T is $array {
		mut array_element := create_array_element(val)

		decoder.decode_value(mut array_element)!
		println(array_element)
		dump(array_element)
		assert &array_element != unsafe { nil }

		val << array_element
	} $else $if T is $map {
		mut map_value := create_map_value(val)

		decoder.decode_value(mut map_value)!
		println(map_value)
		dump(map_value)
		assert &map_value != unsafe { nil }

		val['key'] = map_value
	}
}

fn create_array_element[T](array []T) T {
	a := T{}
	dump(a)
	dump(a.str)
	assert a.str != unsafe { nil }
	return a
}

fn create_map_value[K, V](map_ map[K]V) V {
	a := V{}
	dump(a)
	dump(a.str)
	assert a.str != unsafe { nil }
	return a
}

fn test_main() {
	assert decode[[]string]('["1", "2", "3"]')! == ['']
	assert decode[[]int]('[1, 2, 3]')! == [0]
	assert decode[map[string]string]('{"val": "2"}')! == {
		'key': ''
	}
	assert decode[map[string]int]('{"a": 1}')! == {
		'key': 0
	}
}
