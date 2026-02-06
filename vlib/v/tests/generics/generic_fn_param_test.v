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
	} $else $if T is $map {
		mut map_value := create_map_value(val)

		decoder.decode_value(mut map_value)!
	}
}

fn create_array_element[T](array []T) T {
	return T{}
}

fn create_map_value[K, V](map_ map[K]V) V {
	return V{}
}

fn test_main() {
	decode[[]int]('[1, 2, 3]')!
	decode[[]string]('["1", "2", "3"]')!
	decode[map[string]int]('{"a": 1}')!
	decode[map[string]string]('{"val": "2"}')!
}
