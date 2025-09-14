struct Decoder {}

pub fn decode[T](val string) !T {
	mut decoder := Decoder{}
	mut result := T{}
	decoder.decode_value(mut result)!
	return result
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is $array {
		decoder.decode_array(mut val)!
		return
	} $else $if T is $option {
		val = none
	}
}

fn (mut decoder Decoder) decode_array[T](mut val []T) ! {
	$if T is $option {
		val << none
		val << 1
	} $else {
		val << 1
	}

	mut array_element := T{}
	decoder.decode_value(mut array_element)!
	val << array_element
}

fn test_main() {
	assert decode[[]int]('')! == [1, 0]
	assert decode[[]?int]('')! == [?int(none), 1, none]
}
