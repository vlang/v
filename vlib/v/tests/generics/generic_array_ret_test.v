module main

fn decode_primitive[T]() !T {
	$if T is int {
		return T(1)
	} $else $if T is u8 {
		return T(10)
	} $else $if T is u16 {
		return T(100)
	}
	return error('decode_primitive: not found')
}

fn decode_array[T](_ []T) ![]T {
	mut arr := []T{}
	arr << decode_primitive[T]()!
	return arr
}

fn decode[T]() !T {
	$if T is $array {
		a := decode_array(T{})!
		return a
	}
	return error('decode: not found')
}

fn test_main() {
	assert decode[[]int]()! == [1]
	assert decode[[]u8]()! == [u8(10)]
	assert decode[[]u16]()! == [u16(100)]
}
