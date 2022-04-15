fn test_generics_with_nested_generic_fn_inst_call() {
	value := [u8(105), 116, 32, 119, 111, 114, 107, 115, 33, 33]
	decoded := decode_arr<string>(value)
	dump(decoded)
	assert decoded.len == 4
	assert decoded[0] == 'it works!!'
	assert decoded[1] == 'it works!!'
	assert decoded[2] == 'it works!!'
	assert decoded[3] == 'it works!!'
}

fn decode_arr<T>(buf []u8) []T {
	arr_size := decode<u32>(buf[0..4])
	mut ret := []T{cap: int(arr_size)}

	for _ in 0 .. arr_size {
		ret << decode<T>(buf[..])
	}
	return ret
}

fn decode<T>(buf []u8) T {
	$if T is u32 {
		return u32(buf.len)
	} $else $if T is string {
		return buf.bytestr()
	} $else {
		panic('"$T.name" is not supported for `decode`')
	}
}
