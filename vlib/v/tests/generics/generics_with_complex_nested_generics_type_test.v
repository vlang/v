fn test_generics_with_complex_nested_generics_type() {
	mut buf := []u8{}
	initial[string, u64](buf)
}

fn initial[K, V](buf []u8) map[K]V {
	mut ret := map[K]V{}
	for _ in 0 .. 3 {
		k := get[K](buf)
		v := get[V](buf)
		ret[k] = v
	}
	println(ret)
	assert '${ret}' == "{'get': 22}"
	return ret
}

fn get[T](buf []u8) T {
	$if T is string {
		return buf.bytestr() + 'get'
	} $else $if T is u64 {
		return u64(22)
	} $else {
		panic('oops!')
	}
}
