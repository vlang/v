fn test_generic_fn_infer_fn_type_argument() {
	to_r := fn (x int) rune {
		return [`😺`, `😸`, `😹`, `😻`, `😾`][x - 1]
	}
	to_f64 := fn (x int) f64 {
		return f64(x) + 0.123
	}
	to_s := fn (x int) string {
		return ['One', 'Two', 'Three', 'Four', 'Five'][x - 1]
	}

	items := [1, 2, 3, 4, 5]

	ret_r := fmap(to_r, items)
	println('${ret_r.map(rune(it))}')
	assert '${ret_r.map(rune(it))}' == '[`😺`, `😸`, `😹`, `😻`, `😾`]'

	// returns random same number for every item in array
	ret_f64 := fmap(to_f64, items)
	println(ret_f64)
	assert ret_f64 == [1.123, 2.123, 3.123, 4.123, 5.123]

	ret_s := fmap(to_s, items)
	println(ret_s)
	assert ret_s == ['One', 'Two', 'Three', 'Four', 'Five']
}

// [noah04 #14214] code
fn fmap<I, O>(func fn (I) O, list []I) []O {
	return []O{len: list.len, init: func(list[it])}
}
