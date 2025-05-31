struct MmapRangeLocal {}

fn addr2range(none_ bool) ?(&MmapRangeLocal, ?u64, u64) {
	if none_ {
		return none
	} else {
		return &MmapRangeLocal{}, none, 1
	}
}

fn foo(val ?int) (?int, ?int) {
	return val, none
}

fn test_multi_return() {
	a, b := foo(100)
	assert a? == 100
	assert b == none
}

fn tuple() ?(int, int) {
	return 1, 2
}

fn tuple2() ?(string, int) {
	return '', 2
}

fn tuple3() ?(?int, ?int) {
	return none
}

fn tuple4(none_ bool) ?(?int, ?int) {
	if none_ {
		return none, none
	} else {
		return 1, none
	}
}

fn test_tuple_1() {
	a, b := tuple()?
	assert a == 1
	assert b == 2
}

fn test_tuple_2() {
	a, b := tuple2()?
	assert a == ''
	assert b == 2
}

fn test_tuple_3() {
	a, b := tuple3() or { return }
	assert false
}

fn test_tuple_4() {
	if _, _ := tuple4(true) {
		assert false
	}
	a, b := tuple4(false)?
	assert a? == 1
	assert b == none
}

fn test_none_ret() {
	if _, _, _ := addr2range(true) {
		assert false
	}
	_, b, c := addr2range(false)?
	assert b == none
	assert c == 1
}
