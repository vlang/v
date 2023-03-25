struct MmapRangeLocal {}

fn addr2range() ?(&MmapRangeLocal, ?u64, u64) {
	return none
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
	return none, none
}

fn tuple4() ?(?int, ?int) {
	return none
}

fn test_tuple_1() {
	a, b := tuple()
	assert a == 1
	assert b == 2
}

fn test_tuple_2() {
	a, b := tuple2()
	assert a == ''
	assert b == 2
}

fn test_tuple_3() {
	a, b := tuple3()
	assert a == none
	assert b == none
}

fn test_tuple_4() {
	a, b := tuple4()
	assert a == none
	assert b == none
}

fn test_none_ret() {
	_, b, c := addr2range()
	assert b == none
	assert c == 0
}
