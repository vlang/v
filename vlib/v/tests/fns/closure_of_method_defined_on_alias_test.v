type UInt = u32

type Label = int

fn (me UInt) member() u32 {
	println('member called')
	return me * 10
}

fn (me Label) str() string {
	return 'alias'
}

fn call_string_callback(callback fn () string) string {
	return callback()
}

fn test_1() {
	println('start')
	x := UInt(4).member
	println('med')
	res := x()
	println('done')
	assert res == 40
}

fn test_plain_primitive_method_value_takes_precedence_over_alias_method() {
	assert call_string_callback(int(7).str) == '7'
}
