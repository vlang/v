type UInt = u32

fn (me UInt) member() u32 {
	println('member called')
	return me * 10
}

fn test_1() {
	println('start')
	x := UInt(4).member
	println('med')
	res := x()
	println('done')
	assert res == 40
}
