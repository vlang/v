type UInt = u32

fn (me UInt) member() {
	println('member called')
}

fn test_1() {
	println('start')
	x := UInt(4).member
	println('med')
	x()
	println('done')
}
