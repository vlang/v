type MySum = string | voidptr

fn t(a MySum) MySum {
	return dump(a)
}

fn test_main() {
	a := 'foo'
	assert t(a) == MySum('foo')

	b := voidptr(0x100)
	assert t(b) == MySum(voidptr(0x100))
}
