fn f(u usize) usize
fn g(i isize) isize

fn test_usize() {
	mut u := usize(3)
	u += u32(1)
	assert u == 4
	u = 4
	u++
	assert u == 5
	assert u.str() == '5'
	$if x64 {
		assert '${usize(0x140000000):X}' == '140000000'
	}
}

fn test_isize() {
	mut i := isize(-3)
	i -= int(1)
	assert i == -4
	i = -5
	i += 2
	assert i == -3
	assert i.str() == '-3'
	$if x64 {
		assert '${isize(0x140000000):X}' == '140000000'
	}
}
