fn f(u usize) usize
fn g(i isize) isize

// TODO refactor once `str` method is implemented

fn test_usize() {
	mut u := usize(3)
	u += u32(1)
	// assert u == 4
	if u != 4 {
		assert false
	}
	u = 4
	u++
	// assert u == 5
	if u != 5 {
		assert false
	}
	// assert u.str() == '5'
}

fn test_isize() {
	mut i := isize(-3)
	i -= int(1)
	// assert i == -4
	if i != -4 {
		assert false
	}
	i = -5
	i += 2
	// assert i == -3
	if i != -3 {
		assert false
	}
	// assert i.str() == '-2'
}
