fn f(u usize) usize
fn g(i isize) isize

fn test_size() {
	mut u := usize(3)
	u += u32(1)
	assert u == 4
	u = 4
	u++
	assert u == 5
	assert u.str() == '5'

	mut i := isize(-3)
	i -= int(1)
	assert i == -4
	i = -4
	i += 2
	assert i == -2
	assert i.str() == '-2'
}
