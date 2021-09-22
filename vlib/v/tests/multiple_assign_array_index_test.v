fn round(a int, b int, c int) (int, int, int) {
	mut ax := a
	mut bx := b
	mut cx := c

	ax += bx
	bx ^= cx
	cx -= 1

	return ax, bx, cx
}

fn round_assignment(mut res []int) {
	_ = res[2]
	res[0], res[1], res[2] = round(res[0], res[1], res[2])
}

fn test_multiple_assign_array_index() {
	mut a := []int{len: 3}
	round_assignment(mut a)

	println(a)
	assert a == [0, 0, -1]
}
