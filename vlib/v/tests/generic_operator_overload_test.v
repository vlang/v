struct Matrix<T> {
	row int
	col int
mut:
	data [][]T
}

fn from_array<T>(arr [][]T) Matrix<T> {
	return Matrix<T>{
		row: arr.len
		col: arr[0].len
		data: arr.clone()
	}
}

fn (m1 Matrix<T>) + (m2 Matrix<T>) Matrix<T> {
	if m1.row != m2.row || m1.col != m2.col {
		panic('Addition can only be performed on matrix with same size')
	}
	mut res := m1
	for i in 0 .. m2.row {
		for j in 0 .. m2.col {
			res.data[i][j] += m2.data[i][j]
		}
	}
	return res
}

fn (m1 Matrix<T>) == (m2 Matrix<T>) bool {
	return m1.row == m2.row && m1.col == m2.col && m1.data == m2.data
}

fn (m1 Matrix<T>) < (m2 Matrix<T>) bool {
	return m1.row < m2.row && m1.col < m2.col
}

fn test_generic_operator_overload() {
	mut a1 := from_array([[1, 2, 3], [4, 5, 6]])
	a2 := from_array([[7, 8, 9], [10, 11, 12]])

	plus_ret := a1 + a2
	println(plus_ret)
	assert plus_ret.row == 2
	assert plus_ret.col == 3
	assert plus_ret.data == [[8, 10, 12], [14, 16, 18]]

	a1 += a2
	println(a1)
	assert a1.row == 2
	assert a1.col == 3
	assert a1.data == [[15, 18, 21], [24, 27, 30]]

	eq_ret := a1 == a2
	println(eq_ret)
	assert !eq_ret

	ne_ret := a1 != a2
	println(ne_ret)
	assert ne_ret

	lt_ret := a1 < a2
	println(lt_ret)
	assert !lt_ret

	le_ret := a1 <= a2
	println(le_ret)
	assert le_ret

	gt_ret := a1 > a2
	println(gt_ret)
	assert !gt_ret

	ge_ret := a1 >= a2
	println(ge_ret)
	assert ge_ret
}
