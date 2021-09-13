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

fn test_generic_operator_overload() {
	result := from_array([[1, 2, 3], [4, 5, 6]]) + from_array([[7, 8, 9], [10, 11, 12]])
	println(result)
	assert result.row == 2
	assert result.col == 3
	assert result.data == [[8, 10, 12], [14, 16, 18]]
}
