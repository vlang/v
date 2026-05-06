const fixed_array_new_syntax_rows = 2
const fixed_array_new_syntax_cols = 2

fn test_fixed_array_new_syntax_1d() {
	arr1 := [4]f32[1, 2, 3, 4]
	arr2 := [..]f32[1, 2, 3, 4]
	assert arr1 == [f32(1), 2, 3, 4]!
	assert arr2 == arr1
}

fn test_fixed_array_new_syntax_2d() {
	arr1 := [2][2]int[
		[1, 2],
		[3, 4],
	]
	arr2 := [..][..]int[
		[1, 2],
		[3, 4],
	]
	assert arr1[0][0] == 1
	assert arr1[1][1] == 4
	assert arr2 == arr1
}

fn test_fixed_array_new_syntax_with_consts_and_idents() {
	row1 := [..]int[1, 2]
	row2 := [..]int[3, 4]
	arr := [fixed_array_new_syntax_rows][fixed_array_new_syntax_cols]int[
		row1,
		row2,
	]
	assert arr[0] == row1
	assert arr[1] == row2
}
