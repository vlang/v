fn append_i(mut a []int) {
	a << 1 // ensure single element << works
	a << [2, 3] // .. and array << works too
}

fn append_s(mut a []string) {
	a << 'a'
	a << ['b', 'c']
}

fn append_2d(mut a [][]int) {
	a << [1]
	a << [2, 3, 4]
}

fn test_appending_to_mutable_int_array_args() {
	mut xxx := [10, 20, 30]
	append_i(mut xxx)
	assert xxx == [10, 20, 30, 1, 2, 3]
}

fn test_appending_to_mutable_string_array_args() {
	mut xxx := ['x', 'y', 'z']
	append_s(mut xxx)
	assert xxx == ['x', 'y', 'z', 'a', 'b', 'c']
}

fn test_appending_to_mutable_2d_array_args() {
	mut xxx := [][]int{}
	xxx << [1, 2, 3]
	xxx << [3, 4, 5]
	append_2d(mut xxx)
	assert xxx == [[1, 2, 3], [3, 4, 5], [1], [2, 3, 4]]
}
