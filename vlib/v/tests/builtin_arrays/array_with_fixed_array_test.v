fn fixed() [2]int {
	return [9, 9]!
}

fn test_main() {
	arr := [1, 2]!
	mut net := [][2]int{len: 4}
	net[1] = [1, 1]!
	net[0] = net[1]
	net[2] = arr
	net[3] = fixed()
	assert net[0] == [1, 1]!
	assert net[1] == [1, 1]!
	assert net[2] == [1, 2]!
	assert net[3] == [9, 9]!
}
