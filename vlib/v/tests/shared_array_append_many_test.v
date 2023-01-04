fn test_shared_array_append_many() {
	shared a := []int{}
	lock a {
		a << [1, 2, 3]
	}
}
