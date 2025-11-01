fn test_scoped_defer() {
	mut res := 0

	defer(fn) {
		res++
		assert res == 5
	}
	{
		res++
		defer {
			res++
			assert res == 4
		}
		{
			res += 1
			defer {
				res++
				assert res == 3
			}
		} // <- Block 2 ends. Defer 3 executes. res = 3.
	} // <- Block 1 ends. Defer 2 executes. res = 4.
} // <- 'test_scoped_defer' ends. Defer 1 executes. res = 5.
