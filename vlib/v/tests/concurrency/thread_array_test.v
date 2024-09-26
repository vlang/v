fn test_assign_spawn_to_element() {
	mut threads := []thread{len: 1}

	threads[0] = spawn fn () {
		a := 1
		dump(a)
		assert true
	}()

	threads.wait()
}
