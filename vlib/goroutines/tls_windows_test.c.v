module goroutines

fn test_windows_native_atomic_fetch_add_returns_previous_values() {
	mut value32 := u32(0xffff_fffe)
	assert C.goroutines_atomic_fetch_add_u32(&value32, 1) == u32(0xffff_fffe)
	assert value32 == u32(0xffff_ffff)
	assert C.goroutines_atomic_fetch_add_u32(&value32, 1) == u32(0xffff_ffff)
	assert value32 == 0

	mut signed := i32(-10)
	assert C.goroutines_atomic_fetch_add_i32(&signed, 3) == -10
	assert signed == -7
	assert C.goroutines_atomic_fetch_sub_i32(&signed, -4) == -7
	assert signed == -3

	mut value64 := u64(0xffff_ffff_ffff_fffe)
	assert C.goroutines_atomic_fetch_add_u64(&value64, 1) == u64(0xffff_ffff_ffff_fffe)
	assert value64 == u64(0xffff_ffff_ffff_ffff)
	assert C.goroutines_atomic_fetch_add_u64(&value64, 1) == u64(0xffff_ffff_ffff_ffff)
	assert value64 == 0
}

fn test_windows_owned_fiber_conversion_can_be_repeated() {
	// This file starts no scheduler workers, and the main thread is not a fiber.
	for _ in 0 .. 2 {
		owned := convert_thread_to_fiber()
		assert owned.is_thread_fiber
		borrowed := convert_thread_to_fiber()
		assert !borrowed.is_thread_fiber
		assert borrowed.uctx == owned.uctx
		convert_fiber_to_thread(borrowed)
		assert current_fiber() == owned.uctx
		convert_fiber_to_thread(owned)
	}
}
