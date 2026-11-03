module goroutines

struct TlsThreadResult {
	initial voidptr
	current voidptr
	cleared voidptr
}

fn tls_machine_worker(mp &Machine, ready chan bool, resume chan bool) TlsThreadResult {
	initial := voidptr(get_current_m())
	set_current_m(mp)
	ready <- true
	_ := <-resume
	current := voidptr(get_current_m())
	set_current_m(unsafe { nil })
	return TlsThreadResult{
		initial: initial
		current: current
		cleared: voidptr(get_current_m())
	}
}

fn test_windows_current_machine_is_thread_local() {
	main_m := get_current_m()
	first_m := &Machine{ id: 1 }
	second_m := &Machine{ id: 2 }
	ready := chan bool{}
	resume_first := chan bool{}
	resume_second := chan bool{}
	first := spawn tls_machine_worker(first_m, ready, resume_first)
	_ := <-ready
	after_first := get_current_m()
	second := spawn tls_machine_worker(second_m, ready, resume_second)
	_ := <-ready
	after_second := get_current_m()
	resume_first <- true
	first_result := first.wait()
	after_first_exit := get_current_m()
	resume_second <- true
	second_result := second.wait()
	after_second_exit := get_current_m()

	assert main_m != unsafe { nil }
	assert after_first == main_m
	assert after_second == main_m
	assert after_first_exit == main_m
	assert after_second_exit == main_m
	assert first_result.initial == unsafe { nil }
	assert first_result.current == voidptr(first_m)
	assert first_result.cleared == unsafe { nil }
	assert second_result.initial == unsafe { nil }
	assert second_result.current == voidptr(second_m)
	assert second_result.cleared == unsafe { nil }
}

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
