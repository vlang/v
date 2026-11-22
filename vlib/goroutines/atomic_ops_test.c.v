// vtest build: (linux || macos) && !musl? && !self_ubuntu_musl_ci? // needs ucontext
module goroutines

fn test_native_atomic_helpers_use_the_supplied_address() {
	mut value := u32(10)
	C.goroutines_atomic_store_u32(&value, 42)
	assert C.goroutines_atomic_load_u32(&value) == 42
	assert C.goroutines_atomic_fetch_add_u32(&value, 3) == 42
	mut expected := u32(45)
	assert C.goroutines_atomic_cas_u32(&value, &expected, 90)
	expected = 12
	assert !C.goroutines_atomic_cas_u32(&value, &expected, 99)
	assert expected == 90
	assert value == 90

	mut signed := i32(-10)
	assert C.goroutines_atomic_fetch_add_i32(&signed, 3) == -10
	assert C.goroutines_atomic_fetch_sub_i32(&signed, 5) == -7
	assert signed == -12
	mut wide := u64(1) << 40
	assert C.goroutines_atomic_fetch_add_u64(&wide, 7) == u64(1) << 40
	assert wide == (u64(1) << 40) + 7

	first := 1
	second := 2
	mut slot := voidptr(&first)
	assert C.goroutines_atomic_load_ptr(&slot) == voidptr(&first)
	C.goroutines_atomic_store_ptr(&slot, voidptr(&second))
	assert C.goroutines_atomic_exchange_ptr(&slot, voidptr(&first)) == voidptr(&second)
	assert C.goroutines_atomic_take_ptr(&slot, voidptr(&first))
	assert C.goroutines_atomic_load_ptr(&slot) == unsafe { nil }
	assert !C.goroutines_atomic_take_ptr(&slot, voidptr(&second))
}
