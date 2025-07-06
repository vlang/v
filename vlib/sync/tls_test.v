import sync
import time

// Testing basic data types
fn test_basic_tls() {
	println('--- Testing basic datatypes TLS ---')

	// invalid type
	sync.new_tls[string]('hello world') or { assert err.msg().contains('invalid type') }

	mut tls_i8 := sync.new_tls[i8](-3)!
	assert tls_i8.get()! == -3
	tls_i8.set(-4)!
	assert tls_i8.get()! == -4
	tls_i8.destroy()!
	if v := tls_i8.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_i8.set(-5) or { assert err.msg().contains('already destroyed') }

	mut tls_i16 := sync.new_tls[i16](-57)!
	assert tls_i16.get()! == -57
	tls_i16.set(-58)!
	assert tls_i16.get()! == -58
	tls_i16.destroy()!
	if v := tls_i16.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_i16.set(-59) or { assert err.msg().contains('already destroyed') }

	mut tls_i32 := sync.new_tls[i32](-570)!
	assert tls_i32.get()! == -570
	tls_i32.set(-580)!
	assert tls_i32.get()! == -580
	tls_i32.destroy()!
	if v := tls_i32.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_i32.set(-59) or { assert err.msg().contains('already destroyed') }

	mut tls_i64 := sync.new_tls[i64](-5700)!
	assert tls_i64.get()! == -5700
	tls_i64.set(-5800)!
	assert tls_i64.get()! == -5800
	tls_i64.destroy()!
	if v := tls_i64.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_i64.set(-5900) or { assert err.msg().contains('already destroyed') }

	mut tls_u8 := sync.new_tls[u8](3)!
	assert tls_u8.get()! == 3
	tls_u8.set(4)!
	assert tls_u8.get()! == 4
	tls_u8.destroy()!
	if v := tls_u8.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_u8.set(5) or { assert err.msg().contains('already destroyed') }

	mut tls_u16 := sync.new_tls[i16](57)!
	assert tls_u16.get()! == 57
	tls_u16.set(58)!
	assert tls_u16.get()! == 58
	tls_u16.destroy()!
	if v := tls_u16.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_u16.set(59) or { assert err.msg().contains('already destroyed') }

	mut tls_u32 := sync.new_tls[u32](570)!
	assert tls_u32.get()! == 570
	tls_u32.set(580)!
	assert tls_u32.get()! == 580
	tls_u32.destroy()!
	if v := tls_u32.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_u32.set(590) or { assert err.msg().contains('already destroyed') }

	mut tls_u64 := sync.new_tls[u64](5700)!
	assert tls_u64.get()! == 5700
	tls_u64.set(5800)!
	assert tls_u64.get()! == 5800
	tls_u64.destroy()!
	if v := tls_u64.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_u64.set(5900) or { assert err.msg().contains('already destroyed') }

	mut tls_isize := sync.new_tls[isize](-57000)!
	assert tls_isize.get()! == -57000
	tls_isize.set(-58000)!
	assert tls_isize.get()! == -58000
	tls_isize.destroy()!
	if v := tls_isize.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_isize.set(-59000) or { assert err.msg().contains('already destroyed') }

	mut tls_usize := sync.new_tls[usize](57000)!
	assert tls_usize.get()! == 57000
	tls_usize.set(58000)!
	assert tls_usize.get()! == 58000
	tls_usize.destroy()!
	if v := tls_usize.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_usize.set(59000) or { assert err.msg().contains('already destroyed') }

	mut tls_int := sync.new_tls[int](-32767)!
	assert tls_int.get()! == -32767
	tls_int.set(-32768)!
	assert tls_int.get()! == -32768
	tls_int.destroy()!
	if v := tls_int.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_int.set(-32769) or { assert err.msg().contains('already destroyed') }

	mut tls_f32 := sync.new_tls[f32](1.5)!
	assert tls_f32.get()! == 1.5
	tls_f32.set(2.5)!
	assert tls_f32.get()! == 2.5
	tls_f32.destroy()!
	if v := tls_f32.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_f32.set(3.5) or { assert err.msg().contains('already destroyed') }

	mut tls_f64 := sync.new_tls[f64](-1.5)!
	assert tls_f64.get()! == -1.5
	tls_f64.set(-2.5)!
	assert tls_f64.get()! == -2.5
	tls_f64.destroy()!
	if v := tls_f64.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
	tls_f64.set(-3.5) or { assert err.msg().contains('already destroyed') }
}

// Testing pointer type
struct PtrData {
	id  int
	val string
}

fn test_pointer_tls() {
	println('--- Testing pointer TLS ---')

	data := &PtrData{
		id:  100
		val: 'initial'
	}
	mut tls := sync.new_tls[&PtrData](data) or { panic('Failed to create TLS: ${err}') }

	// Verify initial value
	assert tls.get()!.id == 100
	assert tls.get()!.val == 'initial'

	// Update and verify new value
	new_data := &PtrData{
		id:  200
		val: 'updated'
	}
	tls.set(new_data)!
	assert tls.get()!.id == 200
	assert tls.get()!.val == 'updated'

	tls.destroy()!
	// Attempt to set value after destruction
	tls.set(data) or { assert err.msg().contains('already destroyed') }

	// Attempt to get value after destruction
	if res_get := tls.get() {
		assert false
	} else {
		assert err.msg().contains('already destroyed')
	}
}

// Thread worker function
fn thread_worker(tls &sync.ThreadLocalStorage[u64], thread_id u64) {
	mut mut_tls := unsafe { tls }
	// Each thread sets a unique value
	mut_tls.set(thread_id * 100) or { panic(err) }

	// Simulate work with a delay
	time.sleep(10 * time.millisecond)

	// Retrieve and verify thread-private value
	val := mut_tls.get() or { panic(err) }
	assert val == thread_id * 100
	println('Thread ${thread_id}: TLS value = ${val}')
}

// Testing multi-thread isolation
fn test_thread_isolation() {
	println('--- Testing multi-thread isolation ---')

	// Create shared TLS instance
	mut tls := sync.new_tls[u64](3366) or { panic(err) }
	defer { tls.destroy() or { panic(err) } }

	// Create multiple threads
	mut threads := []thread{}
	for i in 1 .. 5 {
		threads << spawn thread_worker(&tls, u64(i))
	}

	// Wait for all threads to complete
	threads.wait()

	// Verify main thread value remains unchanged
	println('Main thread value after operations: ${tls.get()!}')
	assert tls.get()! == 3366
}

// Thread worker function
fn worker(int_tls &sync.ThreadLocalStorage[int],
	f64_tls &sync.ThreadLocalStorage[f64]) {
	mut mut_int_tls := unsafe { int_tls }
	mut mut_f64_tls := unsafe { f64_tls }
	// Update integer value
	mut_int_tls.set(100) or { panic(err) }
	v := mut_int_tls.get() or { panic(err) }
	assert v == 100

	// Update floating-point value
	mut_f64_tls.set(2.5) or { panic(err) }
	s := mut_f64_tls.get() or { panic(err) }
	assert s == 2.5
}

// Testing cross-thread type safety
fn test_cross_thread_type() {
	println('--- Testing cross-thread type safety ---')

	mut int_tls := sync.new_tls[int](42) or { panic(err) }
	defer { int_tls.destroy() or { panic(err) } }

	mut f64_tls := sync.new_tls[f64](1.5) or { panic(err) }
	defer { f64_tls.destroy() or { panic(err) } }

	shared_int_tls := &int_tls
	shared_f64_tls := &f64_tls

	// Create and wait for threads
	threads := [
		spawn worker(shared_int_tls, shared_f64_tls),
		spawn worker(shared_int_tls, shared_f64_tls),
	]
	threads.wait()

	// Verify original values in main thread
	assert int_tls.get()! == 42
	assert f64_tls.get()! == 1.5
}
