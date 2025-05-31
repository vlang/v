import sync
import sync.stdatomic

const iterations_per_cycle = 100_000

struct Counter {
mut:
	counter u64
}

// without proper synchronization this would fail
fn test_count_10_times_1_cycle_should_result_10_cycles_with_sync() {
	desired_iterations := 10 * iterations_per_cycle
	mut wg := sync.new_waitgroup()
	mut counter := &Counter{}
	wg.add(10)
	for i := 0; i < 10; i++ {
		spawn count_one_cycle(mut counter, mut wg)
	}
	wg.wait()
	assert counter.counter == u64(desired_iterations)
	eprintln('   with synchronization the counter is: ${counter.counter:10} , expectedly == ${desired_iterations:10}')
}

// This test just to make sure that we have an anti-test to prove it works
fn test_count_10_times_1_cycle_should_not_be_10_cycles_without_sync() {
	desired_iterations := 10 * iterations_per_cycle
	mut wg := sync.new_waitgroup()
	mut counter := &Counter{}
	wg.add(10)
	for i := 0; i < 10; i++ {
		spawn count_one_cycle_without_sync(mut counter, mut wg)
	}
	wg.wait()
	// Note: we do not assert here, just print, because sometimes by chance counter.counter may be == desired_iterations
	eprintln('without synchronization the counter is: ${counter.counter:10} , expectedly != ${desired_iterations:10}')
}

fn test_atomic_count_plus_one_u64() {
	mut c := u64(0)
	assert stdatomic.add_u64(&c, 1) == 1
}

fn test_atomic_count_plus_one_i64() {
	mut c := i64(0)
	assert stdatomic.add_i64(&c, 1) == 1
}

fn test_atomic_count_plus_greater_than_one_u64() {
	mut c := u64(0)
	assert stdatomic.add_u64(&c, 10) == 10
}

fn test_atomic_count_plus_greater_than_one_i64() {
	mut c := i64(0)
	assert stdatomic.add_i64(&c, 10) == 10
}

fn test_atomic_count_minus_one_u64() {
	mut c := u64(1)
	assert stdatomic.sub_u64(&c, 1) == 0
}

fn test_atomic_count_minus_one_i64() {
	mut c := i64(0)
	assert stdatomic.sub_i64(&c, 1) == -1
}

fn test_atomic_count_minus_greater_than_one_u64() {
	mut c := u64(0)
	stdatomic.store_u64(&c, 10)
	assert stdatomic.sub_u64(&c, 10) == 0
}

fn test_atomic_count_minus_greater_than_one_i64() {
	mut c := i64(0)
	stdatomic.store_i64(&c, 10)
	assert stdatomic.sub_i64(&c, 20) == -10
}

// count_one_cycle counts the common counter iterations_per_cycle times in thread-safe way
fn count_one_cycle(mut counter Counter, mut group sync.WaitGroup) {
	for i := 0; i < iterations_per_cycle; i++ {
		stdatomic.add_u64(&counter.counter, 1)
	}
	group.done()
}

// count_one_cycle_without_sync counts the common counter iterations_per_cycle times in none thread-safe way
fn count_one_cycle_without_sync(mut counter Counter, mut group sync.WaitGroup) {
	for i := 0; i < iterations_per_cycle; i++ {
		counter.counter++
	}
	group.done()
}

fn test_atomic_vals() {
	mut v_bool := stdatomic.new_atomic(false)
	v_bool.store(true)
	assert v_bool.load() == true
	v_bool.store(false)
	assert v_bool.load() == false

	mut v_i8 := stdatomic.new_atomic(i8(-33))
	v_i8.store(-34)
	assert v_i8.load() == -34
	v_i8.add(10)
	assert v_i8.load() == -24
	v_i8.sub(7)
	assert v_i8.load() == -31

	mut v_u8 := stdatomic.new_atomic(u8(33))
	v_u8.store(34)
	assert v_u8.load() == 34
	v_u8.add(10)
	assert v_u8.load() == 44
	v_u8.sub(7)
	assert v_u8.load() == 37

	mut v_i16 := stdatomic.new_atomic(i16(-333))
	v_i16.store(-334)
	assert v_i16.load() == -334
	v_i16.add(10)
	assert v_i16.load() == -324
	v_i16.sub(7)
	assert v_i16.load() == -331

	mut v_u16 := stdatomic.new_atomic(u16(333))
	v_u16.store(334)
	assert v_u16.load() == 334
	v_u16.add(10)
	assert v_u16.load() == 344
	v_u16.sub(7)
	assert v_u16.load() == 337

	mut v_i32 := stdatomic.new_atomic(i32(-3333))
	v_i32.store(-3334)
	assert v_i32.load() == -3334
	v_i32.add(10)
	assert v_i32.load() == -3324
	v_i32.sub(7)
	assert v_i32.load() == -3331

	mut v_u32 := stdatomic.new_atomic(u32(3333))
	v_u32.store(3334)
	assert v_u32.load() == 3334
	v_u32.add(10)
	assert v_u32.load() == 3344
	v_u32.sub(7)
	assert v_u32.load() == 3337

	mut v_i64 := stdatomic.new_atomic(i64(-33333))
	v_i64.store(-33334)
	assert v_i64.load() == -33334
	v_i64.add(10)
	assert v_i64.load() == -33324
	v_i64.sub(7)
	assert v_i64.load() == -33331

	mut v_u64 := stdatomic.new_atomic(u64(33333))
	v_u64.store(33334)
	assert v_u64.load() == 33334
	v_u64.add(10)
	assert v_u64.load() == 33344
	v_u64.sub(7)
	assert v_u64.load() == 33337

	mut v_int := stdatomic.new_atomic(int(-44))
	v_int.store(-45)
	assert v_int.load() == -45
	v_int.add(10)
	assert v_int.load() == -35
	v_int.sub(7)
	assert v_int.load() == -42

	mut v_isize := stdatomic.new_atomic(isize(-55))
	v_isize.store(-56)
	assert v_isize.load() == -56
	v_isize.add(10)
	assert v_isize.load() == -46
	v_isize.sub(7)
	assert v_isize.load() == -53

	mut v_usize := stdatomic.new_atomic(usize(55))
	v_usize.store(56)
	assert v_usize.load() == 56
	v_usize.add(10)
	assert v_usize.load() == 66
	v_usize.sub(7)
	assert v_usize.load() == 59
}
