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
	assert v_bool.swap(true) == false
	assert v_bool.swap(false) == true
	assert v_bool.compare_and_swap(false, true) == true
	assert v_bool.load() == true

	mut v_i8 := stdatomic.new_atomic(i8(-33))
	v_i8.store(-34)
	assert v_i8.load() == -34
	assert v_i8.add(10) == -34
	assert v_i8.load() == -24
	assert v_i8.sub(7) == -24
	assert v_i8.load() == -31
	mut new_i8 := i8(-20)
	assert v_i8.swap(new_i8) == -31
	assert v_i8.swap(-31) == new_i8
	assert v_i8.compare_and_swap(-31, new_i8) == true
	assert v_i8.compare_and_swap(new_i8, -32) == true
	assert v_i8.load() == -32

	mut v_u8 := stdatomic.new_atomic(u8(33))
	v_u8.store(34)
	assert v_u8.load() == 34
	assert v_u8.add(10) == 34
	assert v_u8.load() == 44
	assert v_u8.sub(7) == 44
	assert v_u8.load() == 37
	mut new_u8 := u8(20)
	assert v_u8.swap(new_u8) == 37
	assert v_u8.swap(37) == new_u8
	assert v_u8.compare_and_swap(37, new_u8) == true
	assert v_u8.compare_and_swap(new_u8, 38) == true
	assert v_u8.load() == 38

	mut v_i16 := stdatomic.new_atomic(i16(-333))
	v_i16.store(-334)
	assert v_i16.load() == -334
	assert v_i16.add(10) == -334
	assert v_i16.load() == -324
	assert v_i16.sub(7) == -324
	assert v_i16.load() == -331
	mut new_i16 := i16(-200)
	assert v_i16.swap(new_i16) == -331
	assert v_i16.swap(-331) == new_i16
	assert v_i16.compare_and_swap(-331, new_i16) == true
	assert v_i16.compare_and_swap(new_i16, -332) == true
	assert v_i16.load() == -332

	mut v_u16 := stdatomic.new_atomic(u16(333))
	v_u16.store(334)
	assert v_u16.load() == 334
	assert v_u16.add(10) == 334
	assert v_u16.load() == 344
	assert v_u16.sub(7) == 344
	assert v_u16.load() == 337
	mut new_u16 := u16(200)
	assert v_u16.swap(new_u16) == 337
	assert v_u16.swap(337) == new_u16
	assert v_u16.compare_and_swap(337, new_u16) == true
	assert v_u16.compare_and_swap(new_u16, 332) == true
	assert v_u16.load() == 332

	mut v_i32 := stdatomic.new_atomic(i32(-3333))
	v_i32.store(-3334)
	assert v_i32.load() == -3334
	assert v_i32.add(10) == -3334
	assert v_i32.load() == -3324
	assert v_i32.sub(7) == -3324
	assert v_i32.load() == -3331
	mut new_i32 := i32(-2000)
	assert v_i32.swap(new_i32) == -3331
	assert v_i32.swap(-3331) == new_i32
	assert v_i32.compare_and_swap(-3331, new_i32) == true
	assert v_i32.compare_and_swap(new_i32, -3332) == true
	assert v_i32.load() == -3332

	mut v_u32 := stdatomic.new_atomic(u32(3333))
	v_u32.store(3334)
	assert v_u32.load() == 3334
	assert v_u32.add(10) == 3334
	assert v_u32.load() == 3344
	assert v_u32.sub(7) == 3344
	assert v_u32.load() == 3337
	mut new_u32 := u32(2000)
	assert v_u32.swap(new_u32) == 3337
	assert v_u32.swap(3337) == new_u32
	assert v_u32.compare_and_swap(3337, new_u32) == true
	assert v_u32.compare_and_swap(new_u32, 3338) == true
	assert v_u32.load() == 3338

	mut v_i64 := stdatomic.new_atomic(i64(-33333))
	v_i64.store(-33334)
	assert v_i64.load() == -33334
	assert v_i64.add(10) == -33334
	assert v_i64.load() == -33324
	assert v_i64.sub(7) == -33324
	assert v_i64.load() == -33331
	mut new_i64 := i64(-20000)
	assert v_i64.swap(new_i64) == -33331
	assert v_i64.swap(-33331) == new_i64
	assert v_i64.compare_and_swap(-33331, new_i64) == true
	assert v_i64.compare_and_swap(new_i64, -33332) == true
	assert v_i64.load() == -33332

	mut v_u64 := stdatomic.new_atomic(u64(33333))
	v_u64.store(33334)
	assert v_u64.load() == 33334
	assert v_u64.add(10) == 33334
	assert v_u64.load() == 33344
	assert v_u64.sub(7) == 33344
	assert v_u64.load() == 33337
	mut new_u64 := u64(20000)
	assert v_u64.swap(new_u64) == 33337
	assert v_u64.swap(33337) == new_u64
	assert v_u64.compare_and_swap(33337, new_u64) == true
	assert v_u64.compare_and_swap(new_u64, 33338) == true
	assert v_u64.load() == 33338

	mut v_int := stdatomic.new_atomic(int(-44))
	v_int.store(-45)
	assert v_int.load() == -45
	assert v_int.add(10) == -45
	assert v_int.load() == -35
	assert v_int.sub(7) == -35
	assert v_int.load() == -42
	mut new_int := int(-40000)
	assert v_int.swap(new_int) == -42
	assert v_int.swap(-42) == new_int
	assert v_int.compare_and_swap(-42, new_int) == true
	assert v_int.compare_and_swap(new_int, -40001) == true
	assert v_int.load() == -40001

	mut v_isize := stdatomic.new_atomic(isize(-55))
	v_isize.store(-56)
	assert v_isize.load() == -56
	assert v_isize.add(10) == -56
	assert v_isize.load() == -46
	assert v_isize.sub(7) == -46
	assert v_isize.load() == -53
	mut new_isize := isize(-50000)
	assert v_isize.swap(new_isize) == -53
	assert v_isize.swap(-53) == new_isize
	assert v_isize.compare_and_swap(-53, new_isize) == true
	assert v_isize.compare_and_swap(new_isize, -50001) == true
	assert v_isize.load() == -50001

	mut v_usize := stdatomic.new_atomic(usize(55))
	v_usize.store(56)
	assert v_usize.load() == 56
	assert v_usize.add(10) == 56
	assert v_usize.load() == 66
	assert v_usize.sub(7) == 66
	assert v_usize.load() == 59
	mut new_usize := usize(50000)
	assert v_usize.swap(new_usize) == 59
	assert v_usize.swap(59) == new_usize
	assert v_usize.compare_and_swap(59, new_usize) == true
	assert v_usize.compare_and_swap(new_usize, 60) == true
	assert v_usize.load() == 60

	mut val_1 := int(100)
	mut ptr_1 := voidptr(&val_1)
	mut val_2 := int(200)
	mut ptr_2 := voidptr(&val_2)
	mut v_voidptr := stdatomic.new_atomic(ptr_1)
	assert v_voidptr.load() == ptr_1
	v_voidptr.store(ptr_2)
	assert v_voidptr.load() == ptr_2
	assert v_voidptr.swap(ptr_1) == ptr_2
	assert v_voidptr.swap(ptr_2) == ptr_1
	assert v_voidptr.compare_and_swap(ptr_2, ptr_1) == true
	assert v_voidptr.load() == ptr_1

	// just for compile
	C.atomic_thread_fence(C.memory_order_relaxed)
	C.cpu_relax()
}
