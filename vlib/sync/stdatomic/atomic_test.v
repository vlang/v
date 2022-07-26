import sync
import sync.stdatomic

const iterations_per_cycle = 100_000

struct Counter {
mut:
	counter u64
}

// without proper syncronization this would fail
fn test_count_10_times_1_cycle_should_result_10_cycles_with_sync() {
	desired_iterations := 10 * iterations_per_cycle
	mut wg := sync.new_waitgroup()
	mut counter := &Counter{}
	wg.add(10)
	for i := 0; i < 10; i++ {
		go count_one_cycle(mut counter, mut wg)
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
		go count_one_cycle_without_sync(mut counter, mut wg)
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
