import atomic2
import sync

struct Counter {
mut:
	counter u64 = 0
}

// without proper syncronization this would fail
fn test_count_100_milion_should_result_100_million() {
	mut wg := sync.new_waitgroup()
	mut counter := &Counter{}
	wg.add(10)
	for i := 0; i < 10; i++ {
		go count_ten_million(mut counter, mut wg)
	}
	wg.wait()
	assert counter.counter == 10000000
}

// This test just to make sure that we have an anti-test to prove it works
fn test_count_100_milion_should_fail_100_million_without_sync() {
	mut wg := sync.new_waitgroup()
	mut counter := &Counter{}
	wg.add(10)
	for i := 0; i < 10; i++ {
		go count_ten_million_without_sync(mut counter, mut wg)
	}
	wg.wait()
	assert counter.counter != 10000000
}

fn test_count_plus_one_u64() {
	mut c := u64(0)
	atomic2.add_u64(&c, 1)
	assert c == 1
}

fn test_count_plus_one_i64() {
	mut c := i64(0)
	atomic2.add_i64(&c, 1)
	assert c == 1
}

fn test_count_plus_greater_than_one_u64() {
	mut c := u64(0)
	atomic2.add_u64(&c, 10)
	assert c == 10
}

fn test_count_plus_greater_than_one_i64() {
	mut c := i64(0)
	atomic2.add_i64(&c, 10)
	assert c == 10
}

fn test_count_minus_one_u64() {
	mut c := u64(1)
	atomic2.sub_u64(&c, 1)
	assert c == 0
}

fn test_count_minus_one_i64() {
	mut c := i64(0)
	atomic2.sub_i64(&c, 1)
	assert c == -1
}

fn test_count_minus_greater_than_one_u64() {
	mut c := u64(10)
	atomic2.sub_u64(&c, 10)
	assert c == 0
}

fn test_count_minus_greater_than_one_i64() {
	mut c := i64(10)
	atomic2.sub_i64(&c, 20)
	assert c == -10
}

// count_ten_million counts the common counter 10 million times in thread-safe way
fn count_ten_million(mut counter Counter, mut group sync.WaitGroup) {
	for i := 0; i < 1000000; i++ {
		atomic2.add_u64(&counter.counter, 1)
	}
	group.done()
}

// count_ten_million_without_sync counts the common counter 10 million times in none thread-safe way
fn count_ten_million_without_sync(mut counter Counter, mut group sync.WaitGroup) {
	for i := 0; i < 1000000; i++ {
		counter.counter++
	}
	group.done()
}
