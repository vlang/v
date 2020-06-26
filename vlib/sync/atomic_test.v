import sync

struct Counter {
	mut:
	counter u64 = 0
}

// without proper syncronization this would fail
fn test_count_100_milion_should_result_100_million() {
	mut wg := sync.new_waitgroup()
	mut counter := Counter{}

	wg.add(10)

		for i:=0 ; i<10; i++ {
		go count_ten_million(mut &counter, wg)
	}

	wg.wait()

	assert counter.counter == 10000000

}

// This test just to make sure that we have an anti-test to prove it works
fn test_count_100_milion_should_fail_100_million_without_sync() {
	mut wg := sync.new_waitgroup()
	mut counter := Counter{}

	wg.add(10)

		for i:=0 ; i<10; i++ {
		go count_ten_million_without_sync(mut &counter, wg)
	}

	wg.wait()

	assert counter.counter != 10000000

}

// count_ten_million counts the common counter 10 million times in thread-safe way
fn count_ten_million(mut counter &Counter, group &sync.WaitGroup) {
	for i:=0; i < 1000000; i++ {
		sync.add_u64(&counter.counter, 1)
	}
	group.done()
}

// count_ten_million_without_sync counts the common counter 10 million times in none thread-safe way
fn count_ten_million_without_sync(mut counter &Counter, group &sync.WaitGroup) {
	for i:=0; i < 1000000; i++ {
		counter.counter++
	}
	group.done()
}