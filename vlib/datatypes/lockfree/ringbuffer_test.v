// vtest retry: 2
import time
import sync
import datatypes.lockfree

fn testsuite_begin() {
	spawn fn () {
		println('watchdog thread started...')
		// Ensure that the whole program will finish,
		// even in the case of a deadlock (which seems to happen on windows)
		time.sleep(10_000 * time.millisecond)
		exit(2)
	}()
}

// Test basic push and pop operations
fn test_push_and_pop() {
	mut r := lockfree.new_ringbuffer[int](2)

	r.push(3)
	r.push(4)

	mut oldest_value := r.pop()

	assert oldest_value == 3

	r.push(5)

	oldest_value = r.pop()

	assert oldest_value == 4
}

// Test clear functionality and empty state
fn test_clear_and_empty() {
	mut r := lockfree.new_ringbuffer[int](4)
	r.push(3)
	r.push(4)

	oldest_value := r.pop()
	assert oldest_value == 3

	r.clear()

	assert r.is_empty() == true
}

// Test capacity tracking and full state detection
fn test_capacity_and_is_full() {
	mut r := lockfree.new_ringbuffer[int](4)

	assert r.capacity() == 4

	r.push(3)
	r.push(4)
	r.push(5)
	r.push(6)

	assert r.is_full() == true
}

// Test occupied slots vs remaining capacity
fn test_occupied_and_remaining() {
	mut r := lockfree.new_ringbuffer[int](4)

	r.push(3)
	r.push(4)

	assert r.occupied() == r.remaining()
}

// Test batch push/pop operations
fn test_push_and_pop_many() {
	mut r := lockfree.new_ringbuffer[int](4)
	a := [1, 2, 3, 4]
	r.push_many(a)

	assert r.is_full() == true

	mut b := []int{len: 4}
	r.pop_many(mut b)

	assert a == b
}

// Test single producer single consumer mode
fn test_spsc_mode() {
	println('===== Testing SPSC Mode =====')
	mut rb := lockfree.new_ringbuffer[int](1024, mode: .spsc)

	// Basic push/pop functionality
	assert rb.try_push(42) == true
	assert rb.try_push(100) == true
	assert rb.occupied() == 2

	item1 := rb.try_pop() or { panic('Expected value') }
	assert item1 == 42
	item2 := rb.try_pop() or { panic('Expected value') }
	assert item2 == 100
	assert rb.is_empty() == true

	// Boundary capacity testing
	for i in 0 .. 1024 {
		assert rb.try_push(i) == true
	}
	assert rb.is_full() == true
	assert rb.try_push(1024) == false

	for i in 0 .. 1024 {
		item := rb.try_pop() or { panic('Expected value') }
		assert item == i
	}
	assert rb.is_empty() == true

	println('SPSC basic tests passed')

	// Performance measurement
	start := time.now()
	mut producer := spawn fn (mut rb lockfree.RingBuffer[int]) {
		for i in 0 .. 100000 {
			rb.push(i)
		}
	}(mut rb)

	mut consumer := spawn fn (mut rb lockfree.RingBuffer[int]) {
		for i in 0 .. 100000 {
			item := rb.pop()
			assert item == i
		}
	}(mut rb)

	producer.wait()
	consumer.wait()
	duration := time.since(start)
	println('SPSC performance: ${duration} for 100k items')
}

fn push_ring_values(mut rb lockfree.RingBuffer[int], start int, count int) {
	for i in 0 .. count {
		rb.push(start + i)
	}
}

fn pop_ring_values(mut rb lockfree.RingBuffer[int], count int) []int {
	mut values := []int{len: count}
	for i in 0 .. count {
		values[i] = rb.pop()
	}
	return values
}

fn assert_ring_values(chunks [][]int, total int) {
	// With exactly `total` items, uniqueness and range prove full coverage.
	mut seen := []bool{len: total}
	mut count := 0
	for chunk in chunks {
		count += chunk.len
		for value in chunk {
			assert value >= 0 && value < total && !seen[value], 'unexpected or duplicate item ${value}'
			seen[value] = true
		}
	}
	assert count == total
}

// Test single producer multiple consumers mode
fn test_spmc_mode() {
	println('===== Testing SPMC Mode =====')
	mut rb := lockfree.new_ringbuffer[int](1024, mode: .spmc)
	consumers := 4
	items_per_consumer := 25000
	total_items := consumers * items_per_consumer

	producer := spawn push_ring_values(mut rb, 0, total_items)
	mut readers := []thread []int{cap: consumers}
	for _ in 0 .. consumers {
		readers << spawn pop_ring_values(mut rb, items_per_consumer)
	}
	producer.wait()
	assert_ring_values(readers.wait(), total_items)
	println('SPMC test passed with ${consumers} consumers')
}

// Test multiple producers single consumer mode
fn test_mpsc_mode() {
	println('===== Testing MPSC Mode =====')
	mut rb := lockfree.new_ringbuffer[int](1024, mode: .mpsc)
	producers := 4
	items_per_producer := 25000
	total_items := producers * items_per_producer

	consumer := spawn pop_ring_values(mut rb, total_items)
	mut writers := []thread{cap: producers}
	for i in 0 .. producers {
		writers << spawn push_ring_values(mut rb, i * items_per_producer, items_per_producer)
	}
	writers.wait()
	assert_ring_values([consumer.wait()], total_items)
	println('MPSC test passed with ${producers} producers')
}

// Test multiple producers multiple consumers mode
fn test_mpmc_mode() {
	println('===== Testing MPMC Mode =====')
	mut rb := lockfree.new_ringbuffer[int](1024, mode: .mpmc)
	producers := 4
	consumers := 4
	items_per_producer := 10000
	total_items := producers * items_per_producer

	mut writers := []thread{cap: producers}
	for i in 0 .. producers {
		writers << spawn push_ring_values(mut rb, i * items_per_producer, items_per_producer)
	}
	mut readers := []thread []int{cap: consumers}
	for _ in 0 .. consumers {
		readers << spawn pop_ring_values(mut rb, items_per_producer)
	}
	writers.wait()
	assert_ring_values(readers.wait(), total_items)
	println('MPMC test passed with ${producers} producers and ${consumers} consumers')
}

// Test buffer clear functionality
fn test_clear_function() {
	println('===== Testing Clear Function =====')
	mut rb := lockfree.new_ringbuffer[int](1024, mode: .mpmc)

	// Fill buffer partially
	for i in 0 .. 512 {
		rb.push(i)
	}
	assert rb.occupied() == 512

	// Clear buffer verification
	assert rb.clear() == true
	assert rb.is_empty() == true
	assert rb.try_pop() == none

	// Concurrent clear test
	mut wg := sync.new_waitgroup()
	producers := 4
	items_per_producer := 1000

	// Producer threads
	for i in 0 .. producers {
		wg.add(1)
		spawn fn (mut rb lockfree.RingBuffer[int], id int, count int, mut wg sync.WaitGroup) {
			for j in 0 .. count {
				rb.push(id * 1000 + j)
			}
			wg.done()
		}(mut rb, i, items_per_producer, mut wg)
	}

	// Clear thread
	spawn fn (mut rb lockfree.RingBuffer[int]) {
		time.sleep(1 * time.millisecond) // Allow producers to start
		for i in 0 .. 5 {
			if rb.clear() {
				println('Clear successful ${i}')
				time.sleep(2 * time.millisecond)
			} else {
				println('Clear failed ${i}')
			}
		}
	}(mut rb)

	wg.wait()
	println('Clear function test passed')
}

// Test edge case scenarios
fn test_edge_cases() {
	println('===== Testing Edge Cases =====')
	mut rb := lockfree.new_ringbuffer[int](4, mode: .spsc)

	// Empty buffer tests
	assert rb.is_empty() == true
	assert rb.try_pop() == none
	assert rb.remaining() == 4

	// Full buffer tests
	assert rb.try_push(1) == true
	assert rb.try_push(2) == true
	assert rb.try_push(3) == true
	assert rb.try_push(4) == true
	assert rb.is_full() == true
	assert rb.try_push(5) == false
	assert rb.remaining() == 0

	// Pop then push again
	item := rb.try_pop() or { panic('Expected value') }
	assert item == 1
	assert rb.try_push(5) == true
	assert rb.is_full() == true

	// Clear and reuse
	assert rb.clear() == true
	assert rb.is_empty() == true
	assert rb.try_push(10) == true
	assert rb.try_pop() or { panic('Expected value') } == 10

	println('Edge cases test passed')
}

// Test batch operations functionality
fn test_batch_operations() {
	println('===== Testing Batch Operations =====')
	mut rb := lockfree.new_ringbuffer[int](1024, mode: .mpmc)

	// Batch push operation
	items := []int{len: 100, init: index}
	pushed := rb.try_push_many(items)
	assert pushed == 100
	assert rb.occupied() == 100

	// Batch pop operation
	mut result := []int{len: 100}
	popped := rb.try_pop_many(mut result)
	assert popped == 100
	for i in 0 .. 100 {
		assert result[i] == i
	}
	assert rb.is_empty() == true

	println('Batch operations test passed')
}
