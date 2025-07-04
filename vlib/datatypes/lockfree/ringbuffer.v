module lockfree

// This design is ported from the DPDK rte_ring library.
// Source: https://doc.dpdk.org/guides/prog_guide/ring_lib.html

// RingBufferMode Operation modes for the ring buffer.
pub enum RingBufferMode {
	spsc = 0 // Single Producer, Single Consumer (optimized for single-threaded access)
	spmc = 1 // Single Producer, Multiple Consumers (one writer, multiple readers)
	mpsc = 2 // Multiple Producers, Single Consumer (multiple writers, one reader)
	mpmc = 3 // Multiple Producers, Multiple Consumers (default, fully concurrent)
}

// RingBufferStat holds performance counters for ring buffer operations.
pub struct RingBufferStat {
pub mut:
	push_full_count      u32 // Times producers encountered full buffer
	push_fail_count      u32 // Times producers failed to reserve space
	push_wait_prev_count u32 // Times producers waited for predecessors
	push_waiting_count   u32 // Current number of producers in waiting state
	pop_empty_count      u32 // Times consumers found empty buffer
	pop_fail_count       u32 // Times consumers failed to reserve items
	pop_wait_prev_count  u32 // Times consumers waited for predecessors
	pop_waiting_count    u32 // Current number of consumers in waiting state
}

// RingBufferParam Configuration parameters for ring buffer creation.
// - max_waiting_prod_cons: Setting this to a larger value may improve performance,
//   but in scenarios with many producers/consumers, it could lead to severe contention issues.
@[params]
pub struct RingBufferParam {
pub:
	mode                  RingBufferMode = .mpmc // Default to most concurrent mode
	max_waiting_prod_cons int            = 1     // Max allowed waiting producers/consumers before rejecting operations
}

// RingBuffer Lock-free multiple producer/multiple consumer ring buffer.
// Requires explicit initialization
@[noinit]
pub struct RingBuffer[T] {
mut:
	mode                  u32                      // Current operation mode (from RingBufferMode)
	capacity              u32                      // Total capacity (always power of two)
	mask                  u32                      // Bitmask for index calculation (capacity - 1)
	clear_flag            u32                      // Flag indicating clear operation in progress
	max_waiting_prod_cons u32                      // Max allowed waiting producers/consumers
	pad0                  [cache_line_size - 20]u8 // Padding to align to cache line boundary

	// Producer state (isolated to prevent false sharing)
	prod_head u32                     // Producer head (next write position)
	pad1      [cache_line_size - 4]u8 // Cache line padding
	prod_tail u32                     // Producer tail (last committed position)
	pad2      [cache_line_size - 4]u8 // Cache line padding

	// Consumer state (isolated to prevent false sharing)
	cons_head u32                     // Consumer head (next read position)
	pad3      [cache_line_size - 4]u8 // Cache line padding
	cons_tail u32                     // Consumer tail (last committed position)
	pad4      [cache_line_size - 4]u8 // Cache line padding

	// Data storage area
	slots []T // Array holding actual data elements

	// Performance counters
	push_full_count      u32 // Count of full buffer encounters
	push_fail_count      u32 // Count of failed push attempts
	push_wait_prev_count u32 // Count of waits for previous producers
	push_waiting_count   u32 // Current number of waiting producers
	pop_empty_count      u32 // Count of empty buffer encounters
	pop_fail_count       u32 // Count of failed pop attempts
	pop_wait_prev_count  u32 // Count of waits for previous consumers
	pop_waiting_count    u32 // Current number of waiting consumers
}

// new_ringbuffer creates a new lock-free ring buffer.
// Note: The buffer capacity will be expanded to the next power of two
//       for efficient modulo operations using bitwise AND.
//       The actual capacity may be larger than the requested `size`.
pub fn new_ringbuffer[T](size u32, param RingBufferParam) &RingBuffer[T] {
	// Ensure capacity is power of two for efficient modulo operations
	capacity := next_power_of_two(size)
	mask := capacity - 1

	// Initialize data storage array
	mut slots := []T{len: int(capacity)}

	rb := &RingBuffer[T]{
		mode:                  u32(param.mode)
		max_waiting_prod_cons: u32(param.max_waiting_prod_cons)
		capacity:              capacity
		mask:                  mask
		slots:                 slots
	}

	// Disable Valgrind checking for performance
	$if valgrind ? {
		C.VALGRIND_HG_DISABLE_CHECKING(rb, sizeof(RingBuffer[T]))
	}
	return rb
}

// is_multiple_producer checks if current mode is multiple producer.
@[inline]
fn is_multiple_producer(mode u32) bool {
	return mode & 0x02 != 0
}

// is_multiple_consumer checks if current mode is multiple consumer.
@[inline]
fn is_multiple_consumer(mode u32) bool {
	return mode & 0x01 != 0
}

// try_push tries to push a single item non-blocking.
@[inline]
pub fn (mut rb RingBuffer[T]) try_push(item T) bool {
	return rb.try_push_many([item]) == 1
}

// try_push_many tries to push multiple items non-blocking.
@[direct_array_access]
pub fn (mut rb RingBuffer[T]) try_push_many(items []T) u32 {
	n := u32(items.len)
	if n == 0 {
		return 0
	}

	// Check if clear operation is in progress or too many producers are waiting
	if C.atomic_load_u32(&rb.clear_flag) != 0 || (is_multiple_producer(rb.mode)
		&& C.atomic_load_u32(&rb.push_waiting_count) > rb.max_waiting_prod_cons) {
		return 0
	}

	capacity := rb.capacity
	mut success := false
	mut attempts := 0
	mut old_head := u32(0)
	mut new_head := u32(0)

	// Attempt to reserve space in the buffer
	for !success && attempts < 10 {
		old_head = C.atomic_load_u32(&rb.prod_head)

		// Memory barrier for weak memory models
		$if !x64 && !x32 {
			C.atomic_thread_fence(C.memory_order_acquire)
		}

		// Calculate available space using unsigned arithmetic
		free_entries := capacity + C.atomic_load_u32(&rb.cons_tail) - old_head

		// Check if there's enough space
		if n > free_entries {
			$if debug_ringbuffer ? {
				C.atomic_fetch_add_u32(&rb.push_full_count, 1)
			}
			return 0
		}

		// Calculate new head position after adding items
		new_head = old_head + n
		if is_multiple_producer(rb.mode) {
			// Atomic compare-and-swap for multiple producers
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_BEFORE(&rb.prod_head)
			}
			success = C.atomic_compare_exchange_weak_u32(&rb.prod_head, &old_head, new_head)
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_AFTER(&rb.prod_head)
			}
		} else {
			// Direct update for single producer
			rb.prod_head = new_head
			success = true
		}
		attempts++
	}

	// Exit if space reservation failed
	if !success {
		$if debug_ringbuffer ? {
			C.atomic_fetch_add_u32(&rb.push_fail_count, 1)
		}
		return 0
	}

	// Write data to the reserved slots
	for i in 0 .. n {
		index := (old_head + i) & rb.mask
		$if valgrind ? {
			C.VALGRIND_HG_DISABLE_CHECKING(&rb.slots[index], sizeof(T))
			C.ANNOTATE_HAPPENS_BEFORE(&rb.slots[index])
		}
		rb.slots[index] = items[i]
		$if valgrind ? {
			C.ANNOTATE_HAPPENS_AFTER(&rb.slots[index])
		}
	}

	mut add_once := true
	mut backoff := 1
	if is_multiple_producer(rb.mode) {
		// Increment waiting producer count
		C.atomic_fetch_add_u32(&rb.push_waiting_count, 1)

		mut attempts_wait := 1
		// Wait for previous producers to complete their writes
		for C.atomic_load_u32(&rb.prod_tail) != old_head {
			// Exponential backoff to reduce CPU contention
			for _ in 0 .. backoff {
				C.cpu_relax() // Low-latency pause instruction
			}
			backoff = int_min(backoff * 2, 1024)
			attempts_wait++
			$if debug_ringbuffer ? {
				if attempts_wait > 100 && add_once {
					C.atomic_fetch_add_u32(&rb.push_wait_prev_count, 1)
					add_once = false
				}
			}
		}

		// Decrement waiting producer count
		C.atomic_fetch_sub_u32(&rb.push_waiting_count, 1)
	}

	// Make data visible to consumers
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_BEFORE(&rb.prod_tail)
	}
	C.atomic_store_u32(&rb.prod_tail, new_head)
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_AFTER(&rb.prod_tail)
	}
	return n
}

// try_pop tries to pop a single item non-blocking.
@[inline]
pub fn (mut rb RingBuffer[T]) try_pop() ?T {
	mut items := []T{len: 1}
	if rb.try_pop_many(mut items) == 1 {
		return items[0]
	}
	return none // Buffer empty
}

// try_pop_many tries to pop multiple items non-blocking.
@[direct_array_access]
pub fn (mut rb RingBuffer[T]) try_pop_many(mut items []T) u32 {
	n := u32(items.len)
	if n == 0 {
		return 0
	}

	// Check if clear operation is in progress or too many consumers are waiting
	if C.atomic_load_u32(&rb.clear_flag) != 0 || (is_multiple_consumer(rb.mode)
		&& C.atomic_load_u32(&rb.pop_waiting_count) > rb.max_waiting_prod_cons) {
		return 0
	}

	mut success := false
	mut attempts := 0
	mut old_head := u32(0)
	mut new_head := u32(0)

	// Attempt to reserve data for reading
	for !success && attempts < 10 {
		old_head = C.atomic_load_u32(&rb.cons_head)
		// Memory barrier for weak memory models
		$if !x64 && !x32 {
			C.atomic_thread_fence(C.memory_order_acquire)
		}

		// Calculate available items to read
		entries := C.atomic_load_u32(&rb.prod_tail) - old_head

		// Check if enough data is available
		if n > entries {
			$if debug_ringbuffer ? {
				C.atomic_fetch_add_u32(&rb.pop_empty_count, 1)
			}
			return 0
		}

		// Calculate new head position after reading
		new_head = old_head + n
		if is_multiple_consumer(rb.mode) {
			// Atomic compare-and-swap for multiple consumers
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_BEFORE(&rb.cons_head)
			}
			success = C.atomic_compare_exchange_weak_u32(&rb.cons_head, &old_head, new_head)
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_AFTER(&rb.cons_head)
			}
		} else {
			// Direct update for single consumer
			rb.cons_head = new_head
			success = true
		}
		attempts++
	}

	// Exit if data reservation failed
	if !success {
		C.atomic_fetch_add_u32(&rb.pop_fail_count, 1)
		return 0
	}

	// Read data from reserved slots
	for i in 0 .. n {
		index := (old_head + i) & rb.mask
		$if valgrind ? {
			C.ANNOTATE_HAPPENS_BEFORE(&rb.slots[index])
		}
		items[i] = rb.slots[index]
		$if valgrind ? {
			C.ANNOTATE_HAPPENS_AFTER(&rb.slots[index])
		}
	}

	mut add_once := true
	mut backoff := 1
	// For multiple consumers: wait for previous consumers to complete
	if is_multiple_consumer(rb.mode) {
		// Increment waiting consumer count
		C.atomic_fetch_add_u32(&rb.pop_waiting_count, 1)

		mut attempts_wait := 1
		// Wait for previous consumers to complete their reads
		for C.atomic_load_u32(&rb.cons_tail) != old_head {
			// Exponential backoff to reduce CPU contention
			for _ in 0 .. backoff {
				C.cpu_relax() // Low-latency pause instruction
			}
			backoff = int_min(backoff * 2, 1024)
			attempts_wait++
			$if debug_ringbuffer ? {
				if attempts_wait > 100 && add_once {
					C.atomic_fetch_add_u32(&rb.pop_wait_prev_count, 1)
					add_once = false
				}
			}
		}

		// Decrement waiting consumer count
		C.atomic_fetch_sub_u32(&rb.pop_waiting_count, 1)
	}

	// Free up buffer space
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_BEFORE(&rb.cons_tail)
	}
	C.atomic_store_u32(&rb.cons_tail, new_head)
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_AFTER(&rb.cons_tail)
	}
	return n
}

// push blocking push of a single item.
@[inline]
pub fn (mut rb RingBuffer[T]) push(item T) {
	mut backoff := 1
	// Retry until successful
	for {
		if rb.try_push(item) {
			return
		}
		// Exponential backoff to reduce contention
		for _ in 0 .. backoff {
			C.cpu_relax() // Pause before retry
		}
		backoff = int_min(backoff * 2, 1024)
	}
}

// pop blocking pop of a single item.
@[inline]
pub fn (mut rb RingBuffer[T]) pop() T {
	mut backoff := 1
	// Retry until successful
	for {
		if item := rb.try_pop() {
			return item
		}
		// Exponential backoff to reduce contention
		for _ in 0 .. backoff {
			C.cpu_relax() // Pause before retry
		}
		backoff = int_min(backoff * 2, 1024)
	}
	return T(0) // Default value (should never be reached)
}

// push_many blocking push of multiple items.
@[inline]
pub fn (mut rb RingBuffer[T]) push_many(items []T) {
	mut backoff := 1
	for {
		n := rb.try_push_many(items)
		if n == items.len {
			break
		} else {
			// Exponential backoff when buffer is full
			for _ in 0 .. backoff {
				C.cpu_relax() // Pause when buffer is full
			}
			backoff = int_min(backoff * 2, 1024)
		}
	}
}

// pop_many blocking pop of multiple items.
@[inline]
pub fn (mut rb RingBuffer[T]) pop_many(mut result []T) {
	n := result.len
	if n == 0 {
		return
	}
	mut backoff := 1
	for {
		ret := rb.try_pop_many(mut result)
		if ret == n {
			break
		} else {
			// Exponential backoff when buffer is empty
			for _ in 0 .. backoff {
				C.cpu_relax() // Pause when buffer is empty
			}
			backoff = int_min(backoff * 2, 1024)
		}
	}
}

// is_empty checks if the buffer is empty.
@[inline]
pub fn (rb RingBuffer[T]) is_empty() bool {
	return rb.occupied() == 0
}

// is_full checks if the buffer is full.
@[inline]
pub fn (rb RingBuffer[T]) is_full() bool {
	return rb.occupied() >= rb.capacity
}

// capacity returns the total capacity of the buffer.
@[inline]
pub fn (rb RingBuffer[T]) capacity() u32 {
	return rb.capacity
}

// occupied returns the number of occupied slots.
@[inline]
pub fn (rb RingBuffer[T]) occupied() u32 {
	// Memory barrier for weak memory models
	$if !x64 && !x32 {
		C.atomic_thread_fence(C.memory_order_acquire)
	}

	prod_tail := C.atomic_load_u32(&rb.prod_tail)
	cons_tail := C.atomic_load_u32(&rb.cons_tail)

	// Handle potential overflow
	used := if prod_tail >= cons_tail {
		prod_tail - cons_tail
	} else {
		(max_u32 - cons_tail) + prod_tail + 1
	}

	return used
}

// remaining returns the number of free slots.
@[inline]
pub fn (rb RingBuffer[T]) remaining() u32 {
	return rb.capacity - rb.occupied()
}

// clear clears the ring buffer and resets all pointers.
pub fn (mut rb RingBuffer[T]) clear() bool {
	mut clear_flag := u32(0)
	mut attempts := 0
	max_attempts := 1000

	// Acquire clear flag using atomic CAS
	for {
		if C.atomic_compare_exchange_weak_u32(&rb.clear_flag, &clear_flag, 1) {
			break
		}
		clear_flag = u32(0)
		C.cpu_relax()
		attempts++
		if attempts > max_attempts {
			return false // Failed to acquire clear flag
		}
	}

	// Wait for producers to finish with exponential backoff
	mut backoff := 1
	mut prod_wait := 0
	for {
		prod_head := C.atomic_load_u32(&rb.prod_head)
		prod_tail := C.atomic_load_u32(&rb.prod_tail)
		if prod_head == prod_tail {
			break
		}
		// Exponential backoff wait
		for _ in 0 .. backoff {
			C.cpu_relax()
		}
		backoff = int_min(backoff * 2, 1024)

		prod_wait++
		if prod_wait > max_attempts {
			// Force advance producer tail
			C.atomic_store_u32(&rb.prod_tail, prod_head)
			break
		}
	}

	// Wait for consumers to finish with exponential backoff
	backoff = 1
	mut cons_wait := 0
	for {
		cons_head := C.atomic_load_u32(&rb.cons_head)
		cons_tail := C.atomic_load_u32(&rb.cons_tail)

		if cons_head == cons_tail {
			break
		}

		// Exponential backoff wait
		for _ in 0 .. backoff {
			C.cpu_relax()
		}
		backoff = int_min(backoff * 2, 1024)

		cons_wait++
		if cons_wait > max_attempts {
			// Force advance consumer tail
			C.atomic_store_u32(&rb.cons_tail, cons_head)
			break
		}
	}

	// Reset all pointers to zero
	C.atomic_store_u32(&rb.prod_head, 0)
	C.atomic_store_u32(&rb.prod_tail, 0)
	C.atomic_store_u32(&rb.cons_head, 0)
	C.atomic_store_u32(&rb.cons_tail, 0)

	C.atomic_store_u32(&rb.push_full_count, 0)
	C.atomic_store_u32(&rb.push_fail_count, 0)
	C.atomic_store_u32(&rb.push_wait_prev_count, 0)
	C.atomic_store_u32(&rb.push_waiting_count, 0)
	C.atomic_store_u32(&rb.pop_empty_count, 0)
	C.atomic_store_u32(&rb.pop_fail_count, 0)
	C.atomic_store_u32(&rb.pop_wait_prev_count, 0)
	C.atomic_store_u32(&rb.pop_waiting_count, 0)
	// Release clear flag
	C.atomic_store_u32(&rb.clear_flag, 0)
	return true // Clear operation successful
}

// stat retrieves current performance statistics of the ring buffer.
//
// This method fetches all recorded operation counters:
// - push_full_count:      Times producers encountered full buffer
// - push_fail_count:      Times producers failed to reserve space
// - push_wait_prev_count: Times producers waited for predecessors
// - push_waiting_count:   Current number of producers in waiting state
// - pop_empty_count:      Times consumers found empty buffer
// - pop_fail_count:       Times consumers failed to reserve items
// - pop_wait_prev_count:  Times consumers waited for predecessors
// - pop_waiting_count:    Current number of consumers in waiting state
pub fn (rb RingBuffer[T]) stat() RingBufferStat {
	$if debug_ringbuffer ? {
		return RingBufferStat{
			push_full_count:      C.atomic_load_u32(&rb.push_full_count)
			push_fail_count:      C.atomic_load_u32(&rb.push_fail_count)
			push_wait_prev_count: C.atomic_load_u32(&rb.push_wait_prev_count)
			push_waiting_count:   C.atomic_load_u32(&rb.push_waiting_count)
			pop_empty_count:      C.atomic_load_u32(&rb.pop_empty_count)
			pop_fail_count:       C.atomic_load_u32(&rb.pop_fail_count)
			pop_wait_prev_count:  C.atomic_load_u32(&rb.pop_wait_prev_count)
			pop_waiting_count:    C.atomic_load_u32(&rb.pop_waiting_count)
		}
	}
	return RingBufferStat{}
}
