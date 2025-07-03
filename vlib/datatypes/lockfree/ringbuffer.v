module lockfree

// copy from vlib/sync/stdatomic/1.declarations.c.v, as we don't import `sync`
fn C.atomic_load_u32(voidptr) u32
fn C.atomic_store_u32(voidptr, u32)
fn C.atomic_compare_exchange_weak_u32(voidptr, voidptr, u32) bool
fn C.atomic_thread_fence(int)
fn C.cpu_relax()

fn C.ANNOTATE_RWLOCK_CREATE(voidptr)
fn C.ANNOTATE_RWLOCK_ACQUIRED(voidptr, int)
fn C.ANNOTATE_RWLOCK_RELEASED(voidptr, int)
fn C.ANNOTATE_RWLOCK_DESTROY(voidptr)

$if valgrind ? {
	#flag -I/usr/include/valgrind
	#include <valgrind/helgrind.h>
}

// Define cache line size to prevent false sharing between CPU cores
const cache_line_size = 64

// RingBufferMode Operation modes for the ring buffer
pub enum RingBufferMode {
	spsc = 0 // Single Producer, Single Consumer (optimized for single-threaded access)
	spmc = 1 // Single Producer, Multiple Consumers (one writer, multiple readers)
	mpsc = 2 // Multiple Producers, Single Consumer (multiple writers, one reader)
	mpmc = 3 // Multiple Producers, Multiple Consumers (default, fully concurrent)
}

// RingBufferParam Configuration parameters for ring buffer creation
@[params]
pub struct RingBufferParam {
pub:
	mode RingBufferMode = .mpmc // Default to most concurrent mode
}

// RingBuffer Lock-free multiple producer/multiple consumer ring buffer
// Requires explicit initialization
@[noinit]
pub struct RingBuffer[T] {
mut:
	mode       u32                      // Current operation mode (from RingBufferMode)
	capacity   u32                      // Total capacity (always power of two)
	mask       u32                      // Bitmask for index calculation (capacity - 1)
	clear_flag u32                      // Flag indicating clear operation in progress
	pad0       [cache_line_size - 16]u8 // Padding to align to cache line boundary

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
}

// new_ringbuffer creates a new lock-free ring buffer
pub fn new_ringbuffer[T](size u32, param RingBufferParam) &RingBuffer[T] {
	// Ensure capacity is power of two for efficient modulo operations
	capacity := next_power_of_two(size)
	mask := capacity - 1

	// Initialize data storage array
	mut slots := []T{len: int(capacity)}

	rb := &RingBuffer[T]{
		mode:     u32(param.mode)
		capacity: capacity
		mask:     mask
		slots:    slots
	}

	// Disable Valgrind checking for performance
	$if valgrind ? {
		C.VALGRIND_HG_DISABLE_CHECKING(rb, sizeof(RingBuffer[T]))
	}
	return rb
}

// next_power_of_two calculates the smallest power of two >= n
@[inline]
fn next_power_of_two(n u32) u32 {
	if n == 0 {
		return 1
	}
	mut x := n - 1
	// Efficient bit manipulation to find next power of two
	x |= x >> 1
	x |= x >> 2
	x |= x >> 4
	x |= x >> 8
	x |= x >> 16
	return x + 1
}

// is_single_producer checks if current mode is single producer
@[inline]
fn is_single_producer(mode u32) bool {
	return mode & 0x02 == 0
}

// is_single_consumer checks if current mode is single consumer
@[inline]
fn is_single_consumer(mode u32) bool {
	return mode & 0x01 == 0
}

// try_push tries to push a single item non-blocking.
pub fn (mut rb RingBuffer[T]) try_push(item T) bool {
	return rb.try_push_many([item]) == 1
}

// try_push_many tries to push multiple items non-blocking.
pub fn (mut rb RingBuffer[T]) try_push_many(items []T) u32 {
	n := u32(items.len)
	if n == 0 {
		return 0
	}

	// Check if clear operation is in progress
	if C.atomic_load_u32(&rb.clear_flag) != 0 {
		return 0
	}

	capacity := rb.capacity
	mut success := false
	mut attempts := 0
	mut old_head := u32(0)
	mut new_head := u32(0)

	// Attempt to reserve space in the buffer
	for !success && attempts < 10 {
		old_head = rb.prod_head

		// Memory barrier for weak memory models
		$if !x64 && !x32 {
			C.atomic_thread_fence(C.memory_order_acquire)
		}

		// Calculate available space using unsigned arithmetic
		free_entries := capacity + rb.cons_tail - old_head

		// Check if there's enough space
		if n > free_entries {
			return 0
		}

		// Calculate new head position after adding items
		new_head = old_head + n
		if is_single_producer(rb.mode) {
			// Direct update for single producer
			rb.prod_head = new_head
			success = true
		} else {
			// Atomic compare-and-swap for multiple producers
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_BEFORE(&rb.prod_head)
			}
			success = C.atomic_compare_exchange_weak_u32(&rb.prod_head, &old_head, new_head)
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_AFTER(&rb.prod_head)
			}
		}
		attempts++
	}

	// Exit if space reservation failed
	if !success {
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

	// For multiple producers: wait for previous producers to complete
	if !is_single_producer(rb.mode) {
		mut attempts_wait := 1
		for rb.prod_tail != old_head {
			C.cpu_relax() // Low-latency pause instruction
			attempts_wait++
		}
	}

	// Make data visible to consumers
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_BEFORE(&rb.prod_tail)
	}
	rb.prod_tail = new_head
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_AFTER(&rb.prod_tail)
	}
	return n
}

// try_pop tries to pop a single item non-blocking.
pub fn (mut rb RingBuffer[T]) try_pop() ?T {
	mut items := []T{len: 1}
	if rb.try_pop_many(mut items) == 1 {
		return items[0]
	}
	return none // Buffer empty
}

// try_pop_many tries to pop multiple items non-blocking.
pub fn (mut rb RingBuffer[T]) try_pop_many(mut items []T) u32 {
	n := u32(items.len)
	if n == 0 {
		return 0
	}

	// Check if clear operation is in progress
	if C.atomic_load_u32(&rb.clear_flag) != 0 {
		return 0
	}

	mut success := false
	mut attempts := 0
	mut old_head := u32(0)
	mut new_head := u32(0)

	// Attempt to reserve data for reading
	for !success && attempts < 10 {
		old_head = rb.cons_head
		// Memory barrier for weak memory models
		$if !x64 && !x32 {
			C.atomic_thread_fence(C.memory_order_acquire)
		}

		// Calculate available items to read
		entries := rb.prod_tail - old_head

		// Check if enough data is available
		if n > entries {
			return 0
		}

		// Calculate new head position after reading
		new_head = old_head + n
		if is_single_consumer(rb.mode) {
			// Direct update for single consumer
			rb.cons_head = new_head
			success = true
		} else {
			// Atomic compare-and-swap for multiple consumers
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_BEFORE(&rb.cons_head)
			}
			success = C.atomic_compare_exchange_weak_u32(&rb.cons_head, &old_head, new_head)
			$if valgrind ? {
				C.ANNOTATE_HAPPENS_AFTER(&rb.cons_head)
			}
		}
		attempts++
	}

	// Exit if data reservation failed
	if !success {
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

	// For multiple consumers: wait for previous consumers to complete
	if !is_single_consumer(rb.mode) {
		mut attempts_wait := 1
		for rb.cons_tail != old_head {
			C.cpu_relax() // Low-latency pause instruction
			attempts_wait++
		}
	}

	// Free up buffer space
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_BEFORE(&rb.cons_tail)
	}
	rb.cons_tail = new_head
	$if valgrind ? {
		C.ANNOTATE_HAPPENS_AFTER(&rb.cons_tail)
	}
	return n
}

// push blocking push of a single item.
pub fn (mut rb RingBuffer[T]) push(item T) {
	// Retry until successful
	for {
		if rb.try_push(item) {
			return
		}
		C.cpu_relax() // Pause before retry
	}
}

// pop blocking pop of a single item.
pub fn (mut rb RingBuffer[T]) pop() T {
	// Retry until successful
	for {
		if item := rb.try_pop() {
			return item
		}
		C.cpu_relax() // Pause before retry
	}
	return T(0) // Default value (should never be reached)
}

// push_many blocking push of multiple items.
pub fn (mut rb RingBuffer[T]) push_many(items []T) {
	for {
		n := rb.try_push_many(items)
		if n == items.len {
			break
		} else {
			C.cpu_relax() // Pause when buffer is full
		}
	}
}

// pop_many blocking pop of multiple items.
pub fn (mut rb RingBuffer[T]) pop_many(n u32) []T {
	mut result := []T{len: int(n)}

	for {
		ret := rb.try_pop_many(mut result)
		if ret == n {
			break
		} else {
			C.cpu_relax() // Pause when buffer is empty
		}
	}
	return result
}

// is_empty checks if the buffer is empty.
pub fn (rb RingBuffer[T]) is_empty() bool {
	return rb.occupied() == 0
}

// is_full checks if the buffer is full.
pub fn (rb RingBuffer[T]) is_full() bool {
	return rb.occupied() >= rb.capacity
}

// capacity returns the total capacity of the buffer.
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

	// Release clear flag
	C.atomic_store_u32(&rb.clear_flag, 0)
	return true // Clear operation successful
}
