// vtest vflags: -gc vgc
// vtest build: !race?
// gc_heap_usage() and gc_memory_use() mean the same with vgc as with Boehm's
// GC_get_heap_usage_safe() and GC_get_memory_use():
// - heap_size: the heap memory obtained from the OS, without the part returned to it;
// - free_bytes: the part of heap_size in free blocks;
// - unmapped_bytes: free heap memory returned to the OS;
// - bytes_since_gc: the bytes allocated since the last collection;
// - total_bytes: the bytes allocated since the program started;
// - gc_memory_use(): the heap memory in use, heap_size - free_bytes.
// The test passes with `-gc boehm` as well.
const mb = usize(1024 * 1024)

// has_collector reports whether a collector manages the heap: with `-gc none`, all the values
// are 0.
fn has_collector() bool {
	$if vgc ? {
		return true
	} $else $if gcboehm ? {
		return true
	} $else {
		return false
	}
}

// allocate_garbage allocates n KB in objects of 1 KB, and keeps none of them.
@[noinline]
fn allocate_garbage(n int) int {
	mut total := 0
	for i in 0 .. n {
		a := []u8{len: 1024, init: u8(i)}
		total += a.len
	}
	return total
}

// clear_dead_stack overwrites the stack below the caller, where dead frames can still hold
// pointers that a conservative collector would take for live ones.
@[noinline]
fn clear_dead_stack() usize {
	buf := [4096]usize{}
	return buf[0] + buf[4095]
}

// memory_use_with_a_large_object allocates a large object, and returns the memory in use while
// the object is live. The object is garbage after the return.
@[noinline]
fn memory_use_with_a_large_object(size int) usize {
	mut big := []u8{len: size}
	big[0] = 1
	big[size - 1] = 2
	return gc_memory_use() + usize(big[0] + big[size - 1] - 3)
}

fn test_bytes_since_gc_and_total_bytes_count_the_allocations() {
	if !has_collector() {
		return
	}
	gc_collect()
	// No automatic collection may start the count again in between.
	gc_disable()
	before := gc_heap_usage()
	assert allocate_garbage(4096) == 4096 * 1024
	after := gc_heap_usage()
	gc_enable()
	assert after.bytes_since_gc >= before.bytes_since_gc + 4 * mb
	assert after.total_bytes >= before.total_bytes + 4 * mb
	assert after.bytes_since_gc <= after.total_bytes
	gc_collect()
	collected := gc_heap_usage()
	// A collection starts the count again. The total only grows.
	assert collected.bytes_since_gc < mb
	assert collected.total_bytes >= after.total_bytes
}

fn test_memory_use_is_the_heap_without_its_free_blocks() {
	if !has_collector() {
		return
	}
	allocate_garbage(1024)
	u := gc_heap_usage()
	assert u.heap_size > 0
	assert u.free_bytes <= u.heap_size
	assert gc_memory_use() == u.heap_size - u.free_bytes
}

fn test_a_large_object_is_in_use_until_it_is_collected() {
	if !has_collector() {
		return
	}
	// A conservative collector keeps an object that a word in the stacks or the globals happens
	// to point into. With TinyCC on Windows, the heap starts at a few MB, where some constants
	// in the data segment point. A new object, at another address, is freed then.
	mut freed := false
	for _ in 0 .. 4 {
		gc_collect()
		before := gc_heap_usage()
		base := gc_memory_use()
		with_big := memory_use_with_a_large_object(int(32 * mb))
		assert with_big >= base + 32 * mb
		assert clear_dead_stack() == 0
		gc_collect()
		after := gc_heap_usage()
		// The heap memory obtained from the OS is never given up, only unmapped.
		assert after.heap_size + after.unmapped_bytes >= before.heap_size + before.unmapped_bytes
		if gc_memory_use() <= with_big - 32 * mb {
			$if vgc ? {
				// vgc returns the pages of a free span to the OS at once.
				assert after.unmapped_bytes >= before.unmapped_bytes + 32 * mb
			}
			freed = true
			break
		}
	}
	assert freed
}
