// vtest vflags: -gc boehm_leak
// Regression test for issue #28896. A managed V array points one header past the
// start of its Boehm block, and `-gc boehm_leak` builds Boehm with GC_DEBUG,
// which puts its own debug header in front of every object as well. The
// prebuilt libgc linked on Windows with tcc does not recognise interior pointers
// by default, so a collection freed array blocks that a heap object still used,
// and the next free() of one aborted with "Invalid pointer passed to free()".
fn C.GC_base(voidptr) voidptr
fn C.GC_size(voidptr) usize
fn C.GC_gcollect()
fn C.GC_noop1(u64)
fn C.GC_get_all_interior_pointers() int

const block_ints = 2048

@[heap]
struct ArrayHolder {
mut:
	items []int
}

// The array gets a Boehm large block of its own, so a dropped block cannot hide
// behind live neighbours.
@[noinline]
fn new_array_holder() &ArrayHolder {
	mut holder := &ArrayHolder{}
	for i in 0 .. block_ints {
		holder.items << i
	}
	return holder
}

// clear_stack_residue overwrites the dead stack below the caller. Boehm scans
// the stack conservatively, and a stale copy of a pointer into the array block,
// left behind by building the array, would otherwise keep the block alive by
// itself and hide the bug.
@[noinline]
fn clear_stack_residue() {
	mut scratch := [1024]u64{}
	unsafe {
		C.memset(&scratch[0], 0, sizeof(scratch))
		// Keeps the compiler from dropping the zeroing as a dead store.
		C.GC_noop1(u64(&scratch[0]))
	}
}

@[noinline]
fn churn_allocations() {
	for i in 0 .. 64 {
		mut junk := []u8{len: 256}
		junk[0] = u8(i)
	}
}

// array_block_intact reports whether the holder's array is still backed by the
// large Boehm block it was allocated in: Boehm must still know a block there, of
// at least the array's size, starting just before V's data (V's array header
// plus Boehm's debug header). A dropped block is either unknown to GC_base or
// has been reused for smaller objects.
@[noinline]
fn array_block_intact(holder &ArrayHolder) bool {
	block := unsafe { voidptr(u64(holder.items.data) - u64(holder.items.offset)) }
	base := C.GC_base(block)
	if base == unsafe { nil } {
		return false
	}
	return C.GC_size(base) >= usize(block_ints * sizeof(int)) && u64(block) - u64(base) < 256
}

fn test_boehm_leak_recognises_interior_pointers() {
	$if gcboehm_leak ? {
		assert C.GC_get_all_interior_pointers() == 1
	} $else {
		eprintln('skipping: not a -gc boehm_leak build')
		assert true
	}
}

fn test_boehm_leak_collection_keeps_a_heap_held_array_block() {
	$if gcboehm_leak ? {
		holder := new_array_holder()
		clear_stack_residue()
		for _ in 0 .. 3 {
			churn_allocations()
			C.GC_gcollect()
		}
		// Checked through GC_base rather than by freeing, so a dropped block
		// fails an assertion instead of reaching Boehm's abort.
		assert array_block_intact(holder)
		assert holder.items.flags.has(.managed)
		assert holder.items.len == block_ints
		assert holder.items[0] == 0
		assert holder.items[block_ints - 1] == block_ints - 1
	} $else {
		eprintln('skipping: not a -gc boehm_leak build')
		assert true
	}
}
