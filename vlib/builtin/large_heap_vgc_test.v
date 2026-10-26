// vtest vflags: -gc vgc
// vtest build: !race?
// vgc gets its heap from the OS in arenas of 64 MB, and an object larger than that in an arena
// of its own. The heap was limited to 64 arenas (4 GB), and only the first 64 MB of a larger
// object were found from a pointer into it.
@[has_globals]
module builtin

// A page more than an arena: every such object takes an arena of its own.
const vgc_test_big_size = uint(vgc_arena_size + vgc_page_size)

// vgc_test_big_heap_enabled reports whether the test that needs more than 4 GB of address space
// runs. Its objects are never written, so on Linux, macOS and the BSDs they take address space,
// but almost no memory. Windows commits every arena, so there it runs with VTEST_VGC_BIG_HEAP=1
// only. VTEST_VGC_BIG_HEAP=0 turns it off on small machines.
fn vgc_test_big_heap_enabled() bool {
	$if x32 {
		return false
	}
	env := unsafe { &u8(C.getenv(c'VTEST_VGC_BIG_HEAP')) }
	if env != unsafe { nil } && unsafe { env[0] } != 0 {
		return unsafe { env[0] } != `0`
	}
	$if windows {
		return false
	} $else {
		return true
	}
}

// vgc_test_allocate_big allocates n objects of vgc_test_big_size bytes without writing them,
// then writes their first and last bytes, and checks that vgc finds each one from both ends.
fn vgc_test_allocate_big(n int) []voidptr {
	mut objs := []voidptr{cap: n}
	for _ in 0 .. n {
		p := vgc_malloc_noscan_opts(vgc_test_big_size, false)
		assert p != unsafe { nil }
		objs << p
	}
	for p in objs {
		last := voidptr(uint(p) + vgc_test_big_size - 1)
		unsafe {
			*(&u8(p)) = 1
			*(&u8(last)) = 2
		}
		span := vgc_find_span(p)
		assert span != unsafe { nil }
		assert span.base == uint(p)
		assert vgc_find_span(last) == span
	}
	return objs
}

// vgc_test_clear_dead_stack overwrites the stack below the caller, where dead frames can still
// hold pointers that the conservative stack scan would take for live ones.
@[noinline]
fn vgc_test_clear_dead_stack() uint {
	buf := [4096]uint{}
	return buf[0] + buf[4095]
}

// vgc_test_spans_of returns the span of every object.
@[noinline]
fn vgc_test_spans_of(objs []voidptr) []&VGC_Span {
	mut spans := []&VGC_Span{cap: objs.len}
	for p in objs {
		spans << vgc_find_span(p)
	}
	return spans
}

// vgc_test_drop clears the pointers, so that a collection frees the objects.
fn vgc_test_drop(mut objs []voidptr) {
	for i in 0 .. objs.len {
		objs[i] = unsafe { nil }
	}
	objs.clear()
}

fn test_a_pointer_past_the_first_64_mb_of_an_object_is_found() {
	$if vgc ? {
		size := uint(96 * 1024 * 1024)
		p := vgc_malloc_noscan_opts(size, false)
		assert p != unsafe { nil }
		span := vgc_find_span(p)
		assert span != unsafe { nil }
		assert vgc_find_span(voidptr(uint(p) + 80 * 1024 * 1024)) == span
		assert vgc_find_span(voidptr(uint(p) + size - 1)) == span
	}
}

fn test_the_arena_table_grows() {
	$if vgc ? {
		start := vgc_heap.narenas
		mut objs := vgc_test_allocate_big(12)
		assert vgc_heap.narenas >= start + 12
		// The arenas that were there before are still found.
		assert vgc_find_span(voidptr(objs[0])) != unsafe { nil }
		vgc_test_drop(mut objs)
	}
}

fn test_the_heap_grows_past_64_arenas() {
	$if vgc ? {
		if !vgc_test_big_heap_enabled() {
			eprintln('skipped: set VTEST_VGC_BIG_HEAP=1 to allocate more than 4 GB of address space')
			return
		}
		mut objs := vgc_test_allocate_big(70)
		assert vgc_heap.narenas > 70
		narenas := vgc_heap.narenas
		spans := vgc_test_spans_of(objs)
		vgc_test_drop(mut objs)
		assert vgc_test_clear_dead_stack() == 0
		gc_collect()
		// The collection freed the objects, in all the arenas. A few of them can stay: in a heap
		// of 4.4 GB, the conservative scan takes some constants in the data segment for pointers
		// into them (0x300000001 keeps the object at 0x300000000 alive).
		mut freed := 0
		for span in spans {
			if !span.in_use {
				freed++
			}
		}
		assert freed >= spans.len / 2
		// New objects reuse the freed spans instead of new arenas.
		mut again := vgc_test_allocate_big(70)
		assert vgc_heap.narenas <= narenas + 70 - freed
		vgc_test_drop(mut again)
	}
}
