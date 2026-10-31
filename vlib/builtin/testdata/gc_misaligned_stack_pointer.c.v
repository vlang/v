module main

// A pointer that a function keeps only in a stack slot whose address is not a multiple
// of the pointer size must still keep its object alive. LLVM's SROA (clang -O3, as used
// by -prod) makes such slots: when only the leading small fields of a struct are read,
// the rest of a copy, padding first, goes to a temporary aligned like that offset, so
// the data pointer of the `[]string` after three `bool`s below sat at an address that
// is not a multiple of 8. Without the fix, the bundled libgc freed those items.
struct Filter {
mut:
	a     bool
	b     bool
	c     bool
	items []string
}

struct Holder {
mut:
	f Filter
}

const words = ['alpha'.repeat(3), 'beta'.repeat(3), 'gamma'.repeat(3), 'delta'.repeat(3)]

// GC_noop1 receives stack addresses, so the buffers behind them stay in memory.
fn C.GC_noop1(uint)

@[noinline]
fn flags(a bool, b bool) int {
	return int(a) + int(b)
}

@[noinline]
fn fresh_items() []string {
	mut items := []string{len: words.len}
	for i, w in words {
		items[i] = w.clone()
	}
	return items
}

@[noinline]
fn refill(mut h Holder) {
	h.f.items = fresh_items()
}

@[noinline]
fn new_holder() &Holder {
	mut h := &Holder{
		f: Filter{
			a: true
			b: true
			c: true
		}
	}
	refill(mut h)
	return h
}

// churn reuses the size classes of the blocks above, so a freed one is overwritten.
@[noinline]
fn churn() {
	for _ in 0 .. 2000 {
		mut junk := []string{len: words.len, init: 'junk'}
		junk[0] = 'z'.repeat(15)
		mut raw := []u64{len: 8, init: 0}
		_ = junk
		_ = raw
	}
}

// scrub overwrites stale copies of earlier pointers in the stack below the caller.
@[noinline]
fn scrub() {
	mut buf := [4096]u8{}
	unsafe { C.memset(&buf[0], 0, 4096) }
	C.GC_noop1(uint(&buf[0]))
}

// copy_through_padding moves `h.f` into a local, clears the source and collects while
// the local holds the only reference to the items.
@[noinline]
fn copy_through_padding(mut h Holder) int {
	x := h.f
	h.f = Filter{}
	n := flags(x.a, x.b)
	gc_collect()
	churn()
	h.f = x
	return n
}

@[noinline]
fn items_intact(h &Holder) bool {
	return h.f.items == words
}

@[noinline]
fn new_block() voidptr {
	mut a := []u64{len: 8, init: 0x5a5a_5a5a_5a5a_5a5a}
	return a.data
}

// misaligned_slot keeps a block only at an odd offset of a stack buffer.
@[noinline]
fn misaligned_slot() bool {
	mut slot := [24]u8{}
	mut p := new_block()
	unsafe { C.memcpy(&slot[3], &p, sizeof(voidptr)) }
	C.GC_noop1(uint(&slot[0]))
	p = unsafe { nil }
	gc_collect()
	churn()
	mut q := unsafe { nil }
	unsafe { C.memcpy(&q, &slot[3], sizeof(voidptr)) }
	words64 := unsafe { &u64(q) }
	for i in 0 .. 8 {
		if unsafe { words64[i] } != 0x5a5a_5a5a_5a5a_5a5a {
			return false
		}
	}
	return true
}

fn main() {
	mut h := new_holder()
	mut lost_items := 0
	mut lost_blocks := 0
	for _ in 0 .. 50 {
		scrub()
		copy_through_padding(mut h)
		if !items_intact(h) {
			lost_items++
			refill(mut h)
		}
		scrub()
		if !misaligned_slot() {
			lost_blocks++
		}
	}
	if lost_items != 0 || lost_blocks != 0 {
		eprintln('collected while live: items ${lost_items}/50, blocks ${lost_blocks}/50')
		exit(1)
	}
}
