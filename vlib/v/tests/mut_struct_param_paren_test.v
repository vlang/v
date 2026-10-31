// vtest vflags: -W
// Parentheses do not change what a `mut` struct parameter is: `(hdr)` is the
// same reference as `hdr`. vfmt would remove the parentheses under test, hence
// `vfmt off`.
struct ParenHeader {
mut:
	prev u64
}

struct ParenMagazine {
mut:
	next &ParenMagazine = unsafe { nil }
	id   int
}

struct ParenDepot {
mut:
	head &ParenMagazine  = unsafe { nil }
	tail &&ParenMagazine = unsafe { nil }
}

// vfmt off
fn paren_address(mut hdr ParenHeader) u64 {
	return u64((hdr))
}

fn paren_is_nil(mut hdr ParenHeader) bool {
	return (hdr) == unsafe { nil }
}

fn paren_voidptr_identity(p voidptr) voidptr {
	return p
}

fn paren_as_voidptr(mut hdr ParenHeader) u64 {
	return u64(paren_voidptr_identity((hdr)))
}

fn (mut d ParenDepot) insert_tail(mut mag ParenMagazine) {
	unsafe {
		*d.tail = (mag)
		d.tail = &mag.next
	}
}
// vfmt on

fn test_parenthesized_mut_struct_param_is_the_same_reference() {
	mut header := ParenHeader{}
	assert paren_address(mut header) == u64(&header)
	assert !paren_is_nil(mut header)
	assert paren_as_voidptr(mut header) == u64(&header)
	mut depot := ParenDepot{}
	unsafe {
		depot.tail = &depot.head
	}
	mut first := &ParenMagazine{
		id: 1
	}
	depot.insert_tail(mut first)
	assert depot.head == first
}
