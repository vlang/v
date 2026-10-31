// vtest vflags: -W
// A `mut` struct parameter is a reference: like V1, it can be cast to its address
// and compared with nil.
struct MutParamHeader {
mut:
	prev u64
}

fn mut_param_address(mut hdr MutParamHeader) u64 {
	hdr.prev = 1
	return u64(hdr)
}

fn mut_param_is_nil(mut hdr MutParamHeader) bool {
	return hdr == unsafe { nil }
}

fn test_mut_struct_param_casts_to_its_address() {
	mut header := MutParamHeader{}
	assert mut_param_address(mut header) == u64(&header)
	assert header.prev == 1
}

fn test_mut_struct_param_compares_with_nil() {
	mut header := MutParamHeader{}
	assert !mut_param_is_nil(mut header)
}

struct MutParamMagazine {
mut:
	next &MutParamMagazine = unsafe { nil }
	id   int
}

struct MutParamDepot {
mut:
	head &MutParamMagazine  = unsafe { nil }
	tail &&MutParamMagazine = unsafe { nil }
}

fn (mut d MutParamDepot) insert_tail(mut mag MutParamMagazine) {
	unsafe {
		*d.tail = mag
		d.tail = &mag.next
	}
}

fn test_mut_struct_param_can_be_stored_as_a_pointer() {
	mut depot := MutParamDepot{}
	unsafe {
		depot.tail = &depot.head
	}
	mut first := &MutParamMagazine{
		id: 7
	}
	mut second := &MutParamMagazine{
		id: 8
	}
	depot.insert_tail(mut first)
	depot.insert_tail(mut second)
	assert depot.head == first
	assert depot.head.next == second
	assert depot.head.next.id == 8
}

fn mut_param_as_voidptr(mut hdr MutParamHeader) u64 {
	return u64(mut_param_voidptr_identity(hdr))
}

fn mut_param_voidptr_identity(p voidptr) voidptr {
	return p
}

fn test_mut_struct_param_passes_its_address_as_voidptr() {
	mut header := MutParamHeader{}
	assert mut_param_as_voidptr(mut header) == u64(&header)
}
