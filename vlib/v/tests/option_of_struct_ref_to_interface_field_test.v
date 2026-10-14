@[has_globals]
module main

interface Counter {
mut:
	next() int
}

struct RealCounter {
mut:
	n int
}

fn (mut r RealCounter) next() int {
	r.n++
	return r.n
}

struct Holder {
pub mut:
	slot ?Counter
}

fn make_counter(ok bool) ?&RealCounter {
	if ok {
		return &RealCounter{}
	}
	return none
}

fn test_option_of_struct_ref_assigned_to_option_of_interface() {
	mut h := &Holder{}
	mut c := make_counter(true)
	h.slot = c
	if mut s := h.slot {
		assert s.next() == 1
		assert s.next() == 2
	}
}

fn test_none_option_of_struct_ref_assigned_to_option_of_interface() {
	mut h := &Holder{}
	h.slot = make_counter(false)
	assert h.slot == none
}
