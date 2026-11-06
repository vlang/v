module fastc

import v.pref

// The smart-cast subject `w.h.v` reads through a pointer field (`h &Holder`), so its
// first hop must be `->`. The receiver typing that chooses the separator must keep
// this chain's pointer hop when a member smart-cast narrows the path.
fn test_fastc_smartcast_member_chain_through_pointer_uses_arrow() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main

type Val = int | string

struct Holder {
mut:
	v Val
}

struct Wrap {
	h &Holder
}

fn read(w Wrap) int {
	if w.h.v is int {
		return w.h.v + 4
	}
	return 0
}
', 'pointer_receiver_smartcast.v', prefs) or { panic(err) }
	assert source.contains('(w.h->v)')
	assert !source.contains('w.h.v')
}
