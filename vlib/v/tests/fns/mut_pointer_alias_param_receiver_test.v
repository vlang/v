type Handle = voidptr

fn (mut h Handle) clear() {
	h = Handle(unsafe { nil })
}

// A `mut` receiver takes the parameter by implicit reference, so the slot of
// `mut h Handle` must be forwarded as is, even though its value is a pointer.
fn clear_mut_handle_param(mut h Handle) {
	h.clear()
}

fn test_mut_pointer_alias_param_forwarded_to_mut_receiver() {
	mut h := Handle(voidptr(u64(0x1234)))
	clear_mut_handle_param(mut h)
	assert voidptr(h) == unsafe { nil }
}
