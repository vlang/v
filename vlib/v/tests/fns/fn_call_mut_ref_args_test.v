@[heap]
struct Client {
mut:
	next &Client = unsafe { nil }
	prev &Client = unsafe { nil }
}

fn init_vm1(mut head &Client) {
	for c := head; c; c = c.next {
	}
}

fn init_vm2(mut head &Client) {
	for c := head; c == unsafe { nil }; c = c.next {
	}
}

fn test_fn_call_mut_ref_args() {
	mut head := &Client{}
	init_vm1(mut &head)
	init_vm2(mut &head)
	assert true
}
