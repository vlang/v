struct Holder {
mut:
	err ?IError
}

fn (mut h Holder) set_err(e IError) {
	h.err = e
}

fn test_direct_assignment() {
	mut h := Holder{}
	assert h.err == none
	h.err = error('boom')
	got := h.err or { panic('expected Some, got none') }
	assert got.msg() == 'boom'
}

fn test_method_assignment() {
	mut h := Holder{}
	h.set_err(error('method boom'))
	got := h.err or { panic('expected Some, got none') }
	assert got.msg() == 'method boom'
}
