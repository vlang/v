struct Holder {
mut:
	err ?IError
}

interface Any {}

type ErrorOrText = IError | string

fn make_payload_error() IError {
	return error('sum payload')
}

fn test_error_is_successful_optional_sum_payload() {
	mut value := ?ErrorOrText(none)
	value = make_payload_error()
	got := value or { panic('expected sum payload') }
	assert got is IError
}

fn test_error_is_successful_optional_interface_payload() {
	mut value := ?Any(none)
	value = error('boom')
	got := value or { panic('expected payload') }
	assert got is IError
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
