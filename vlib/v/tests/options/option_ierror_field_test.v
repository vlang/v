struct Holder {
mut:
	err ?IError
}

interface Any {}

type ErrorOrText = IError | string

struct ConcreteError {
	reason string
}

fn (e ConcreteError) msg() string { return e.reason }

fn (_ ConcreteError) code() int { return 0 }

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

fn test_error_is_optional_failure_for_concrete_error_payload() {
	mut value := ?ConcreteError(none)
	value = error('boom')
	if _ := value {
		assert false
	} else {
		assert err.msg() == 'boom'
	}
}

fn test_error_is_optional_failure_for_pointer_interface_payload() {
	mut value := ?&IError(none)
	value = error('boom')
	if _ := value {
		assert false
	} else {
		assert err.msg() == 'boom'
	}
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
