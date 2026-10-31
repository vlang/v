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

fn test_concrete_error_is_successful_optional_payload() {
	mut value := ?ConcreteError(none)
	value = ConcreteError{ reason: 'ok' }
	got := value or { panic('expected concrete payload') }
	assert got.msg() == 'ok'
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

fn optional_error_payload() ?IError {
	return error('payload')
}

fn test_returned_error_is_an_option_payload() {
	value := optional_error_payload() or { panic('missing error payload') }
	assert value.msg() == 'payload'
}

@[noinline]
fn optional_local_error() ?IError {
	local := ConcreteError{ reason: 'local' }
	return &local
}

fn test_returned_error_payload_outlives_local_value() {
	value := optional_local_error() or { panic('missing local payload') }
	other := optional_local_error() or { panic('missing second payload') }
	assert value.msg() == 'local'
	assert other.msg() == 'local'
}

fn conditional_option_error(present bool) ?IError {
	return if present { error('conditional') } else { none }
}

fn test_conditional_error_is_an_option_payload() {
	value := conditional_option_error(true) or { panic('missing conditional payload') }
	assert value.msg() == 'conditional'
	assert conditional_option_error(false) == none
}
