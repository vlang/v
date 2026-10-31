type Any = int | string

fn identity[T](value T) T {
	return value
}

fn unwrap[T](value ?T) T {
	return value or { panic('none') }
}

struct Holder {
mut:
	value ?Any
}

fn test_generic_optional_assignment_infers_payload_from_argument() {
	mut value := ?Any(none)
	value = identity(Any(3))
	assert (value or { Any(0) }) == Any(3)
	value = unwrap(?Any(Any('hello')))
	assert (value or { Any(0) }) == Any('hello')
	mut holder := Holder{}
	holder.value = identity(Any(4))
	assert (holder.value or { Any(0) }) == Any(4)
	mut number := ?int(none)
	number = identity(7)
	assert (number or { 0 }) == 7
}

fn contextual[T](value T) ?T {
	$if T is f64 {
		return value
	} $else {
		return none
	}
}

fn test_optional_generic_return_keeps_numeric_literal_context() {
	mut number := ?f64(none)
	number = contextual(1)
	assert number? == 1.0
	number = contextual(1.5)
	assert number? == 1.5
	typed := f64(2)
	number = contextual(typed)
	assert number? == 2.0
	number = contextual[f64](3)
	assert number? == 3.0
}

fn optional_pair[T](_first T, second T) ?T {
	return second
}

fn test_optional_generic_return_keeps_typed_argument_priority() {
	mut number := ?f32(none)
	typed := f32(2)
	number = optional_pair(1, typed)
	assert number? == typed
	number = optional_pair(typed, 3)
	assert number? == f32(3)
}
