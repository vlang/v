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
