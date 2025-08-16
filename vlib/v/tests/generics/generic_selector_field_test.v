module main

struct Demo[T] {
mut:
	val T
}

type DemoType = int | []DemoType

fn test_main() {
	assert decode[Demo[[]DemoType]]() == Demo[[]DemoType]{}
	assert decode[Demo[DemoType]]() == Demo[DemoType]{}
}

fn decode[T]() T {
	mut typ := T{}
	typ.val = decode_x(typ.val)
	return typ
}

fn decode_x[T](_ T) T {
	mut field := T{}
	$if T is int {
		field = 0
	} $else $if T is $array {
		field = []DemoType{}
	}
	return field
}
