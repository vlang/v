module main

struct AnyStruct[T] {
	val T
}

fn decode_struct[T]() T {
	mut typ := T{}
	$for field in T.fields {
		typ.$(field.name) = decode_field(typ.$(field.name))
	}
	return typ
}

fn decode_field[T](_ T) T {
	mut field := T{}
	return field
}

type Any = int | string | []Any

fn test_main() {
	decode_struct[AnyStruct[Any]]()
	decode_struct[AnyStruct[[]Any]]()
}
