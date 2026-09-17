struct GenericMethodContext {
mut:
	calls int
}

struct NestedResult[T] {
	data []T
}

fn success[T](data []T) NestedResult[T] {
	return NestedResult[T]{
		data: data
	}
}

fn (mut ctx GenericMethodContext) json[T](_value T) int {
	ctx.calls++
	return ctx.calls
}

fn test_generic_method_infers_from_nested_generic_return() {
	items := [1, 2, 3]
	mut ctx := GenericMethodContext{}
	assert ctx.json(success(items)) == 1
}
