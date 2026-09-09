struct Foo[T] {
mut:
	field_fn fn (T) T = unsafe { nil }
}

fn (f &Foo[T]) call_field_fn(x T) T {
	return f.field_fn(x)
}

fn test_generic_struct_field_fn_with_multiple_instantiations() {
	mut foo_int := Foo[int]{
		field_fn: fn (x int) int {
			return x + 1
		}
	}
	mut foo_string := Foo[string]{
		field_fn: fn (x string) string {
			return x + ' world'
		}
	}

	result1 := foo_int.call_field_fn(2)
	result2 := foo_string.call_field_fn('joe')

	println(result1)
	println(result2)

	assert result1 == 3
	assert result2 == 'joe world'
}
