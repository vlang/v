module main

type MyAlias = f32

const two = MyAlias(2)

fn mul[T](a T, b T) T {
	return a * b
}

fn test_generic_alias_fn_op_call() {
	assert mul[MyAlias](two, two) == f32(4.0)
	assert mul[f32](two, two) == f32(4.0)
	assert two * two == f32(4.0)
}
