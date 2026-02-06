pub struct Generic[D] {
pub mut:
	field D
}

pub fn (j Generic[D]) foo() D {
	return j.field
}

pub fn new_generic[T](field T) Generic[T] {
	return Generic[T]{
		field: field
	}
}

fn test_generic_struct_with_inconsistent_generic_types() {
	my_generic1 := new_generic[int](2)
	r1 := my_generic1.foo()
	println(r1)
	assert r1 == 2

	my_generic2 := new_generic[string]('hello')
	r2 := my_generic2.foo()
	println(r2)
	assert r2 == 'hello'
}
