module main

pub struct Person {
pub mut:
	id int
}

fn test_generics_struct_init_in_generic_fn() {
	leo := Person{
		id: 1
	}
	println(leo)
	assert leo.id == 1

	ret := do(leo)
	println(ret)
	assert ret.id == 2
}

pub fn do<T>(t T) T {
	max := T{
		id: 2
	}
	return max
}
