module main

pub struct Reptile {}

pub struct Insect {}

fn test_generics_with_nested_generic_fn_infer_call() {
	mut r := Reptile{}
	mut i := Insect{}

	ret1 := node_search(mut r)
	println(ret1)
	assert ret1 == Reptile{}

	ret2 := node_search(mut i)
	println(ret2)
	assert ret2 == Insect{}
}

pub fn node_search<T>(mut t T) T {
	return hydrate(mut t)
}

fn hydrate<T>(mut t T) T {
	return t
}
