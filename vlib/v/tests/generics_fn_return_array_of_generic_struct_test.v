pub struct Abcd<T> {
}

pub fn iterators<T>() []&Abcd<T> {
	return []&Abcd<T>{}
}

fn test_generic_fn_return_array_of_generic_struct() {
	a := iterators<f64>()
	println(a)
	assert '$a' == '[]'
}
