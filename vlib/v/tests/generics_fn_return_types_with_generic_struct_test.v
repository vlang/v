struct Abcd<T> {
}

fn iterators_array<T>() []&Abcd<T> {
	return []&Abcd<T>{}
}

fn test_generic_fn_return_array_of_generic_struct() {
	a := iterators_array<f64>()
	println(a)
	assert '${a}' == '[]'
}

fn iterators_chan<T>() chan Abcd<T> {
	return chan Abcd<T>{}
}

fn test_generic_fn_return_chan_of_generic_struct() {
	a := iterators_chan<f64>()
	println(a)
	assert typeof(a).name == 'chan Abcd<f64>'
}

fn iterators_map<T>() map[string]&Abcd<T> {
	return map[string]&Abcd<T>{}
}

fn test_generic_fn_return_map_of_generic_struct() {
	a := iterators_map<f64>()
	println(a)
	assert '${a}' == '{}'
}
