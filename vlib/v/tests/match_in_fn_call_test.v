struct Data {
	array []int
}

fn (d Data) len() int {
	return d.array.len
}

fn make_result() []Data {
	return []
}

fn f_doesnotcompile(d Data) []Data {
	return match d.len() {
		1 { make_result() }
		else { make_result() }
	}
}

fn f_compiles1(d Data) []Data {
	return match d.array.len {
		1 { make_result() }
		else { make_result() }
	}
}

fn f_compiles2(d Data) []Data {
	length := d.array.len
	return match length {
		1 { make_result() }
		else { make_result() }
	}
}

fn test_match_in_fn_call() {
	println(f_doesnotcompile({}))
	assert f_doesnotcompile({}) == []Data{}
	println(f_compiles1({}))
	assert f_compiles1({}) == []Data{}
	println(f_compiles2({}))
	assert f_compiles2({}) == []Data{}
}
