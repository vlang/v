[params]
struct Data {
	array []int
}

fn (d Data) len() int {
	return d.array.len
}

fn make_result() []Data {
	return []
}

// make_result2 is here to ensure that
// the branches contain different code,
// so the tests passes even with -cstrict -cc gcc
fn make_result2() []Data {
	return []
}

fn f_doesnotcompile(d Data) []Data {
	return match d.len() {
		1 { make_result() }
		else { make_result2() }
	}
}

fn f_compiles1(d Data) []Data {
	return match d.array.len {
		1 { make_result() }
		else { make_result2() }
	}
}

fn f_compiles2(d Data) []Data {
	length := d.array.len
	return match length {
		1 { make_result() }
		else { make_result2() }
	}
}

fn test_match_in_fn_call() {
	println(f_doesnotcompile())
	assert f_doesnotcompile() == []Data{}
	println(f_compiles1())
	assert f_compiles1() == []Data{}
	println(f_compiles2())
	assert f_compiles2() == []Data{}
}
