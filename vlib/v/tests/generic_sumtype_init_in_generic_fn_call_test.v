pub type Result<S> = Ok<S> | string

pub fn (x Result<S>) unwrap<S>() ?S {
	match x {
		Ok<S> {
			return x.value
		}
		string {
			return error(x)
		}
	}
}

struct Ok<S> {
	value S
}

pub fn ok<S>(value S) Result<S> {
	return Ok<S>{value}
}

fn test_generic_symtype_init_in_generic_fn_call() {
	x := ok<int>(42)
	ret := x.unwrap() or { 0 }

	println(ret)
	assert ret == 42
}
