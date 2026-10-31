// A comptime method call forwards the caller's own `mut` params with `mut`,
// like any other call. The checker used to reject it ("cannot use `&[]u8` as
// `&&[]u8`") when the call was returned or used as a statement in a
// non-generic function. Only a `mut x &T` param makes a `mut` argument a type
// error: see vlib/v/checker/tests/comptime_call_method_mut_pointer_err.vv.
struct Sink {}

struct Counter {
mut:
	n int
}

fn (s &Sink) write(x int, mut out []u8) int {
	out << u8(`a` + x)
	return out.len
}

fn (s &Sink) bump(mut c Counter) int {
	c.n += 10
	return c.n
}

// The call returned, as a router's dispatch does.
fn return_write(app &Sink, mut out []u8) int {
	$for method in Sink.methods {
		$if method.name == 'write' {
			return app.$method(1, mut out)
		}
	}
	return -1
}

fn return_bump(app &Sink, mut c Counter) int {
	$for method in Sink.methods {
		$if method.name == 'bump' {
			return app.$method(mut c)
		}
	}
	return -1
}

// The call as a statement, its result unused.
fn call_each(app &Sink, mut out []u8, mut c Counter) {
	$for method in Sink.methods {
		$if method.name == 'write' {
			app.$method(2, mut out)
		} $else $if method.name == 'bump' {
			app.$method(mut c)
		}
	}
}

// A plain `mut` local passed with `mut` (also rejected before), as a statement
// and returned.
fn write_local(app &Sink) string {
	mut out := []u8{}
	$for method in Sink.methods {
		$if method.name == 'write' {
			app.$method(5, mut out)
		}
	}
	return out.bytestr()
}

fn return_local_write(app &Sink) int {
	mut out := []u8{}
	$for method in Sink.methods {
		$if method.name == 'write' {
			return app.$method(6, mut out)
		}
	}
	return -1
}

// Forms that compiled before: the call inside an expression, and any call in
// a generic function.
fn add_write(app &Sink, mut out []u8) int {
	mut total := 0
	$for method in Sink.methods {
		$if method.name == 'write' {
			total += app.$method(3, mut out)
		}
	}
	return total
}

fn return_write_generic[T](app &T, mut out []u8) int {
	$for method in T.methods {
		$if method.name == 'write' {
			return app.$method(4, mut out)
		}
	}
	return -1
}

fn test_returned_call_forwards_mut_params() {
	mut out := []u8{}
	mut c := Counter{1}
	assert return_write(&Sink{}, mut out) == 1
	assert return_bump(&Sink{}, mut c) == 11
	// the callee changed the caller's values, not copies
	assert out.bytestr() == 'b'
	assert c.n == 11
}

fn test_statement_call_forwards_mut_params() {
	mut out := []u8{}
	mut c := Counter{1}
	call_each(&Sink{}, mut out, mut c)
	assert out.bytestr() == 'c'
	assert c.n == 11
}

fn test_expression_and_generic_calls_forward_mut_params() {
	mut out := []u8{}
	assert add_write(&Sink{}, mut out) == 1
	assert return_write_generic(&Sink{}, mut out) == 2
	assert out.bytestr() == 'de'
}

fn test_calls_take_mut_locals() {
	assert return_local_write(&Sink{}) == 1
	assert write_local(&Sink{}) == 'f'
}
