// A comptime method call forwards the caller's own `mut` params with `mut`,
// like any other call. (Only a `mut x &T` param is a type error for a `mut`
// argument: see vlib/v/checker/tests/comptime_call_method_mut_pointer_err.vv.)
struct Sink {}

struct Counter {
mut:
	n int
}

fn (s &Sink) write(x int, mut out []u8) int {
	out << u8(`a` + x)
	return out.len
}

fn (s &Sink) bump(mut c Counter) {
	c.n += 10
}

fn forward[T](app &T, mut out []u8, mut c Counter) int {
	mut total := 0
	$for method in T.methods {
		$if method.name == 'write' {
			total += app.$method(1, mut out)
		} $else $if method.name == 'bump' {
			app.$method(mut c)
		}
	}
	return total
}

fn forward_concrete(app &Sink, mut out []u8) int {
	mut total := 0
	$for method in Sink.methods {
		$if method.name == 'write' {
			total += app.$method(2, mut out)
		}
	}
	return total
}

fn test_forwarded_mut_params_reach_the_caller() {
	mut out := []u8{}
	mut c := Counter{1}
	assert forward(&Sink{}, mut out, mut c) == 1
	assert forward_concrete(&Sink{}, mut out) == 2
	assert out.bytestr() == 'bc'
	assert c.n == 11
}
