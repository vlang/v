// vtest vflags: -enable-globals

__global it = 99

struct Receiver {}

fn (r Receiver) any(f fn () bool) bool {
	return f()
}

fn test_function_literal_uses_global_it() {
	outer := [10, 20]
	r := Receiver{}
	assert outer.all(r.any(fn () bool { return it == 99 }))
}
