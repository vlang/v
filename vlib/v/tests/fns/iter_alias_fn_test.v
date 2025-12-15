type MyFn = fn ()

struct Iter {
	f MyFn = fn () {}
mut:
	done bool
}

fn (mut it Iter) next() ?&MyFn {
	if it.done {
		return none
	}
	it.done = true
	return &it.f
}

fn test_main() {
	mut c := 0
	for p in Iter{} {
		println(p)
		p()
		c += 1
	}
	assert c == 1
}
