@[params]
struct Params {
mut:
	a bool
	x int
}

fn foo(mut opts Params) bool {
	opts.x++
	if opts.x < 2 {
		foo(opts)
		opts.a = true
	}
	return opts.a
}

fn test_call() {
	assert foo()
}
