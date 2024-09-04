pub type MyCallback = fn () | fn (ctx voidptr)

fn my_lower_level_func(func fn (ctx voidptr), ctx voidptr) {
	println('Bar')
}

fn my_func(cb MyCallback, ctx voidptr) {
	my_lower_level_func(fn [cb] (ctx voidptr) {
		match cb {
			fn () {
				cb()
			}
			fn (ctx voidptr) {
				cb(ctx)
			}
		}
	}, ctx)
}

fn test_closure_variable_in_smartcast() {
	my_func(fn () {
		println('Foo')
	}, unsafe { nil })
	assert true
}
