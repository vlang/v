pub type MyCallback = fn() | fn(ctx voidptr)

fn my_lower_level_func(func fn (ctx voidptr), ctx voidptr) {
	println('Bar')
}

fn my_func(cb MyCallback, ctx voidptr) {
	my_lower_level_func(fn [cb] (ctx voidptr) {
		match cb {
			fn() {
				cb()
			}
			fn(ctx voidptr) {
				cb(ctx)
			}
		}
	}, ctx)
}

@[unsafe]
fn test_closure_variable_in_smartcast() {
	my_func(fn () {
		println('Foo')
	}, unsafe { nil })
	assert true
}

fn named_callback_kind(callback MyCallback) int {
	return match callback {
		fn() { 1 }
		fn(context voidptr) { 2 }
	}
}

type NumericCallback = fn(value rune) | fn(value u32)

fn numeric_callback_kind(callback NumericCallback) int {
	return match callback {
		fn(scalar rune) { 1 }
		fn(scalar u32) { 2 }
	}
}

fn test_function_variant_patterns_ignore_names_and_preserve_exact_parameter_types() {
	assert named_callback_kind(MyCallback(fn () {})) == 1
	assert named_callback_kind(MyCallback(fn (pointer voidptr) {})) == 2
	assert numeric_callback_kind(NumericCallback(fn (value rune) {})) == 1
	assert numeric_callback_kind(NumericCallback(fn (value u32) {})) == 2
}
