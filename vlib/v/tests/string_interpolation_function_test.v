fn show(a string) string {
	return a
}

fn test_function_interpolation() {
	f := fn () (string, bool) {
		return 'aaa', true
	}
	println(f)
	assert '${f}' == 'fn () (string, bool)'

	println(show)
	assert '${show}' == 'fn (string) string'
}

struct Info {
	aa fn () string
	bb int
}

fn test_function_interpolation_in_struct() {
	a := Info{
		aa: fn () string {
			return 'aaa'
		}
		bb: 22
	}
	println(a)
	assert '${a}'.contains(': fn () string')
}

fn test_function_interpolation_in_array() {
	f := [fn () string {
		return 'aaa'
	}, fn () string {
		return 'bbb'
	}]
	println(f)
	assert '${f}' == '[fn () string, fn () string]'
}

fn test_function_interpolation_in_map() {
	m := {
		'aaa': fn () string {
			return 'aaa'
		}
		'bbb': fn () string {
			return 'bbb'
		}
	}
	println(m)
	assert '${m}'.contains(': fn () string')
}
