struct Test {
	a ?[]int
	b ?[]string
	c ?[]f64
	d ?[][]string
}

fn run_loop[U](val []U) []string {
	mut out := []string{}
	$for field in U.fields {
		variable := val.$(field.name)
		for element in variable {
			out << '${element}'
		}
	}
	return out
}

fn test_main() {
	test := Test{
		a: [1, 2, 3]
		b: ['foo', 'bar']
		c: [1.2, 2.3]
		d: [['foo'], ['bar']]
	}

	assert run_loop(test.a?).str() == "['[1, 2, 3]: 1', '[1, 2, 3]: 2', '[1, 2, 3]: 3']"
	assert run_loop(test.b?).str() == "['['foo', 'bar']: foo', '['foo', 'bar']: bar']"
	assert run_loop(test.c?).str() == "['[1.2, 2.3]: 1.2', '[1.2, 2.3]: 2.3']"
	assert run_loop(test.d?).str() == "['[['foo'], ['bar']]: ['foo']', '[['foo'], ['bar']]: ['bar']']"
}
