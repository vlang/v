struct Test {
	a ?[]int
	b ?[]string
	c ?[]f64
	d ?[][]string
}

fn run_loop[U](val U, field_name string) []string {
	mut out := []string{}
	$for field in U.fields {
		variable := val.$(field.name)
		if field_name == field.name {
			for element in variable {
				println(element)
				out << element.str()
			}
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

	// println(run_loop(test, 'a'))
	assert run_loop(test, 'a').str() == "['1', '2', '3']"
	assert run_loop(test, 'b').str() == "['foo', 'bar']"
	assert run_loop(test, 'c').str() == "['1.2', '2.3']"
	assert run_loop(test, 'd').str() == "['['foo']', '['bar']']"
}
