struct Test {
	a ?[]int
	b ?[]string
	c ?[]f64
	d ?[][]string
}

fn test_for_in_option_fields() {
	mut out := []string{}
	test := Test{
		a: [1, 2, 3]
		b: ['foo', 'bar']
		c: [1.2, 2.3]
		d: [['foo'], ['bar']]
	}

	for element in test.a {
		out << '${element}'
	}
	dump(out)
	assert out == ['1', '2', '3']
	out.clear()

	for element in test.b {
		out << '${element}'
	}
	dump(out)
	assert out == ['foo', 'bar']
	out.clear()

	for element in test.c {
		out << '${element}'
	}
	dump(out)
	assert out == ['1.2', '2.3']
	out.clear()

	for element in test.d {
		out << '${element}'
	}
	dump(out)
	assert out == ["['foo']", "['bar']"]
	out.clear()
}
