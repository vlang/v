struct Test {
	a ?int
	b ?f64
	c ?string
	d int
	e f64
	f string
}

fn test_option_comptime() {
	mut opts := []string{}
	$for f in Test.fields {
		$if f.typ is $option {
			opts << f.name
		}
	}
	assert opts == ['a', 'b', 'c']
}
