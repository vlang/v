struct Test {
	a shared int
	b shared f64
	c shared string
	d int
	e f64
	f string
}

fn test_shared_comptime() {
	mut shares := []string{}
	$for f in Test.fields {
		$if f.typ is $shared {
			shares << f.name
		}
	}
	assert shares == ['a', 'b', 'c']
}
