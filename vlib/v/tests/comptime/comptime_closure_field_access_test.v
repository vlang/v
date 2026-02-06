struct TestObj {
mut:
	a int
	b int
}

fn test_main() {
	x := &TestObj{
		a: 1
		b: 1
	}
	$for field in TestObj.fields {
		run_fn(fn [x, field] () {
			(*x).$(field.name) += 1
		})
	}
	assert x.a == 2
	assert x.b == 2
}

fn run_fn(f fn ()) {
	f()
}
