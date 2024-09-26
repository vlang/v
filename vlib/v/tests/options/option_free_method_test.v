pub struct Test2 {
	a int
}

pub struct Test {
	a Test2
	b ?string
	c ?Test2
	d ?&Test2
}

@[manualfree]
fn test_main() {
	t := Test{
		b: 'b'
	}

	println('t: ${t}')

	defer {
		unsafe {
			t.free()
		}
	}

	mut t2 := ?Test(Test{
		b: 'b'
	})
	println('t: ${t2}')

	defer {
		unsafe {
			t2?.free()
		}
	}
}
