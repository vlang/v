module main

pub struct Opt {
	test ?Test
}

pub struct Test {
	a string
	b string @[skip]
}

fn test_main() {
	t := Opt{
		test: Test{
			a: 'a'
			b: 'b'
		}
	}

	defer {
		unsafe {
			t.free()
		}
	}
}
