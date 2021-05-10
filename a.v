struct Test {
	a string
}

fn (test Test) abc(a string) {}

fn main() {
	if true {
		test := Test{
			a: 'abc'
		}
		mut a := ''
		defer {
			a = if a.len == 0 { 'test' } else { 'abc' }
			test.abc(a)
		}
	}
}
