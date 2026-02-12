// vtest vflags: -autofree
// Test that autofree doesn't generate duplicate free statements
// after return in or blocks with string interpolation

fn main() {
	x := 'test'
	_ := somefn() or {
		msg := 'fail'
		println('Error: ${msg}')
		return
	}
	println('Success')
}

fn somefn() !int {
	return error('test')
}
