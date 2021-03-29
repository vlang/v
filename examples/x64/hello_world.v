fn test_fn() {
	println('Hello worl from testfn')
}

fn main() {
	println('x64 test')
	mut i := 0
	for i < 5 {
		// for _ in 0 .. 5 {
		println('Hello world from V x64 machine code generator!')
		i++
	}
	println('Hello again!')
	test_fn()
	println('done')
}
