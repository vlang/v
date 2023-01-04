fn test_fn() {
	println('test fn')
}

fn main() {
	println('native test')
	mut i := 0
	for i < 5 {
		println('Hello world from V native machine code generator, in a while style for loop!')
		i++
	}
	for _ in 1 .. 5 {
		println('Hello world from V native machine code generator, in a range loop!')
	}
	println('Hello again!')
	test_fn()
	println('done')
}
