// fn println(s string) { }

// fn test_fn() {
// println('test fn')
//}

fn main() {
	println('native test')
	mut i := 0
	for i < 5 {
		println('Hello world from V native machine code generator!')
		i++
	}
	// not implemented for the native backend yet
	for _ in 1 .. 5 {
		println('Hello world from V native machine code generator!')
	}
	/*
	println('Hello again!')
	//test_fn()
	println('done')
	*/
}
