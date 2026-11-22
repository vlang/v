module main

struct Abc {
	x int
}

fn f(x Abc) {
	println(x)
}

fn main() {
	first := f1()
	second := f2()
	first.wait()
	second.wait()
	println('OK')
}
