module main

fn b() int {
	f := fn (i int) int {
		return i
	}
	return f(1)
}

fn main() {
	println(a())
	println(b())
	println('OK')
}
