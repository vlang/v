fn f(mut x &int) {
	x = 44
}

fn main() {
	mut x := 42
	f(mut x)
	println(x)
}