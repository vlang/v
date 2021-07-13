fn modify(mut x &int) {
	x = 128
}

fn main() {
	mut x := 0
	modify(mut x)
	println(x)
}
