fn main() {
	a := 6
	mut b := &a
	c(b)
}

fn c(x &int) {
	println(x)
}
