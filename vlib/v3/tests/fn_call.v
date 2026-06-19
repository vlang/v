fn add(a int, b int) int {
	return a + b
}

fn greet(name string) {
	println('hello, ' + name)
}

fn main() {
	x := add(3, 4)
	println(x)
	greet('world')
}
