fn f() !string {
	return 'abc'
}

fn g(s string) {
	println(s)
}

fn main() {
	g(f()!)
}
