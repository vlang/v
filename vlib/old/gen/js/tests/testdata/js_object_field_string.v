fn contains_hello(s string) bool {
	return s.contains('hello.v:')
}

fn get_stack() string {
	e := JS.Error{}
	return e.stack
}

fn main() {
	e := JS.Error{}
	println(e.stack.contains('hello.v:'))
	println(contains_hello(e.stack))
	println(get_stack().contains('hello.v:'))
}
