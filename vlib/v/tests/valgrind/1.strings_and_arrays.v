import os

fn return_array(array_arg []string) []int { // array argument must not be freed
	s := [1, 2, 3] // escaping array must not be freed
	return s
}

fn foo() {
	nums := [1, 2, 3] // local array must be freed
	println(nums)
	nums_copy := nums // array assignments call .clone()
	println(nums_copy)
	name := 'Peter' // string literals mustn't be freed
	str_inter := 'hello, $name' // concatenated strings must be freed
	// nums.free() // this should result in a double free and a CI error
}

fn handle_strings(s, p string) int {
	return 0
}

fn handle_int(n int) {
}

fn str_tmp_expr() {
	println('a' + 'b') // tmp expression result must be freed
	handle_strings('c' + 'd', 'e' + 'f') // multiple tmp expressions must be freed
	handle_int(handle_strings('x' + 'y', 'f')) // exprs 2 levels deep must bee freed
}

struct Foo {
	a int
	b string
}

fn str_inter() {
	a := 10
	println('a = $a')
	// foo := Foo{10, 'x' + 'x'}
	// println('foo = $foo') // TODO
}

fn str_replace() {
	s := 'hello world'
	r := s.replace('hello', 'hi')
	cloned := s.replace('hello', 'hi').clone()
	cloned2 := r.clone()
	println(s)
	println(r)
}

fn main() {
	println('start')
	foo()
	str_tmp_expr()
	str_inter()
	// str_replace()
	println('end')
}
