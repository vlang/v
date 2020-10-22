fn return_array(array_arg []string) []int { // array argument must not be freed
	s := [1, 2, 3] // escaping array must not be freed
	return s
}

fn simple() {
	nums := [1, 2, 3] // local array must be freed
	println(nums)
	nums_copy := nums // array assignments call .clone()
	println(nums_copy)
	name := 'Peter' // string literals mustn't be freed
	str_inter := 'hello, $name' // concatenated strings must be freed
	// nums.free() // this should result in a double free and a CI error
}

fn handle_strings(s string, p string) int {
	return 0
}

fn handle_int(n int) {
}

fn add_strings(a string, b string) string {
	return a + b
}

fn str_tmp_expr() {
	println('a' + 'b') // tmp expression result must be freed
	handle_strings('c' + 'd', 'e' + 'f') // multiple tmp expressions must be freed
	handle_int(handle_strings('x' + 'y', 'f')) // exprs 2 levels deep must bee freed
	handle_strings('1', add_strings('2', '3')) // test a fn that returns a string
}

fn str_tmp_expr_advanced() {
	// t1 = 'c' + 'd'
	// t2 = 'e + f'
	// t3 = add_strings(t2, 'g')
	// handle_strings(t1, t3)
	// t1.free()
	// t2.free()
	// t3.free()
	//
	handle_strings('c' + 'd', add_strings('e' + 'f', 'g')) // both lvl 1 and lvl2 exprs must be freed
}

fn str_tmp_expr_advanced_var_decl() {
	a := handle_strings('c' + 'd', add_strings('e' + 'f', 'g')) // both lvl 1 and lvl2 exprs must be freed
	println(a)
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
	mut s := 'hello world'
	s = s.replace('hello', 'hi')
	println(s)
	/*
	r := s.replace('hello', 'hi')
	cloned := s.replace('hello', 'hi').clone()
	cloned2 := r.clone()
	println(s)
	println(r)
	*/
}

fn fooo(s string) {
}

fn str_replace2() {
	mut s := 'hello world'
	s = s.replace('hello', 'hi').replace('world', 'planet')
	println(s)
}

fn reassign_str() {
	mut s := 'a' + 'b'
	s = 'x' + 'y' // 'a' + 'b' must be freed before the re-assignment
}

fn match_expr() string {
	x := 2
	res := match x {
		1 { 'one' }
		2 { 'two' }
		else { 'unknown' }
	}
	return res
}

fn opt(s string) ?int {
	return 1
}

fn optional_str() {
	q := 'select'
	s := 'query: select'
	// optional fn args must be freed
	pos2 := opt('query:$q') or {
		// pos := s.index('query: $q') or {
		println('exiting')
		return
	}
	println(pos2 + 1)
	// optional method args must be freed
	pos := s.index('query: $q') or {
		println('exiting')
		return
	}
	println(pos + 1)
}

fn return_error_with_freed_expr() ?string {
	if true {
		msg := 'oops'
		return error('hm $msg')
	}
	return 'ok'
}

fn optional_return() {
	return_error_with_freed_expr() or {
		return
	}
}

fn handle_string(s string) bool {
	return true
}

fn if_cond() {
	// handle_string('1' + '2')
	if handle_string('1' + '2') {
		// if '1' + '2' == '12' {
		println('yes')
	} else {
		println('no')
	}
}

fn addition_with_tmp_expr() {
	x := 1 + handle_strings('a' + 'b', 'c')
	println(x)
}

fn tt() {
	// time.parse_rfc2822('1234')
}

fn main() {
	println('start')
	simple()
	str_tmp_expr()
	str_tmp_expr_advanced()
	str_tmp_expr_advanced_var_decl()
	str_inter()
	match_expr()
	reassign_str()
	optional_str()
	optional_return()
	str_replace()
	str_replace2()
	if_cond()
	addition_with_tmp_expr()
	println('end')
}

/*
s := s.replace().replace()
tmp := s.replace()
s.free()
s = tmp.replace()
tmp.free()
*/
