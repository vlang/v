import os
import strings

// This program is built and run via Valgrind to ensure there are no leaks with -autofree
fn simple() {
	nums := [1, 2, 3] // local array must be freed
	println(nums)
	nums_copy := nums.clone() // array assignments call .clone()
	println(nums_copy)
	name := 'Peter' // string literals mustn't be freed
	str_inter := 'hello, $name' // concatenated strings must be freed
	// nums.free() // this should result in a double free and a CI error
	if true {
		// test the freeing of local vars in a new scope
		nums2 := [4, 5, 6]
		str_inter2 := 'hello, $name'
		println(nums2)
	}
	arr := return_array([])
	println(arr)
}

fn return_array(array_arg []string) []int { // array argument must not be freed
	s := [1, 2, 3] // escaping array must not be freed
	return s
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
	s = s.replace('hello', 'hi') // s can't be freed as usual before the assignment, since it's used in the right expr
	println(s)
	//
	mut s2 := 'aa' + 'bb'
	s2 = s2.replace('a', 'c')
	println(s2)
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
	mut z := '1' + '2'
	if true {
		println('KEK')
		z = 'foo'
	}
	mut x := 'a'
	x = 'b' // nothing has to be freed here
	//
	mut s := 'a' + 'b'
	s = 'x' + 'y' // 'a' + 'b' must be freed before the re-assignment
	s = s + '!' // old s ref must be copied and freed after the assignment, since s is still used in the right expr
}

struct Foo2 {
mut:
	nums []int
}

fn reassign_arr() {
	mut x := [1, 2, 3]
	// x must be freed before the re-assignment
	x = [4, 5, 6]
	mut foo := Foo2{[10, 20, 30]}
	foo.nums = [40, 50, 60] // same with struct fields
	foo.nums = [70, 80, 90]
	// TODO remove this once structs are freed automatically
	foo.nums.free()
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
	// test assigning an optional to an existing var
	mut p := 0
	for {
		p = opt('query:$q') or { break }
		break
	}
}

fn return_error_with_freed_expr() ?string {
	if true {
		msg := 'oops'
		return error('hm $msg')
	}
	return 'ok'
}

fn optional_return() {
	return_error_with_freed_expr() or { return }
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

fn get_string(s string) string {
	return s.clone() // TODO handle returning the argument without clone()
}

fn if_expr() string {
	a := if true { get_string('a' + 'b') } else { get_string('c' + 'd') }
	return a
}

fn return_if_expr() string {
	return if true { get_string('a' + 'b') } else { get_string('c' + 'd') }
}

fn loop_map() {
	m := map{
		'UK':     'London'
		'France': 'Paris'
	}
	// keys must be freed
	for country, capital in m {
		println(country + capital)
	}
}

fn free_map() {
	nums := [1, 2, 3]
	/*
	nums2 := nums.map(it + handle_strings('a' + 'b', 'c'))
	println(nums2)
	*/
}

fn free_inside_opt_block() {
	x := opt('a' + 'b') or {
		get_string('c' + 'd') // c+d must be freed before a+b
		return
	}
}

fn free_before_return() {
	s := 'a' + 'b'
	println(s)
	if true {
		return
	}
}

fn free_before_return_bool() bool {
	s := 'a' + 'b'
	println(s)
	return true
}

fn free_before_break() {
	s := 'a' + 'b'
	for {
		q := [1, 2, 3]
		break
	}
	for {
		aa := [1, 2, 3]
		if true {
			// breaking should free only vars in the closest for loop's scope
			// `qq`, not `s`
			break
		}
		// nested 1
		for {
			bb := [4, 5, 6]
			if true {
				break
			}
			// nested 2
			for {
				cc := [7, 8, 9]
				if true {
					if true {
						break
					}
					break
				}
			}
		}
	}
	mut i := 0
	for {
		i++
		qq := [1, 2, 3]
		if i > 10 {
			break
		}
		if true {
			continue
		}
	}
	x := ['1', '2', '3']
	for n in x {
		f := 'f'
		println('$n => $f')
	}
}

struct User {
	name string
	age  int
}

fn free_array_except_returned_element() {
	user := get_user()
	println(user)
}

fn get_user() User {
	users := [User{'Peter', 25}, User{'Alice', 21}]
	user := users[0] // has to be cloned, since `users` are going to be freed at the end of the function
	return user
}

fn get_user2() User {
	users := [User{'Peter', 25}, User{'Alice', 21}]
	return users[0] // has to be cloned, since `users` are going to be freed at the end of the function
}

fn string_array_get() {
	s := ['a', 'b', 'c']
	x := s[0]
	println(s)
}

fn comp_if() {
	// compif pos used to be 0, if it was the first statement in a block, vars wouldn't be freed
	$if macos {
		println('macos')
	}
	s := 'a' + 'b'
	println(s)
}

fn anon_fn() {
}

fn return_sb_str() string {
	mut sb := strings.new_builder(100)
	sb.write_string('hello')
	return sb.str() // sb should be freed, but only after .str() is called
}

fn parse_header0(s string) ?string {
	if !s.contains(':') {
		return error('missing colon in header')
	}
	words := s.split_nth(':', 2)
	return words[0]
}

fn advanced_optionals() {
	s := parse_header0('foo:bar') or { return }
}

fn main() {
	println('start')
	simple()
	reassign_str()
	reassign_arr()
	str_tmp_expr()
	str_tmp_expr_advanced()
	str_tmp_expr_advanced_var_decl()
	str_inter()
	match_expr()
	// optional_str()
	// optional_return()
	str_replace()
	str_replace2()
	if_cond()
	addition_with_tmp_expr()
	q := if_expr()
	s := return_if_expr()
	free_inside_opt_block()
	comp_if()
	free_before_return()
	free_before_return_bool()
	free_before_break()
	s2 := return_sb_str()
	// free_map()
	// loop_map()
	// advanced_optionals()
	free_array_except_returned_element()
	println('end')
}

/*
s := s.replace().replace()
tmp := s.replace()
s.free()
s = tmp.replace()
tmp.free()
*/
