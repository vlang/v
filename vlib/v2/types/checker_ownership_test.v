// vtest build: !windows
module types

import os

// v2_ownership_exe returns the path to the v2_ownership binary, building it if needed.
// This binary is compiled with -d ownership so that ownership checking is enabled.
fn v2_ownership_exe() string {
	vroot := @VMODROOT
	v2_ownership := os.join_path(vroot, 'cmd', 'v2', 'v2_ownership')
	if os.is_file(v2_ownership) {
		return v2_ownership
	}
	// Try to build v2_ownership with -d ownership
	vexe := os.join_path(vroot, 'v')
	res := os.execute('${os.quoted_path(vexe)} -d ownership -o ${os.quoted_path(v2_ownership)} ${os.quoted_path(os.join_path(vroot,
		'cmd', 'v2', 'v2.v'))}')
	if res.exit_code != 0 {
		panic('failed to build v2_ownership: ${res.output}')
	}
	return v2_ownership
}

// run_ownership_check compiles the given V code with -ownership and returns
// (exit_code, stderr output).
fn run_ownership_check(code string) (int, string) {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_ownership_test_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_file := os.join_path(tmp_dir, 'test.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	v2 := v2_ownership_exe()
	res := os.execute('${os.quoted_path(v2)} -ownership -o ${os.join_path(tmp_dir, 'out')} ${os.quoted_path(tmp_file)} 2>&1')
	return res.exit_code, res.output
}

// === Assignment move tests ===

fn test_ownership_move_on_assign() {
	code := "
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1
	println(s1)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: use of moved value'
	assert output.contains('use of moved value: `s1`'), 'error should mention s1, got: ${output}'
	assert output.contains('value moved to `s2`'), 'error should mention s2, got: ${output}'
}

fn test_ownership_no_move_without_to_owned() {
	code := "
fn main() {
	s1 := 'hello'
	s2 := s1
	println(s1)
	println(s2)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'normal strings should not trigger ownership errors'
}

fn test_ownership_clone_prevents_move() {
	code := "
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1.clone()
	println(s1)
	println(s2)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, '.clone() should prevent move'
}

// === Function call move tests ===

fn test_ownership_move_on_fn_call() {
	code := "
fn takes_ownership(s string) {
	println(s)
}

fn main() {
	s := 'hello'.to_owned()
	takes_ownership(s)
	println(s)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: use after move into function'
	assert output.contains('use of moved value: `s`'), 'error should mention s, got: ${output}'
	assert output.contains('takes_ownership'), 'error should mention the function name, got: ${output}'
}

fn test_ownership_fn_call_clone_ok() {
	code := "
fn takes_ownership(s string) {
	println(s)
}

fn main() {
	s := 'hello'.to_owned()
	takes_ownership(s.clone())
	println(s)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'clone() arg should not move ownership'
}

// === Primitives don't move ===

fn test_ownership_int_does_not_move() {
	code := '
fn makes_copy(x int) {
	println(x)
}

fn main() {
	x := 5
	makes_copy(x)
	y := x
	println(y)
}
'
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'int should not trigger ownership move'
}

// === Combined scenario (Rust book example) ===

fn test_ownership_combined_rust_example() {
	code := "
fn takes_ownership(some_string string) {
	println(some_string)
}

fn makes_copy(some_integer int) {
	println(some_integer)
}

fn main() {
	s := 'hello'.to_owned()
	takes_ownership(s)
	s2 := s
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s used after move into takes_ownership'
	assert output.contains('use of moved value: `s`'), 'got: ${output}'
}

// === Return value ownership transfer tests ===

fn test_ownership_gives_ownership() {
	// A function that creates and returns an owned value transfers ownership to caller
	code := "
fn gives_ownership() string {
	s := 'hello'.to_owned()
	return s
}

fn main() {
	s1 := gives_ownership()
	s2 := s1
	println(s1)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s1 from gives_ownership() is owned and was moved'
	assert output.contains('use of moved value: `s1`'), 'got: ${output}'
	assert output.contains('value moved to `s2`'), 'got: ${output}'
}

fn test_ownership_gives_ownership_ok() {
	// Using the return value without moving is fine
	code := "
fn gives_ownership() string {
	s := 'hello'.to_owned()
	return s
}

fn main() {
	s1 := gives_ownership()
	println(s1)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'using gives_ownership() result directly should be fine'
}

fn test_ownership_takes_and_gives_back() {
	// A function that returns its parameter transfers ownership through
	code := "
fn takes_and_gives_back(s string) string {
	return s
}

fn main() {
	s1 := 'hello'.to_owned()
	s2 := takes_and_gives_back(s1)
	s3 := s2
	println(s2)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s2 is owned (via takes_and_gives_back) and was moved to s3'
	assert output.contains('use of moved value: `s2`'), 'got: ${output}'
}

fn test_ownership_takes_and_gives_back_original_moved() {
	// After calling takes_and_gives_back(s1), s1 is moved
	code := "
fn takes_and_gives_back(s string) string {
	return s
}

fn main() {
	s1 := 'hello'.to_owned()
	s2 := takes_and_gives_back(s1)
	println(s1)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s1 was moved into takes_and_gives_back'
	assert output.contains('use of moved value: `s1`'), 'got: ${output}'
}

fn test_ownership_full_rust_example() {
	// Full Rust book example: gives_ownership + takes_and_gives_back
	code := "
fn gives_ownership() string {
	s := 'hello'.to_owned()
	return s
}

fn takes_and_gives_back(a_string string) string {
	return a_string
}

fn main() {
	s1 := gives_ownership()
	s2 := 'hello'.to_owned()
	s3 := takes_and_gives_back(s2)
	println(s1)
	println(s3)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'valid Rust book example should compile without errors'
}

// === References and borrowing tests ===

fn test_borrow_immutable_no_move() {
	// Passing &s borrows it without moving — can still use s after
	code := "
fn calculate_length(s &string) int {
	return s.len
}

fn main() {
	s1 := 'hello'.to_owned()
	len := calculate_length(&s1)
	println(s1)
	println(len)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'passing &s should borrow, not move'
}

fn test_borrow_multiple_immutable_ok() {
	// Multiple immutable borrows of the same value are allowed
	code := "
fn calculate_length(s &string) int {
	return s.len
}

fn main() {
	s1 := 'hello'.to_owned()
	len1 := calculate_length(&s1)
	len2 := calculate_length(&s1)
	println(s1)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'multiple immutable borrows should be allowed'
}

fn test_borrow_var_blocks_move() {
	// r := &s1 creates a borrow that blocks moving s1
	code := "
fn main() {
	s1 := 'hello'.to_owned()
	r := &s1
	s2 := s1
	println(s2)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: cannot move s1 while borrowed by r'
	assert output.contains('cannot move `s1` because it is borrowed'), 'got: ${output}'
	assert output.contains('borrowed by `r`'), 'got: ${output}'
}

fn test_borrow_blocks_reassign() {
	// Cannot reassign a variable while it is borrowed
	code := "
fn main() {
	mut s := 'hello'.to_owned()
	r := &s
	s = 'world'.to_owned()
	println(r)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: cannot assign to s while borrowed by r'
	assert output.contains('cannot assign to `s` because it is borrowed'), 'got: ${output}'
}

fn test_borrow_mut_param_ok() {
	// Passing mut s borrows mutably — still usable after call returns
	code := "
fn append_world(mut s string) {
	s = s + ' world'
}

fn main() {
	mut s := 'hello'.to_owned()
	append_world(mut s)
	println(s)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'mut borrow via call should not move ownership'
}

fn test_borrow_two_immutable_vars_ok() {
	// Two immutable variable borrows are allowed
	code := "
fn main() {
	s := 'hello'.to_owned()
	r1 := &s
	r2 := &s
	println(r1)
	println(r2)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'two immutable borrows should be allowed'
}

// === Direct return expression ownership tests ===

fn test_ownership_direct_return_to_owned() {
	// A function that directly returns .to_owned() (no intermediate variable)
	// should still be marked as giving ownership
	code := "
fn gives_ownership() string {
	return 'hello'.to_owned()
}

fn main() {
	s1 := gives_ownership()
	s2 := s1
	println(s1)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s1 from gives_ownership() is owned and was moved'
	assert output.contains('use of moved value: `s1`'), 'got: ${output}'
}

fn test_ownership_direct_return_to_owned_ok() {
	// Direct return of .to_owned() — using result without moving is fine
	code := "
fn gives_ownership() string {
	return 'hello'.to_owned()
}

fn main() {
	s1 := gives_ownership()
	println(s1)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'using gives_ownership() result directly should be fine'
}

fn test_ownership_direct_return_ownership_fn() {
	// A function that returns the result of another ownership-giving function
	// should also be marked as giving ownership
	code := "
fn inner() string {
	return 'hello'.to_owned()
}

fn outer() string {
	return inner()
}

fn main() {
	s1 := outer()
	s2 := s1
	println(s1)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s1 from outer() is owned (transitively) and was moved'
	assert output.contains('use of moved value: `s1`'), 'got: ${output}'
}

fn test_ownership_returns_param_wrong_arg_no_false_positive() {
	// fn pick(a, b string) returns a — only the first param is returned.
	// If b is owned but a is not, the result should NOT be owned.
	code := "
fn pick(a string, b string) string {
	return a
}

fn main() {
	a := 'hello'
	b := 'world'.to_owned()
	result := pick(a, b)
	r2 := result
	println(result)
}
"
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'pick returns a (not owned), not b (owned) — result should not be owned'
}

fn test_ownership_returns_param_correct_arg() {
	// fn pick(a, b string) returns b — when b is owned, result should be owned.
	code := "
fn pick(a string, b string) string {
	return b
}

fn main() {
	a := 'hello'
	b := 'world'.to_owned()
	result := pick(a, b)
	r2 := result
	println(result)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'pick returns b (owned) — result should be owned and moved to r2'
	assert output.contains('use of moved value: `result`'), 'got: ${output}'
}

fn test_ownership_move_into_struct_field() {
	code := "
struct Probe {
	text string
}

fn main() {
	s := 'hello'.to_owned()
	_ := Probe{
		text: s
	}
	println(s)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s moved into struct field'
	assert output.contains('use of moved value: `s`'), 'got: ${output}'
}

fn test_ownership_move_into_array_literal() {
	code := "
fn main() {
	s := 'hello'.to_owned()
	_ := [s]
	println(s)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s moved into array literal'
	assert output.contains('use of moved value: `s`'), 'got: ${output}'
}

fn test_ownership_move_into_array_append() {
	code := "
fn main() {
	mut items := []string{}
	s := 'hello'.to_owned()
	items << s
	println(s)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s moved into array append'
	assert output.contains('use of moved value: `s`'), 'got: ${output}'
}

fn test_ownership_move_into_map_literal() {
	code := "
fn main() {
	s := 'hello'.to_owned()
	_ := {'k': s}
	println(s)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: s moved into map literal'
	assert output.contains('use of moved value: `s`'), 'got: ${output}'
}
