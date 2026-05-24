// Re-enabled for Copy/Owned trait test extension
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
	res :=
		os.execute('${os.quoted_path(v2)} -ownership -o ${os.join_path(tmp_dir, 'out')} ${os.quoted_path(tmp_file)} 2>&1')
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
	println(*r1)
	println(*r2)
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

// === Rc / Arc — shared-ownership wrappers for Rust source translation ===
//
// Any struct named `Rc` or `Arc` (under any module) is treated as `Owned` by
// the ownership checker — matching Rust's semantics where Rc/Arc are *not*
// Copy and require an explicit `.clone()` to share. The wrapper itself
// doesn't need `implements Owned`, so a Rust-to-V translator can emit the
// types verbatim without extra annotations.

fn test_rc_moves_on_assign() {
	code := '
struct Rc[T] {
	value T
}

fn main() {
	r1 := Rc[int]{value: 42}
	r2 := r1
	println(r1.value)
	_ = r2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'Rc[T] should move on assign'
	assert output.contains('use of moved value: `r1`'), 'got: ${output}'
	assert output.contains('value moved to `r2`'), 'got: ${output}'
}

fn test_arc_moves_on_assign() {
	code := '
struct Arc[T] {
	value T
}

fn main() {
	a1 := Arc[int]{value: 42}
	a2 := a1
	println(a1.value)
	_ = a2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'Arc[T] should move on assign'
	assert output.contains('use of moved value: `a1`'), 'got: ${output}'
}

fn test_rc_clone_prevents_move() {
	// `.clone()` is the Rust escape hatch — calling it on an Rc returns
	// a fresh handle, leaving the source still usable. The check passes
	// even if downstream C codegen fails (we don't gate on exit_code).
	code := '
struct Rc[T] {
	value T
}

fn (r &Rc[int]) clone() Rc[int] {
	return Rc[int]{value: r.value}
}

fn main() {
	r1 := Rc[int]{value: 42}
	r2 := r1.clone()
	println(r1.value)
	println(r2.value)
}
'
	_, output := run_ownership_check(code)
	assert !output.contains('use of moved value'), 'clone should prevent move, got: ${output}'
}

fn test_rc_borrow_does_not_move() {
	// `&r1` is a borrow, not a move — source remains usable.
	code := '
struct Rc[T] {
	value T
}

fn takes_ref(r &Rc[int]) int {
	return r.value
}

fn main() {
	r1 := Rc[int]{value: 42}
	x := takes_ref(&r1)
	println(r1.value)
	println(x)
}
'
	_, output := run_ownership_check(code)
	assert !output.contains('use of moved value'), 'borrow should not move, got: ${output}'
}

fn test_rc_recognised_under_module_prefix() {
	// The detector strips module prefixes — a struct emitted by the
	// translator as `sync.Rc` (mangled to `sync__Rc`) is recognised too.
	// We can't easily express a fully-qualified struct in a single-file
	// test, so we use a name that contains the canonical short name as
	// the last segment.
	code := '
struct my__Rc[T] {
	value T
}

fn main() {
	r1 := my__Rc[int]{value: 1}
	r2 := r1
	println(r1.value)
	_ = r2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'module-prefixed Rc should still be tracked'
	assert output.contains('use of moved value: `r1`'), 'got: ${output}'
}

// === Owned marker interface (non-string types) ===

fn test_owned_struct_move_on_assign() {
	code := '
struct Resource implements Owned {
	handle int
}

fn main() {
	r := Resource{handle: 42}
	r2 := r
	println(r.handle)
	_ = r2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: Owned struct moved on assign'
	assert output.contains('use of moved value: `r`'), 'got: ${output}'
	assert output.contains('has type `Resource`'), 'got: ${output}'
}

fn test_owned_struct_no_move_when_unmarked() {
	// A struct that does NOT `implements Owned` keeps existing V semantics:
	// no ownership tracking, no move-on-assign.
	code := '
struct Foo {
	x int
}

fn main() {
	f := Foo{x: 1}
	f2 := f
	println(f.x)
	println(f2.x)
}
'
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'unmarked struct should not be tracked'
}

fn test_copy_struct_can_be_reused() {
	// A struct that `implements Copy` is always copyable.
	code := '
struct Point implements Copy {
	x int
	y int
}

fn main() {
	p := Point{x: 1, y: 2}
	p2 := p
	println(p.x + p2.y)
}
'
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, 'Copy struct should be reusable after assign'
}

fn test_owned_struct_move_into_fn() {
	code := '
struct Resource implements Owned {
	handle int
}

fn take(r Resource) {
	println(r.handle)
}

fn main() {
	r := Resource{handle: 1}
	take(r)
	println(r.handle)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: Owned struct moved into fn'
	assert output.contains('use of moved value: `r`'), 'got: ${output}'
	assert output.contains('moved into function `take`'), 'got: ${output}'
}

fn test_owned_struct_borrow_ok() {
	code := '
struct Resource implements Owned {
	handle int
}

fn borrow(r &Resource) int {
	return r.handle
}

fn main() {
	r := Resource{handle: 7}
	h := borrow(&r)
	println(r.handle)
	println(h)
}
'
	exit_code, _ := run_ownership_check(code)
	assert exit_code == 0, '&r should borrow, not move'
}

fn test_owned_struct_returned_from_fn_is_owned() {
	// A function returning an Owned-marked struct produces an owned value
	// at the call site, regardless of any pre-scan: the type alone is
	// enough to trigger ownership tracking.
	code := '
struct Resource implements Owned {
	handle int
}

fn make_res() Resource {
	return Resource{handle: 9}
}

fn main() {
	r := make_res()
	r2 := r
	println(r.handle)
	_ = r2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: r owned via return type'
	assert output.contains('use of moved value: `r`'), 'got: ${output}'
	assert output.contains('has type `Resource`'), 'got: ${output}'
}

fn test_owned_struct_move_into_struct_field() {
	code := '
struct Resource implements Owned {
	handle int
}

struct Holder {
	res Resource
}

fn main() {
	r := Resource{handle: 4}
	_ := Holder{res: r}
	println(r.handle)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: r moved into Holder.res field'
	assert output.contains('use of moved value: `r`'), 'got: ${output}'
}

fn test_owned_struct_borrow_blocks_move() {
	code := '
struct Resource implements Owned {
	handle int
}

fn main() {
	r := Resource{handle: 3}
	rr := &r
	r2 := r
	println(rr.handle)
	_ = r2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: cannot move r while borrowed'
	assert output.contains('cannot move `r` because it is borrowed'), 'got: ${output}'
}

fn test_owned_struct_diagnostic_uses_type_name() {
	// Verify the diagnostic message references the actual user-defined
	// type, not the placeholder "string".
	code := '
struct Widget implements Owned {
	id int
}

fn main() {
	w := Widget{id: 1}
	w2 := w
	println(w.id)
	_ = w2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail'
	assert output.contains('has type `Widget`'), 'diagnostic should name Widget, got: ${output}'
	assert !output.contains('has type `string`'), 'diagnostic should not say string, got: ${output}'
}

// === Drop interface tests ===

fn test_drop_struct_with_method_ok() {
	// A struct that implements Drop and provides a `drop()` method compiles.
	code := '
struct Resource implements Owned, Drop {
mut:
	handle int
}

fn (mut r Resource) drop() {
	r.handle = 0
}

fn main() {
	r := Resource{handle: 42}
	println(r.handle)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'Drop with drop() method should compile: ${output}'
}

fn test_drop_struct_missing_method_errors() {
	// `implements Drop` without a `drop()` method must fail with a clear
	// diagnostic pointing at the contract.
	code := '
struct Leaky implements Owned, Drop {
	handle int
}

fn main() {
	l := Leaky{handle: 1}
	println(l.handle)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'missing drop() should fail'
	assert output.contains('struct `Leaky` implements `Drop`'), 'got: ${output}'
	assert output.contains('drop(mut self)'), 'diagnostic should reference required signature, got: ${output}'
}

fn test_drop_only_marker_still_requires_method() {
	// Drop without Owned still requires drop(); both marker interfaces are
	// independent and the Drop contract stands on its own.
	code := '
struct Handle implements Drop {
	fd int
}

fn main() {
	h := Handle{fd: 3}
	println(h.fd)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'Drop without method should fail even without Owned'
	assert output.contains('struct `Handle` implements `Drop`'), 'got: ${output}'
}

fn test_drop_unrelated_struct_unaffected() {
	// A struct with neither Owned nor Drop should keep compiling as before;
	// the new validator must not affect unmarked types.
	code := '
struct Plain {
	x int
}

fn main() {
	p := Plain{x: 1}
	println(p.x)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'unmarked struct must not be affected: ${output}'
}

fn test_drop_with_move_still_diagnoses_use() {
	// A Drop+Owned struct that gets moved still produces the standard
	// "use of moved value" diagnostic. Drop scheduling and move tracking
	// must coexist on the same binding.
	code := '
struct Conn implements Owned, Drop {
mut:
	socket int
}

fn (mut c Conn) drop() {
	c.socket = -1
}

fn main() {
	c := Conn{socket: 7}
	c2 := c
	println(c.socket)
	_ = c2
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'should fail: c moved into c2'
	assert output.contains('use of moved value: `c`'), 'got: ${output}'
	assert output.contains('has type `Conn`'), 'got: ${output}'
}

// === Consuming-self method tests ===
// A by-value receiver (`fn (s Foo)`) on an Owned type CONSUMES the receiver,
// matching Rust\'s `fn(self)` vs `fn(&self)`. A `&` or `mut` receiver borrows
// and never moves. Only fires when the receiver\'s static type opts into the
// `Owned` marker — plain V types keep their auto-clone semantics.

fn test_consume_self_method_moves_receiver() {
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn (c Conn) close() {
	println(c.socket)
}

fn main() {
	c := Conn{socket: 7}
	c.close()
	c.close()
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'second call should fail: receiver moved'
	assert output.contains('use of moved value: `c`'), 'got: ${output}'
	assert output.contains('moved into function `close`'), 'should name the consuming method, got: ${output}'
	assert output.contains('has type `Conn`'), 'should show the Owned type, got: ${output}'
}

fn test_consume_self_borrow_receiver_does_not_move() {
	// `(c &Conn)` is a borrow — calling it any number of times must not
	// consume the receiver.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn (c &Conn) peek() int {
	return c.socket
}

fn main() {
	c := Conn{socket: 7}
	_ := c.peek()
	_ := c.peek()
	_ := c.peek()
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'borrow-receiver methods must not move: ${output}'
}

fn test_consume_self_mut_receiver_does_not_move() {
	// `(mut c Conn)` is a mutable borrow — also not a move.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn (mut c Conn) bump() {
	c.socket = c.socket + 1
}

fn main() {
	mut c := Conn{socket: 7}
	c.bump()
	c.bump()
	c.bump()
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'mut-receiver methods must not move: ${output}'
}

fn test_consume_self_use_after_consume_fails() {
	// After a consuming call, any subsequent use of the receiver — even
	// reading a field — must produce a use-after-move diagnostic.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn (c Conn) close() {
}

fn main() {
	c := Conn{socket: 7}
	c.close()
	println(c.socket)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'field read after consume should fail'
	assert output.contains('use of moved value: `c`'), 'got: ${output}'
}

fn test_consume_self_plain_struct_unaffected() {
	// CRITICAL: a struct WITHOUT `implements Owned` is plain V — its
	// value-receiver methods continue to auto-clone and never trigger the
	// consuming-self rule. This preserves stdlib compatibility.
	code := '
struct Plain {
mut:
	x int
}

fn (p Plain) describe() string {
	return "x"
}

fn main() {
	p := Plain{x: 1}
	println(p.describe())
	println(p.describe())
	println(p.describe())
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'plain (un-marked) struct must not be affected: ${output}'
}

fn test_consume_self_string_methods_unaffected() {
	// `string` is only owned-tracked when explicitly upgraded via `.to_owned()`.
	// A plain string literal stays auto-clone, and its many built-in
	// value-receiver methods (`split`, `trim`, ...) must keep working.
	code := "
fn main() {
	s := 'hello world'
	parts := s.split(' ')
	println(parts.len)
	println(s.len)
}
"
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'plain string methods must not be affected: ${output}'
}

// === Match-arm ownership transfer tests ===
// Each match arm gets independent ownership tracking (a move in arm 0 must
// not be visible to arm 1\'s body). After the match, a var moved in ANY
// non-terminating arm is moved. Arms that end in `return` don\'t contribute
// to the post-match state. Mirrors the if/else merge already in place.

fn test_match_arms_are_independent() {
	// Every arm consumes `c` — that\'s fine because each arm starts from
	// the pre-match snapshot. Without per-arm restore, arm 1 would see arm
	// 0\'s move and the compile would fail spuriously.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn take(c Conn) {
	_ = c
}

fn main() {
	c := Conn{socket: 7}
	choice := 1
	match choice {
		0 { take(c) }
		1 { take(c) }
		else { take(c) }
	}
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'each arm should start from pre-match state: ${output}'
}

fn test_match_consume_then_use_after_fails() {
	// Consuming in an arm should poison the var after the match — any
	// subsequent use is use-after-move.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn take(c Conn) {
	_ = c
}

fn main() {
	c := Conn{socket: 7}
	choice := 1
	match choice {
		0 { take(c) }
		1 { take(c) }
		else {}
	}
	println(c.socket)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'use after consume-in-arm should fail'
	assert output.contains('use of moved value: `c`'), 'got: ${output}'
}

fn test_match_terminating_arm_does_not_propagate_move() {
	// An arm that ends in `return` exits the function — its moves should
	// not appear in the after-match state, so code after the match can
	// still use the var.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn take(c Conn) {
	_ = c
}

fn check(choice int) {
	c := Conn{socket: 7}
	match choice {
		0 {
			take(c)
			return
		}
		else {}
	}
	println(c.socket)
}

fn main() {
	check(1)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'terminating arm move must not poison post-match state: ${output}'
}

fn test_match_all_arms_terminate_restores_before_state() {
	// If every arm ends in return, the after-match block is unreachable.
	// The before-state should be restored so any (unreachable) code below
	// type-checks cleanly without spurious move errors.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn take(c Conn) {
	_ = c
}

fn check(choice int) int {
	c := Conn{socket: 7}
	match choice {
		0 {
			take(c)
			return 0
		}
		1 {
			take(c)
			return 1
		}
		else {
			take(c)
			return 2
		}
	}
	return -1
}

fn main() {
	check(0)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'all-arms-return match should not poison state: ${output}'
}

fn test_match_non_terminating_arm_propagates_move() {
	// At least one arm consumes and falls through — the var IS moved after
	// the match and subsequent use must be diagnosed.
	code := '
struct Conn implements Owned {
mut:
	socket int
}

fn take(c Conn) {
	_ = c
}

fn check(choice int) {
	c := Conn{socket: 7}
	match choice {
		0 { take(c) }
		else {}
	}
	println(c.socket)
}

fn main() {
	check(0)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code != 0, 'fall-through consuming arm should poison post-match use'
	assert output.contains('use of moved value: `c`'), 'got: ${output}'
}

fn test_match_no_moves_unaffected() {
	// Plain match on a non-owned var should be entirely untouched — this
	// is the legacy V codepath that must keep working.
	code := '
fn main() {
	x := 3
	match x {
		0 { println("zero") }
		1 { println("one") }
		else { println("other") }
	}
	println(x)
}
'
	exit_code, output := run_ownership_check(code)
	assert exit_code == 0, 'plain match should not be affected: ${output}'
}
