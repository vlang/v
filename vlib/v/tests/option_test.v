// TODO: remove this after the deprecation period for `?Type` representing both Result and Option passes.
fn opt_err_with_code(code int) ?string {
	return error_with_code('hi', code)
}

fn test_err_with_code() {
	if w := opt_err_with_code(137) {
		assert false
		_ := w
	} else {
		assert err.msg() == 'hi; code: 137'
		assert err.code() == 137
	}
	v := opt_err_with_code(56) or {
		assert err.msg() == 'hi; code: 56'
		assert err.code() == 56
		return
	}
	assert false
	_ := v
}

fn opt_err() ?string {
	return error('hi')
}

fn test_err() {
	v := opt_err() or {
		assert err.msg() == 'hi'
		return
	}
	assert false
	println(v) // suppress not used error
}

fn err_call(ok bool) ?int {
	if !ok {
		return error('Not ok!')
	}
	return 42
}

fn ret_none() ?int {
	// return error('wtf') //none
	return none
}

fn test_option_for_base_type_without_variable() {
	mut val := err_call(true) or {
		assert false
		0
	}
	assert val == 42
	val = ret_none() or { return }
	assert false
	// This is invalid:
	// x := 5 or {
	// return
	// }
}

fn test_if_opt() {
	if val := err_call(false) {
		assert val == 42
	}
	assert 1 == 1
}

fn test_if_else_opt() {
	if val := err_call(true) {
		assert val == 42
	} else {
		assert false
	}
	if _ := err_call(false) {
		assert false
	} else {
		assert err.msg().len != 0
	}
}

fn for_opt_default() ?string {
	return error('awww')
}

fn test_opt_default() {
	a := for_opt_default() or {
		// panic(err)
		'default'
	}
	assert a == 'default'
}

fn foo_ok() ?int {
	return 777
}

fn foo_str() ?string {
	return 'something'
}

fn propagate_optional(b bool) ?int {
	a := err_call(b)?
	return a
}

fn propagate_different_type(b bool) ?bool {
	err_call(b)?
	return true
}

fn test_propagation() {
	println(1)
	a := propagate_optional(true) or { 0 }
	println(2)
	assert a == 42
	println(3)
	if _ := propagate_optional(false) {
		assert false
	}
	println(4)
	b := propagate_different_type(true) or { false }
	assert b == true
	println(5)
	if _ := propagate_different_type(false) {
		assert false
	}
	println(6)
}

fn test_q() {
	assert foo_ok()? == 777
}

fn or_return_val() int {
	a := ret_none() or { return 1 }
	return a
}

fn or_return_error() ?int {
	a := ret_none() or { return error('Nope') }
	return a
}

fn or_return_none() ?int {
	a := ret_none() or { return none }
	return a
}

fn test_or_return() {
	assert or_return_val() == 1
	if _ := or_return_error() {
		assert false
	} else {
		assert err.msg().len != 0
	}
	if _ := or_return_none() {
		assert false
	} else {
		assert err.msg().len == 0
	}
}

fn test_reassignment() {
	mut x2 := foo_ok() or {
		assert false
		return
	}
	assert x2 == 777
	x2 = 100
	assert x2 == 100
	x2++
	assert x2 == 101
	//
	mut x3 := 0
	x3 = foo_ok() or {
		assert false
		return
	}
	assert x3 == 777
}

struct Person {
mut:
	name  string
	age   int
	title ?string
}

fn test_field_or() {
	name := foo_str() or { 'nada' }
	assert name == 'something'
	/*
	QTODO
	mut p := Person{}
	p.name = foo_str() or {
		'nothing'
	}
	assert p.name == 'something'
	p.age = foo_ok() or {
		panic('no age')
	}
	assert p.age == 777
	mytitle := p.title or {
		'default'
	}
	assert mytitle == 'default'
	*/
}

struct Thing {
mut:
	opt ?int
}

struct Thing2 {
mut:
	opt ?Thing
}

fn test_opt_field() {
	/*
	QTODO
	mut t := Thing{}
	t.opt = 5
	val := t.opt or { return }
	assert val == 5
	*/
}

fn opt_ptr(a &int) ?&int {
	if isnil(a) {
		return none
	}
	return unsafe { a }
}

fn test_opt_ptr() {
	if true {
	}
	//
	else {
	}
	a := 3
	mut r := opt_ptr(&a) or { &int(0) }
	assert r == &a
	r = opt_ptr(&int(0)) or { return }
	assert false
}

/*
// QTODO
fn multi_return_opt(err bool) (string, string) {
	if err {
		return error('oops')
	}
	return 'hello', 'v'
}

fn test_multi_return_opt() {
	a, b := multi_return_opt(false) or {
		panic(err)
	}
	assert a == 'hello' && b == 'v'
	_, _ := multi_return_opt(true) or {
		assert err == 'oops'
		return
	}
}
*/

fn test_optional_val_with_empty_or() {
	ret_none() or {}
	assert true
}

fn test_optional_void_return_types_of_anon_fn() {
	f := fn (i int) ? {
		if i == 0 {
			return error('0')
		}

		return
	}

	f(0) or {
		assert err.msg() == '0'
		return
	}
}

struct Foo {
	f fn (int) ?
}

fn test_option_void_return_types_of_anon_fn_in_struct() {
	foo := Foo{
		f: fn (i int) ? {
			if i == 0 {
				return error('0')
			}

			return
		}
	}

	foo.f(0) or {
		assert err.msg() == '0'
		return
	}
}

type AA = BB | CC

struct BB {
	str string
}

struct CC {
	str string
}

fn optional_sum_type(a int) ?AA {
	match a {
		1 {
			return BB{'Test'}
		}
		2 {
			return CC{'Test'}
		}
		else {
			return error('Wrong number')
		}
	}
}

fn test_optional_sum_type() {
	res1 := optional_sum_type(1) or {
		assert false
		BB{}
	}
	res2 := optional_sum_type(2) or {
		assert false
		CC{}
	}
	if res1 is BB {
		assert res1.str == 'Test'
	} else {
		assert false
	}
	if res2 is CC {
		assert res2.str == 'Test'
	} else {
		assert false
	}
	optional_sum_type(3) or {
		assert true
		return
	}
	assert false
}

struct MultiOptionalFieldTest {
	a ?int
	b ?int
}

fn foo() ?int {
	return 0
}

fn foo2() ?int {
	for _ in 0 .. 5 {
		return foo() or { continue }
	}
	return 0
}

fn test_return_or() {
	x := foo2() or { return }
	assert x == 0
}

// For issue #16058: cgen error: exists spaces in the name of the ?&C.struct
fn get_opt_pointer_to_c_struct() ?&C.stat {
	return none
}

fn test_optional_ref_c_struct_gen() {
	_ := get_opt_pointer_to_c_struct() or { &C.stat{} }
}

// For issue #16070: cgen error: missing * of optional non-ref structs
fn get_opt_to_c_struct() ?C.stat {
	return none
}

fn test_optional_c_struct_gen() {
	_ := get_opt_to_c_struct() or { C.stat{} }
}

// For issue #16062: checker disallow the return of voidptr(nil) in or block
struct Bar {}

fn get_bar(should_return_value bool) ?&Bar {
	if should_return_value {
		return unsafe { nil }
	}
	return none
}

fn test_() {
	a := get_bar(true)?
	assert a == unsafe { nil }
	//
	x := get_bar(false) or {
		assert true
		unsafe { nil }
	}
	assert x == unsafe { nil }
	//
	get_bar(false) or { unsafe { nil } }
	assert true
}
