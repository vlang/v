import time
// 1 line comment // 1 line comment

/*
multi line comment (1)
multi line comment (2)
multi line comment (3)
*/
/*
multi line comment (1)
	/*
		nested comment
	*/
	/*nested comment*/
	/*nested comment
*/
	/* nested comment */
	/* /* nested comment */ */
	multi line comment (2)
*/
type MyFn1 = fn (int) string

type MyFn2 = fn (a int, b int) int

type MyFn3 = fn (int, int)

fn myfn4(string)

fn foobar()

fn slopediv(num u32, den u32) int

type F1 = fn ()

type F2 = fn (voidptr)

type F3 = fn (voidptr, voidptr)

type F4 = fn (voidptr) int

type F5 = fn (int, int) int

type F6 = fn (int, int)

type F7 = fn (time.Time, int)

type MyTime = time.Time
type F8 = fn (MyTime)

interface MyInterface {}

type F9 = fn (MyInterface)

fn C.atoi(&u8) int
fn C.freec(ptr voidptr)

[trusted]
fn C.exitc(code int)

// above checks attribute doesn't conflict with `freec` return type

fn foo() {
}

type ActionfV = fn ()

type ActionfP1 = fn (voidptr)

type ActionfP2 = fn (voidptr, voidptr)

// TODO
fn modify_array(mut a []int) {
	a[0] = 10
	for i in 0 .. a.len {
		a[i] = a[i] * 2
	}
	// a << 888
}

fn modify_array2(mut a []int) {
	a[0] = 10
	for i in 0 .. a.len {
		a[i] = a[i] * 2
	}
	// a << 888
}

fn test_mut_array() {
	mut nums := [1, 2, 3]
	modify_array(mut nums)
	// assert nums.len == 4
	// println(nums)
	assert nums[0] == 20
	assert nums[1] == 4
	assert nums[2] == 6
	// assert nums[3] == 888
	// workaround for // [91, 32, -33686272] windows bug
	println(nums.clone())
}

fn mod_struct(mut user User) {
	user.age++
}

struct User {
mut:
	age int
}

fn test_mut_struct() {
	mut user := User{18}
	mod_struct(mut user)
	assert user.age == 19
}

/*
fn mod_ptr(mut buf &byte) {
	buf[0] = 77
}

fn test_mut_ptr() {
	buf := malloc(10)
	mod_ptr(mut buf)
	assert buf[0] == 77
}
*/

fn assert_in_bool_fn(v int) bool {
	assert v < 3
	return true
}

fn test_assert_in_bool_fn() {
	assert_in_bool_fn(2)
}

type MyFn = fn (int) int

fn test(n int) int {
	return n + 1000
}

struct MySt {
	f MyFn
}

fn test_fn_type_call() {
	mut arr := []MyFn{}
	arr << MyFn(test)
	// TODO: `arr[0](10)`
	// assert arr[0](10) == 1010
	x1 := arr[0]
	x2 := x1(10)
	assert x2 == 1010
	st := MySt{
		f: test
	}
	assert st.f(10) == 1010
	st1 := &MySt{
		f: test
	}
	assert st1.f(10) == 1010
}

fn ff() fn () int {
	return fn () int {
		return 22
	}
}

fn test_fn_return_fn() {
	f := ff()
	assert f() == 22
}
