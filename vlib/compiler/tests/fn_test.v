// 1 line comment

/* 1 line comment */

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

type myfn fn (int) string

type myfn2 fn (a int, b int) int

type myfn3 fn (int, int)

fn myfn4(string)

fn foobar()

fn slopediv(num u32, den u32) int

type f1 fn ()

type f2 fn (voidptr)

type f3 fn (voidptr, voidptr)

type f4 fn (voidptr) int

type f5 fn (int, int) int

type f6 fn (int, int)

fn C.atoi(byteptr) int

fn foo() {
}

type actionf_v fn ()

type actionf_p1 fn (voidptr)

type actionf_p2 fn (voidptr, voidptr)

// TODO
fn modify_array(a mut []int) {
	a[0] = 10
	for i in 0..a.len {
		a[i] = a[i] * 2
	}
	//a << 888
}

fn test_mut_array() {
	mut nums := [1, 2, 3]
	modify_array(mut nums)
	//assert nums.len == 4
	// println(nums)
	assert nums[0] == 20
	assert nums[1] == 4
	assert nums[2] == 6
	//assert nums[3] == 888
	// workaround for // [91, 32, -33686272] windows bug
	println(nums.clone())
}

fn mod_struct(user mut User) {
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

fn mod_ptr(buf mut byteptr) {
	buf[0] = 77
}

fn test_mut_ptr() {
	buf := malloc(10)
	mod_ptr(mut buf)
	assert buf[0] == 77
}

fn high_fn(f fn(int) int) {

}

fn high_fn_array(f fn(a []int) []int) {

}

fn high_fn_multi_return(a int, b fn (c []int, d []string) ([]int, []string)) {

}

fn sqr(x int) int {
	return x * x
}

fn test_fns() {
	// no asserts for now, just test function declarations above
	high_fn(sqr)
}


fn test_anon_fn() {
	/*
	high_fn(fn (x int) int {
		println('hello')
		return x + 1
	})
	*/
}

fn assert_in_bool_fn(v int) bool {
	assert v < 3
	return true
}

fn test_assert_in_bool_fn() {
	assert_in_bool_fn(2)
}

type MyFn fn (int) int
fn test(n int) int {
    return n + 1000
}
struct MySt {
    f MyFn
}
fn test_fn_type_call() {
    mut arr := []MyFn
    arr << MyFn(test)
    assert arr[0](10) == 1010

    st := MySt{f:test}
    assert st.f(10) == 1010

	st1 := &MySt{f:test}
    assert st1.f(10) == 1010
}



