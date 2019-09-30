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

fn myprint(s string, ..) {
	println('my print')
	println('// comment')
	println('/* comment */')
	println('/* /* comment */ */')
}

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

fn test_fns() {
	// no asserts for now, just test function declarations above
}

