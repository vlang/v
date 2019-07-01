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

fn test_fns() {
	// no asserts for now, just test function declarations above 
} 

