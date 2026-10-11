module main

type MySumType = MyStructA | MyStructB

struct MyStructA {
mut:
	test bool
}

struct MyStructB {
}

fn test_main() {
	mut my_struct := MySumType(MyStructA{ test: true })
	assert (my_struct as MyStructA).test
	but_why(mut my_struct)
	assert !(my_struct as MyStructA).test
}

fn but_why(mut passed MySumType) {
	if mut passed is MyStructA {
		passed.test = false
	}
}
