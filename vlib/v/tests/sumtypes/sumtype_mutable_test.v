module main

type MySumType = MyStructA | MyStructB

struct MyStructA {
mut:
	test bool
}

struct MyStructB {
}

fn test_main() {
	mut my_struct := &MyStructA{
		test: false
	}
	if $d('mutable_sumtype', false) {
		assert my_struct.test == false
		my_struct.test = true
		assert my_struct.test == true
		but_why(mut my_struct)
		assert my_struct.test == false
	}
}

fn but_why(mut passed MySumType) {
	if mut passed is MyStructA {
		passed.test = false
	}
}
