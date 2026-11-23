module main

fn f1() thread {
	x := Abc{111}
	return spawn f(x)
}
