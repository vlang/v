module main

fn f2() thread {
	x := Abc{222}
	return spawn f(x)
}
