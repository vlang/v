module main

import time

struct Abc {
	x int
}

fn f(x Abc) {
	println(x)
}

fn main() {
	f1()
	f2()
	time.sleep(1 * time.second)
	println('OK')
}
