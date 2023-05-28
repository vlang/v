// Build with
// v -use-coroutines simple_coroutines.v
//
import coroutines
import time

fn foo(a int) {
	for {
		println('hello from foo() a=${a}')
		coroutines.sleep(1 * time.second)
	}
}

fn foo2(a int) {
	for {
		println('hello from foo2() a=${a}')
		coroutines.sleep(2 * time.second)
	}
}

fn foo3(a int) {
	for {
		println('hello from foo3() a=${a}')
		coroutines.sleep(3 * time.second)
	}
}

fn main() {
	go foo(10)
	go foo2(20)
	go foo3(30)
	for {
		println('hello from MAIN')
		coroutines.sleep(1 * time.second)
	}
	println('done')
}
