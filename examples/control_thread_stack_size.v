
module main

fn main() {
	th := spawn my_print()
	th.wait()	
	th1 := spawn my_print1()
	th1.wait()
	th2 := spawn my_print2()
	th2.wait()
	th3 := spawn my_print3()
	th3.wait()
}

// default stack size
fn my_print() {
	println('hi')
}

// 32MB  stack size
[spawn_stack: 32M]
fn my_print1() {
	println('hello word')
}

// 64KB  stack size
[spawn_stack: 65536]
fn my_print2() {
	println('ahoj svet')
}

// 128KB  stack size
[spawn_stack: 128K]
fn my_print3() {
	println('здравей свят')
}
