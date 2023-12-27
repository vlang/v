module main

fn main() {
	th := spawn my_print1()
	th.wait()
	th2 := spawn my_print2()
	th2.wait()
}

// default stack size
fn my_print1() {
	println('hello word')
}

// 2MB  stack size
@[spawn_stack: 2097152]
fn my_print2() {
	println('ahoj svet')
}
