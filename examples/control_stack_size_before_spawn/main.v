module main

fn main() {
	// default stack
	th := spawn my_print()
	th.wait()
}

fn my_print() {
	// set stack size to 1024
	// bytes ?
	// TODO const Kb, Mb and b?
	.$stack_size(1024)
	// ...
	// posible works
	th := spawn my_print2()
	th.wait()
	// default stack
	th2 := spawn my_print3()
	th2.wait()
}

fn my_print3() {
	println('ahoj svet')
}

fn my_print2() {
	println('hello word')
}
