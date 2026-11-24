module main

fn main() {
	_ := vcalloc_heap(-1)
	println('negative allocation was accepted')
}
