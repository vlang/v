module main

enum State {
	idle
	run
}

fn make_explicit() map[State]int {
	return {
		.idle: 10
		.run:  20
	}
}

fn make_from_context() map[State]int {
	return {
		.idle: 30
		.run:  40
	}
}

fn main() {
	a := make_explicit()
	b := make_from_context()
	println(a[State.idle])
	println(a[State.run])
	println(b[State.idle])
	println(b[State.run])
}
