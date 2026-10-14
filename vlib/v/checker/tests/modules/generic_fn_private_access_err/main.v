module main

import privlib

struct Wrapper {}

struct Box[T] {
	v T
}

// A generic function without an instance.
fn unused[T](mut s privlib.S, v T) T {
	s.secret = 1
	return v
}

// A generic function with an instance.
fn used[T](mut s privlib.S, v T) T {
	println(s.secret)
	s.counter = 2
	s.items[0].weight = 3
	s.hidden[0].a = 4
	_ = privlib.S{
		secret: 5
	}
	_ = privlib.Hidden{}
	println(privlib.private_fn())
	println(s.private_method())
	println(privlib.private_const)
	// What is public stays usable.
	s.shown = 6
	s.items[0].count = 7
	s.hidden[0].b = 8
	return v
}

// A generic method.
fn (w Wrapper) set[T](mut s privlib.S, v T) T {
	s.secret = 9
	return v
}

// A method of a generic struct.
fn (b Box[T]) set(mut s privlib.S) T {
	s.secret = 10
	return b.v
}

fn main() {
	mut s := privlib.new()
	println(used(mut s, 1))
	println(Wrapper{}.set(mut s, 'a'))
	println(Box[int]{
		v: 2
	}.set(mut s))
}
