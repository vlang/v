module privlib

pub struct Item {
mut:
	weight int
pub mut:
	count int
}

struct Hidden {
mut:
	a int
pub mut:
	b int
}

pub struct S {
mut:
	secret  int
	counter int
pub:
	ro int
pub mut:
	shown  int
	items  []Item
	hidden []Hidden
}

const private_const = 40

fn private_fn() int {
	return private_const + 1
}

fn (s S) private_method() int {
	return s.secret + s.counter
}

pub fn (s S) total() int {
	return s.private_method() + s.items[0].weight + s.hidden[0].a
}

pub fn new() S {
	return S{
		ro:     2
		items:  [Item{}]
		hidden: [Hidden{}]
	}
}

// touch uses what is private to this module, whichever module asks for an instance of it.
pub fn touch[T](mut s S, v T) T {
	s.secret = private_fn()
	s.counter = s.secret + private_const
	s.items[0].weight = 3
	s.hidden[0].a = 4
	h := Hidden{
		a: s.private_method()
	}
	s.shown = h.a
	return v
}

// reset reaches the private fields through its type parameter.
pub fn reset[T](mut s T) {
	s.secret = 1
	s.counter = 2
}

// Stack keeps its elements in a private field.
pub struct Stack[T] {
mut:
	elements []T
pub mut:
	pushes int
}

pub fn (mut s Stack[T]) push(v T) {
	s.elements << v
	s.pushes++
}

fn (s Stack[T]) last_index() int {
	return s.elements.len - 1
}

pub fn (s Stack[T]) top() T {
	return s.elements[s.last_index()]
}

pub fn (s Stack[T]) len() int {
	return s.elements.len
}

// Pair has private fields of two type parameters.
pub struct Pair[A, B] {
	first  A
	second B
}

pub fn pair[A, B](a A, b B) Pair[A, B] {
	return Pair[A, B]{
		first:  a
		second: b
	}
}

pub fn (p Pair[A, B]) left() A {
	return p.first
}

pub fn (p Pair[A, B]) right() B {
	return p.second
}
