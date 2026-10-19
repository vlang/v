module main

import privlib

struct User {
	id int
}

struct Local {
	privlib.S
	own int
}

type MyS = privlib.S

// What is public in another module is usable next to values of a type parameter.
fn public_use[T](mut s privlib.S, xs []T) int {
	s.shown += xs.len
	s.items[0].count = s.ro
	s.hidden[0].b = s.total()
	return s.shown + s.items[0].count + s.hidden[0].b
}

fn pushed[T](mut st privlib.Stack[T], v T) int {
	st.push(v)
	return st.pushes + st.len()
}

fn left_of[A, B](p privlib.Pair[A, B]) A {
	return p.left()
}

// A field promoted through a struct of this module stays accessible, as in a plain function.
fn promoted[T](l Local, v T) int {
	_ = v
	return l.secret + l.ro + l.own
}

// An alias of this module gives it the fields of its struct, as in a plain function.
fn through_alias[T](m MyS, v T) int {
	_ = v
	return m.secret
}

fn main() {
	mut s := privlib.new()
	// Generic functions of `privlib` use what is private to `privlib`.
	println(privlib.touch(mut s, 5))
	println(privlib.touch(mut s, User{ id: 6 }).id)
	println(s.total())
	privlib.reset(mut s)
	println(s.total())
	// Its generic structs keep the types of this module in their private fields.
	mut st := privlib.Stack[User]{}
	st.push(User{ id: 7 })
	println(pushed(mut st, User{ id: 8 }))
	println(st.top().id)
	p := privlib.pair(User{ id: 9 }, 'nine')
	println(left_of(p).id)
	println(p.right())
	// Generic functions of this module use what is public in `privlib`.
	println(public_use(mut s, [1, 2, 3]))
	println(promoted(Local{ own: 10 }, 'a'))
	println(through_alias(MyS(s), 1.5))
}
