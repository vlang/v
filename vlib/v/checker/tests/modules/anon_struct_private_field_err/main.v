module main

import amod

fn main() {
	mut outer := amod.Outer{}
	outer.inner.value = outer.inner.deep.shown
	println(outer.inner.secret)
	outer.inner.deep.hidden = 1
	deep := outer.inner.deep
	println(deep.hidden)
	println(outer.inner.public)
	println(outer.inner.private)
	outer.inner.counter = 2
}

fn init_literals(arg int) []amod.Outer {
	return [
		amod.Outer{
			inner: struct {
				value:  1
				secret: 2
			}
		},
		amod.Outer{
			inner: struct {
				secret: arg
			}
		},
	]
}
