module main

import amod

fn main() {
	mut outer := amod.Outer{}
	outer.inner.value = outer.inner.deep.shown
	println(outer.inner.secret)
	outer.inner.deep.hidden = 1
	deep := outer.inner.deep
	println(deep.hidden)
}
