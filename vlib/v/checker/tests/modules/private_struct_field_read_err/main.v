module main

import fieldvis

struct Local {
	fieldvis.Inner
	own int
}

fn main() {
	mut b := fieldvis.new_box()
	println(b.private)
	println(b.secret)
	b.secret = 5
	println(b.hidden)
	println(b.Inner.hidden)
	println(b.readable + b.writable + b.shown + b.global + b.sum())
	b.writable = 6
	b.global = 7
	ptr := &b
	println(ptr.secret)
	// A fn field can be called like a method, but not read as a value.
	println(b.cb())
	cb := b.cb
	println(cb())
	alias := fieldvis.BoxAlias(b)
	println(alias.secret + alias.readable)
	local := Local{
		own: 7
	}
	println(local.own + local.hidden + local.shown)
	ref := unsafe { fieldvis.BoxRef(&b) }
	println(ref.secret + ref.readable)
	pp := &ptr
	println(pp.secret + pp.readable)
	config := fieldvis.new_config()
	println(config.secret + config.shown)
}
