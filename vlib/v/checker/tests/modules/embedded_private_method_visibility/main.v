module main

import drawlib

struct TextBox {
	drawlib.TextDrawer
}

fn (mut tb TextBox) set() {
	tb.runes << `A`
	tb.calc_wordwrap()
}

fn main() {
	mut tb := TextBox{}
	tb.set()
	println(tb.runes.len)
}
