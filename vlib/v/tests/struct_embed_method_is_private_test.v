import view

pub struct TextBox {
	view.TextDrawer
}

fn (mut tb TextBox) set(x int, y int) {
	tb.runes << `A`
	tb.calc_wordwrap()
}

fn test_struct_embed_method_is_private() {
	mut tb := TextBox{}
	tb.set(11, 22)
	println(tb)
	assert tb.runes.len == 1
	assert tb.runes[0] == `A`
}
