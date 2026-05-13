module drawlib

pub struct TextDrawer {
mut:
	runes []rune
}

fn (mut td TextDrawer) calc_wordwrap() {
	if td.runes.len == 0 {
		td.runes << `Z`
	}
}
