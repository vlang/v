module ui

fn test_color_table_holds_the_sixteen_ansi_colours_first() {
	assert color_table.len == 256
	assert color_table[0] == 0x000000
	assert color_table[1] == 0x800000
	assert color_table[2] == 0x008000
	assert color_table[3] == 0x808000
	assert color_table[4] == 0x000080
	assert color_table[5] == 0x800080
	assert color_table[6] == 0x008080
	assert color_table[7] == 0xc0c0c0
	assert color_table[8] == 0x808080
	assert color_table[9] == 0xff0000
	assert color_table[10] == 0x00ff00
	assert color_table[11] == 0xffff00
	assert color_table[12] == 0x0000ff
	assert color_table[13] == 0xff00ff
	assert color_table[14] == 0x00ffff
	assert color_table[15] == 0xffffff
}

fn test_color_table_holds_the_six_by_six_by_six_cube() {
	assert color_table[16] == 0x000000
	assert color_table[17] == 0x00005f
	assert color_table[21] == 0x0000ff
	assert color_table[22] == 0x005f00
	assert color_table[46] == 0x00ff00
	assert color_table[196] == 0xff0000
	assert color_table[201] == 0xff00ff
	assert color_table[231] == 0xffffff
}

fn test_color_table_holds_twenty_four_greys() {
	assert color_table[232] == 0x080808
	assert color_table[233] == 0x121212
	assert color_table[255] == 0xeeeeee
	for i in 0 .. 24 {
		level := 8 + i * 10
		assert color_table[232 + i] == (u32(level) << 16) | (u32(level) << 8) | u32(level)
	}
}

fn test_color_hex_pads_every_component_to_two_digits() {
	assert Color{}.hex() == '#000000'
	assert Color{
		r: 1
		g: 2
		b: 3
	}.hex() == '#010203'
	assert Color{
		r: 0
		g: 255
		b: 0
	}.hex() == '#00ff00'
	assert Color{
		r: 255
		g: 255
		b: 255
	}.hex() == '#ffffff'
}

fn test_rgb2ansi_returns_the_palette_index_of_an_exact_match() {
	assert rgb2ansi(255, 0, 0) == 196
	assert rgb2ansi(0, 255, 0) == 46
	assert rgb2ansi(0, 0, 255) == 21
	assert rgb2ansi(255, 255, 0) == 226
	assert rgb2ansi(255, 0, 255) == 201
	assert rgb2ansi(0, 255, 255) == 51
	assert rgb2ansi(255, 255, 255) == 231
	assert rgb2ansi(0, 0, 0) == 16
}

fn test_rgb2ansi_approximates_when_no_palette_entry_matches() {
	assert rgb2ansi(1, 2, 3) == 16
	assert rgb2ansi(0, 128, 0) == 34
	assert rgb2ansi(95, 135, 175) == 67
}

fn test_rgb2ansi_maps_greys_onto_the_greyscale_ramp() {
	assert rgb2ansi(128, 128, 128) == 244
	assert rgb2ansi(95, 95, 95) == 59
	assert rgb2ansi(16, 16, 16) == 233
}

fn test_rgb2basic_ansi_picks_the_nearest_of_the_first_eight_colours() {
	assert rgb2basic_ansi(0, 0, 0) == 0
	assert rgb2basic_ansi(255, 0, 0) == 1
	assert rgb2basic_ansi(0, 255, 0) == 2
	assert rgb2basic_ansi(0, 0, 255) == 4
	assert rgb2basic_ansi(255, 255, 255) == 7
	assert rgb2basic_ansi(95, 135, 175) == 6
}

fn test_set_color_writes_a_24_bit_sequence_when_rgb_is_enabled() {
	mut ctx := Context{}
	ctx.enable_rgb = true
	ctx.set_color(Color{
		r: 12
		g: 34
		b: 56
	})
	ctx.set_bg_color(Color{
		r: 1
		g: 2
		b: 3
	})
	assert ctx.print_buf.bytestr() == '\x1b[38;2;12;34;56m\x1b[48;2;1;2;3m'
}

fn test_set_color_writes_a_256_colour_sequence_by_default() {
	mut ctx := Context{}
	ctx.set_color(Color{
		r: 255
		g: 0
		b: 0
	})
	ctx.set_bg_color(Color{
		r: 0
		g: 0
		b: 255
	})
	assert ctx.print_buf.bytestr() == '\x1b[38;5;196m\x1b[48;5;21m'
}

fn test_set_color_falls_back_to_the_basic_palette() {
	mut ctx := Context{}
	ctx.enable_ansi256 = false
	ctx.set_color(Color{
		r: 255
		g: 0
		b: 0
	})
	ctx.set_bg_color(Color{
		r: 0
		g: 0
		b: 255
	})
	assert ctx.print_buf.bytestr() == '\x1b[31m\x1b[44m'
}
