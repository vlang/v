// radare - LGPL - Copyright 2013-2020 - pancake, xarkes
// ansi 256 color extension for r_cons
// https://en.wikipedia.org/wiki/ANSI_color

module ui

const (
	value_range = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff]!
	color_table = init_color_table()
)

[direct_array_access]
fn init_color_table() []int {
	mut color_table_ := []int{len: 256}
	// ansi colors
	color_table_[0] = 0x000000
	color_table_[1] = 0x800000
	color_table_[2] = 0x008000
	color_table_[3] = 0x808000
	color_table_[4] = 0x000080
	color_table_[5] = 0x800080
	color_table_[6] = 0x008080
	color_table_[7] = 0xc0c0c0
	color_table_[8] = 0x808080
	color_table_[9] = 0xff0000
	color_table_[10] = 0x00ff00
	color_table_[11] = 0xffff00
	color_table_[12] = 0x0000ff
	color_table_[13] = 0xff00ff
	color_table_[14] = 0x00ffff
	color_table_[15] = 0xffffff
	// color palette
	for i in 0 .. 216 {
		r := value_range[(i / 36) % 6]
		g := value_range[(i / 6) % 6]
		b := value_range[i % 6]
		color_table_[i + 16] = ((r << 16) & 0xffffff) + ((g << 8) & 0xffff) + (b & 0xff)
	}
	// grayscale
	for i in 0 .. 24 {
		r := 8 + (i * 10)
		color_table_[i + 232] = ((r << 16) & 0xffffff) + ((r << 8) & 0xffff) + (r & 0xff)
	}
	return color_table_
}

fn clamp(x int, y int, z int) int {
	if x < y {
		return y
	}
	if x > z {
		return z
	}
	return x
}

fn approximate_rgb(r int, g int, b int) int {
	grey := r > 0 && r < 255 && r == g && r == b
	if grey {
		return 232 + int(f64(r) / (255 / 24.1))
	}
	k := int(256.0 / 6)
	r2 := clamp(r / k, 0, 5)
	g2 := clamp(g / k, 0, 5)
	b2 := clamp(b / k, 0, 5)
	return 16 + (r2 * 36) + (g2 * 6) + b2
}

fn lookup_rgb(r int, g int, b int) int {
	color := (r << 16) + (g << 8) + b
	// lookup extended colors only, coz non-extended can be changed by users.
	for i in 16 .. 256 {
		if color_table[i] == color {
			return i
		}
	}
	return -1
}

// converts an RGB color to an ANSI 256-color, approximating it to the nearest available color
// if an exact match is not found
fn rgb2ansi(r int, g int, b int) int {
	c := lookup_rgb(r, g, b)
	if c == -1 {
		return approximate_rgb(r, g, b)
	}
	return c
}
