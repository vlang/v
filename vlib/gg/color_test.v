// vtest build: !musl? // gx now transitively imports sokol, and that needs GL/gl.h, which is not installed on the musl CIs
import gg

fn test_hex() {
	// valid colors
	// a := gg.hex(0x6c5ce7ff)
	// b := gg.rgba(108, 92, 231, 255)
	assert gg.hex(0xff6600) == gg.rgb(255, 102, 0) // orange
	// doesn't give right value with short hex value
	short := gg.hex(0xfff)
	assert short != gg.white
	// assert short == gg.Color{0, 0, 15, 255}
}

fn test_color_from_string() {
	// 6 digit #RRGGBB
	assert gg.color_from_string('#FF0000') == gg.red
	assert gg.color_from_string('#00FF00') == gg.green
	assert gg.color_from_string('#0000FF') == gg.blue
	// 3 digit shorthand #RGB: every digit is doubled
	assert gg.color_from_string('#F00') == gg.red
	assert gg.color_from_string('#0F0') == gg.green
	assert gg.color_from_string('#00F') == gg.blue
	assert gg.color_from_string('#FFF') == gg.white
	assert gg.color_from_string('#000') == gg.black
	assert gg.color_from_string('#ABC') == gg.rgb(0xAA, 0xBB, 0xCC)
	// 4 digit shorthand #RGBA: every digit is doubled, the last two give the alpha
	assert gg.color_from_string('#F008') == gg.rgba(255, 0, 0, 0x88)
	// 8 digit #RRGGBBAA: the last two digits give the alpha
	assert gg.color_from_string('#FF000080') == gg.rgba(255, 0, 0, 0x80)
	// unrecognised forms are black, like unknown color names
	assert gg.color_from_string('#ggg') == gg.black
	assert gg.color_from_string('#12zz34') == gg.black
	assert gg.color_from_string('#12345x78') == gg.black
	assert gg.color_from_string('#abcdef') == gg.rgb(0xAB, 0xCD, 0xEF)
	assert gg.color_from_string('#') == gg.black
	assert gg.color_from_string('#F') == gg.black
	assert gg.color_from_string('#12345') == gg.black
	assert gg.color_from_string('red') == gg.red
	assert gg.color_from_string('not-a-color') == gg.black
}

fn test_add() {
	a := gg.rgba(100, 100, 100, 100)
	b := gg.rgba(100, 100, 100, 100)
	r := gg.rgba(200, 200, 200, 200)
	assert (a + b) == r

	assert gg.red + gg.green == gg.yellow
	assert gg.red + gg.blue == gg.magenta
	assert gg.green + gg.blue == gg.cyan
}

fn test_sub() {
	a := gg.rgba(100, 100, 100, 50)
	b := gg.rgba(100, 100, 100, 100)
	r := gg.rgba(0, 0, 0, 50)
	assert (a - b) == r

	assert gg.white - gg.green == gg.magenta
	assert gg.white - gg.blue == gg.yellow
	assert gg.white - gg.red == gg.cyan
}

fn test_mult() {
	// normalized channel-wise multiply with rounding: (c1 * c2 + 127) / 255
	a := gg.rgba(10, 10, 10, 10)
	b := gg.rgba(10, 10, 10, 10)
	assert (a * b) == gg.rgba(0, 0, 0, 0)
	c := gg.rgba(200, 200, 200, 200)
	assert (c * c) == gg.rgba(157, 157, 157, 157)
	// white is the neutral element
	assert gg.white * gg.white == gg.white
	assert gg.red * gg.white == gg.red
	assert gg.red * gg.red == gg.red
}

fn test_div() {
	a := gg.rgba(100, 100, 100, 100)
	b := gg.rgba(10, 10, 10, 10)
	r := gg.rgba(10, 10, 10, 10)
	assert (a / b) == r
	// a zero divisor channel saturates instead of panicking
	z := gg.rgba(0, 0, 0, 0)
	assert (a / z) == gg.rgba(255, 255, 255, 255)
	assert (z / z) == gg.rgba(0, 0, 0, 0)
}

fn test_rgba8() {
	assert gg.white.rgba8() == -1
	assert gg.black.rgba8() == 255
	assert gg.red.rgba8() == -16776961
	assert gg.green.rgba8() == 16711935
	assert gg.blue.rgba8() == 65535
}

fn test_bgra8() {
	assert gg.white.bgra8() == -1
	assert gg.black.bgra8() == 255
	assert gg.red.bgra8() == 65535
	assert gg.green.bgra8() == 16711935
	assert gg.blue.bgra8() == -16776961
}

fn test_abgr8() {
	assert gg.white.abgr8() == -1
	assert gg.black.abgr8() == -16777216
	assert gg.red.abgr8() == -16776961
	assert gg.green.abgr8() == -16711936
	assert gg.blue.abgr8() == -65536
}

fn test_over() {
	// shorten names:
	r := gg.red
	g := gg.green
	b := gg.blue
	y := gg.yellow
	semi_r := gg.Color{255, 0, 0, 128}
	semi_g := gg.Color{0, 255, 0, 128}
	semi_b := gg.Color{0, 0, 255, 128}
	// fully opaque colors, should be preserved when laid *over* any others:
	assert b.over(g) == b
	assert r.over(g) == r
	assert y.over(r) == y
	assert g.over(r) == g

	// half transparent pure colors, *over* pure colors, should preserve them correspondingly:
	assert semi_r.over(r) == gg.Color{255, 0, 0, 255} // preserve pure red
	assert semi_r.over(g) == gg.Color{128, 126, 0, 255}
	assert semi_r.over(b) == gg.Color{128, 0, 126, 255}

	assert semi_g.over(r) == gg.Color{126, 128, 0, 255}
	assert semi_g.over(g) == gg.Color{0, 255, 0, 255} // preserve pure green
	assert semi_g.over(b) == gg.Color{0, 128, 126, 255}

	assert semi_b.over(r) == gg.Color{126, 0, 128, 255}
	assert semi_b.over(g) == gg.Color{0, 126, 128, 255}
	assert semi_b.over(b) == gg.Color{0, 0, 255, 255} // preserve pure blue

	// fully transparent over fully transparent stays transparent (no divide by zero)
	transparent := gg.Color{
		r: 0
		g: 0
		b: 0
		a: 0
	}
	assert transparent.over(transparent) == transparent
}
