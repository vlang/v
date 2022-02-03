import gg.colors

fn test_hex() {
	// valid colors
	a := colors.hex(0x6c5ce7ff)
	b := colors.rgba(108, 92, 231, 255)
	assert a == b
	// doesn't give right value with short hex value
	short := colors.hex(0xfff)
	assert short != colors.white
	assert short == colors.Color{0, 0, 15, 255}
}

fn test_add() {
	a := colors.rgba(100, 100, 100, 100)
	b := colors.rgba(100, 100, 100, 100)
	r := colors.rgba(200, 200, 200, 200)
	assert (a + b) == r
	//
	assert colors.red + colors.green == colors.yellow
	assert colors.red + colors.blue == colors.magenta
	assert colors.green + colors.blue == colors.cyan
}

fn test_sub() {
	a := colors.rgba(100, 100, 100, 50)
	b := colors.rgba(100, 100, 100, 100)
	r := colors.rgba(0, 0, 0, 100)
	assert (a - b) == r
	//
	assert colors.white - colors.green == colors.magenta
	assert colors.white - colors.blue == colors.yellow
	assert colors.white - colors.red == colors.cyan
}

fn test_mult() {
	a := colors.rgba(10, 10, 10, 10)
	b := colors.rgba(10, 10, 10, 10)
	r := colors.rgba(100, 100, 100, 100)
	assert (a * b) == r
}

fn test_div() {
	a := colors.rgba(100, 100, 100, 100)
	b := colors.rgba(10, 10, 10, 10)
	r := colors.rgba(10, 10, 10, 10)
	assert (a / b) == r
}

fn test_rgba8() {
	assert colors.white.rgba8() == -1
	assert colors.black.rgba8() == 255
	assert colors.red.rgba8() == -16776961
	assert colors.green.rgba8() == 16711935
	assert colors.blue.rgba8() == 65535
}

fn test_bgra8() {
	assert colors.white.bgra8() == -1
	assert colors.black.bgra8() == 255
	assert colors.red.bgra8() == 65535
	assert colors.green.bgra8() == 16711935
	assert colors.blue.bgra8() == -16776961
}

fn test_abgr8() {
	assert colors.white.abgr8() == -1
	assert colors.black.abgr8() == -16777216
	assert colors.red.abgr8() == -16776961
	assert colors.green.abgr8() == -16711936
	assert colors.blue.abgr8() == -65536
}

fn test_over() {
	// shorten names:
	r := colors.red
	g := colors.green
	b := colors.blue
	y := colors.yellow
	semi_r := colors.Color{255, 0, 0, 128}
	semi_g := colors.Color{0, 255, 0, 128}
	semi_b := colors.Color{0, 0, 255, 128}
	// fully opaque colors, should be preserved when layed *over* any others:
	assert b.over(g) == b
	assert r.over(g) == r
	assert y.over(r) == y
	assert g.over(r) == g

	// half transparent pure colors, *over* pure colors, should preserve them correspondingly:
	assert semi_r.over(r) == colors.Color{255, 0, 0, 255} // preserve pure red
	assert semi_r.over(g) == colors.Color{128, 126, 0, 255}
	assert semi_r.over(b) == colors.Color{128, 0, 126, 255}

	assert semi_g.over(r) == colors.Color{126, 128, 0, 255}
	assert semi_g.over(g) == colors.Color{0, 255, 0, 255} // preserve pure green
	assert semi_g.over(b) == colors.Color{0, 128, 126, 255}

	assert semi_b.over(r) == colors.Color{126, 0, 128, 255}
	assert semi_b.over(g) == colors.Color{0, 126, 128, 255}
	assert semi_b.over(b) == colors.Color{0, 0, 255, 255} // preserve pure blue
}
