import gx

fn test_hex() {
	// valid colors
	a := gx.hex(0x6c5ce7ff)
	b := gx.rgba(108, 92, 231, 255)
	assert a == b
	// doesn't give right value with short hex value
	short := gx.hex(0xfff)
	assert short != gx.white
	assert short == gx.Color{0, 0, 15, 255}
}

fn test_add() {
	a := gx.rgba(100, 100, 100, 100)
	b := gx.rgba(100, 100, 100, 100)
	r := gx.rgba(200, 200, 200, 200)
	assert (a + b) == r
	//
	assert gx.red + gx.green == gx.yellow
	assert gx.red + gx.blue == gx.magenta
	assert gx.green + gx.blue == gx.cyan
}

fn test_sub() {
	a := gx.rgba(100, 100, 100, 50)
	b := gx.rgba(100, 100, 100, 100)
	r := gx.rgba(0, 0, 0, 100)
	assert (a - b) == r
	//
	assert gx.white - gx.green == gx.magenta
	assert gx.white - gx.blue == gx.yellow
	assert gx.white - gx.red == gx.cyan
}

fn test_mult() {
	a := gx.rgba(10, 10, 10, 10)
	b := gx.rgba(10, 10, 10, 10)
	r := gx.rgba(100, 100, 100, 100)
	assert (a * b) == r
}

fn test_div() {
	a := gx.rgba(100, 100, 100, 100)
	b := gx.rgba(10, 10, 10, 10)
	r := gx.rgba(10, 10, 10, 10)
	assert (a / b) == r
}

fn test_rgba8() {
	assert gx.white.rgba8() == -1
	assert gx.black.rgba8() == 255
	assert gx.red.rgba8() == -16776961
	assert gx.green.rgba8() == 16711935
	assert gx.blue.rgba8() == 65535
}

fn test_bgra8() {
	assert gx.white.bgra8() == -1
	assert gx.black.bgra8() == 255
	assert gx.red.bgra8() == 65535
	assert gx.green.bgra8() == 16711935
	assert gx.blue.bgra8() == -16776961
}

fn test_abgr8() {
	assert gx.white.abgr8() == -1
	assert gx.black.abgr8() == -16777216
	assert gx.red.abgr8() == -16776961
	assert gx.green.abgr8() == -16711936
	assert gx.blue.abgr8() == -65536
}

fn test_over() {
	// shorten names:
	r := gx.red
	g := gx.green
	b := gx.blue
	y := gx.yellow
	semi_r := gx.Color{255, 0, 0, 128}
	semi_g := gx.Color{0, 255, 0, 128}
	semi_b := gx.Color{0, 0, 255, 128}
	// fully opaque colors, should be preserved when layed *over* any others:
	assert b.over(g) == b
	assert r.over(g) == r
	assert y.over(r) == y
	assert g.over(r) == g

	// half transparent pure colors, *over* pure colors, should preserve them correspondingly:
	assert semi_r.over(r) == gx.Color{255, 0, 0, 255} // preserve pure red
	assert semi_r.over(g) == gx.Color{128, 126, 0, 255}
	assert semi_r.over(b) == gx.Color{128, 0, 126, 255}

	assert semi_g.over(r) == gx.Color{126, 128, 0, 255}
	assert semi_g.over(g) == gx.Color{0, 255, 0, 255} // preserve pure green
	assert semi_g.over(b) == gx.Color{0, 128, 126, 255}

	assert semi_b.over(r) == gx.Color{126, 0, 128, 255}
	assert semi_b.over(g) == gx.Color{0, 126, 128, 255}
	assert semi_b.over(b) == gx.Color{0, 0, 255, 255} // preserve pure blue
}
