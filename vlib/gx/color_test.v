import gx

fn test_hex() {
	// valid colors
	a := gx.hex(0x6c5ce7ff)
	b := gx.rgba(108, 92, 231, 255)
	assert a.eq(b)
	// doesn't give right value with short hex value
	short := gx.hex(0xfff)
	assert !short.eq(gx.white)
}

fn test_add() {
	a := gx.rgba(100, 100, 100, 100)
	b := gx.rgba(100, 100, 100, 100)
	r := gx.rgba(200, 200, 200, 200)
	assert (a + b).eq(r)
}

fn test_sub() {
	a := gx.rgba(100, 100, 100, 100)
	b := gx.rgba(100, 100, 100, 100)
	r := gx.rgba(0, 0, 0, 0)
	assert (a - b).eq(r)
}

fn test_mult() {
	a := gx.rgba(10, 10, 10, 10)
	b := gx.rgba(10, 10, 10, 10)
	r := gx.rgba(100, 100, 100, 100)
	assert (a * b).eq(r)
}

fn test_div() {
	a := gx.rgba(100, 100, 100, 100)
	b := gx.rgba(10, 10, 10, 10)
	r := gx.rgba(10, 10, 10, 10)
	assert (a / b).eq(r)
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
