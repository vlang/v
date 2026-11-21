module fx

fn test_pass_chain_empty() {
	px := [u8(10), 20, 30, 255]
	c := new_pass_chain()
	out := c.apply(px, 1, 1)
	assert out == px
	assert px[0] == 10
}

fn test_pass_chain_order() {
	px := [u8(200), 100, 50, 255]
	mut c := new_pass_chain()
	c.add(.invert)
	c.add(.grayscale)
	out := c.apply(px, 1, 1)
	assert out[0] == out[1] && out[1] == out[2]
	assert out[0] != px[0]
	c.clear()
	assert c.ops.len == 0
}

fn test_pass_chain_params() {
	mut c := new_pass_chain()
	c.brightness = 2
	c.add(.brightness)
	out := c.apply([u8(100), 100, 100, 255], 1, 1)
	assert out[0] == 200
}
