module fx

fn test_post_grayscale_and_invert() {
	px := [u8(255), 0, 0, 255, 0, 0, 255, 255]
	g := post_grayscale(px, 2, 1)
	assert g[0] == g[1] && g[1] == g[2]
	assert g[3] == 255
	assert g[4] == g[5] && g[5] == g[6]
	inv := post_invert(px, 2, 1)
	assert inv[0] == 0 && inv[1] == 255 && inv[2] == 255
	assert inv[3] == 255
}

fn test_post_brightness_contrast_saturate() {
	px := [u8(100), 100, 100, 255]
	assert post_brightness(px, 1, 1, 2)[0] == 200
	assert post_contrast(px, 1, 1, 1)[0] == 100
	assert post_contrast(px, 1, 1, 2)[0] >= 72
	desat := post_saturate([u8(200), 100, 50, 255], 1, 1, 0)
	assert desat[0] == desat[1] && desat[1] == desat[2]
}

fn test_post_vignette_and_blur() {
	px := [u8(200), 200, 200, 255, 200, 200, 200, 255, 200, 200, 200, 255, 200, 200, 200, 255]
	v := post_vignette(px, 2, 2, 1)
	assert v[0] < 200
	assert v[3] == 255
	b := post_box_blur(px, 2, 2, 1)
	assert b.len == px.len
	assert b[0] == 200
	assert post_box_blur(px, 2, 2, 0) == px
}

fn test_post_tonemap_bloom() {
	px := [u8(200), 200, 200, 255]
	t := post_tonemap(px, 1, 1)
	assert t[0] < 200 && t[0] > 0
	bl := post_bloom(px, 1, 1, 0.5, 0.5, 1)
	assert bl[0] >= px[0]
	assert bl[3] == 255
}
