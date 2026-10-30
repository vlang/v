module gg

fn test_pixel_scissor_uses_fractional_window_edges() {
	menu_top := 26.0
	clip := pixel_scissor_rect(0, menu_top, 958.0 / 1.5, 1078.0 / 1.5 - menu_top, 1.5)
	assert clip == PixelScissorRect{
		x:      0
		y:      39
		width:  958
		height: 1039
	}
}

fn test_pixel_scissor_rounds_fractional_edges_outward() {
	clip := pixel_scissor_rect(447.1, 718.1, 958.0 / 1.5 - 447.1, 1078.0 / 1.5 - 718.1,
		1.5)
	assert clip == PixelScissorRect{
		x:      670
		y:      1077
		width:  288
		height: 1
	}
}
