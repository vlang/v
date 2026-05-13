module gg

import math

const rect_empty_bounds_tolerance = f32(0.001)

fn rect_empty_bounds_match(a f32, b f32) bool {
	return math.abs(a - b) <= rect_empty_bounds_tolerance
}

fn test_rect_empty_screen_bounds_keep_the_top_left_corner_aligned() {
	tleft_x, tleft_y, bright_x, bright_y := rect_empty_screen_bounds(1.0, 12, 34, 56, 78)
	assert rect_empty_bounds_match(tleft_x, 12.1)
	assert rect_empty_bounds_match(tleft_y, 34.1)
	assert rect_empty_bounds_match(bright_x, 67.9)
	assert rect_empty_bounds_match(bright_y, 111.9)
}

fn test_rect_empty_screen_bounds_apply_scale_before_the_pixel_offsets() {
	tleft_x, tleft_y, bright_x, bright_y := rect_empty_screen_bounds(2.0, 3, 4, 5, 6)
	assert rect_empty_bounds_match(tleft_x, 6.1)
	assert rect_empty_bounds_match(tleft_y, 8.1)
	assert rect_empty_bounds_match(bright_x, 15.9)
	assert rect_empty_bounds_match(bright_y, 19.9)
}
