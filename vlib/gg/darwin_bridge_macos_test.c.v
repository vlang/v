module gg

import macos

fn C.CGColorSpaceCreateDeviceRGB() voidptr
fn C.CGColorSpaceRelease(color_space voidptr)
fn C.CGBitmapContextCreate(data voidptr, width uint, height uint, bits_per_component uint, bytes_per_row uint, color_space voidptr, bitmap_info u32) voidptr
fn C.CGContextRelease(context voidptr)
fn C.CGContextSetRGBFillColor(context voidptr, red f64, green f64, blue f64, alpha f64)
fn C.CGContextSetRGBStrokeColor(context voidptr, red f64, green f64, blue f64, alpha f64)

fn test_darwin_rectangle_bridge_uses_sdk_geometry() {
	assert C.gg_darwin_mouse_in_rect(2, 3, 0, 0, 5, 6, false)
	assert !C.gg_darwin_mouse_in_rect(7, 3, 0, 0, 5, 6, false)
	mut pixels := [64]u8{}
	color_space := C.CGColorSpaceCreateDeviceRGB()
	defer {
		C.CGColorSpaceRelease(color_space)
	}
	context := C.CGBitmapContextCreate(&pixels[0], 4, 4, 8, 16, color_space, 1)
	assert context != unsafe { nil }
	defer {
		C.CGContextRelease(context)
	}
	C.CGContextSetRGBFillColor(context, 1, 0, 0, 1)
	C.gg_darwin_fill_rect(context, 1, 1, 2, 2)
	assert pixels[..4] == [u8(0), 0, 0, 0]
	assert pixels[20..24] != [u8(0), 0, 0, 0]
}

fn test_darwin_ellipse_bridges_use_sdk_geometry() {
	mut pixels := [2304]u8{}
	color_space := C.CGColorSpaceCreateDeviceRGB()
	defer {
		C.CGColorSpaceRelease(color_space)
	}
	context := C.CGBitmapContextCreate(&pixels[0], 32, 18, 8, 128, color_space, 1)
	assert context != unsafe { nil }
	defer {
		C.CGContextRelease(context)
	}
	C.CGContextSetRGBFillColor(context, 1, 0, 0, 1)
	C.gg_darwin_fill_ellipse(context, 2, 2, 12, 8)
	C.CGContextSetRGBStrokeColor(context, 0, 0, 1, 1)
	C.gg_darwin_stroke_ellipse(context, 20, 2, 8, 12)
	assert pixels[..4] == [u8(0), 0, 0, 0]
	// Bitmap rows run opposite to CoreGraphics drawing coordinates.
	fill_center := ((17 - 6) * 32 + 8) * 4
	assert pixels[fill_center..fill_center + 4] != [u8(0), 0, 0, 0]
	fill_corner := ((17 - 2) * 32 + 2) * 4
	assert pixels[fill_corner..fill_corner + 4] == [u8(0), 0, 0, 0]
	stroke_center := ((17 - 8) * 32 + 24) * 4
	assert pixels[stroke_center..stroke_center + 4] == [u8(0), 0, 0, 0]
	stroke_edge := ((17 - 8) * 32 + 20) * 4
	assert pixels[stroke_edge..stroke_edge + 4] != [u8(0), 0, 0, 0]
}

fn test_darwin_attribute_bridge_returns_objects() {
	for key in [C.gg_darwin_color_attribute(), C.gg_darwin_font_attribute(), C.gg_darwin_device_size()] {
		assert key != unsafe { nil }
		assert macos.msg_u64(key, 'length') > 0
	}
}

fn test_darwin_diagonal_lines_stay_on_the_native_surface() {
	mut pixels := [4096]u8{}
	color_space := C.CGColorSpaceCreateDeviceRGB()
	defer { C.CGColorSpaceRelease(color_space) }
	context := C.CGBitmapContextCreate(&pixels[0], 32, 32, 8, 128, color_space, 1)
	assert context != unsafe { nil }
	defer { C.CGContextRelease(context) }
	C.CGContextSetLineWidth(context, 7)
	C.CGContextSetRGBStrokeColor(context, 0, 0, 1, 1)
	for reversed in [false, true] {
		for i in 0 .. pixels.len {
			pixels[i] = 0
		}
		if reversed {
			darwin_stroke_line(context, 24, 24, 4, 4, red)
		} else {
			darwin_stroke_line(context, 4, 4, 24, 24, red)
		}
		center := ((31 - 14) * 32 + 14) * 4
		corner := ((31 - 4) * 32 + 24) * 4
		assert pixels[center..center + 4] != [u8(0), 0, 0, 0]
		assert pixels[corner..corner + 4] == [u8(0), 0, 0, 0]
		// A subsequent wide blue stroke checks that both width and color were restored.
		C.CGContextBeginPath(context)
		C.CGContextMoveToPoint(context, 4, 28)
		C.CGContextAddLineToPoint(context, 24, 28)
		C.CGContextStrokePath(context)
		restored := ((31 - 25) * 32 + 14) * 4
		assert pixels[restored + 2] > 0
		assert pixels[restored] == 0
	}
}
