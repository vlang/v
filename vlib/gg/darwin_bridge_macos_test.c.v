module gg

import macos

fn C.CGColorSpaceCreateDeviceRGB() voidptr
fn C.CGColorSpaceRelease(color_space voidptr)
fn C.CGBitmapContextCreate(data voidptr, width uint, height uint, bits_per_component uint, bytes_per_row uint, color_space voidptr, bitmap_info u32) voidptr
fn C.CGContextRelease(context voidptr)
fn C.CGContextSetRGBFillColor(context voidptr, red f64, green f64, blue f64, alpha f64)

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

fn test_darwin_attribute_bridge_returns_objects() {
	for key in [C.gg_darwin_color_attribute(), C.gg_darwin_font_attribute(), C.gg_darwin_device_size()] {
		assert key != unsafe { nil }
		assert macos.msg_u64(key, 'length') > 0
	}
}
