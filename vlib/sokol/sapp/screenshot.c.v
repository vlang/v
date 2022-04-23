module sapp

[heap]
pub struct Screenshot {
	width  int
	height int
	size   int
mut:
	pixels &u8
}

[manualfree]
pub fn screenshot_window() &Screenshot {
	img_width := width()
	img_height := height()
	img_size := img_width * img_height * 4
	img_pixels := unsafe { &u8(malloc(img_size)) }
	C.v_sapp_gl_read_rgba_pixels(0, 0, img_width, img_height, img_pixels)
	return &Screenshot{
		width: img_width
		height: img_height
		size: img_size
		pixels: img_pixels
	}
}

// free - free *only* the Screenshot pixels.
[unsafe]
pub fn (mut ss Screenshot) free() {
	unsafe {
		free(ss.pixels)
		ss.pixels = &u8(0)
	}
}

// destroy - free the Screenshot pixels,
// then free the screenshot data structure itself.
[unsafe]
pub fn (mut ss Screenshot) destroy() {
	unsafe { ss.free() }
	unsafe { free(ss) }
}
