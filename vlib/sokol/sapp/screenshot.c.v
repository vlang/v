module sapp

@[heap]
pub struct Screenshot {
	width  int
	height int
	size   int
mut:
	pixels &u8 = unsafe { nil }
}

@[manualfree]
fn screenshot_window_checked() !&Screenshot {
	img_width := width()
	img_height := height()
	img_size := img_width * img_height * 4
	img_pixels := unsafe { &u8(malloc(img_size)) }
	readback_status := C.v_sapp_read_rgba_pixels(0, 0, img_width, img_height, img_pixels)
	if readback_status != 0 {
		unsafe { free(img_pixels) }
		return error('sokol.sapp screenshot readback failed with code ${readback_status}')
	}
	return &Screenshot{
		width:  img_width
		height: img_height
		size:   img_size
		pixels: img_pixels
	}
}

// screenshot_window captures the current backend framebuffer/window contents at call time.
@[manualfree]
pub fn screenshot_window() &Screenshot {
	return screenshot_window_checked() or { panic(err) }
}

// free - free *only* the Screenshot pixels.
@[unsafe]
pub fn (mut ss Screenshot) free() {
	unsafe {
		free(ss.pixels)
		ss.pixels = &u8(nil)
	}
}

// destroy - free the Screenshot pixels,
// then free the screenshot data structure itself.
@[unsafe]
pub fn (mut ss Screenshot) destroy() {
	unsafe { ss.free() }
	unsafe { free(ss) }
}
