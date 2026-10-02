module sapp

// Screenshot is the result of reading the framebuffer back.
//
// `width`, `height` and `size` are `pub`: `screenshot_window` is public and returns one of these,
// and a struct whose fields are all module private leaves that call with nothing the caller can
// show. Every other struct in sokol declares its dimensions this way.
//
// `pixels` stays private on purpose. It is a `malloc`ed buffer with `free` and `destroy` methods on
// this struct, so making it writable would let a caller swap the pointer and have `free` release
// storage this module does not own. Use `pixels()` to read it.
@[heap]
pub struct Screenshot {
pub:
	width  int
	height int
	size   int
mut:
	pixels &u8 = unsafe { nil }
}

// pixels returns the start of the RGBA pixel data, or nil for a Screenshot that has already been
// freed. The buffer stays owned by the Screenshot and is released by `free` or `destroy`.
pub fn (ss &Screenshot) pixels() &u8 {
	return ss.pixels
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
