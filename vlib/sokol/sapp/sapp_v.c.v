module sapp

import os

// v_sapp_gl_read_rgba_pixels reads pixles from the OpenGL buffer into `pixels`.
fn C.v_sapp_gl_read_rgba_pixels(x int, y int, width int, height int, pixels charptr)

// screenshot takes a screenshot of the current window.
[inline]
pub fn screenshot(path string) ? {
	if !path.ends_with('.ppm') {
		return error(@MOD + '.' + @FN + ' currently only supports .ppm files.')
	}

	w := width()
	h := height()

	size := w * h * 4 //
	mut pixels := []byte{len: size, init: 0}

	C.v_sapp_gl_read_rgba_pixels(0, 0, w, h, pixels.data)

	// TODO use separate thread for writing the data
	// TODO use stbi to support more formats
	// stbi.write_png(path, w, h, components, pixels.data, 3 * w)
	// stbi.write_tga(path, w, h, components, pixels.data)
	write_rgba_to_ppm(path, w, h, 4, pixels) ?

	unsafe {
		pixels.free()
	}
}

// write_rgba_to_ppm writes `pixels` data in RGBA format to PPM3 format.
fn write_rgba_to_ppm(path string, w int, h int, components int, pixels []byte) ? {
	mut f_out := os.create(path) ?
	f_out.writeln('P3') ?
	f_out.writeln('$w $h') ?
	f_out.writeln('255') ?
	for i := h - 1; i >= 0; i-- {
		for j := 0; j < w; j++ {
			idx := i * w * components + j * components
			r := int(pixels[idx])
			g := int(pixels[idx + 1])
			b := int(pixels[idx + 2])
			f_out.write_string('$r $g $b ') ?
		}
	}
	f_out.close()
}
