module sapp

import os
import sokol.gfx

// screenshot takes a screenshot of the current window.
[inline]
pub fn screenshot(path string) ? {
	if !path.ends_with('.ppm') {
		return error(@MOD + '.' + @FN + ' currently only supports .ppm files.')
	}

	w := width()
	h := height()

	size := w*h*4
	mut pixels := []byte{len: size}

	C.v_sapp_gl_read_rgba_pixels(0, 0, w, h, pixels.data)

	mut f_out := os.create(path) ?
	f_out.writeln('P3') ?
	f_out.writeln('$w $h') ?
	f_out.writeln('255') ?
	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			r := int(pixels[i*w*4 + j*4])
			g := int(pixels[i*w*4 + j*4 +1])
			b := int(pixels[i*w*4 + j*4+2])
			//println('$r $g $b')
			f_out.write_string('$r $g $b ') ?
		}
	}
	f_out.close()

	//pixels.clear()
	unsafe {
		pixels.free()
	}

}
