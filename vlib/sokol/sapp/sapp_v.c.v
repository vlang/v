module sapp

import os
import stbi

// v_sapp_gl_read_rgba_pixels reads pixles from the OpenGL buffer into `pixels`.
fn C.v_sapp_gl_read_rgba_pixels(x int, y int, width int, height int, pixels charptr)

// screenshot takes a screenshot of the current window and
// saves it to `path`. The format is inferred from the extension
// of the file name in `path`.
//
// Supported formats are: `.png`, `.ppm`.
pub fn screenshot(path string) ? {
	match os.file_ext(path) {
		'.png' {
			return screenshot_png(path)
		}
		'.ppm' {
			return screenshot_ppm(path)
		}
		else {
			return error(@MOD + '.' + @FN + ' currently only supports .png and .ppm files.')
		}
	}
}

// screenshot_ppm takes a screenshot of the current window and
// saves it to `path` as a .ppm file.
[manualfree]
pub fn screenshot_ppm(path string) ? {
	ss := screenshot_window()
	write_rgba_to_ppm(path, ss.width, ss.height, 4, ss.pixels) ?
	unsafe { ss.destroy() }
}

// screenshot_png takes a screenshot of the current window and
// saves it to `path` as a .png file.
[manualfree]
pub fn screenshot_png(path string) ? {
	ss := screenshot_window()
	stbi.set_flip_vertically_on_write(true)
	stbi.stbi_write_png(path, ss.width, ss.height, 4, ss.pixels, ss.width * 4) ?
	unsafe { ss.destroy() }
}

// write_rgba_to_ppm writes `pixels` data in RGBA format to PPM3 format.
fn write_rgba_to_ppm(path string, w int, h int, components int, pixels &u8) ? {
	mut f_out := os.create(path) ?
	defer {
		f_out.close()
	}
	f_out.writeln('P3') ?
	f_out.writeln('$w $h') ?
	f_out.writeln('255') ?
	for i := h - 1; i >= 0; i-- {
		for j := 0; j < w; j++ {
			idx := i * w * components + j * components
			unsafe {
				r := int(pixels[idx])
				g := int(pixels[idx + 1])
				b := int(pixels[idx + 2])
				f_out.write_string('$r $g $b ') ?
			}
		}
	}
}
