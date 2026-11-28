module gg

// This file holds pure-V, CPU-side operations on the raw RGBA bytes of an
// Image. gg stores its pixel data as four bytes per pixel (see image.c.v,
// where sokol uploads are always sized 4 * width * height), so all offsets
// below use a fixed stride of 4 even when nr_channels is smaller.

const max_image_bytes = 2_147_483_647 // largest length a V slice (i32) can hold

// image_valid reports whether img has pixel data that can be addressed.
fn image_valid(img &Image) bool {
	return img != unsafe { nil } && img.data != unsafe { nil } && img.width > 0 && img.height > 0
}

// write_rgba stores one RGBA pixel at byte index i of d.
fn write_rgba(d &u8, i int, c Color) {
	unsafe {
		d[i] = c.r
		d[i + 1] = c.g
		d[i + 2] = c.b
		d[i + 3] = c.a
	}
}

// image_draw_pixel sets the pixel at (x, y) to c. Out-of-bounds coordinates,
// empty images and nil pixel data are ignored.
pub fn image_draw_pixel(img &Image, x int, y int, c Color) {
	if !image_valid(img) || img.nr_channels != 4 || x < 0 || y < 0 || x >= img.width || y >= img.height {
		return
	}
	d := unsafe { &u8(img.data) }
	write_rgba(d, int((i64(y) * i64(img.width) + i64(x)) * 4), c)
}

// image_fill replaces every pixel of img with c. It does nothing for empty
// images or nil pixel data.
pub fn image_fill(img &Image, c Color) {
	if !image_valid(img) || img.nr_channels != 4 {
		return
	}
	d := unsafe { &u8(img.data) }
	mut i := 0
	for _ in 0 .. img.width * img.height {
		write_rgba(d, i, c)
		i += 4
	}
}

// image_draw_rect fills the rectangle (x, y, w, h) with c. The rectangle is
// clipped to the image bounds; a fully outside or empty rectangle is a no-op.
pub fn image_draw_rect(img &Image, x int, y int, w int, h int, c Color) {
	if !image_valid(img) || img.nr_channels != 4 || w <= 0 || h <= 0 {
		return
	}
	// Compute clip bounds in i64 so x+w/y+h cannot overflow an int.
	mut x0 := i64(0)
	if x > 0 {
		x0 = i64(x)
	}
	mut y0 := i64(0)
	if y > 0 {
		y0 = i64(y)
	}
	mut x1 := i64(img.width)
	if i64(x) + i64(w) < x1 {
		x1 = i64(x) + i64(w)
	}
	mut y1 := i64(img.height)
	if i64(y) + i64(h) < y1 {
		y1 = i64(y) + i64(h)
	}
	if x1 <= x0 || y1 <= y0 {
		return
	}
	d := unsafe { &u8(img.data) }
	for yy := y0; yy < y1; yy++ {
		base := (yy * i64(img.width) + x0) * 4
		for xx := x0; xx < x1; xx++ {
			write_rgba(d, int(base + (xx - x0) * 4), c)
		}
	}
}

// image_flip_horizontal mirrors img left-to-right in place.
pub fn image_flip_horizontal(img &Image) {
	if !image_valid(img) || img.nr_channels != 4 {
		return
	}
	d := unsafe { &u8(img.data) }
	for y in 0 .. img.height {
		row := i64(y) * i64(img.width) * 4
		for x in 0 .. img.width / 2 {
			li := int(row + i64(x) * 4)
			ri := int(row + i64(img.width - 1 - x) * 4)
			for k in 0 .. 4 {
				unsafe {
					tmp := d[li + k]
					d[li + k] = d[ri + k]
					d[ri + k] = tmp
				}
			}
		}
	}
}

// image_flip_vertical mirrors img top-to-bottom in place.
pub fn image_flip_vertical(img &Image) {
	if !image_valid(img) || img.nr_channels != 4 {
		return
	}
	d := unsafe { &u8(img.data) }
	row_bytes := img.width * 4
	for y in 0 .. img.height / 2 {
		top := i64(y) * i64(img.width) * 4
		bottom := i64(img.height - 1 - y) * i64(img.width) * 4
		for i in 0 .. row_bytes {
			ti := int(top + i64(i))
			bi := int(bottom + i64(i))
			unsafe {
				tmp := d[ti]
				d[ti] = d[bi]
				d[bi] = tmp
			}
		}
	}
}

// image_rotate_90 rotates img 90 degrees clockwise. Because width and height
// are exchanged, a new RGBA buffer is installed on img (the previous buffer
// is owned by the original loader and is intentionally not freed here).
pub fn image_rotate_90(mut img &Image) {
	if !image_valid(img) || img.nr_channels != 4 {
		return
	}
	src := unsafe { &u8(img.data) }
	w := img.width
	h := img.height
	mut out := []u8{len: w * h * 4}
	for y in 0 .. h {
		for x in 0 .. w {
			s := int((i64(y) * i64(w) + i64(x)) * 4)
			// Source (x, y) of a WxH image maps to (H-1-y, x) of a HxW one.
			d := int((i64(x) * i64(h) + i64(h - 1 - y)) * 4)
			for k in 0 .. 4 {
				out[d + k] = unsafe { src[s + k] }
			}
		}
	}
	img.data = out.data
	img.width = h
	img.height = w
}

// image_crop returns a new Image containing the part of img inside the
// rectangle (x, y, w, h). The rectangle is clipped to the source bounds; the
// function returns an error when the clipped rectangle is empty.
pub fn image_crop(img &Image, x int, y int, w int, h int) !Image {
	if !image_valid(img) {
		return error('gg.image_crop: invalid source image')
	}
	if img.nr_channels != 4 {
		return error('gg.image_crop: expected 4-channel RGBA data')
	}
	if w <= 0 || h <= 0 {
		return error('gg.image_crop: empty crop size ${w}x${h}')
	}
	mut x0 := i64(0)
	if x > 0 {
		x0 = i64(x)
	}
	mut y0 := i64(0)
	if y > 0 {
		y0 = i64(y)
	}
	mut x1 := i64(img.width)
	if i64(x) + i64(w) < x1 {
		x1 = i64(x) + i64(w)
	}
	mut y1 := i64(img.height)
	if i64(y) + i64(h) < y1 {
		y1 = i64(y) + i64(h)
	}
	if x1 <= x0 || y1 <= y0 {
		return error('gg.image_crop: crop rectangle is outside the image')
	}
	cw := int(x1 - x0)
	ch := int(y1 - y0)
	nbytes := i64(cw) * i64(ch) * 4
	if nbytes > max_image_bytes {
		return error('gg.image_crop: resulting image is too large')
	}
	mut out := []u8{len: int(nbytes)}
	src := unsafe { &u8(img.data) }
	for yy := 0; yy < ch; yy++ {
		src_row := (y0 + i64(yy)) * i64(img.width)
		dst_row := i64(yy) * i64(cw)
		for xx := 0; xx < cw; xx++ {
			s := int((src_row + x0 + i64(xx)) * 4)
			d := int((dst_row + i64(xx)) * 4)
			for k in 0 .. 4 {
				out[d + k] = unsafe { src[s + k] }
			}
		}
	}
	return Image{
		width:       cw
		height:      ch
		nr_channels: 4
		ok:          true
		data:        out.data
	}
}

// image_resize returns a new Image scaled to (w, h) with nearest-neighbour
// sampling. stbi.resize_uint8 is not used here: it operates on stbi.Image and
// allocates with the C allocator, while these helpers stay purely in V over
// gg.Image.data. Empty images and non-positive sizes return an error.
pub fn image_resize(img &Image, w int, h int) !Image {
	if !image_valid(img) {
		return error('gg.image_resize: invalid source image')
	}
	if img.nr_channels != 4 {
		return error('gg.image_resize: expected 4-channel RGBA data')
	}
	if w <= 0 || h <= 0 {
		return error('gg.image_resize: empty target size ${w}x${h}')
	}
	nbytes := i64(w) * i64(h) * 4
	if nbytes > max_image_bytes {
		return error('gg.image_resize: resulting image is too large')
	}
	mut out := []u8{len: int(nbytes)}
	src := unsafe { &u8(img.data) }
	for dy in 0 .. h {
		mut sy := int(i64(dy) * i64(img.height) / i64(h))
		if sy >= img.height {
			sy = img.height - 1
		}
		for dx in 0 .. w {
			mut sx := int(i64(dx) * i64(img.width) / i64(w))
			if sx >= img.width {
				sx = img.width - 1
			}
			s := int((i64(sy) * i64(img.width) + i64(sx)) * 4)
			d := int((i64(dy) * i64(w) + i64(dx)) * 4)
			for k in 0 .. 4 {
				out[d + k] = unsafe { src[s + k] }
			}
		}
	}
	return Image{
		width:       w
		height:      h
		nr_channels: 4
		ok:          true
		data:        out.data
	}
}
