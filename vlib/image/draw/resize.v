module draw

import image
import image.color
import math

// Interpolation chooses the sampling method used by resize.
pub enum Interpolation {
	nearest_neighbor
	bilinear
}

// ResizeOptions selects resize interpolation, defaulting to bilinear.
@[params]
pub struct ResizeOptions {
pub:
	interpolation Interpolation = .bilinear
}

// resize scales src to width by height, preserving alpha and respecting source bounds.
pub fn resize(src image.Image, width int, height int, options ResizeOptions) !image.NRGBA {
	bounds := src.bounds()
	sw := bounds.dx()
	sh := bounds.dy()
	if width <= 0 || height <= 0 || sw <= 0 || sh <= 0
		|| width > max_i32 / 4 / height {
		return error('image.draw: invalid or oversized resize dimensions')
	}
	mut dst := image.new_nrgba(image.rect(0, 0, width, height))
	for y in 0 .. height {
		for x in 0 .. width {
			if options.interpolation == .nearest_neighbor {
				sx := bounds.min.x + int(i64(x) * sw / width)
				sy := bounds.min.y + int(i64(y) * sh / height)
				dst.set(x, y, src.at(sx, sy))
				continue
			}
			fx := (f64(x) + 0.5) * sw / width - 0.5
			fy := (f64(y) + 0.5) * sh / height - 0.5
			x0 := int(math.floor(fx))
			y0 := int(math.floor(fy))
			wx := fx - x0
			wy := fy - y0
			mut channels := [f64(0), 0, 0, 0]
			for dy in 0 .. 2 {
				for dx in 0 .. 2 {
					sx := bounds.min.x + math.max(0, math.min(sw - 1, x0 + dx))
					sy := bounds.min.y + math.max(0, math.min(sh - 1, y0 + dy))
					weight := (if dx == 0 { 1 - wx } else { wx }) *
						(if dy == 0 { 1 - wy } else { wy })
					r, g, b, a := src.at(sx, sy).rgba()
					channels[0] += r * weight
					channels[1] += g * weight
					channels[2] += b * weight
					channels[3] += a * weight
				}
			}
			// Interpolate premultiplied channels so transparent pixels do not cause fringes.
			dst.set(x, y, color.RGBA64{
				r: u16(math.round(channels[0]))
				g: u16(math.round(channels[1]))
				b: u16(math.round(channels[2]))
				a: u16(math.round(channels[3]))
			})
		}
	}
	return dst
}
