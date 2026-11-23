module svg

import image
import math

// Each glyph is a 3x5 bitmap stored row-major as 15 characters ('1' = ink).
const glyph_patterns = {
	'A': '010101111101101'
	'B': '110101110101110'
	'C': '011100100100011'
	'D': '110101101101110'
	'E': '111100110100111'
	'F': '111100110100100'
	'G': '011100101101011'
	'H': '101101111101101'
	'I': '111010010010111'
	'J': '001001001101010'
	'K': '101101110101101'
	'L': '100100100100111'
	'M': '101111111101101'
	'N': '101111111111101'
	'O': '010101101101010'
	'P': '110101110100100'
	'Q': '010101101111011'
	'R': '110101110101101'
	'S': '011100010001110'
	'T': '111010010010010'
	'U': '101101101101111'
	'V': '101101101101010'
	'W': '101101111111101'
	'X': '101101010101101'
	'Y': '101101010010010'
	'Z': '111001010100111'
	'0': '111101101101111'
	'1': '010110010010111'
	'2': '110001010100111'
	'3': '111001011001111'
	'4': '101101111001001'
	'5': '111100111001111'
	'6': '111100111101111'
	'7': '111001010010010'
	'8': '111101111101111'
	'9': '111101111001111'
	' ': '000000000000000'
	'.': '000000000000010'
	',': '000000000010010'
	'!': '010010010000010'
	'?': '110001010000010'
	'-': '000000111000000'
	':': '000010000010000'
}

// text_bounds estimates the width and height of a text item using the
// built-in 3x5 font. It is used for layout and for rasterizer fallbacks.
pub fn text_bounds(text string, size f32) (f32, f32) {
	scale := math.max(1, int(size / 5))
	advance := f32(3 * scale + scale)
	return f32(text.len) * advance, 5 * f32(scale)
}

// default_text_renderer is a compact pure-V bitmap text renderer. It supports
// ASCII uppercase letters, digits and common punctuation; other code points
// are drawn as a replacement box.
pub fn default_text_renderer(mut img image.RGBA, item TextItem, pos Vec2, size f32, color Color, transform Transform) {
	scale := math.max(1, int(size / 5))
	advance := 3 * scale + scale
	angle := math.atan2(transform.b, transform.a)
	co := f32(math.cos(angle))
	si := f32(math.sin(angle))
	for i in 0 .. item.text.len {
		c := item.text[i..i + 1].to_upper()
		pattern := if c in glyph_patterns { glyph_patterns[c] } else { '111101101101111' }
		for row in 0 .. 5 {
			for col in 0 .. 3 {
				if pattern[row * 3 + col] == `1` {
					lx := f32(i * advance + col * scale)
					ly := f32(row * scale)
					px := int(pos.x + lx * co - ly * si)
					py := int(pos.y + lx * si + ly * co) - 4 * scale
					fill_text_pixel(mut img, px, py, scale, color)
				}
			}
		}
	}
}

fn fill_text_pixel(mut img image.RGBA, x int, y int, scale int, c Color) {
	for dy in 0 .. scale {
		for dx in 0 .. scale {
			px := x + dx
			py := y + dy
			if px < 0 || py < 0 || px >= img.rect.max.x || py >= img.rect.max.y {
				continue
			}
			i := py * img.stride + px * 4
			img.pix[i] = c.r
			img.pix[i + 1] = c.g
			img.pix[i + 2] = c.b
			if c.a > 0 {
				img.pix[i + 3] = c.a
			}
		}
	}
}
