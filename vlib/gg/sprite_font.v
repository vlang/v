// Bitmap ("sprite") font support for gg.
//
// A SpriteFont is loaded from an AngelCode BMFont `.fnt` text file and stores
// the source rectangle of every glyph in the associated texture atlas. The
// glyph rectangles can be used directly with gg's image drawing API (see
// `glyph_rect` and `draw_sprite_text`), or queried for text measurements with
// `measure_sprite_text`.

module gg

import os

// SpriteGlyph describes one glyph inside a BMFont texture atlas.
//
// `x`, `y`, `width` and `height` are the source rectangle in the atlas.
// `xoffset`/`yoffset` position the glyph relative to the text pen, and
// `xadvance` is how far the pen moves after drawing the glyph. All values are
// in the unscaled units stored in the BMFont file.
pub struct SpriteGlyph {
pub:
	id       rune
	x        int
	y        int
	width    int
	height   int
	xoffset  int
	yoffset  int
	xadvance int
}

// SpriteFont is a parsed BMFont atlas.
//
// `glyphs` maps a codepoint to its SpriteGlyph, `line_height` and
// `texture_path` come from the `common` and `page` lines of the font file.
// Set `image` to the loaded atlas image (for example with
// `ctx.create_image(font.texture_path)`) before calling `draw_sprite_text`.
pub struct SpriteFont {
pub mut:
	glyphs       map[rune]SpriteGlyph
	line_height  int
	texture_path string
	image        &Image = unsafe { nil }
}

// load_bmfont parses an AngelCode BMFont text file at `path` and returns the
// resulting SpriteFont. The `common` line provides the line height, `page`
// lines provide the atlas texture path, and every `char` line provides one
// glyph. Files using binary BMFont data are not supported.
pub fn load_bmfont(path string) !SpriteFont {
	data := os.read_file(path)!
	mut font := SpriteFont{}
	for raw in data.split_into_lines() {
		line := raw.trim_space()
		if line.len == 0 {
			continue
		}
		fields := line.fields()
		if fields.len == 0 {
			continue
		}
		match fields[0] {
			'common' {
				font.line_height = fnt_int(line, 'lineHeight')
			}
			'page' {
				if font.texture_path == '' {
					font.texture_path = fnt_attr(line, 'file') or { '' }
				}
			}
			'char' {
				id := fnt_int(line, 'id')
				if id > 0 {
					font.glyphs[rune(id)] = SpriteGlyph{
						id:       rune(id)
						x:        fnt_int(line, 'x')
						y:        fnt_int(line, 'y')
						width:    fnt_int(line, 'width')
						height:   fnt_int(line, 'height')
						xoffset:  fnt_int(line, 'xoffset')
						yoffset:  fnt_int(line, 'yoffset')
						xadvance: fnt_int(line, 'xadvance')
					}
				}
			}
			else {}
		}
	}
	if font.glyphs.len == 0 {
		return error('gg: no glyphs found in BMFont file "${path}"')
	}
	return font
}

// measure_sprite_text returns the width and height (in unscaled font units)
// needed to draw `text` with `font`. Newlines advance vertically and reset
// the horizontal pen. Missing glyphs advance by half a line height.
pub fn measure_sprite_text(font &SpriteFont, text string) (f32, f32) {
	line_h := f32(font.line_height)
	mut pen_x := f32(0)
	mut pen_y := f32(0)
	mut max_bottom := line_h
	for ch in text {
		if ch == `\n` {
			pen_y += line_h
			pen_x = 0
			continue
		}
		if g := font.glyphs[ch] {
			pen_x += f32(g.xadvance)
			bottom := f32(g.yoffset + g.height)
			if bottom > max_bottom {
				max_bottom = bottom
			}
		} else {
			pen_x += line_h * 0.5
		}
	}
	return pen_x, pen_y + max_bottom
}

// draw_sprite_text draws `text` at screen position (`x`, `y`) using `font`,
// scaled by `scale` and tinted with `color`. The atlas image must be assigned
// to `font.image` first. Each glyph is drawn with `draw_image_with_config`,
// using its atlas rectangle as the source (`part_rect`) and its offset plus
// advance for the destination (`img_rect`).
pub fn draw_sprite_text(ctx &Context, font &SpriteFont, text string, x int, y int, scale f32, color Color) {
	img := font.image
	if img == unsafe { nil } || !img.ok {
		return
	}
	line_h := f32(font.line_height) * scale
	mut pen_x := f32(x)
	mut pen_y := f32(y)
	for ch in text {
		if ch == `\n` {
			pen_y += line_h
			pen_x = f32(x)
			continue
		}
		if g := font.glyphs[ch] {
			if g.width > 0 && g.height > 0 {
				ctx.draw_image_with_config(
					img:       img
					img_rect:  Rect{pen_x + f32(g.xoffset) * scale, pen_y + f32(g.yoffset) * scale, f32(g.width) * scale, f32(g.height) * scale}
					part_rect: Rect{f32(g.x), f32(g.y), f32(g.width), f32(g.height)}
					color:     color
				)
			}
			pen_x += f32(g.xadvance) * scale
		} else {
			pen_x += line_h * 0.5
		}
	}
}

// glyph_rect returns the source rectangle `(x, y, width, height)` of `ch`
// inside the atlas, or `none` when the font has no glyph for that codepoint.
// The rectangle is in unscaled font units and can be passed as the
// `part_rect` of a gg `DrawImageConfig` (the same mechanism used internally
// by `draw_sprite_text`).
pub fn glyph_rect(font &SpriteFont, ch rune) ?(int, int, int, int) {
	g := font.glyphs[ch] or { return none }
	return g.x, g.y, g.width, g.height
}

// fnt_attr extracts the value of `key=...` from one BMFont line. Quoted
// values (such as the texture file name) are unquoted before being returned.
fn fnt_attr(line string, key string) ?string {
	needle := key + '='
	idx := line.index(needle) or { return none }
	start := idx + needle.len
	if start >= line.len {
		return ''
	}
	if line[start] == `"` {
		mut end := start + 1
		for end < line.len && line[end] != `"` {
			end++
		}
		if end >= line.len {
			return none
		}
		return line[start + 1..end]
	}
	mut end := start
	for end < line.len && line[end] != ` ` && line[end] != `\t` {
		end++
	}
	return line[start..end]
}

// fnt_int extracts an integer attribute from one BMFont line, returning 0
// when the attribute is absent or malformed.
fn fnt_int(line string, key string) int {
	val := fnt_attr(line, key) or { return 0 }
	return val.int()
}
