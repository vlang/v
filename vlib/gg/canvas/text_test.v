module canvas

import crypto.sha256
import os

// The expected hashes were captured from LibWeb's Skia (m144, CPU raster)
// drawing the same runs into BGRA8 premultiplied sRGB surfaces with the
// Chromium text settings Vox uses (subpixel positioning, linear metrics,
// zero contrast). The glyph masks come from CoreGraphics, so the hashes are
// specific to the macOS fonts and CoreGraphics of the capturing host.

struct TextCase {
	name       string
	font       string
	size       f32
	antialias  bool // font-smoothing: antialiased (unhinted, gamma 1)
	width      int
	height     int
	background u32 // premultiplied 0xAARRGGBB, 0 for transparent
	matrix     [6]f32
	color      [4]u8
	origin     Point
	glyphs     []u16
	positions  []Point
	sha256     string
}

const identity_6 = [f32(1), 0, 0, 1, 0, 0]!

fn text_cases() []TextCase {
	arial := '/System/Library/Fonts/Supplemental/Arial.ttf'
	return [
		TextCase{
			name:      'black on transparent'
			font:      arial
			size:      16
			width:     120
			height:    40
			matrix:    identity_6
			color:     [u8(0), 0, 0, 255]!
			origin:    Point{4.25, 24}
			glyphs:    [u16(36), 37, 38, 39, 40]
			positions: [Point{0, 0}, Point{10.3, 0}, Point{21.7, 0}, Point{33.1, 0}, Point{44.9, 0}]
			sha256:    'a9ba124bbc22bb2a0c111dda5bff4850ce64112355f59304b2e8ea25c915cf68'
		},
		TextCase{
			name:       'opaque color on white'
			font:       arial
			size:       16
			width:      120
			height:     40
			background: 0xffffffff
			matrix:     identity_6
			color:      [u8(20), 60, 200, 255]!
			origin:     Point{4.6, 24.3}
			glyphs:     [u16(68), 69, 70, 71]
			positions:  [Point{0, 0}, Point{9.4, 0}, Point{18.2, 0}, Point{27.7, 0}]
			sha256:     '4f3d855ba0ebbfb2322961b4d65a95582f5e5d20a3ff934ada693fc754e80dd9'
		},
		TextCase{
			name:       'translucent color'
			font:       arial
			size:       13
			width:      120
			height:     40
			background: 0xffdce6f0
			matrix:     identity_6
			color:      [u8(200), 30, 30, 128]!
			origin:     Point{2.1, 20.5}
			glyphs:     [u16(68), 69, 70, 71, 72]
			positions:  [Point{0, 0}, Point{9.4, 0}, Point{18.2, 0}, Point{27.7, 0}, Point{37.1, 0}]
			sha256:     '510e57e9444e1c30173027a189cfbee7b91a239b2af29033aabd0516f6ddfff8'
		},
		TextCase{
			name:       'unhinted antialiased'
			font:       '/System/Library/Fonts/Supplemental/Georgia.ttf'
			size:       15
			antialias:  true
			width:      120
			height:     40
			background: 0xffffffff
			matrix:     identity_6
			color:      [u8(0), 0, 0, 255]!
			origin:     Point{3.3, 25}
			glyphs:     [u16(40), 41, 42]
			positions:  [Point{0, 0}, Point{9.4, 0}, Point{18.2, 0}]
			sha256:     '7287c7cf944fbf639b14dae4bafb5fee6e7623a83ebc520a65ee0c6ad682e310'
		},
		TextCase{
			name:       'rotated translucent'
			font:       '/System/Library/Fonts/Geneva.ttf'
			size:       15.14
			width:      160
			height:     64
			background: 0xff9211cc
			matrix:     [f32(0.991236), -0.1321, 0.1321, 0.991236, 8.841, 2.488]!
			color:      [u8(125), 140, 70, 175]!
			origin:     Point{11.856, 23.959}
			glyphs:     [u16(25), 27, 177, 185, 110, 127, 21]
			positions:  [Point{0, 0.9395}, Point{9.9026, 0}, Point{18.4504, 0}, Point{29.5345, 0},
				Point{34.6033, 0}, Point{42.1437, 0}, Point{51.343, 0}]
			sha256:     '89f87e394683aee0d4f21f00fe3b5a7699d906ad2636e499a3fdd99e91079b16'
		},
	]
}

fn test_glyph_runs_match_skia() {
	$if !macos {
		return
	}
	for c in text_cases() {
		if !os.exists(c.font) {
			eprintln('skipping ${c.name}: ${c.font} is missing')
			continue
		}
		typeface := Typeface.from_file(c.font)!
		mut font := Font{
			typeface:       typeface
			size:           c.size
			subpixel:       true
			linear_metrics: true
			edging:         .subpixel_antialias
		}
		if c.antialias {
			font.edging = .antialias
			font.hinting = .none
		}
		mut surface := new_raster_surface(
			width:  c.width
			height: c.height
			props:  SurfaceProps{
				text_contrast: 0
				text_gamma:    if c.antialias { f32(1) } else { f32(0) }
			}
		)!
		surface.clear(c.background)
		m := c.matrix
		surface.set_matrix(Matrix.values([m[0], m[2], m[4], m[1], m[3], m[5], 0, 0, 1]!))
		mut positions := []Point{}
		for p in c.positions {
			positions << Point{p.x + c.origin.x, p.y + c.origin.y}
		}
		surface.draw_glyphs(font, c.glyphs, positions, Point{}, Paint.rgba8(c.color[0],
			c.color[1], c.color[2], c.color[3]))!
		mut bytes := []u8{len: surface.pixels.len * 4}
		for i, p in surface.pixels {
			bytes[4 * i] = u8(p)
			bytes[4 * i + 1] = u8(p >> 8)
			bytes[4 * i + 2] = u8(p >> 16)
			bytes[4 * i + 3] = u8(p >> 24)
		}
		assert sha256.sum(bytes).hex() == c.sha256, c.name
	}
}

fn test_gradient_text_is_rejected_without_changing_pixels() {
	$if !macos {
		return
	}
	face := Typeface.from_file('/System/Library/Fonts/Supplemental/Arial.ttf')!
	mut s := new_raster_surface(width: 40, height: 40)!
	s.clear(0xff123456)
	paint := Paint{
		gradient: Gradient{
			p1:     Point{40, 0}
			colors: [Color4f{1, 0, 0, 1}, Color4f{0, 0, 1, 1}]
		}
		dither:   true
	}
	s.draw_glyphs(Font{ typeface: face }, [u16(36)], [Point{5, 25}], Point{}, paint) or {
		assert err.msg().contains('gradient text')
		assert s.pixels.all(it == 0xff123456)
		return
	}
	assert false, 'unsupported gradient text was drawn as a solid color'
}

fn test_lowp_div255_is_exact() {
	// The NEON lowp div255 rounds correctly for every product of two bytes.
	for v in u32(0) .. 255 * 255 + 1 {
		assert div255(v) == (v + 127) / 255
	}
}

fn test_pipeline_color_matches_append_constant_color() {
	// uniform_color slots hold (uint16_t)(premul * 255 + 0.5) of the float color.
	c := PipelineColor.from_sk_color(0x80c81e1e)
	assert c.r == 100 && c.g == 15 && c.b == 15 && c.a == 128
	assert !c.opaque
	assert PipelineColor.from_sk_color(0xff142896).opaque
}

fn test_packed_glyph_id_subpixel_buckets() {
	// x keeps two subpixel bits, y is snapped when the baseline is horizontal.
	assert pack_glyph_id(7, Point{10.125, 3.5}, 3, 0) == u32(7) << 2
	assert pack_glyph_id(7, Point{10.375, 3.5}, 3, 0) == (u32(7) << 2 | 1)
	assert pack_glyph_id(7, Point{-0.375, 3.5}, 3, 0) == (u32(7) << 2 | 2)
	assert pack_glyph_id(7, Point{0.5, 0.75}, 3, u32(3) << 18) == (u32(7) << 2 | 2 | u32(3) << 18)
}

fn test_unsupported_text_reports_errors() {
	$if !macos {
		return
	}
	path := '/System/Library/Fonts/Supplemental/Arial.ttf'
	if !os.exists(path) {
		return
	}
	typeface := Typeface.from_file(path)!
	mut surface := new_raster_surface(width: 32, height: 32)!
	paint := Paint.rgba8(0, 0, 0, 255)
	surface.draw_glyphs(Font{ typeface: typeface, size: 300 }, [u16(36)], [Point{1, 30}],
		Point{}, paint) or { assert err.msg().contains('path rendering') }
	surface.draw_glyphs(Font{ typeface: typeface, size: 12, embolden: true }, [u16(36)],
		[Point{1, 30}], Point{}, paint) or { assert err.msg().contains('bold') }
}

fn draw_arial_sample(typeface &Typeface) ![]u32 {
	mut surface := new_raster_surface(width: 80, height: 32)!
	surface.clear(0xffffffff)
	font := Font{
		typeface:       typeface
		size:           16
		subpixel:       true
		linear_metrics: true
		edging:         .subpixel_antialias
	}
	positions := [Point{4.25, 22}, Point{14.55, 22}, Point{25.95, 22}]
	surface.draw_glyphs(font, [u16(36), 37, 38], positions, Point{}, Paint.rgba8(20, 60, 200,
		255))!
	return surface.pixels.clone()
}

fn test_typeface_from_ct_font_draws_like_its_source() {
	$if !macos {
		return
	}
	Typeface.from_ct_font(unsafe { nil }) or { assert err.msg().contains('nil CTFont') }
	path := '/System/Library/Fonts/Supplemental/Arial.ttf'
	if !os.exists(path) {
		return
	}
	source := Typeface.from_file(path)!
	wrapped := Typeface.from_ct_font(source.ct_font)!
	assert wrapped.id != source.id
	assert wrapped.has_color_glyphs == source.has_color_glyphs
	expected := draw_arial_sample(source)!
	assert expected.any(it != u32(0xffffffff))
	assert draw_arial_sample(wrapped)! == expected
}
