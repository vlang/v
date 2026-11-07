// vtest build: linux
module canvas

import os

$if linux {
	#include "@DIR/testdata/text_freetype_reference.h"
}

fn C.v_canvas_test_ft_glyph_index(data &u8, length u64, codepoint u32) int
fn C.v_canvas_test_ft_glyph_count(data &u8, length u64) int
fn C.v_canvas_test_ft_render(data &u8, length u64, glyph u16, size f32, matrix &f32, x f32, y f32, monochrome int, width int, height int, background u32, color u32, pixels &u32) int

// This font is versioned with V, not selected from the host's font collection.
const linux_text_font = $embed_file('../../../examples/assets/fonts/RobotoMono-Regular.ttf').to_bytes()

fn linux_text_glyph() u16 {
	index := C.v_canvas_test_ft_glyph_index(linux_text_font.data, u64(linux_text_font.len), `A`)
	assert index > 0
	return u16(index)
}

fn linux_text_surface() !RasterSurface {
	return new_raster_surface(
		width:  72
		height: 64
		props:  SurfaceProps{ text_gamma: 1, text_contrast: 0 }
	)
}

struct LinuxTextCase {
	matrix     Matrix = Matrix.identity()
	position   Point  = Point{8.25, 36.5}
	background u32
	color      [4]u8 = [u8(20), 60, 200, 255]!
}

fn test_linux_text_pixels_match_an_independent_freetype_reference() {
	$if linux {
		mut face := Typeface.from_data(linux_text_font)!
		defer { face.free() }
		glyph := linux_text_glyph()
		cases := [
			LinuxTextCase{},
			LinuxTextCase{ background: 0xffffffff, color: [u8(0), 0, 0, 255]! },
			LinuxTextCase{ background: 0x80552810, color: [u8(40), 150, 210, 127]! },
			LinuxTextCase{ position: Point{-4.75, 9.5}, background: 0xffaabbcc },
			LinuxTextCase{ position: Point{65.25, 65.75} },
			LinuxTextCase{ matrix: Matrix.values([f32(2), 0, 0, 0, 1, 0, 0, 0, 1]!) },
			LinuxTextCase{ matrix: Matrix.values([f32(1), 0.5, 0, 0, 1, 0, 0, 0, 1]!) },
			LinuxTextCase{ matrix: Matrix.values([f32(-1), 0, 60, 0, 1, 0, 0, 0, 1]!) },
			LinuxTextCase{ matrix: Matrix.values([f32(0), -1, 56, 1, 0, 16, 0, 0, 1]!) },
		]
		for edging in [FontEdging.antialias, .subpixel_antialias, .alias] {
			for c in cases {
				mut surface := linux_text_surface()!
				surface.clear(c.background)
				surface.set_matrix(c.matrix)
				surface.draw_glyphs(Font{ typeface: face, size: 24, hinting: .none, edging: edging, subpixel: true, baseline_snap: false },
					[glyph], [c.position], Point{}, Paint.rgba8(c.color[0], c.color[1], c.color[2], c.color[3]))!
				mut expected := []u32{len: surface.pixels.len}
				defer { unsafe { expected.free() } }
				m := c.matrix.m
				linear := [m[0], m[1], m[3], m[4]]!
				x := m[0] * c.position.x + m[1] * c.position.y + m[2]
				y := m[3] * c.position.x + m[4] * c.position.y + m[5]
				color := u32(c.color[3]) << 24 | u32(c.color[0]) << 16 | u32(c.color[1]) << 8 | u32(c.color[2])
				assert C.v_canvas_test_ft_render(linux_text_font.data, u64(linux_text_font.len), glyph,
					24, &linear[0], x, y, int(edging == .alias), surface.width, surface.height,
					c.background, color, expected.data) == 0
				for i, pixel in surface.pixels {
					assert pixel == expected[i], '${edging}: pixel ${i % surface.width},${i / surface.width}, ${c}'
				}
				assert surface.pixels.any(it != c.background)
			}
		}
	}
}

fn test_linux_typeface_owns_data_and_loads_the_same_file() {
	$if linux {
		mut data := linux_text_font.clone()
		defer { unsafe { data.free() } }
		mut face := Typeface.from_data(data)!
		defer { face.free() }
		for i in 0 .. data.len {
			data[i] = 0
		}
		path := os.join_path(@VMODROOT, 'examples', 'assets', 'fonts', 'RobotoMono-Regular.ttf')
		mut from_file := Typeface.from_file(path)!
		defer { from_file.free() }
		assert face.id != from_file.id
		mut surface := linux_text_surface()!
		mut expected := linux_text_surface()!
		glyph := linux_text_glyph()
		surface.draw_glyphs(Font{ typeface: face }, [glyph], [Point{4, 24}], Point{}, Paint{})!
		expected.draw_glyphs(Font{ typeface: from_file }, [glyph], [Point{4, 24}], Point{}, Paint{})!
		assert surface.pixels == expected.pixels
		assert surface.pixels.any(it != 0)
	}
}

fn test_linux_text_preserves_sizes_below_one_point() {
	$if linux {
		mut face := Typeface.from_data(linux_text_font)!
		defer { face.free() }
		glyph := linux_text_glyph()
		position := Point{4.25, 4.5}
		mut full_size := linux_text_surface()!
		full_size.draw_glyphs(Font{ typeface: face, size: 1, hinting: .none, subpixel: true, baseline_snap: false },
			[glyph], [position], Point{}, Paint{})!
		for size in [f32(0.25), 0.5, 0.75] {
			mut surface := linux_text_surface()!
			surface.draw_glyphs(Font{ typeface: face, size: size, hinting: .none, subpixel: true, baseline_snap: false },
				[glyph], [position], Point{}, Paint{})!
			mut expected := []u32{len: surface.pixels.len}
			defer { unsafe { expected.free() } }
			// FreeType clamps a requested point size below one. The independent
			// oracle loads at one point and scales a detached outline explicitly.
			linear := [size, f32(0), 0, size]!
			assert C.v_canvas_test_ft_render(linux_text_font.data, u64(linux_text_font.len), glyph,
				1, &linear[0], position.x, position.y, 0, surface.width, surface.height, 0,
				0xff000000, expected.data) == 0
			for i, pixel in surface.pixels {
				assert pixel == expected[i], 'size ${size}, pixel ${i}'
			}
			assert surface.pixels.any(it != 0)
			assert surface.pixels != full_size.pixels
		}
	}
}

fn test_linux_text_origin_translation_and_clipping() {
	$if linux {
		mut face := Typeface.from_data(linux_text_font)!
		defer { face.free() }
		font := Font{ typeface: face, size: 20, hinting: .none, subpixel: true }
		glyph := linux_text_glyph()
		paint := Paint.rgba8(60, 120, 180, 128)
		mut direct := linux_text_surface()!
		mut origin := linux_text_surface()!
		mut transformed := linux_text_surface()!
		direct.draw_glyphs(font, [glyph, glyph], [Point{7.25, 25}, Point{23.25, 25}], Point{}, paint)!
		origin.draw_glyphs(font, [glyph, glyph], [Point{0.25, 20}, Point{16.25, 20}], Point{7, 5}, paint)!
		transformed.set_matrix(Matrix.values([f32(1), 0, 7, 0, 1, 5, 0, 0, 1]!))
		transformed.draw_glyphs(font, [glyph, glyph], [Point{0.25, 20}, Point{16.25, 20}], Point{}, paint)!
		assert direct.pixels == origin.pixels
		assert direct.pixels == transformed.pixels
		assert direct.pixels.any(it != 0)
		for pixel in direct.pixels {
			alpha := pixel >> 24
			assert pixel & 255 <= alpha
			assert (pixel >> 8) & 255 <= alpha
			assert (pixel >> 16) & 255 <= alpha
		}
		mut offscreen := linux_text_surface()!
		offscreen.clear(0xff123456)
		offscreen.draw_glyphs(font, [glyph, glyph], [Point{-1000, 20}, Point{1000, 20}], Point{}, paint)!
		assert offscreen.pixels.all(it == 0xff123456)
	}
}

fn test_linux_text_cache_is_bounded_and_faces_own_their_strikes() {
	$if linux {
		clear_text_cache()
		defer { clear_text_cache() }
		mut first := Typeface.from_data(linux_text_font)!
		mut second := Typeface.from_data(linux_text_font)!
		defer { first.free() }
		defer { second.free() }
		glyph := linux_text_glyph()
		mut surface := linux_text_surface()!
		for i in 0 .. max_text_strikes {
			surface.draw_glyphs(Font{ typeface: first, size: f32(8 + i) }, [glyph], [Point{1, 50}], Point{}, Paint{})!
		}
		assert strike_cache.strikes.len == max_text_strikes
		oldest := strike_cache.strikes[0].rec.key()
		second_key := strike_cache.strikes[1].rec.key()
		surface.draw_glyphs(Font{ typeface: first, size: 8 }, [glyph], [Point{1, 50}], Point{}, Paint{})!
		surface.draw_glyphs(Font{ typeface: second }, [glyph], [Point{1, 50}], Point{}, Paint{})!
		assert strike_cache.strikes.len == max_text_strikes
		assert strike_cache.strikes.any(it.rec.key() == oldest)
		assert !strike_cache.strikes.any(it.rec.key() == second_key)
		first.free()
		first.free()
		assert strike_cache.strikes.len == 1
		assert strike_cache.strikes[0].rec.typeface_id == second.id
		before := surface.pixels.clone()
		defer { unsafe { before.free() } }
		surface.draw_glyphs(Font{ typeface: first }, [glyph], [Point{1, 25}], Point{}, Paint{}) or {
			assert err.msg().contains('freed')
			assert surface.pixels == before
			clear_text_cache()
			clear_text_cache()
			assert strike_cache.strikes.len == 0
			surface.draw_glyphs(Font{ typeface: second }, [glyph], [Point{1, 25}], Point{}, Paint{})!
			assert strike_cache.strikes.len == 1
			return
		}
		assert false, 'drawing with a freed typeface succeeded'
	}
}

fn test_linux_glyph_cache_evicts_and_regenerates_identical_masks() {
	$if linux {
		clear_text_cache()
		defer { clear_text_cache() }
		mut face := Typeface.from_data(linux_text_font)!
		defer { face.free() }
		assert C.v_canvas_test_ft_glyph_count(linux_text_font.data, u64(linux_text_font.len)) > 512
		rec := make_rec(Font{ typeface: face, size: 64 }, 0xff000000, SurfaceProps{}, RasterMatrix.identity())!
		mut strike := find_or_create_strike(rec, face)!
		packed := u32(linux_text_glyph()) << 2
		first := strike.glyph(packed)!
		expected := first.image.clone()
		defer { unsafe { expected.free() } }
		assert expected.len > 0
		for id in 0 .. 512 {
			strike.glyph(u32(id) << 2)!
			assert strike.glyphs.len <= max_strike_glyphs
			assert strike.image_bytes <= max_strike_image_bytes
		}
		assert packed !in strike.glyph_index
		regenerated := strike.glyph(packed)!
		assert regenerated.image == expected
	}
}

fn linux_expect_draw_error(mut surface RasterSurface, font Font, glyphs []u16, positions []Point, paint Paint, message string) {
	before := surface.pixels.clone()
	defer { unsafe { before.free() } }
	surface.draw_glyphs(font, glyphs, positions, Point{}, paint) or {
		assert err.msg().contains(message)
		assert surface.pixels == before
		return
	}
	assert false, 'expected a text error containing ${message}'
}

fn test_linux_text_reports_invalid_and_unsupported_inputs() {
	$if linux {
		if _ := Typeface.from_data([]u8{}) {
			assert false, 'empty font accepted'
		} else {
			assert err.msg().contains('empty font')
		}
		if _ := Typeface.from_data([u8(0), 1, 2, 3]) {
			assert false, 'invalid font accepted'
		} else {
			assert err.msg().contains('FreeType')
		}
		if _ := Typeface.from_ct_font(unsafe { nil }) {
			assert false, 'CTFont accepted on Linux'
		} else {
			assert err.msg().contains('macOS CoreText')
		}
		mut face := Typeface.from_data(linux_text_font)!
		defer { face.free() }
		glyph := linux_text_glyph()
		font := Font{ typeface: face }
		positions := [Point{1, 25}]
		mut surface := linux_text_surface()!
		surface.clear(0xff123456)
		linux_expect_draw_error(mut surface, Font{}, [glyph], positions, Paint{}, 'without a typeface')
		linux_expect_draw_error(mut surface, Font{ typeface: face, embolden: true }, [glyph], positions, Paint{}, 'bold')
		linux_expect_draw_error(mut surface, Font{ typeface: face, size: 300 }, [glyph], positions, Paint{}, 'path rendering')
		linux_expect_draw_error(mut surface, Font{ typeface: face, size: -1 }, [glyph], positions, Paint{}, 'invalid text size')
		linux_expect_draw_error(mut surface, font, [glyph], []Point{}, Paint{}, 'positions')
		linux_expect_draw_error(mut surface, font, [u16(65535)], positions, Paint{}, 'FreeType could not render glyph')
		linux_expect_draw_error(mut surface, font, [glyph], positions, Paint{ style: .stroke }, 'stroked text')
		gradient := Paint{
			gradient: Gradient{
				p1:     Point{40, 0}
				colors: [
					Color4f{1, 0, 0, 1},
					Color4f{0, 0, 1, 1},
				]
			}
		}
		linux_expect_draw_error(mut surface, font, [glyph], positions, gradient, 'gradient text')
		surface.set_matrix(Matrix.values([f32(1), 0, 0, 0, 1, 0, 0.1, 0, 1]!))
		linux_expect_draw_error(mut surface, font, [glyph], positions, Paint{}, 'perspective')
		surface.set_matrix(Matrix.identity())
		surface.draw_glyphs(Font{ typeface: face, size: 0 }, [glyph], positions, Point{}, Paint{})!
		assert surface.pixels.all(it == 0xff123456)
		surface.props = SurfaceProps{ text_gamma: -1 }
		linux_expect_draw_error(mut surface, font, [glyph], positions, Paint{}, 'gamma or contrast')
	}
}
