// vtest build: linux
module canvas

import math
import sokol.gfx

fn linux_pixel(pixels []u8, width int, x int, y int) []u8 {
	i := (y * width + x) * 4
	return pixels[i..i + 4].clone()
}

fn assert_linux_pixel_near(actual []u8, expected []u8, tolerance int) {
	assert actual.len == 4 && expected.len == 4
	for i in 0 .. 4 {
		assert math.abs(int(actual[i]) - int(expected[i])) <= tolerance, 'pixel ${actual}, expected ${expected}, tolerance ${tolerance}'
	}
}

fn test_linux_context_recreation_and_borrowed_gfx_ownership() {
	for _ in 0 .. 2 {
		mut ctx := new_gpu_context()!
		assert gfx.query_backend() == .glcore33
		mut borrowed := new_gpu_context()!
		borrowed.free()
		assert gfx.is_valid()
		mut surface := ctx.new_surface(width: 4, height: 3)!
		surface.clear(Color4f{1, 0, 0, 1})
		mut pixels := []u8{len: 4 * 3 * 4}
		surface.read_pixels(pixels.data, 4 * 4)!
		assert linux_pixel(pixels, 4, 0, 0) == [u8(0), 0, 255, 255]
		surface.free()
		ctx.free()
		assert !gfx.is_valid()
	}
}

fn test_linux_bgra_transfers_preserve_asymmetric_rows_and_padding() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	width, height, stride := 7, 5, 40
	mut source := []u8{len: stride * height, init: 0xcc}
	for y in 0 .. height {
		for x in 0 .. width {
			i := y * stride + x * 4
			source[i] = u8(10 + x)
			source[i + 1] = u8(20 + y)
			source[i + 2] = u8(30 + y * width + x)
			source[i + 3] = 255
		}
	}
	for samples in [1, 4] {
		mut s := ctx.new_surface(width: width, height: height, sample_count: samples)!
		s.write_pixels(source.data, stride)!
		mut pixels := []u8{len: stride * height, init: 0xab}
		s.read_pixels(pixels.data, stride)!
		for y in 0 .. height {
			assert pixels[y * stride..y * stride + width * 4] == source[y * stride..y * stride +
				width * 4], 'sample count ${samples}, row ${y}'
			assert pixels[y * stride + width * 4..(y + 1) * stride].all(it == 0xab)
		}
		// The first logical row stays at the top after a load-and-draw pass.
		s.draw_rect(Rect.ltrb(1, 0, 4, 2), Paint.rgba8(255, 0, 0, 255))!
		s.read_pixels(pixels.data, stride)!
		for y in 0 .. height {
			for x in 0 .. width {
				i := y * stride + x * 4
				expected := if y < 2 && x >= 1 && x < 4 {
					[u8(0), 0, 255, 255]
				} else {
					source[i..i + 4]
				}
				assert pixels[i..i + 4] == expected, 'sample count ${samples}, (${x}, ${y})'
			}
		}
		s.free()
	}
}

fn test_linux_linear_gradient_records_and_premultiplied_alpha() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	for stop_count in [2, 6] {
		for alpha in [f32(1), f32(0.5)] {
			mut colors := []Color4f{}
			for i in 0 .. stop_count {
				t := f32(i) / f32(stop_count - 1)
				colors << Color4f{1 - t, 0, t, 1}
			}
			mut s := ctx.new_surface(width: 32, height: 16)!
			s.draw_rect(Rect.ltrb(0, 0, 32, 16), Paint{
				color:    Color4f{0, 0, 0, alpha}
				dither:   true
				gradient: Gradient{
					kind:   .linear
					p0:     Point{0, 0}
					p1:     Point{32, 0}
					colors: colors
				}
			})!
			mut pixels := []u8{len: 32 * 16 * 4}
			s.read_pixels(pixels.data, 32 * 4)!
			for x in [2, 8, 24, 29] {
				t := (f32(x) + 0.5) / 32
				expected := [u8(t * alpha * 255 + 0.5), u8(0), u8((1 - t) * alpha * 255 + 0.5),
					u8(alpha * 255 + 0.5)]
				assert_linux_pixel_near(linux_pixel(pixels, 32, x, 4), expected, 2)
				assert_linux_pixel_near(linux_pixel(pixels, 32, x, 12), expected, 2)
			}
			s.free()
		}
	}
}

fn test_linux_radial_gradient_uses_canvas_coordinates_under_a_clip() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	mut s := ctx.new_surface(width: 40, height: 28)!
	defer { s.free() }
	s.clip_rect(Rect.ltrb(18, 7, 22, 11), .difference, false)!
	s.draw_rrect(RRect.oval(Rect.ltrb(4, 2, 36, 26)), Paint{
		anti_alias: true
		dither:     true
		gradient:   Gradient{
			kind:   .radial
			p0:     Point{20, 14}
			radius: 16
			colors: [Color4f{1, 0, 0, 1}, Color4f{0, 0, 1, 1}]
		}
	})!
	mut pixels := []u8{len: 40 * 28 * 4}
	s.read_pixels(pixels.data, 40 * 4)!
	assert linux_pixel(pixels, 40, 20, 8) == [u8(0), 0, 0, 0]
	assert linux_pixel(pixels, 40, 0, 0) == [u8(0), 0, 0, 0]
	for point in [Point{20, 14}, Point{12, 14}, Point{28, 14}, Point{20, 21}] {
		dx, dy := point.x + 0.5 - 20, point.y + 0.5 - 14
		t := f32(math.sqrt(dx * dx + dy * dy)) / 16
		assert_linux_pixel_near(linux_pixel(pixels, 40, int(point.x), int(point.y)), [u8(t *
			255 + 0.5), u8(0), u8((1 - t) * 255 + 0.5), u8(255)], 2)
	}
}

fn test_linux_layer_restores_preserve_origins_and_fractional_clips() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	mut s := ctx.new_surface(width: 32, height: 24)!
	defer { s.free() }
	s.clear(Color4f{0, 0, 1, 1})
	s.clip_rect(Rect.ltrb(5.5, 3.5, 27.5, 20.5), .intersect, true)!
	s.save_layer(none)!
	s.draw_rect(Rect.ltrb(0, 0, 32, 12), Paint.rgba8(255, 0, 0, 255))!
	s.draw_rect(Rect.ltrb(0, 12, 32, 24), Paint.rgba8(0, 255, 0, 255))!
	s.restore()
	assert s.draws.any(it.step == .per_edge_aa_quad)
	mut pixels := []u8{len: 32 * 24 * 4}
	s.read_pixels(pixels.data, 32 * 4)!
	assert linux_pixel(pixels, 32, 10, 6) == [u8(0), 0, 255, 255]
	assert linux_pixel(pixels, 32, 10, 17) == [u8(0), 255, 0, 255]
	assert linux_pixel(pixels, 32, 0, 0) == [u8(255), 0, 0, 255]
	assert_linux_pixel_near(linux_pixel(pixels, 32, 5, 6), [u8(128), 0, 128, 255], 2)
	assert_linux_pixel_near(linux_pixel(pixels, 32, 5, 17), [u8(128), 128, 0, 255], 2)
}

fn test_linux_stencil_path_fill_rules_and_inverse_fills() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	for fill_type in [PathFillType.winding, .even_odd, .inverse_winding, .inverse_even_odd] {
		mut s := ctx.new_surface(width: 40, height: 32)!
		mut b := PathBuilder{}
		// Two overlapping contours force stencil-and-cover. Their overlap is
		// filled for winding and empty for even-odd, with the same orientation.
		b.add_rect(Rect.ltrb(4, 4, 24, 24), .cw, 0)
		b.add_rect(Rect.ltrb(16, 8, 36, 28), .cw, 0)
		b.set_fill_type(fill_type)
		s.draw_path(b.detach(), Paint.rgba8(10, 20, 30, 255))!
		mut pixels := []u8{len: 40 * 32 * 4}
		s.read_pixels(pixels.data, 40 * 4)!
		inverse := fill_type in [.inverse_winding, .inverse_even_odd]
		even_odd := fill_type in [.even_odd, .inverse_even_odd]
		filled, empty := [u8(30), 20, 10, 255], [u8(0), 0, 0, 0]
		assert linux_pixel(pixels, 40, 0, 0) == if inverse { filled } else { empty }
		assert linux_pixel(pixels, 40, 8, 8) == if inverse { empty } else { filled }
		assert linux_pixel(pixels, 40, 20, 12) == if inverse == even_odd { filled } else { empty }
		s.free()
	}
}

fn test_linux_convex_wedges_and_tessellated_strokes_use_paint_records() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	mut s := ctx.new_surface(width: 40, height: 32)!
	defer { s.free() }
	mut b := PathBuilder{}
	b.move_to(4, 4)
	b.line_to(20, 4)
	b.line_to(4, 20)
	b.close()
	s.draw_path(b.detach(), Paint.rgba8(255, 0, 0, 255))!
	b.move_to(26, 4)
	b.line_to(26, 24)
	b.line_to(8, 24)
	s.draw_path(b.detach(), Paint{
		color:        Color4f{0, 1, 0, 1}
		anti_alias:   true
		style:        .stroke
		stroke_width: 4
		cap:          .round
		join:         .round
	})!
	assert s.draws.any(it.step == .wedges_shaded)
	assert s.draws.any(it.step == .tessellate_strokes)
	mut pixels := []u8{len: 40 * 32 * 4}
	s.read_pixels(pixels.data, 40 * 4)!
	assert linux_pixel(pixels, 40, 7, 7) == [u8(0), 0, 255, 255]
	assert linux_pixel(pixels, 40, 26, 12) == [u8(0), 255, 0, 255]
	assert linux_pixel(pixels, 40, 15, 24) == [u8(0), 255, 0, 255]
	assert linux_pixel(pixels, 40, 35, 29) == [u8(0), 0, 0, 0]
}

fn test_linux_mixed_batches_preserve_storage_record_indexes_and_strides() {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	mut s := ctx.new_surface(width: 64, height: 80)!
	defer { s.free() }
	// Distinct solid paints share a draw call and select different SSBO records.
	for i in 0 .. 64 {
		x, y := f32(i % 8 * 8), f32(i / 8 * 8)
		s.draw_rect(Rect.ltrb(x, y, x + 8, y + 8), Paint.rgba8(u8(i * 3),
			u8(255 - i * 2), u8(7 + i), 255))!
	}
	// These other render steps have larger paint records. Their records share
	// the buffer with the solids, so each batch must use its own aligned base.
	mut b := PathBuilder{}
	b.move_to(2, 66)
	b.line_to(18, 66)
	b.line_to(2, 78)
	b.close()
	s.draw_path(b.detach(), Paint.rgba8(255, 0, 0, 255))!
	s.draw_rect(Rect.ltrb(24, 64, 64, 80), Paint{
		dither:   true
		gradient: Gradient{
			kind:   .linear
			p0:     Point{24, 64}
			p1:     Point{64, 64}
			colors: [Color4f{0, 1, 0, 1}, Color4f{0, 0, 1, 1}]
		}
	})!
	mut pixels := []u8{len: 64 * 80 * 4}
	s.read_pixels(pixels.data, 64 * 4)!
	for i in 0 .. 64 {
		assert linux_pixel(pixels, 64, i % 8 * 8 + 4, i / 8 * 8 + 4) == [u8(7 + i), u8(255 - i * 2),
			u8(i * 3), u8(255)], 'solid paint record ${i}'
	}
	assert linux_pixel(pixels, 64, 5, 69) == [u8(0), 0, 255, 255]
	for x in [28, 44, 60] {
		t := (f32(x) + 0.5 - 24) / 40
		assert_linux_pixel_near(linux_pixel(pixels, 64, x, 72), [u8(t * 255 + 0.5),
			u8((1 - t) * 255 + 0.5), u8(0), u8(255)], 2)
	}
}
