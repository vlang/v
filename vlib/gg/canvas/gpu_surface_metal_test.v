// vtest build: macos
module canvas

import crypto.sha256

const gradient_stops = [Color4f{1, 0, 0, 1}, Color4f{0, 1, 0, 1}, Color4f{0, 0, 1, 1}]

fn gradient_scene_hash(mut ctx GpuContext, rotated_radial bool) !string {
	mut s := ctx.new_surface(width: 64, height: 48)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	mut paint := Paint{
		anti_alias: true
		dither:     true
	}
	if rotated_radial {
		s.clip_rrect(RRect.rect_radii(Rect.ltrb(4, 4, 60, 44), [Point{12, 12}, Point{12, 12},
			Point{12, 12}, Point{12, 12}]!), .intersect, true)!
		paint.gradient = Gradient{
			kind:      .radial
			p0:        Point{32, 24}
			radius:    30
			colors:    gradient_stops
			positions: [f32(0), 0.5, 1]
			premul:    true
		}
		s.set_matrix(Matrix.values([f32(0.8660254), -0.5, 20, 0.5, 0.8660254, -6, 0, 0, 1]!))
		s.draw_rect(Rect.ltrb(4, 4, 40, 30), paint)!
	} else {
		paint.gradient = Gradient{
			kind:      .linear
			p0:        Point{4, 4}
			p1:        Point{60, 40}
			colors:    gradient_stops
			positions: [f32(0), 0.5, 1]
		}
		s.draw_rect(Rect.ltrb(2, 2, 62, 46), paint)!
	}
	mut px := []u8{len: 64 * 48 * 4}
	s.read_pixels(px.data, 64 * 4)!
	return sha256.sum(px).hex()[..16]
}

// The SHA-256 prefixes of Skia Graphite's pixels for the same draws: a
// pixel-aligned linear gradient rect (cover bounds), and a rotated rect with
// a premultiplied radial gradient under an rrect clip (analytic rrect with
// the analytic clip shader).
fn test_gradients_match_graphite_pixels() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	assert gradient_scene_hash(mut ctx, false)! == '3b90b310064516c5'
	assert gradient_scene_hash(mut ctx, true)! == '2dded391c5fb0c20'
}

fn gradient_path_hash(mut ctx GpuContext, stroke bool) !string {
	mut s := ctx.new_surface(width: 64, height: 48)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	s.clip_rrect(RRect.rect_radii(Rect.ltrb(3, 3, 61, 45), [Point{8, 8}, Point{8, 8}, Point{8, 8},
		Point{8, 8}]!), .intersect, true)!
	mut b := PathBuilder{}
	if stroke {
		b.move_to(4, 4)
		b.line_to(60, 10)
		b.line_to(20, 44)
	} else {
		b.move_to(32, 2)
		b.line_to(40, 44)
		b.line_to(6, 16)
		b.line_to(58, 16)
		b.line_to(24, 44)
		b.close()
	}
	s.draw_path(b.detach(), Paint{
		anti_alias:   true
		dither:       true
		style:        if stroke { PaintStyle.stroke } else { PaintStyle.fill }
		stroke_width: 3
		gradient:     Gradient{
			kind:      .linear
			p0:        Point{4, 4}
			p1:        Point{60, 40}
			colors:    if stroke {
				[Color4f{1, 0, 0, 1}, Color4f{1, 1, 0, 1}, Color4f{0, 1, 0, 1}, Color4f{0, 1, 1, 1},
					Color4f{0, 0, 1, 1}, Color4f{1, 0, 1, 1}]
			} else {
				gradient_stops
			}
			positions: if stroke {
				[f32(0), 0.2, 0.4, 0.6, 0.8, 1]
			} else {
				[f32(0), 0.5, 1]
			}
			premul:    stroke
		}
	})!
	mut px := []u8{len: 64 * 48 * 4}
	s.read_pixels(px.data, 64 * 4)!
	return sha256.sum(px).hex()[..16]
}

// Graphite's pixels for gradient paths under an rrect clip: a concave fill
// (stencil wedges, then the cover-bounds gradient shader) and a stroke.
fn test_gradient_paths_match_graphite_pixels() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	assert gradient_path_hash(mut ctx, false)! == '034ed0ac42400a65'
	assert gradient_path_hash(mut ctx, true)! == '44d14951e590f9a8'
}

// Graphite's pixels for an inverse even-odd path: even-odd stencil wedges,
// then the inverse cover of the scissor where the stencil is zero.
fn test_inverse_fills_match_graphite_pixels() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: 64, height: 48)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	mut b := PathBuilder{}
	b.move_to(4.4044533115656295, 9.05474489751748)
	b.cubic_to(5.275330990981304, 8.44699277472387, 5.630927183102384, 6.323505941470868,
		2.0960942432789023, 5.116001526384584)
	b.cubic_to(2.6403499688462855, 8.264720441454003, 4.3405849236372935, 6.065771144271345,
		2.2923395909772486, 6.1651676076721085)
	b.close()
	b.set_fill_type(.inverse_even_odd)
	s.draw_path(b.detach(), Paint{})!
	mut px := []u8{len: 64 * 48 * 4}
	s.read_pixels(px.data, 64 * 4)!
	assert sha256.sum(px).hex()[..16] == '9b08627a29d8eb90'
}
