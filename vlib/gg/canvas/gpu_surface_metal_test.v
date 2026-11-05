// vtest build: macos
module canvas

import crypto.sha256

const test_width = 32
const test_height = 24

fn pixel(px []u8, x int, y int) []u8 {
	i := (y * test_width + x) * 4
	return px[i..i + 4].clone()
}

fn render(mut ctx GpuContext, sample_count int, background []u8) ![]u8 {
	mut s := ctx.new_surface(width: test_width, height: test_height, sample_count: sample_count)!
	defer {
		s.free()
	}
	if background.len > 0 {
		s.write_pixels(background.data, test_width * 4)!
	} else {
		s.clear(Color4f{})
	}
	// A pixel-aligned opaque rect renders without coverage or blending.
	s.draw_rect(Rect.ltrb(4, 4, 12, 10), Paint{
		color: Color4f.from_rgba8(10, 20, 30, 255)
	})!
	s.draw_rrect(RRect.rect_radii(Rect.ltrb(14.5, 3, 30.5, 21), [Point{4, 4}, Point{4, 4}, Point{4, 4},
		Point{4, 4}]!), Paint{
		color:      Color4f.from_rgba8(200, 100, 0, 255)
		anti_alias: true
	})!
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	return px
}

fn test_gpu_surface_draws_and_reads_back() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	for samples in [1, 4] {
		px := render(mut ctx, samples, []u8{})!
		// Premultiplied BGRA.
		assert pixel(px, 5, 5) == [u8(30), 20, 10, 255], 'samples ${samples}'
		assert pixel(px, 0, 0) == [u8(0), 0, 0, 0]
		assert pixel(px, 22, 12) == [u8(0), 100, 200, 255]
		// The left edge at x = 14.5 half covers column 14.
		edge := pixel(px, 14, 12)
		assert edge[3] > 96 && edge[3] < 160, 'samples ${samples}: ${edge}'
		assert edge[2] <= edge[3]
	}
	// Uploaded pixels show through where nothing is drawn.
	mut background := []u8{len: test_width * test_height * 4}
	for i in 0 .. background.len / 4 {
		background[i * 4] = 40
		background[i * 4 + 1] = 50
		background[i * 4 + 2] = 60
		background[i * 4 + 3] = 255
	}
	for samples in [1, 4] {
		px := render(mut ctx, samples, background)!
		assert pixel(px, 0, 0) == [u8(40), 50, 60, 255], 'samples ${samples}'
		assert pixel(px, 5, 5) == [u8(30), 20, 10, 255]
		// The half covered edge blends with the uploaded pixel.
		edge := pixel(px, 14, 12)
		assert edge[3] == 255 && edge[0] > 0 && edge[0] < 40, 'samples ${samples}: ${edge}'
	}
}

fn test_multisampled_flushes_keep_earlier_pixels() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height, sample_count: 4)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	s.draw_rect(Rect.ltrb(4, 4, 12, 10), Paint{
		color: Color4f.from_rgba8(10, 20, 30, 255)
	})!
	s.flush()!
	// The next pass loads the resolved pixels before drawing.
	s.draw_rect(Rect.ltrb(16, 4, 24, 10), Paint{
		color: Color4f.from_rgba8(200, 100, 0, 255)
	})!
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 5, 5) == [u8(30), 20, 10, 255]
	assert pixel(px, 17, 5) == [u8(0), 100, 200, 255]
	assert pixel(px, 0, 0) == [u8(0), 0, 0, 0]
}

fn test_precompiled_contexts_compile_nothing_later() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	ctx.precompile()
	shaders, pipelines := ctx.shaders.len, ctx.pipelines.len
	assert ctx.msaa_load_pipeline.id != 0
	mut background := []u8{len: test_width * test_height * 4, init: 255}
	for samples in [1, 4] {
		render(mut ctx, samples, []u8{})!
		render(mut ctx, samples, background)!
	}
	assert ctx.shaders.len == shaders
	assert ctx.pipelines.len == pipelines
}

fn test_difference_clips_cut_out_draws() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	// Snapped to whole pixels: (4, 4, 12, 10).
	s.clip_rect(Rect.ltrb(3.6, 4.4, 11.5, 9.5), .difference, false)!
	paint := Paint{
		color:      Color4f.from_rgba8(10, 20, 30, 255)
		anti_alias: true
	}
	s.draw_rrect(RRect.rect_radii(Rect.ltrb(2, 2, 20, 16), [Point{3, 3}, Point{3, 3}, Point{3, 3},
		Point{3, 3}]!), paint)!
	// A draw inside the clipped-out rect is skipped entirely.
	s.draw_rect(Rect.ltrb(5, 5, 9, 8), paint)!
	assert s.draws.len == 1
	assert s.draws[0].uniforms.clipped
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 6, 6) == [u8(0), 0, 0, 0]
	assert pixel(px, 15, 12) == [u8(30), 20, 10, 255]
	assert pixel(px, 12, 6) == [u8(30), 20, 10, 255]
}

fn test_further_clip_elements_are_drawn_into_the_depth_attachment() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	// Two gaps in a line, like skip-ink underlines: the newest is analytic, the
	// other is drawn into the depth attachment of a 4x MSAA pass.
	s.clip_rect(Rect.ltrb(6, 2, 9, 10), .difference, false)!
	s.clip_rect(Rect.ltrb(14, 2, 17, 10), .difference, false)!
	s.draw_rect(Rect.ltrb(2, 4, 26, 8), Paint{
		color:      Color4f.from_rgba8(10, 20, 30, 255)
		anti_alias: true
	})!
	assert s.draws.len == 1 && s.draws[0].uniforms.clipped
	assert s.clip.elements[0].has_pending_draw()
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 7, 5) == [u8(0), 0, 0, 0]
	assert pixel(px, 15, 5) == [u8(0), 0, 0, 0]
	assert pixel(px, 4, 5) == [u8(30), 20, 10, 255]
	assert pixel(px, 11, 5) == [u8(30), 20, 10, 255]
	assert pixel(px, 20, 5) == [u8(30), 20, 10, 255]
	assert !s.clip.elements[0].has_pending_draw()
}

fn test_double_rrect_with_elliptical_corners_clips_out_the_inner_rrect() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	outer := RRect.rect_radii(Rect.ltrb(2, 2, 30, 14), [Point{6, 4}, Point{6, 4}, Point{6, 4},
		Point{6, 4}]!)
	// Elliptical rings are drawn as the outer rrect with the inner one clipped
	// out through a depth-only wedge draw.
	s.draw_drrect(outer, outer.inset(2, 2), Paint{
		color:      Color4f.from_rgba8(10, 20, 30, 255)
		anti_alias: true
	})!
	assert s.draws.any(it.step == .convex_wedges)
	assert s.clip.saves.len == 1 && s.clip.elements.len == 0
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 16, 8) == [u8(0), 0, 0, 0]
	assert pixel(px, 16, 2) == [u8(30), 20, 10, 255]
	assert pixel(px, 2, 8) == [u8(30), 20, 10, 255]
	assert pixel(px, 0, 0) == [u8(0), 0, 0, 0]
}

fn test_intersect_rrect_clip_rounds_a_rect_draw() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	r := Point{5, 5}
	s.save()
	s.clip_rrect(RRect.rect_radii(Rect.ltrb(2, 2, 26, 14), [r, r, r, r]!), .intersect, true)!
	s.draw_rect(Rect.ltrb(0, 0, 32, 16), Paint{
		color:      Color4f.from_rgba8(10, 20, 30, 255)
		anti_alias: true
	})!
	s.restore()
	// The rect covers the clip, so it is replaced by the clip's rrect.
	assert s.draws.len == 1 && s.draws[0].step == .analytic_rrect
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 2, 2) == [u8(0), 0, 0, 0]
	assert pixel(px, 14, 8) == [u8(30), 20, 10, 255]
	assert pixel(px, 28, 8) == [u8(0), 0, 0, 0]
}

fn test_intersect_clips_are_stenciled_and_inverse_covered_in_the_depth_attachment() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	// Elliptical corners have no analytic clip, and an oval cannot be
	// intersected with them geometrically.
	r := Point{12, 6}
	s.save()
	s.clip_rrect(RRect.rect_radii(Rect.ltrb(2, 2, 30, 22), [r, r, r, r]!), .intersect, true)!
	s.draw_rrect(RRect.oval(Rect.ltrb(0, 0, 20, 24)), Paint{
		color:      Color4f.from_rgba8(10, 20, 30, 255)
		anti_alias: true
	})!
	s.restore()
	s.record_deferred_clip_draws()
	stencil := s.draws.filter(it.step == .stencil_wedges_even_odd)
	cover := s.draws.filter(it.step == .cover_inverse_clip)
	assert stencil.len == 1 && cover.len == 1
	assert cover[0].uniform_index == uniform_index_invalid
	assert stencil[0].order == cover[0].order && cover[0].step_index == 1
	// The inverse cover fills the integer scissor, written right-bottom first.
	sc := cover[0].clip.scissor
	assert cover[0].instance[0..16] == f32_bytes([sc.right(), sc.bottom(), sc.left(), sc.top()])
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 10, 12) == [u8(30), 20, 10, 255]
	// Inside the oval, but left of the clip and in its rounded corner.
	assert pixel(px, 1, 12) == [u8(0), 0, 0, 0]
	assert pixel(px, 4, 3) == [u8(0), 0, 0, 0]
}

fn f32_bytes(values []f32) []u8 {
	mut out := []u8{}
	for v in values {
		put_f32(mut out, v)
	}
	return out
}

fn test_lines_are_stroked_with_caps_by_the_analytic_renderer() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	paint := Paint{
		color:        Color4f.from_rgba8(10, 20, 30, 255)
		style:        .stroke
		stroke_width: 4
		cap:          .square
	}
	s.draw_line(6, 12, 20, 12, paint)!
	assert s.draws.len == 1 && s.draws[0].step == .analytic_rrect
	// -2 marks a stroke, then the line flag, the stroke radius and the
	// square cap's miter join style, the zero radii and the end points.
	assert s.draws[0].instance[0..48] == f32_bytes([f32(-2), 1, 2, 1, 0, 0, 0, 0, 6, 12, 20, 12])
	// A filled line path draws nothing; a stroked one draws the same line.
	mut b := PathBuilder{}
	b.move_to(6, 20)
	b.line_to(20, 20)
	line := b.detach()
	s.draw_path(line, Paint{
		color: paint.color
	})!
	assert s.draws.len == 1
	s.draw_path(line, paint)!
	assert s.draws.len == 2
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	// Square caps extend the line by the stroke radius.
	assert pixel(px, 4, 12) == [u8(30), 20, 10, 255]
	assert pixel(px, 21, 20) == [u8(30), 20, 10, 255]
	assert pixel(px, 13, 16) == [u8(0), 0, 0, 0]
}

fn test_path_clips_are_drawn_into_the_depth_attachment() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	// Clip to a triangle, then clip out a smaller one inside it.
	mut b := PathBuilder{}
	b.move_to(0, 0)
	b.line_to(30, 0)
	b.line_to(0, 22)
	b.close()
	tri := b.detach()
	b.move_to(4, 4)
	b.line_to(10, 4)
	b.line_to(4, 10)
	b.close()
	hole := b.detach()
	s.save()
	s.clip_path(tri, .intersect, true)!
	s.clip_path(hole, .difference, true)!
	s.draw_rect(Rect.ltrb(0, 0, 32, 24), Paint{
		color: Color4f.from_rgba8(10, 20, 30, 255)
	})!
	s.restore()
	s.record_deferred_clip_draws()
	// The intersect triangle is stenciled and inverse-covered; the convex
	// difference triangle is drawn with convex wedges.
	assert s.draws.any(it.step == .cover_inverse_clip)
	assert s.draws.any(it.step == .convex_wedges)
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	assert pixel(px, 14, 3) == [u8(30), 20, 10, 255]
	assert pixel(px, 5, 5) == [u8(0), 0, 0, 0]
	assert pixel(px, 25, 18) == [u8(0), 0, 0, 0]
	// Rect paths clip like clip_rect().
	s.clear(Color4f{})
	b.move_to(2, 2)
	b.line_to(12, 2)
	b.line_to(12, 8)
	b.line_to(2, 8)
	b.close()
	s.save()
	s.clip_path(b.detach(), .intersect, false)!
	assert s.clip.elements.len == 0 || s.clip.elements.all(it.shape.kind == .rect)
	s.restore()
}

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

// SkCanvas skips src-over draws whose alpha rounds to 0 in SkPaint::getAlpha().
fn test_paints_whose_alpha_rounds_to_zero_draw_nothing() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: test_width, height: test_height)!
	defer {
		s.free()
	}
	r := Rect.ltrb(2, 2, 20, 20)
	assert s.quick_reject(r, Paint{ color: Color4f{1, 0, 0, f32(0.0019)} })
	assert !s.quick_reject(r, Paint{ color: Color4f{1, 0, 0, f32(0.002)} })
	assert s.quick_reject(r, Paint{ color: Color4f{1, 0, 0, 0} })
}
