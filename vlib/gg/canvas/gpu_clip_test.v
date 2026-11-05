module canvas

const clip_device = Rect.ltrb(0, 0, 64, 48)

fn identity_transform() Transform {
	return Transform.new(Matrix.identity())
}

fn device_element(shape ClipShape, op ClipOp) RawElement {
	return new_raw_element(clip_device, identity_transform(), shape, op, false)
}

fn add_element(mut c ClipStack, e RawElement) {
	c.add_element(e) or { panic(err) }
}

fn test_difference_elements_combine_like_graphite_save_records() {
	mut c := new_clip_stack(64, 48)
	// A rect covering the device empties the clip.
	add_element(mut c, device_element(rect_shape(Rect.ltrb(-1, -1, 65, 49)), .difference))
	assert c.current().state == .empty
	c = new_clip_stack(64, 48)
	add_element(mut c, device_element(rect_shape(Rect.ltrb(10, 10, 20, 20)), .difference))
	assert c.current().state == .complex && c.elements.len == 1
	// Contained in the stack's inner bounds: no change.
	add_element(mut c, device_element(rect_shape(Rect.ltrb(12, 12, 18, 18)), .difference))
	assert c.elements.len == 1
	// Disjoint: kept, and the outer bounds grow.
	add_element(mut c, device_element(rect_shape(Rect.ltrb(30, 5, 40, 8)), .difference))
	assert c.elements.len == 2
	rec := c.current()
	assert rec.outer.left() == 10 && rec.outer.right() == 40 && rec.outer.top() == 5
	// The larger element becomes the inner bounds.
	assert rec.inner.left() == 10 && rec.inner.right() == 20
	// Containing an older element invalidates it and takes its slot.
	add_element(mut c, device_element(rect_shape(Rect.ltrb(5, 5, 25, 25)), .difference))
	assert c.elements.len == 2
	assert c.elements[0].shape.rect.left() == 5 && !c.elements[0].is_invalid()
	assert c.elements[1].shape.rect.left() == 30
	// Containing the whole stack replaces it.
	add_element(mut c, device_element(rect_shape(Rect.ltrb(0, 0, 50, 40)), .difference))
	assert c.elements.len == 1 && c.current().oldest_valid == 0
}

fn test_intersect_rrect_and_rect_combine_into_one_element() {
	mut c := new_clip_stack(64, 48)
	r := Point{8, 8}
	outer := RRect.rect_radii(Rect.ltrb(1, 0, 60, 40), [r, r, r, r]!)
	add_element(mut c, device_element(rrect_shape(outer), .intersect))
	assert c.current().state == .device_rrect
	// The bottom band of the rrect: their intersection keeps the bottom corners.
	add_element(mut c, device_element(rect_shape(Rect.ltrb(1, 32, 60, 40)), .intersect))
	assert c.elements.len == 1
	e := c.elements[0]
	assert e.shape.kind == .rrect && e.shape.inverted
	assert e.shape.rrect.rect.top() == 32
	assert e.shape.rrect.radii[int(Corner.upper_left)] == Point{0, 0}
	assert e.shape.rrect.radii[int(Corner.lower_right)] == r
}

fn test_save_and_restore_remove_and_revalidate_elements() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: 64, height: 48) or { panic(err) }
	defer {
		s.free()
	}
	s.clip_rect(Rect.ltrb(10, 10, 20, 20), .difference, false) or { panic(err) }
	s.save()
	s.clip_rect(Rect.ltrb(5, 5, 25, 25), .difference, false) or { panic(err) }
	assert s.clip.saves.len == 2
	// The new element contains the older one, which belongs to an earlier
	// save record: it stays on the stack but is no longer visited.
	assert s.clip.elements.len == 2 && s.clip.current().oldest_valid == 1
	s.restore()
	assert s.clip.saves.len == 1 && s.clip.elements.len == 1
	assert s.clip.current().oldest_valid == 0
	// A save without clips is deferred and leaves no save record.
	s.save()
	s.restore()
	assert s.clip.saves.len == 1 && s.clip.saves[0].deferred_saves == 0
}

fn test_subtract_rect_matches_sk_rect_priv_subtract() {
	a := Rect.ltrb(0, 0, 64, 48)
	// Spanning the full height of the left edge leaves an exact rectangle.
	r := subtract_rect(a, Rect.ltrb(-2, -2, 10, 50), true)
	assert r.left() == 10 && r.right() == 64 && r.top() == 0 && r.bottom() == 48
	// A hole in the middle cannot be represented exactly.
	assert subtract_rect(a, Rect.ltrb(10, 10, 20, 20), true) == a
	assert rect_subtract_exact(a, Rect.ltrb(10, 10, 20, 20)) == none
	// The inexact answer is the largest part outside the hole (right of it).
	inexact := subtract_rect(a, Rect.ltrb(10, 10, 20, 20), false)
	assert inexact.left() == 20 && inexact.right() == 64
	// Containing `a` empties it; missing it keeps it.
	assert subtract_rect(a, Rect.ltrb(-1, -1, 70, 50), true).is_empty_negative_or_nan()
	assert subtract_rect(a, Rect.ltrb(70, 0, 80, 10), true) == a
}

fn test_rect_intersects_is_strict_like_graphite() {
	a := Rect.ltrb(0, 0, 10, 10)
	assert a.intersects(Rect.ltrb(9.5, 9.5, 20, 20))
	assert !a.intersects(Rect.ltrb(10, 0, 20, 10))
	assert !a.intersects(Rect.ltrb(0, 10, 10, 20))
}

fn test_analytic_clip_uniforms_select_the_clip_op_and_rounded_edges() {
	diff := analytic_clip_uniforms(can_apply_analytic_clip(rect_shape(Rect.ltrb(10, 8, 30, 20)),
		identity_transform()))
	assert diff.rect == [f32(9.5), 7.5, 30.5, 20.5]!
	assert diff.radius_plus_half == [f32(-1), 1]!
	assert diff.edge_select == [f32(0), 0, 0, 0]!
	// An intersect rrect with two rounded top corners (a tab).
	r := Point{3.5, 3.5}
	tab := RRect.rect_radii(Rect.ltrb(10, 8, 30, 20), [r, r, Point{}, Point{}]!)
	mut shape := rrect_shape(tab)
	shape.inverted = true
	clip := can_apply_analytic_clip(shape, identity_transform())
	assert clip.radius == 3.5 && clip.edge_flags == analytic_left_edge | analytic_top_edge | analytic_right_edge
	u := analytic_clip_uniforms(clip)
	assert u.radius_plus_half == [f32(4), 0.25]!
	assert u.edge_select == [f32(1), 1, 1, 0]!
	// Elliptical corners are not analytic.
	e := RRect.rect_radii(Rect.ltrb(10, 8, 30, 20), [Point{4, 2}, Point{4, 2}, Point{4, 2},
		Point{4, 2}]!)
	assert can_apply_analytic_clip(rrect_shape(e), identity_transform()).is_empty()
}

fn test_snap_scissor_rounds_out_to_the_next_multiple_of_four() {
	a := snap_scissor(Rect.ltrb(5, 5, 10, 10), clip_device)
	assert a.left() == 0 && a.top() == 0 && a.right() == 16 && a.bottom() == 16
	// Edges already on multiples of 4 still gain padding.
	b := snap_scissor(Rect.ltrb(4, 4, 8, 8), clip_device)
	assert b.left() == 0 && b.right() == 12
	// Clipped to the device.
	c := snap_scissor(Rect.ltrb(50, 40, 64, 48), clip_device)
	assert c.right() == 64 && c.bottom() == 48
}

fn test_rect_wedge_patches_encode_lines_as_cubics_around_the_midpoint() {
	mut out := []u8{}
	write_rect_wedge_patches(mut out, Rect.ltrb(3, 6, 9, 12), 5)
	assert out.len == 4 * wedge_instance_stride
	floats := unsafe { &f32(out.data) }
	// The first edge runs from (3, 6) to (9, 6) with control points a third in.
	first := unsafe {
		[floats[0], floats[1], floats[2], floats[3], floats[4], floats[5], floats[6], floats[7]]
	}
	assert first == [f32(3), 6, 5, 6, 7, 6, 9, 6]
	// The fan point is the contour's midpoint.
	assert unsafe { floats[8] } == 6 && unsafe { floats[9] } == 9
	assert unsafe { floats[10] } == depth_as_float(5)
	// The closing edge runs from (3, 12) back to (3, 6).
	last := unsafe { &f32(&out[3 * wedge_instance_stride]) }
	assert unsafe { last[0] } == 3 && unsafe { last[1] } == 12 && unsafe { last[7] } == 6
}

fn test_rrect_paths_follow_sk_path_raw_shapes() {
	r := Point{2, 3}
	rr := RRect.rect_radii(Rect.ltrb(0, 0, 20, 10), [r, r, r, r]!)
	path := element_path(rrect_shape(rr))
	// Start index 6: the left edge's lower end, then up the left side.
	assert path.verbs == [PathVerb.move, .line, .conic, .line, .conic, .line, .conic, .line, .conic,
		.close]
	assert path.points[0] == Point{0, 7}
	assert path.points[1] == Point{0, 3}
	assert path.points[2] == Point{0, 0}
	assert path.points[3] == Point{2, 0}
	assert path.points[12] == path.points[0]
	// The contour ends at its start, so the midpoint averages 8 end points.
	contours := midpoint_contours(path)
	assert contours.len == 1 && contours[0].midpoint == Point{10, 5}
	oval := element_path(rrect_shape(RRect.oval(Rect.ltrb(0, 0, 20, 10))))
	// Start index 3: the left extreme, then the top-left conic.
	assert oval.points[0] == Point{0, 5}
	assert oval.points[1] == Point{0, 0}
	assert oval.points[2] == Point{10, 0}
}

fn test_wedge_templates_follow_fixed_count_wedges() {
	vertices := wedge_vertex_data()
	assert vertices.len == 2 * 34
	assert vertices[..10] == [f32(-1), -1, 0, 0, 0, 1, 1, 1, 2, 1]
	indices := wedge_index_data()
	assert indices.len == 3 * 32
	assert indices[..9] == [u16(0), 1, 2, 1, 3, 2, 1, 4, 3]
	assert wedge_vertex_count(1) == 3
	// n = 2 segments: resolve level 1, one curve triangle plus the fan.
	assert wedge_vertex_count(16) == 6
	assert wedge_vertex_count(17) == 12
	assert wedge_vertex_count(1.0e12) == 32 * 3
}

fn test_conservative_intersect_keeps_the_rounded_corners_inside() {
	r := Point{8, 8}
	outer := RRect.rect_radii(Rect.ltrb(0, 0, 40, 30), [r, r, r, r]!)
	band := conservative_intersect(outer, RRect.from_rect(Rect.ltrb(0, 22, 40, 30)))
	assert band.rect == Rect.ltrb(0, 22, 40, 30)
	assert band.radii == [Point{0, 0}, Point{0, 0}, r, r]!
	// A band cutting through the corners is not a rounded rectangle.
	assert conservative_intersect(outer, RRect.from_rect(Rect.ltrb(0, 26, 40, 30))).is_empty()
}

fn test_rrect_inset_and_contains_follow_sk_rrect() {
	r := Point{8, 8}
	rr := RRect.rect_radii(Rect.ltrb(0, 0, 40, 30), [r, r, r, r]!)
	inner := rr.inset(1, 2)
	assert inner.rect == Rect.ltrb(1, 2, 39, 28)
	assert inner.radii[0] == Point{7, 6}
	assert rr.outset(0.5, 0.5).radii[0] == Point{8.5, 8.5}
	assert rr.contains_rect(Rect.ltrb(8, 0, 32, 30))
	assert !rr.contains_rect(Rect.ltrb(0, 0, 4, 4))
	assert rr.contains_point(20, 0) && !rr.contains_point(0.5, 0.5)
}

fn test_rrect_transform_rotates_corners_by_90_degrees_like_sk_rrect() {
	radii := [Point{1, 2}, Point{3, 4}, Point{5, 6}, Point{7, 8}]!
	rr := RRect.rect_radii(Rect.ltrb(10, 20, 50, 40), radii)
	// SkMatrix::setRotate(90) then a translation: x' = -y + 100, y' = x.
	cw := rr.transform_legacy(Transform.new(Matrix{
		m: [f32(0), -1, 100, 1, 0, 0, 0, 0, 1]!
	})) or { panic('not transformed') }
	assert cw.rect == Rect.ltrb(60, 10, 80, 50)
	// The lower-left corner becomes the upper-left one, with its axes swapped.
	assert cw.radii == [Point{8, 7}, Point{2, 1}, Point{4, 3}, Point{6, 5}]!
	// setRotate(-90): x' = y, y' = -x + 100.
	ccw := rr.transform_legacy(Transform.new(Matrix{
		m: [f32(0), 1, 0, -1, 0, 100, 0, 0, 1]!
	})) or { panic('not transformed') }
	assert ccw.rect == Rect.ltrb(20, 50, 40, 90)
	assert ccw.radii == [Point{4, 3}, Point{6, 5}, Point{8, 7}, Point{2, 1}]!
	// A rotation by 90 degrees with a scale of 2 doubles the swapped radii.
	scaled := rr.transform_legacy(Transform.new(Matrix{
		m: [f32(0), -2, 0, 2, 0, 0, 0, 0, 1]!
	})) or { panic('not transformed') }
	assert scaled.radii[0] == Point{16, 14}
	// Rotations by other angles are not rounded rectangles.
	assert rr.transform_legacy(Transform.new(Matrix{
		m: [f32(0.8660254), -0.5, 0, 0.5, 0.8660254, 0, 0, 0, 1]!
	})) == none
}

fn test_clipped_wedge_and_stroke_uniforms_put_the_clip_after_the_color() {
	clip := analytic_clip_uniforms(AnalyticClip{
		bounds:     Rect.ltrb(2, 3, 20, 30)
		radius:     4
		edge_flags: analytic_left_edge | analytic_bottom_edge
	})
	color := Color4f{0.25, 0.5, 0.75, 1}
	clip_block := [f32(1.5), 2.5, 20.5, 30.5, 4.5, f32(1) / f32(4.5), 0, 0, 1, 0, 0, 1]
	// CombinedUniformData of the clipped convex wedges: color, the clip, then
	// the float4x4 localToDevice.
	wedges := serialize_uniforms([PaintUniforms{
		...local_to_device_uniforms(Matrix.identity())
		color:      color
		with_color: true
		clipped:    true
		clip:       clip
	}])
	assert wedges.len * 4 == step_uniform_size(.wedges_shaded, true)
	assert wedges[..4] == [f32(0.25), 0.5, 0.75, 1]
	assert wedges[4..16] == clip_block
	assert wedges[16] == 1 && wedges[21] == 1 && wedges[31] == 1
	// The clipped strokes: color, the clip, then the 2x2, translation and
	// max scale.
	t := Transform.new(Matrix{
		m: [f32(2), 0, 5, 0, 3, 7, 0, 0, 1]!
	})
	strokes := serialize_uniforms([stroke_uniforms(color, t, AnalyticClip{
		bounds:     Rect.ltrb(2, 3, 20, 30)
		radius:     4
		edge_flags: analytic_left_edge | analytic_bottom_edge
	})])
	assert strokes.len * 4 == step_uniform_size(.tessellate_strokes, true)
	assert strokes[4..16] == clip_block
	assert strokes[16..] == [f32(2), 0, 0, 3, 5, 7, 3, 0]
}
