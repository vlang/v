module canvas

const clip_device = Rect.ltrb(0, 0, 64, 48)

fn test_difference_elements_combine_like_graphite_save_records() {
	mut c := new_clip_stack(64, 48)
	// A rect covering the device empties the clip.
	c.add_difference(Rect.ltrb(-1, -1, 65, 49).intersect(clip_device), clip_device)
	assert c.state == .empty
	c = new_clip_stack(64, 48)
	c.add_difference(Rect.ltrb(10, 10, 20, 20), clip_device)
	assert c.state == .complex && c.elements.len == 1
	// Contained in the stack's inner bounds: no change.
	c.add_difference(Rect.ltrb(12, 12, 18, 18), clip_device)
	assert c.elements.len == 1
	// Disjoint: kept, and the outer bounds grow.
	c.add_difference(Rect.ltrb(30, 5, 40, 8), clip_device)
	assert c.elements.len == 2
	assert c.outer.left() == 10 && c.outer.right() == 40 && c.outer.top() == 5
	// The larger element becomes the inner bounds.
	assert c.inner.left() == 10 && c.inner.right() == 20
	// Containing an older element invalidates it and takes its slot.
	c.add_difference(Rect.ltrb(5, 5, 25, 25), clip_device)
	assert c.elements.len == 2
	assert c.elements[0].rect.left() == 5 && c.elements[0].valid
	assert c.elements[1].rect.left() == 30
	// Containing the whole stack replaces it.
	c.add_difference(Rect.ltrb(0, 0, 50, 40), clip_device)
	assert c.elements.len == 1 && c.oldest_valid == 0
}

fn test_subtract_rect_matches_sk_rect_priv_subtract() {
	a := Rect.ltrb(0, 0, 64, 48)
	// Spanning the full height of the left edge leaves an exact rectangle.
	r := subtract_rect(a, Rect.ltrb(-2, -2, 10, 50), true)
	assert r.left() == 10 && r.right() == 64 && r.top() == 0 && r.bottom() == 48
	// A hole in the middle cannot be represented exactly.
	assert subtract_rect(a, Rect.ltrb(10, 10, 20, 20), true) == a
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

fn test_analytic_clip_uniforms_select_difference() {
	u := analytic_clip_uniforms(Rect.ltrb(10, 8, 30, 20))
	assert u.rect == [f32(9.5), 7.5, 30.5, 20.5]!
	assert u.radius_plus_half == [f32(-1), 1]!
	assert u.edge_select == [f32(0), 0, 0, 0]!
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
	first := unsafe { [floats[0], floats[1], floats[2], floats[3], floats[4], floats[5], floats[6],
		floats[7]] }
	assert first == [f32(3), 6, 5, 6, 7, 6, 9, 6]
	// The fan point is the contour's midpoint.
	assert unsafe { floats[8] } == 6 && unsafe { floats[9] } == 9
	assert unsafe { floats[10] } == depth_as_float(5)
	// The closing edge runs from (3, 12) back to (3, 6).
	last := unsafe { &f32(&out[3 * wedge_instance_stride]) }
	assert unsafe { last[0] } == 3 && unsafe { last[1] } == 12 && unsafe { last[7] } == 6
}
