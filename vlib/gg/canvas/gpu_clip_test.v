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
