module canvas

import math

fn polygon(points []Point, close bool) Path {
	mut b := PathBuilder{}
	for i, p in points {
		if i == 0 {
			b.move_to(p.x, p.y)
		} else {
			b.line_to(p.x, p.y)
		}
	}
	if close {
		b.close()
	}
	return b.detach()
}

fn test_builder_follows_sk_path_builder() {
	mut b := PathBuilder{}
	// A line without a move starts at the origin.
	b.line_to(4, 5)
	// A conic with weight 1 is a quad; a non-finite weight becomes two lines.
	b.conic_to(1, 2, 3, 4, 1)
	b.conic_to(1, 2, 3, 4, f32(math.inf(1)))
	b.close()
	// After a close, the next verb starts at the last move point.
	b.line_to(9, 9)
	p := b.detach()
	assert p.verbs == [PathVerb.move, .line, .quad, .line, .line, .close, .move, .line]
	assert p.points[6] == Point{0, 0}
	// Consecutive moves keep the last one.
	b.move_to(1, 1)
	b.move_to(2, 2)
	assert b.detach().points == [Point{2, 2}]
}

fn test_bounds_ignore_a_trailing_move() {
	mut b := PathBuilder{}
	b.move_to(1, 2)
	b.line_to(5, 3)
	b.close()
	b.move_to(100, 100)
	p := b.detach()
	assert p.bounds() == Rect.ltrb(1, 2, 5, 3)
}

fn test_convexity_matches_sk_path() {
	tri := polygon([Point{10, 5}, Point{50, 20}, Point{15, 40}], true)
	assert tri.convexity() == .convex_cw
	ccw := polygon([Point{10, 5}, Point{15, 40}, Point{50, 20}], true)
	assert ccw.convexity() == .convex_ccw
	star := polygon([Point{32, 2}, Point{40, 44}, Point{6, 16}, Point{58, 16}, Point{24, 44}],
		true)
	assert star.convexity() == .concave
	// Collinear points are degenerate but convex.
	line := polygon([Point{0, 0}, Point{5, 5}, Point{10, 10}], false)
	assert line.convexity() == .convex_degenerate
	// Two contours are never convex.
	mut b := PathBuilder{}
	b.move_to(0, 0)
	b.line_to(4, 0)
	b.line_to(0, 4)
	b.close()
	b.move_to(10, 10)
	b.line_to(14, 10)
	b.line_to(10, 14)
	assert b.detach().convexity() == .concave
	// A trailing move after a convex contour is ignored.
	b.move_to(0, 0)
	b.line_to(4, 0)
	b.line_to(0, 4)
	b.close()
	b.move_to(0, 0)
	assert b.detach().is_convex()
}

fn test_rect_detection_matches_sk_path_is_rect() {
	closed := polygon([Point{1, 2}, Point{9, 2}, Point{9, 7}, Point{1, 7}], true)
	rect, is_closed := closed.is_rect() or { panic('not a rect') }
	assert rect == Rect.ltrb(1, 2, 9, 7) && is_closed
	// Three sides are a rect when filled, but not closed.
	open := polygon([Point{1, 2}, Point{9, 2}, Point{9, 7}, Point{1, 7}], false)
	_, open_closed := open.is_rect() or { panic('not a rect') }
	assert !open_closed
	// Colinear points along a side are allowed; diagonals are not.
	split := polygon([Point{1, 2}, Point{5, 2}, Point{9, 2}, Point{9, 7}, Point{1, 7}], true)
	assert split.is_rect() != none
	assert polygon([Point{1, 2}, Point{9, 3}, Point{9, 7}, Point{1, 7}], true).is_rect() == none
	// Two nested rectangles, the inner one reversed.
	mut b := PathBuilder{}
	for i, p in [Point{0, 0}, Point{20, 0}, Point{20, 10}, Point{0, 10}] {
		if i == 0 {
			b.move_to(p.x, p.y)
		} else {
			b.line_to(p.x, p.y)
		}
	}
	b.close()
	for i, p in [Point{2, 2}, Point{2, 8}, Point{18, 8}, Point{18, 2}] {
		if i == 0 {
			b.move_to(p.x, p.y)
		} else {
			b.line_to(p.x, p.y)
		}
	}
	b.close()
	rects, dirs := b.detach().nested_fill_rects() or { panic('not nested') }
	assert rects[0] == Rect.ltrb(0, 0, 20, 10) && rects[1] == Rect.ltrb(2, 2, 18, 8)
	assert dirs[0] == .cw && dirs[1] == .ccw
}

fn test_add_oval_alone_is_an_oval() {
	mut b := PathBuilder{}
	b.add_oval(Rect.ltrb(0, 0, 20, 10), .cw, 1)
	p := b.detach()
	assert p.is_oval() or { Rect{} } == Rect.ltrb(0, 0, 20, 10)
	// Start index 1: the right extreme.
	assert p.points[0] == Point{20, 5}
	b.add_oval(Rect.ltrb(0, 0, 20, 10), .cw, 1)
	b.line_to(3, 3)
	assert b.detach().is_oval() == none
}

fn test_midpoint_contours_split_at_moves() {
	mut b := PathBuilder{}
	b.move_to(0, 0)
	b.move_to(1, 1)
	b.line_to(3, 1)
	b.line_to(1, 3)
	b.close()
	b.move_to(10, 10)
	b.quad_to(12, 10, 12, 12)
	b.move_to(50, 50)
	contours := midpoint_contours(b.detach())
	assert contours.len == 2
	// The builder keeps only the last of consecutive moves; the trailing move
	// ends the second contour without starting a third.
	assert contours[0].verb_start == 0 && contours[0].point_start == 0
	third := f32(1) / f32(3)
	assert contours[0].midpoint == Point{f32(5) * third, f32(5) * third}
	assert contours[1].verb_start == 4 && contours[1].verb_end == 6
}

fn test_svg_arcs_match_sk_path_builder_bit_for_bit() {
	// Points and weights produced by SkPathBuilder::arcTo() of the pinned
	// Skia for the same arguments.
	mut b := PathBuilder{}
	b.move_to(f32(11.581105139828319), f32(50.0693373303035))
	b.arc_to_svg(f32(11.64792707484055), f32(4.2227373311956375), f32(-99.92622184274818),
		true, .ccw, f32(27.78401298036616), f32(4.989945955549562))
	p := b.detach()
	expected := [u32(0x41394c35), 0x42484700, 0x41c5391e, 0x42a20497, 0x42030473, 0x4269e087,
		0x42236c55, 0x420fb7df, 0x41de45a9, 0x409fada3]
	mut got := []u32{}
	for pt in p.points {
		got << math.f32_bits(pt.x)
		got << math.f32_bits(pt.y)
	}
	$if linux {
		// Independently captured with skia-python 144.1 on Linux x86_64/glibc 2.41.
		// Native sincosf rounds the sine of float32 angle bits 0xc06d4d34 one
		// ULP below Apple's __sincosf_stret, changing only this coordinate.
		// Accept either captured Skia vector on Linux: its libm/architecture
		// need not use glibc's rounding. Keep the macOS fixture exact below.
		// Reproduce the Linux capture with testdata/skia_svg_arc_reference.py.
		mut linux_expected := expected.clone()
		linux_expected[3] = 0x42a20496
		assert got == linux_expected || got == expected
	} $else {
		assert got == expected
	}
	assert p.verbs == [PathVerb.move, .conic, .conic]
	assert p.weights.map(math.f32_bits(it)) == [u32(0x3f3504f2), 0x3f3504f2]
	// Zero radii and coincident end points become lines.
	b.move_to(1, 1)
	b.arc_to_svg(0, 5, 0, false, .cw, 4, 4)
	b.arc_to_svg(3, 3, 0, false, .cw, 4, 4)
	assert b.detach().verbs == [PathVerb.move, .line, .line]
}

fn test_add_rrect_follows_sk_path_raw_shapes() {
	r := Point{5, 4}
	rr := RRect.rect_radii(Rect.ltrb(0, 0, 40, 30), [r, r, r, r]!)
	// SkPathBuilder::addRRect(rrect) starts clockwise at index 6 with a line.
	mut b := PathBuilder{}
	b.add_rrect(rr, .cw, 6)
	cw := b.detach()
	assert cw.verbs == [PathVerb.move, .line, .conic, .line, .conic, .line, .conic, .line, .conic,
		.close]
	assert cw.points == [Point{0, 26}, Point{0, 4}, Point{0, 0}, Point{5, 0}, Point{35, 0},
		Point{40, 0}, Point{40, 4}, Point{40, 26}, Point{40, 30}, Point{35, 30}, Point{5, 30},
		Point{0, 30}, Point{0, 26}]
	assert cw.is_a == .rrect
	// Counter-clockwise from index 7 (the default) also starts with a line.
	b.add_rrect(rr, .ccw, 7)
	ccw := b.detach()
	assert ccw.points == [Point{0, 4}, Point{0, 26}, Point{0, 30}, Point{5, 30}, Point{35, 30},
		Point{40, 30}, Point{40, 26}, Point{40, 4}, Point{40, 0}, Point{35, 0}, Point{5, 0},
		Point{0, 0}, Point{0, 4}]
	// Clockwise from an odd index starts with a conic and leaves the last line
	// to the close.
	b.add_rrect(rr, .cw, 7)
	conic_first := b.detach()
	assert conic_first.verbs == [PathVerb.move, .conic, .line, .conic, .line, .conic, .line, .conic,
		.close]
	assert conic_first.points.len == 12 && conic_first.points[1] == Point{0, 0}
	// Rects and ovals become those contours, and a second contour makes the
	// path general.
	b.add_rrect(RRect.from_rect(Rect.ltrb(1, 2, 9, 7)), .cw, 6)
	assert b.detach().points == [Point{1, 7}, Point{1, 2}, Point{9, 2}, Point{9, 7}]
	b.add_rrect(rr, .cw, 6)
	b.add_rrect(rr.inset(5, 5), .cw, 6)
	assert b.detach().is_a == .general
}

fn test_conservatively_contains_rect_follows_sk_path() {
	// A clockwise triangle with its right angle at the top left.
	tri := polygon([Point{0, 0}, Point{20, 0}, Point{0, 20}], true)
	assert path_conservatively_contains_rect(tri, Rect.ltrb(1, 1, 8, 8))
	assert !path_conservatively_contains_rect(tri, Rect.ltrb(1, 1, 12, 12))
	// The counter-clockwise triangle contains the same rects.
	ccw := polygon([Point{0, 0}, Point{0, 20}, Point{20, 0}], true)
	assert path_conservatively_contains_rect(ccw, Rect.ltrb(1, 1, 8, 8))
	// Concave paths never contain a rect.
	star := polygon([Point{32, 2}, Point{40, 44}, Point{6, 16}, Point{58, 16}, Point{24, 44}],
		true)
	assert !path_conservatively_contains_rect(star, Rect.ltrb(30, 20, 31, 21))
	// An oval is tested against the chords of its conics' halves.
	mut b := PathBuilder{}
	b.add_oval(Rect.ltrb(0, 0, 40, 40), .cw, 1)
	oval := b.detach()
	assert path_conservatively_contains_rect(oval, Rect.ltrb(10, 10, 30, 30))
	assert !path_conservatively_contains_rect(oval, Rect.ltrb(2, 2, 38, 38))
}

fn test_conic_quads_midpoint_follows_sk_conic_chop() {
	// The quarter circle from (20, 0) to (0, 20) around the origin.
	mid := conic_quads_midpoint(Point{20, 0}, Point{20, 20}, Point{0, 20}, f32(math.sqrt2 / 2)) or {
		panic('no midpoint')
	}
	assert math.abs(mid.x - 14.142136) < 1e-5 && math.abs(mid.y - 14.142136) < 1e-5
	assert conic_quads_midpoint(Point{0, 0}, Point{1, 1}, Point{2, 0}, -1) == none
}

fn test_path_contains_point_follows_sk_path_priv_contains() {
	square := polygon([Point{0, 0}, Point{10, 0}, Point{10, 10}, Point{0, 10}], true)
	assert path_contains_point(square, 5, 5)? == true
	assert path_contains_point(square, 15, 5)? == false
	// Points on an edge count as inside.
	assert path_contains_point(square, 0, 5)? == true
	assert path_contains_point(square, 5, 0)? == true
	// An open contour is closed by the edge iterator.
	open := polygon([Point{0, 0}, Point{10, 0}, Point{10, 10}, Point{0, 10}], false)
	assert path_contains_point(open, 5, 5)? == true
	// Nested squares with the even-odd rule leave a hole.
	mut b := PathBuilder{}
	b.add_rect(Rect.ltrb(0, 0, 10, 10), .cw, 0)
	b.add_rect(Rect.ltrb(2, 2, 8, 8), .cw, 0)
	b.set_fill_type(.even_odd)
	nested := b.detach()
	assert path_contains_point(nested, 5, 5)? == false
	assert path_contains_point(nested, 1, 5)? == true
	// Curves are not supported.
	b.add_oval(Rect.ltrb(0, 0, 10, 10), .cw, 1)
	assert path_contains_point(b.detach(), 5, 5) == none
}
