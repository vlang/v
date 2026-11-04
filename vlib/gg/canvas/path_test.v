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
