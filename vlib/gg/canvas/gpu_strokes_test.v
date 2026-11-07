module canvas

import math

fn test_cubic_convex_180_chops_follow_skia() {
	// Expected values from skgpu::tess::FindCubicConvex180Chops() of the
	// pinned Skia.
	t, n, cusps := find_cubic_convex_180_chops([Point{0, 0}, Point{10, 10}, Point{0, 10}, Point{10, 0}]!)
	assert n == 1 && t[0] == 0.5 && cusps
	t2, n2, cusps2 := find_cubic_convex_180_chops([Point{0, 0}, Point{30, 30}, Point{-20, 30},
		Point{10, 0}]!)
	assert n2 == 1 && t2[0] == 0.625 && !cusps2
	_, n3, cusps3 := find_cubic_convex_180_chops([Point{0, 0}, Point{0, 10}, Point{10, 10},
		Point{10, 0}]!)
	assert n3 == 0 && !cusps3
	t4, n4, cusps4 := find_cubic_convex_180_chops([Point{0, 0}, Point{10, 0}, Point{-5, 0},
		Point{5, 0}]!)
	assert n4 == 2 && cusps4
	assert t4[0] == f32(0.276393205) && t4[1] == f32(0.723606825)
}

fn test_chop_cubic_at_splits_at_the_parameter() {
	c := chop_cubic_at([Point{0, 0}, Point{0, 8}, Point{8, 8}, Point{8, 0}]!, 0.5)
	assert c[3] == Point{4, 6}
	assert c[0] == Point{0, 0} && c[6] == Point{8, 0}
}

fn test_stroke_vertex_counts_follow_linear_tolerances() {
	mut t := StrokeTolerances{}
	// A 2px miter stroke at scale 1: radial segments plus 4 join edges.
	t.set_stroke(1, 4, 1)
	assert t.edges_in_joins == 4
	// Round joins add the radial edges of a 180 degree turn.
	mut r := StrokeTolerances{}
	r.set_stroke(10, -1, 1)
	assert r.edges_in_joins > 3
	assert stroke_vertex_count(t) == u32(t.required_stroke_edges() * 2)
}

fn test_closed_rrect_stroke_defers_its_first_patch() {
	r := Point{12, 6}
	rr := RRect.rect_radii(Rect.ltrb(8, 6, 56, 40), [r, r, r, r]!)
	mut w := new_stroke_writer(Matrix.identity(), 1, 1.5, 4, 0.5)
	write_stroke_patches(mut w, element_path(rrect_shape(rr)), .butt) or { panic(err) }
	// Four lines and four conics; the first line is written last, joined to
	// the closing conic.
	assert w.count == 8
	last := unsafe { &f32(&w.out[7 * stroke_instance_stride]) }
	assert unsafe { last[0] } == 8 && unsafe { last[1] } == 34
}

fn stroke_patch_points(w StrokeWriter, i int) []f32 {
	mut out := []f32{}
	for k in 0 .. 8 {
		o := i * stroke_instance_stride + k * 4
		out << math.f32_from_bits(u32(w.out[o]) | (u32(w.out[o + 1]) << 8) | (u32(w.out[o + 2]) << 16) | (u32(w.out[o +
			3]) << 24))
	}
	return out
}

fn test_flat_quad_cusps_stroke_as_a_circle_and_two_lines() {
	// A quad that turns around at its midtangent on the line y = 5.
	assert conic_has_cusp(Point{0, 5}, Point{20, 5}, Point{10, 5})
	t := find_quad_mid_tangent(Point{0, 5}, Point{20, 5}, Point{10, 5})
	cusp := eval_quad_at(Point{0, 5}, Point{20, 5}, Point{10, 5}, t)
	assert cusp.y == 5 && cusp.x > 10 && cusp.x <= 20
	mut b := PathBuilder{}
	b.move_to(0, 5)
	b.quad_to(20, 5, 10, 5)
	mut w := new_stroke_writer(Matrix.identity(), 1, 1, 4, 0.5)
	write_stroke_patches(mut w, b.detach(), .butt)!
	// The circle, the line to the cusp (deferred first, then written by the
	// end caps), the line back.
	circle := stroke_patch_points(w, 0)
	assert circle[0] == cusp.x && circle[6] == cusp.x && circle[1] == 5
	// The conic midtangent of a symmetric turnaround is at its middle.
	assert conic_find_mid_tangent(Point{0, 0}, Point{10, 0}, Point{0, 0}, 1) == f32(0.5)
}

fn test_long_stroked_curves_are_chopped() {
	mut b := PathBuilder{}
	b.move_to(0, 0)
	b.cubic_to(4000, 0, -3000, 4000, 30, 30)
	mut w := new_stroke_writer(Matrix.identity(), 1, 1, 4, 0.5)
	write_stroke_patches(mut w, b.detach(), .butt)!
	n4 := cubic_p4(4, Point{0, 0}, Point{4000, 0}, Point{-3000, 4000}, Point{30, 30},
		Matrix.identity())
	chops := account_for_curve(n4) or { panic('not chopped') }
	// Each piece is a patch (the first one deferred until the caps are
	// written), and the template uses the largest parametric resolve level.
	assert w.count >= chops
	assert w.template.n4 == max_parametric_segments_p4
}
