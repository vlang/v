module canvas

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
