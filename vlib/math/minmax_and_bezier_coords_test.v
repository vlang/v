import math

fn test_minmax_orders_the_pair_ascending() {
	mut lo, mut hi := math.minmax(3.0, 7.0)
	assert lo == 3.0
	assert hi == 7.0

	lo, hi = math.minmax(7.0, 3.0)
	assert lo == 3.0
	assert hi == 7.0

	lo, hi = math.minmax(-4.0, -1.5)
	assert lo == -4.0
	assert hi == -1.5

	lo, hi = math.minmax(5.0, 5.0)
	assert lo == 5.0
	assert hi == 5.0

	lo, hi = math.minmax(0.0, -0.0)
	assert lo == 0.0
	assert hi == 0.0
}

fn test_cubic_bezier_coords_endpoints() {
	// documented: t == 0.0 yields (x0,y0) and t == 1.0 yields (x3,y3)
	x0, x1, x2, x3 := f64(0.0), 0.25, 0.75, 1.0
	y0, y1, y2, y3 := f64(0.0), 2.0, 8.0, 9.0
	start := math.cubic_bezier_coords(0.0, x0, x1, x2, x3, y0, y1, y2, y3)
	assert start.x == x0
	assert start.y == y0
	finish := math.cubic_bezier_coords(1.0, x0, x1, x2, x3, y0, y1, y2, y3)
	assert finish.x == x3
	assert finish.y == y3
}

fn test_cubic_bezier_coords_halfway_is_symmetric() {
	// with the control points mirrored around the midpoint, t == 0.5 lands there
	mid := math.cubic_bezier_coords(0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0)
	assert mid.x == 0.5
	assert mid.y == 0.5
	// the Bernstein weights sum to 1 for every t, so the result stays inside the hull
	for t in [f64(0.0), 0.1, 0.25, 0.5, 0.75, 0.9, 1.0] {
		p := math.cubic_bezier_coords(t, 0.0, 0.25, 0.75, 1.0, 0.0, 2.0, 8.0, 9.0)
		assert p.x >= 0.0 && p.x <= 1.0
		assert p.y >= 0.0 && p.y <= 9.0
	}
}

fn test_cubic_bezier_coords_independent_axes() {
	xc := [f64(0.0), 0.25, 0.75, 1.0]
	yc := [f64(0.0), 2.0, 8.0, 9.0]
	for t in [f64(0.0), 0.3, 0.6, 1.0] {
		p := math.cubic_bezier_coords(t, xc[0], xc[1], xc[2], xc[3], yc[0], yc[1], yc[2], yc[3])
		// changing only x coordinates leaves y unchanged
		q := math.cubic_bezier_coords(t, 1.0, 0.25, 0.75, 1.5, yc[0], yc[1], yc[2], yc[3])
		assert p.y == q.y
		// changing only y coordinates leaves x unchanged
		r := math.cubic_bezier_coords(t, xc[0], xc[1], xc[2], xc[3], 5.0, 2.0, 8.0, 9.0)
		assert p.x == r.x
	}
	// at t == 0.5 all three x coordinate sets give different points
	half := math.cubic_bezier_coords(0.5, xc[0], xc[1], xc[2], xc[3], yc[0], yc[1], yc[2], yc[3])
	other := math.cubic_bezier_coords(0.5, 1.0, 0.25, 0.75, 1.5, yc[0], yc[1], yc[2], yc[3])
	assert half.y == other.y
	assert half.x != other.x
}
