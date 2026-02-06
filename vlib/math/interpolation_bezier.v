module math

// BezierPoint represents point coordinates as floating point numbers.
// This type is used as the output of the cubic_bezier family of functions.
pub struct BezierPoint {
pub mut:
	x f64
	y f64
}

// cubic_bezier returns a linear interpolation between the control points,
// specified by their X and Y coordinates in a single array of points `p`, and given the parameter t,
// varying between 0.0 and 1.0 .
// When `t` == 0.0, the output is P[0] .
// When `t` == 1.0, the output is P[3] .
// The points x[1],y[1] and x[2],y[2], serve as attractors.
@[direct_array_access; inline]
pub fn cubic_bezier(t f64, p []BezierPoint) BezierPoint {
	if p.len != 4 {
		panic('invalid p.len')
	}
	return cubic_bezier_coords(t, p[0].x, p[1].x, p[2].x, p[3].x, p[0].y, p[1].y, p[2].y,
		p[3].y)
}

// cubic_bezier_a returns a linear interpolation between the control points,
// specified by their X and Y coordinates in 2 arrays, and given the parameter t,
// varying between 0.0 and 1.0 .
// When `t` == 0.0, the output is x[0],y[0] .
// When `t` == 1.0, the output is x[3],y[3] .
// The points x[1],y[1] and x[2],y[2], serve as attractors.
@[direct_array_access; inline]
pub fn cubic_bezier_a(t f64, x []f64, y []f64) BezierPoint {
	if x.len != 4 {
		panic('invalid x.len')
	}
	if y.len != 4 {
		panic('invalid y.len')
	}
	return cubic_bezier_coords(t, x[0], x[1], x[2], x[3], y[0], y[1], y[2], y[3])
}

// cubic_bezier_fa returns a linear interpolation between the control points,
// specified by their X and Y coordinates in 2 fixed arrays, and given the parameter t,
// varying between 0.0 and 1.0 .
// When `t` == 0.0, the output is x[0],y[0] .
// When `t` == 1.0, the output is x[3],y[3] .
// The points x[1],y[1] and x[2],y[2], serve as attractors.
@[direct_array_access; inline]
pub fn cubic_bezier_fa(t f64, x [4]f64, y [4]f64) BezierPoint {
	return cubic_bezier_coords(t, x[0], x[1], x[2], x[3], y[0], y[1], y[2], y[3])
}

// cubic_bezier_coords returns a linear interpolation between the control points,
// specified by their X and Y coordinates, and given the parameter t,
// varying between 0.0 and 1.0 .
// When `t` == 0.0, the output is x0,y0 .
// When `t` == 1.0, the output is x3,y3 .
// The points x1,y1 and x2,y2, serve as attractors.
@[inline]
pub fn cubic_bezier_coords(t f64, x0 f64, x1 f64, x2 f64, x3 f64, y0 f64, y1 f64, y2 f64, y3 f64) BezierPoint {
	p0 := pow(1 - t, 3)
	p1 := 3 * t * pow(1 - t, 2)
	p2 := 3 * (1 - t) * pow(t, 2)
	p3 := pow(t, 3)
	xt := p0 * x0 + p1 * x1 + p2 * x2 + p3 * x3
	yt := p0 * y0 + p1 * y1 + p2 * y2 + p3 * y3
	return BezierPoint{xt, yt}
}
