import math

fn test_mix() {
	assert math.mix(0.0, 100.0, 0.0) == 0.0
	assert math.mix(0.0, 100.0, 0.1) == 10.0
	assert math.mix(0.0, 100.0, 0.2) == 20.0
	assert math.mix(0.0, 100.0, 0.5) == 50.0
	assert math.mix(0.0, 100.0, 0.8) == 80.0
	assert math.mix(0.0, 100.0, 0.9) == 90.0
	assert math.mix(0.0, 100.0, 1.0) == 100.0

	assert math.mix(100.0, 500.0, 0.0) == 100.0
	assert math.mix(100.0, 500.0, 0.1) == 140.0
	assert math.mix(100.0, 500.0, 0.2) == 180.0
	assert math.mix(100.0, 500.0, 0.5) == 300.0
	assert math.mix(100.0, 500.0, 0.8) == 420.0
	assert math.mix(100.0, 500.0, 0.9) == 460.0
	assert math.mix(100.0, 500.0, 1.0) == 500.0
}

fn test_clip() {
	assert math.clip(0.0, 10.0, 50.0) == 10.0
	assert math.clip(5.5, 10.0, 50.0) == 10.0
	assert math.clip(10.0, 10.0, 50.0) == 10.0
	assert math.clip(20.0, 10.0, 50.0) == 20.0
	assert math.clip(50.0, 10.0, 50.0) == 50.0
	assert math.clip(80.0, 10.0, 50.0) == 50.0
	assert math.clip(90.5, 10.0, 50.0) == 50.0

	assert math.clip(0, 10, 50) == 10
	assert math.clip(5, 10, 50) == 10
	assert math.clip(10, 10, 50) == 10
	assert math.clip(20, 10, 50) == 20
	assert math.clip(50, 10, 50) == 50
	assert math.clip(80, 10, 50) == 50
	assert math.clip(90, 10, 50) == 50
}

// The test curve control points are taken from: https://cubic-bezier.com/#.19,-0.09,.42,1.19
const b = [
	math.BezierPoint{0, 0},
	math.BezierPoint{0.19, -0.09},
	math.BezierPoint{0.42, 1.19},
	math.BezierPoint{1.0, 1.0},
]
const bx = b.map(it.x)
const by = b.map(it.y)
const bx_fa = [4]f64{init: b[index].x}
const by_fa = [4]f64{init: b[index].y}

fn test_cubic_bezier() {
	assert math.cubic_bezier(0.0, b) == math.BezierPoint{b[0].x, b[0].x}
	assert math.cubic_bezier(0.5, b) == math.BezierPoint{0.35375, 0.5375}
	assert math.cubic_bezier(1.0, b) == math.BezierPoint{b[3].x, b[3].x}
}

fn test_cubic_bezier_a() {
	assert math.cubic_bezier_a(0.0, bx, by) == math.BezierPoint{bx[0], by[0]}
	assert math.cubic_bezier_a(0.5, bx, by) == math.BezierPoint{0.35375, 0.5375}
	assert math.cubic_bezier_a(1.0, bx, by) == math.BezierPoint{bx[3], by[3]}
}

fn test_cubic_bezier_fa() {
	assert math.cubic_bezier_fa(0.0, bx_fa, by_fa) == math.BezierPoint{bx_fa[0], by_fa[0]}
	assert math.cubic_bezier_fa(0.5, bx_fa, by_fa) == math.BezierPoint{0.35375, 0.5375}
	assert math.cubic_bezier_fa(1.0, bx_fa, by_fa) == math.BezierPoint{bx_fa[3], by_fa[3]}
}
