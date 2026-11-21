module gg

fn test_catmull_endpoints() {
	pts := [Vec2{0, 0}, Vec2{10, 0}, Vec2{10, 10}]
	a := sample_catmull(pts, 0)
	assert a.x == 0 && a.y == 0
	b := sample_catmull(pts, 1)
	assert b.x == 10 && b.y == 10
	assert sample_catmull([], 0.5) == Vec2{}
	assert sample_catmull([Vec2{3, 4}], 0.5).x == 3
}

fn test_catmull_midpoint() {
	pts := [Vec2{0, 0}, Vec2{5, 10}, Vec2{10, 0}]
	p := sample_catmull(pts, 0.5)
	assert p.x == 5
	assert p.y == 10
	q := sample_catmull(pts, 0.25)
	assert q.x > 0 && q.x < 5
	assert q.y > 0 && q.y < 10
}

fn test_bezier2() {
	a := bezier2(Vec2{0, 0}, Vec2{0, 10}, Vec2{10, 10}, Vec2{10, 0}, 0)
	assert a.x == 0 && a.y == 0
	b := bezier2(Vec2{0, 0}, Vec2{0, 10}, Vec2{10, 10}, Vec2{10, 0}, 1)
	assert b.x == 10 && b.y == 0
	m := bezier2(Vec2{0, 0}, Vec2{0, 10}, Vec2{10, 10}, Vec2{10, 0}, 0.5)
	assert m.x > 4.9 && m.x < 5.1
	assert m.y > 7.4 && m.y < 7.6
}

fn test_tangent_and_resample() {
	pts := [Vec2{0, 0}, Vec2{10, 0}, Vec2{20, 0}]
	t := catmull_tangent(pts, 0.5)
	assert t.x > 0.99
	out := resample_polyline(pts, 5)
	assert out.len == 5
	assert out[0].x == 0
	assert out[4].x == 20
	assert out[2].x > 9.9 && out[2].x < 10.1
	assert resample_polyline([Vec2{0, 0}, Vec2{0, 0}], 5).len == 0
}
