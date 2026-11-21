module gg

fn test_closest_point_and_distance() {
	a := Vec2{0, 0}
	b := Vec2{10, 0}
	q := closest_point_on_segment(Vec2{5, 4}, a, b)
	assert q.x == 5 && q.y == 0
	start := closest_point_on_segment(Vec2{-3, 2}, a, b)
	assert start.x == 0 && start.y == 0
	assert dist_point_segment(Vec2{5, 3}, a, b) == 3
}

fn test_segment_intersection() {
	p := segments_intersect(Vec2{0, 0}, Vec2{10, 10}, Vec2{0, 10}, Vec2{10, 0})
	assert p != none
	hit := p or { Vec2{} }
	assert hit.x == 5 && hit.y == 5
	assert segments_intersect(Vec2{0, 0}, Vec2{1, 1}, Vec2{0, 1}, Vec2{1, 2}) == none
}

fn test_polygon_area_centroid() {
	square := [Vec2{0, 0}, Vec2{10, 0}, Vec2{10, 10}, Vec2{0, 10}]
	assert polygon_area(square) == 100
	c := polygon_centroid(square)
	assert c.x == 5 && c.y == 5
	assert polygon_area([Vec2{0, 0}, Vec2{1, 1}]) == 0
}

fn test_convex_hull() {
	pts := [Vec2{0, 0}, Vec2{5, 5}, Vec2{10, 0}, Vec2{10, 10}, Vec2{0, 10}, Vec2{5, 1}]
	hull := convex_hull(pts)
	assert hull.len == 4
	assert polygon_area(hull) == 100
	assert convex_hull([Vec2{1, 1}, Vec2{2, 2}]).len == 2
}
