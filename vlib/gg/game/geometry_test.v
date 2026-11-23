module game

import gg

fn test_closest_point_and_distance() {
	a := gg.Vec2{0, 0}
	b := gg.Vec2{10, 0}
	q := closest_point_on_segment(gg.Vec2{5, 4}, a, b)
	assert q.x == 5 && q.y == 0
	start := closest_point_on_segment(gg.Vec2{-3, 2}, a, b)
	assert start.x == 0 && start.y == 0
	assert dist_point_segment(gg.Vec2{5, 3}, a, b) == 3
}

fn test_segment_intersection() {
	p := segments_intersect(gg.Vec2{0, 0}, gg.Vec2{10, 10}, gg.Vec2{0, 10}, gg.Vec2{10, 0})
	assert p != none
	hit := p or { gg.Vec2{} }
	assert hit.x == 5 && hit.y == 5
	assert segments_intersect(gg.Vec2{0, 0}, gg.Vec2{1, 1}, gg.Vec2{0, 1}, gg.Vec2{1, 2}) == none
}

fn test_polygon_area_centroid() {
	square := [gg.Vec2{0, 0}, gg.Vec2{10, 0}, gg.Vec2{10, 10}, gg.Vec2{0, 10}]
	assert polygon_area(square) == 100
	c := polygon_centroid(square)
	assert c.x == 5 && c.y == 5
	assert polygon_area([gg.Vec2{0, 0}, gg.Vec2{1, 1}]) == 0
	// Winding must not change the promised absolute area.
	cw := [gg.Vec2{0, 0}, gg.Vec2{0, 2}, gg.Vec2{2, 2}, gg.Vec2{2, 0}]
	assert polygon_area(cw) == 4
}

fn test_convex_hull() {
	pts := [gg.Vec2{0, 0}, gg.Vec2{5, 5}, gg.Vec2{10, 0}, gg.Vec2{10, 10}, gg.Vec2{0, 10},
		gg.Vec2{5, 1}]
	hull := convex_hull(pts)
	assert hull.len == 4
	assert polygon_area(hull) == 100
	assert convex_hull([gg.Vec2{1, 1}, gg.Vec2{2, 2}]).len == 2
}
