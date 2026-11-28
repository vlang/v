module game

import gg

fn test_point_shapes() {
	r := gg.Rect{10, 10, 100, 50}
	assert point_in_rect(gg.Vec2{20, 20}, r)
	assert !point_in_rect(gg.Vec2{5, 20}, r)
	assert point_in_rect(gg.Vec2{110, 60}, r)
	c := Circle{0, 0, 10}
	assert point_in_circle(gg.Vec2{3, 4}, c)
	assert !point_in_circle(gg.Vec2{10, 10}, c)
	q := closest_point_on_rect(gg.Vec2{0, 20}, r)
	assert q.x == 10 && q.y == 20
}

fn test_shape_overlaps() {
	a := gg.Rect{0, 0, 10, 10}
	b := gg.Rect{5, 5, 10, 10}
	c := gg.Rect{20, 20, 5, 5}
	assert rects_overlap(a, b)
	assert !rects_overlap(a, c)
	assert circles_overlap(Circle{0, 0, 5}, Circle{8, 0, 5})
	assert !circles_overlap(Circle{0, 0, 5}, Circle{20, 0, 5})
	assert circle_rect_overlap(Circle{0, 5, 4}, a)
	assert !circle_rect_overlap(Circle{-10, 5, 4}, a)
}

fn test_point_in_polygon() {
	tri := [gg.Vec2{0, 0}, gg.Vec2{10, 0}, gg.Vec2{0, 10}]
	assert point_in_polygon(gg.Vec2{2, 2}, tri)
	assert !point_in_polygon(gg.Vec2{9, 9}, tri)
	assert !point_in_polygon(gg.Vec2{2, 2}, [gg.Vec2{0, 0}])
}

fn test_rect_mtv() {
	a := gg.Rect{0, 0, 10, 10}
	b := gg.Rect{8, 0, 10, 10}
	m := rect_mtv(a, b)
	assert m.x < 0 && m.y == 0
	assert rect_mtv(a, gg.Rect{50, 50, 5, 5}) == gg.Vec2{}
}

fn test_3d_collisions() {
	box := aabb_from_center(gg.Vec3{0, 0, 0}, gg.Vec3{2, 2, 2})
	assert aabb_overlap(box, aabb_from_center(gg.Vec3{1, 0, 0}, gg.Vec3{2, 2, 2}))
	assert !aabb_overlap(box, aabb_from_center(gg.Vec3{5, 0, 0}, gg.Vec3{2, 2, 2}))
	assert sphere_overlap(gg.Vec3{0, 0, 0}, 1, gg.Vec3{1.5, 0, 0}, 1)
	assert !sphere_overlap(gg.Vec3{0, 0, 0}, 1, gg.Vec3{5, 0, 0}, 1)
	assert sphere_aabb_overlap(gg.Vec3{1.5, 0, 0}, 0.6, box)
	assert !sphere_aabb_overlap(gg.Vec3{5, 0, 0}, 0.6, box)
}

fn test_ray_aabb() {
	box := aabb_from_center(gg.Vec3{0, 0, 0}, gg.Vec3{2, 2, 2})
	hit := ray_aabb(gg.Ray{gg.Vec3{-5, 0, 0}, gg.Vec3{1, 0, 0}}, box)
	assert hit.hit
	assert (hit.distance - 4) < 0.001
	miss := ray_aabb(gg.Ray{gg.Vec3{-5, 5, 0}, gg.Vec3{1, 0, 0}}, box)
	assert !miss.hit
}

// Regression: a distant box must still be reachable. Hardcoded slab bounds of
// +/-999999 made the slab intersection clamp below the true hit distance.
fn test_ray_aabb_distant_box() {
	box := aabb_from_center(gg.Vec3{2_000_000, 0, 0}, gg.Vec3{2, 2, 2})
	hit := ray_aabb(gg.Ray{gg.Vec3{0, 0, 0}, gg.Vec3{1, 0, 0}}, box)
	assert hit.hit
	assert (hit.distance - 1_999_999) < 0.001
	assert hit.point.x > 1_999_998 && hit.point.x < 2_000_000
	miss := ray_aabb(gg.Ray{gg.Vec3{0, 5, 0}, gg.Vec3{1, 0, 0}}, box)
	assert !miss.hit
}
