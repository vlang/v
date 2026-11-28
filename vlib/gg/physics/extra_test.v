module physics

import gg

fn test_collider_overlaps() {
	a := ball(gg.Vec3{0, 0, 0}, 1)
	b := ball(gg.Vec3{1.5, 0, 0}, 1)
	c := ball(gg.Vec3{3, 0, 0}, 1)
	assert overlaps(a, b)
	assert !overlaps(a, c)
	bx := box(gg.Vec3{2, 0, 0}, gg.Vec3{0.5, 0.5, 0.5})
	assert !overlaps(a, bx)
	assert overlaps(ball(gg.Vec3{1.6, 0, 0}, 0.5), bx)
	cap := capsule(gg.Vec3{1, 0, 0}, 0.5, 2)
	assert overlaps(a, cap)
}

fn test_broadphase() {
	cs := [ball(gg.Vec3{0, 0, 0}, 1), ball(gg.Vec3{10, 0, 0}, 1), ball(gg.Vec3{1, 0, 0}, 1)]
	pairs := broadphase(cs)
	assert pairs.len == 1
	assert pairs[0].a == 0 || pairs[0].b == 0
}

fn test_joints_and_controller() {
	a := new_body(1, gg.Vec3{0, 0, 0}, 0.5)
	b := new_body(2, gg.Vec3{3, 0, 0}, 0.5)
	mut j := new_distance_joint(a, b, 2)
	j.solve(0.1)
	assert a.velocity.x > 0 && b.velocity.x < 0
	mut sj := new_spring_joint(a, gg.Vec3{0, 5, 0})
	sj.solve(0.1)
	assert a.velocity.y > 0

	body := new_body(3, gg.Vec3{0, -9, 0}, 0.5)
	mut ctrl := new_character_controller(body)
	ctrl.floor_y = -10
	ctrl.grounded = true
	ctrl.update(0.1, 1, 0, true)
	assert body.velocity.x == 4
	assert body.velocity.y == 5
	assert !ctrl.grounded
}
