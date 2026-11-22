module physics

import gg
import math

fn test_gravity_and_floor() {
	mut world := new_world()
	world.floor_y = 0
	b := new_body(1, gg.Vec3{0, 5, 0}, 1)
	world.add(b)
	for _ in 0 .. 200 {
		world.step(1.0 / 60)
	}
	assert b.position.y >= 1 - 0.001
	assert b.position.y < 1.2
}

fn test_is_static_body_supports() {
	mut world := new_world()
	world.gravity = gg.Vec3{}
	world.floor_y = -100
	mut ground := new_body(1, gg.Vec3{0, 0, 0}, 2)
	ground.is_static = true
	ball := new_body(2, gg.Vec3{0.5, 0, 0}, 1)
	world.add(ground)
	world.add(ball)
	world.step(1.0 / 60)
	assert ground.position.x == 0
	assert ball.position.x > 0.5
}

fn test_sweep_reaches_large_static_body_after_small_one() {
	mut world := new_world()
	world.gravity = gg.Vec3{}
	world.floor_y = -100
	a := new_body(1, gg.Vec3{0, 0, 0}, 0.5)
	mut b := new_body(2, gg.Vec3{2, 0, 0}, 0.5)
	mut c := new_body(3, gg.Vec3{3, 0, 0}, 5)
	b.is_static = true
	c.is_static = true
	world.add(a)
	world.add(b)
	world.add(c)
	world.step(1.0 / 60)
	// The small body at x=2 does not overlap a, but the large one at x=3 does.
	assert a.position.x < -1
}

fn test_non_positive_mass_is_immovable() {
	mut world := new_world()
	world.gravity = gg.Vec3{}
	world.floor_y = -100
	mut a := new_body(1, gg.Vec3{0, 0, 0}, 1)
	a.mass = 0
	b := new_body(2, gg.Vec3{0.5, 0, 0}, 1)
	world.add(a)
	world.add(b)
	world.step(1.0 / 60)
	assert math.is_finite(f64(a.position.x))
	assert math.is_finite(f64(b.position.x))
	assert a.position.x == 0
	assert b.position.x > 0.5
}

fn test_distance_joint_non_positive_mass() {
	mut a := new_body(1, gg.Vec3{0, 0, 0}, 1)
	a.mass = 0
	b := new_body(2, gg.Vec3{2, 0, 0}, 1)
	mut joint := new_distance_joint(a, b, 1)
	joint.solve(1.0 / 60)
	assert math.is_finite(f64(a.velocity.x))
	assert math.is_finite(f64(b.velocity.x))
	assert a.velocity.x == 0
}
