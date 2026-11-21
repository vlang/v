module physics

import gg

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
