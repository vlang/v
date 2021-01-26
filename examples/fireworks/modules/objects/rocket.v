module objects

import gg
import gx
import rand

pub struct Rocket {
pub mut:
	color     gx.Color
	pos       Vector
	vel       Vector
	accel     Vector
	exploded  bool
	particles []Particle
	dead      bool
}

pub fn (rocket Rocket) draw(mut ctx gg.Context) {
	ctx.draw_circle(rocket.pos.x, get_params().height - rocket.pos.y, get_params().rocket_radius,
		rocket.color)
}

pub fn (mut rocket Rocket) explode() {
	rocket.exploded = true

	for _ in 0 .. get_params().offspring_count {
		rocket.spawn_particle()
	}
}

pub fn (mut rocket Rocket) tick(mut ctx gg.Context) {
	if !rocket.exploded {
		if rocket.vel.y <= 1 {
			rocket.explode()
		}

		rocket.accel += get_params().gravity
		rocket.vel += rocket.accel
		rocket.pos += rocket.vel
		rocket.draw(mut ctx)

		rocket.accel = {}
	}

	for mut particle in rocket.particles {
		particle.tick(mut rocket, mut ctx)
	}
}

pub fn new_rocket() Rocket {
	return Rocket{
		color: random_color()
		pos: {
			x: rand.f32_in_range(50, get_params().width - 50)
		}
		vel: {
			x: rand.f32_in_range(-1.5, 1.5)
			y: rand.f32_in_range(5, 7)
		}
	}
}

pub fn (mut rocket Rocket) spawn_particle() {
	rocket.particles << Particle{
		color: rocket.color
		pos: rocket.pos
		accel: random_vector_in_circle().mult(2)
	}
}
