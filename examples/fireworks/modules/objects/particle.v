module objects

import gg
import gx

pub struct Particle {
pub mut:
	color    gx.Color
	pos      Vector
	vel      Vector
	accel    Vector
	lifespan f32 = 255
}

pub fn (particle Particle) draw(mut ctx gg.Context) {
	ctx.draw_circle_filled(particle.pos.x, get_params().height - particle.pos.y, get_params().particle_radius,
		particle.color)
}

pub fn (mut particle Particle) tick(mut rocket Rocket, mut ctx gg.Context) {
	particle.lifespan -= get_params().age_rate
	particle.color.a = u8(particle.lifespan)

	if particle.lifespan <= 0 {
		rocket.dead = true
		return
	}

	particle.accel += get_params().gravity
	particle.vel += particle.accel
	particle.vel = particle.vel.mult(get_params().drag)
	particle.pos += particle.vel
	particle.draw(mut ctx)

	particle.accel = Vector{}
}
