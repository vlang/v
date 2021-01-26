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
	ctx.draw_circle(particle.pos.x, height - particle.pos.y, particle_radius, particle.color)
}

pub fn (mut particle Particle) tick(mut rocket Rocket, mut ctx gg.Context) {
	particle.lifespan -= age_rate
	particle.color.a = byte(particle.lifespan)

	if particle.lifespan <= 0 {
		rocket.dead = true
		return
	}

	particle.accel += gravity
	particle.vel += particle.accel
	particle.vel = particle.vel.mult(drag)
	particle.pos += particle.vel
	particle.draw(mut ctx)

	particle.accel = {}
}
