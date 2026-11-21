module gg

fn test_particle_burst_and_lifetime() {
	mut e := new_emitter(ParticleConfig{
		life:          0.5
		rate:          0
		speed:         0
		max_particles: 8
	})
	e.seed(1)
	e.burst(5, Vec2{10, 20})
	assert e.active_count() == 5
	for p in e.particles {
		if p.active {
			assert p.position.x == 10 && p.position.y == 20
		}
	}
	e.update(0.25)
	assert e.active_count() == 5
	e.update(0.3)
	assert e.active_count() == 0
}

fn test_particle_rate_and_pool() {
	mut e := new_emitter(ParticleConfig{
		rate:          10
		life:          10
		max_particles: 8
	})
	e.seed(42)
	e.update(1)
	assert e.active_count() == 8
	e.update(1)
	assert e.active_count() == 8
}

fn test_particle_determinism() {
	cfg := ParticleConfig{ life: 2, speed: 50, spread: 6.28 }
	mut a := new_emitter(cfg)
	mut b := new_emitter(cfg)
	a.seed(7)
	b.seed(7)
	a.burst(4, Vec2{})
	b.burst(4, Vec2{})
	for i in 0 .. 4 {
		assert a.particles[i].velocity.x == b.particles[i].velocity.x
		assert a.particles[i].velocity.y == b.particles[i].velocity.y
	}
}

fn test_particle_gravity_and_drag() {
	mut e := new_emitter(ParticleConfig{
		life:    10
		speed:   100
		gravity: Vec2{0, 100}
		drag:    1
	})
	e.seed(3)
	e.burst(1, Vec2{})
	before := e.particles[0].velocity.x
	before_mag := if before < 0 { -before } else { before }
	before_y := e.particles[0].velocity.y
	e.update(0.1)
	after := e.particles[0].velocity.x
	after_mag := if after < 0 { -after } else { after }
	assert after_mag < before_mag
	assert e.particles[0].velocity.y > before_y
}
