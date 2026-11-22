// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module fx

import gg.anim
import math
import gg

// Particle is one emitter particle. Dead particles stay in the emitter's pool
// and are reused, so iterating the pool never allocates.
pub struct Particle {
pub mut:
	position gg.Vec2
	velocity gg.Vec2
	life     f32
	max_life f32
	size     f32
	color    gg.Color
	rotation f32
	spin     f32
	active   bool
}

// ParticleConfig describes how an emitter spawns and evolves particles.
pub struct ParticleConfig {
pub mut:
	rate           f32 = 20
	life           f32 = 1
	life_variance  f32
	speed          f32 = 100
	speed_variance f32
	direction      f32
	spread         f32 = 2 * math.pi
	size           f32 = 4
	gravity        gg.Vec2
	drag           f32
	start_color    gg.Color = gg.white
	end_color      gg.Color = gg.Color{0, 0, 0, 0}
	max_particles  int      = 512
}

// ParticleEmitter simulates a pooled particle system. It is pure V, so the
// same simulation runs on every backend; draw() is native only.
pub struct ParticleEmitter {
pub mut:
	config      ParticleConfig
	position    gg.Vec2
	particles   []Particle
	accumulator f32
	rng         u32 = 0x9e3779b9
}

// new_emitter returns an empty emitter with the given configuration.
pub fn new_emitter(config ParticleConfig) ParticleEmitter {
	return ParticleEmitter{ config: config }
}

// seed sets the deterministic random stream used for spawning.
pub fn (mut e ParticleEmitter) seed(value u32) {
	e.rng = value
}

fn (mut e ParticleEmitter) next_f32() f32 {
	e.rng ^= e.rng << 13
	e.rng ^= e.rng >> 17
	e.rng ^= e.rng << 5
	return f32(e.rng & 0x00ffffff) / f32(0x00ffffff)
}

fn (mut e ParticleEmitter) spawn_at(at gg.Vec2) {
	mut idx := -1
	for i in 0 .. e.particles.len {
		if !e.particles[i].active {
			idx = i
			break
		}
	}
	if idx < 0 {
		if e.particles.len >= e.config.max_particles {
			return
		}
		e.particles << Particle{}
		idx = e.particles.len - 1
	}
	c := e.config
	angle := c.direction + (e.next_f32() - 0.5) * c.spread
	speed := c.speed + (e.next_f32() - 0.5) * 2 * c.speed_variance
	mut life := c.life + (e.next_f32() - 0.5) * 2 * c.life_variance
	if life <= 0 {
		life = 0.01
	}
	spin := (e.next_f32() - 0.5) * 4
	e.particles[idx] = Particle{
		position: at
		velocity: gg.Vec2{f32(math.cos(f64(angle))) * speed, f32(math.sin(f64(angle))) * speed}
		life:     life
		max_life: life
		size:     c.size
		color:    c.start_color
		rotation: angle
		spin:     spin
		active:   true
	}
}

// update advances the simulation by dt, spawning new particles from rate and
// integrating all active ones. Spawns per update are capped so a large dt
// cannot freeze the frame; any excess accumulator is dropped.
pub fn (mut e ParticleEmitter) update(dt f32) {
	if dt <= 0 {
		return
	}
	e.accumulator += dt * e.config.rate
	max_spawns := 128
	mut spawned := 0
	for e.accumulator >= 1 && spawned < max_spawns {
		e.accumulator -= 1
		e.spawn_at(e.position)
		spawned++
	}
	if spawned == max_spawns && e.accumulator >= 1 {
		e.accumulator = 0
	}
	c := e.config
	for i in 0 .. e.particles.len {
		mut p := e.particles[i]
		if !p.active {
			continue
		}
		p.life -= dt
		if p.life <= 0 {
			p.active = false
			e.particles[i] = p
			continue
		}
		p.velocity = p.velocity + c.gravity.scale(dt)
		if c.drag > 0 {
			p.velocity = p.velocity.scale(math.clip(1 - c.drag * dt, 0, 1))
		}
		p.position = p.position + p.velocity.scale(dt)
		p.rotation += p.spin * dt
		p.color = anim.lerp_color(c.start_color, c.end_color, 1 - p.life / p.max_life)
		e.particles[i] = p
	}
}

// burst spawns count particles immediately at at, ignoring rate.
pub fn (mut e ParticleEmitter) burst(count int, at gg.Vec2) {
	for _ in 0 .. count {
		e.spawn_at(at)
	}
}

// active_count returns the number of live particles.
pub fn (e &ParticleEmitter) active_count() int {
	mut n := 0
	for p in e.particles {
		if p.active {
			n++
		}
	}
	return n
}
