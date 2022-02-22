// Copyright(C) 2019 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package
module particle

import particle.vec2
import rand
import sokol.sgl

pub struct SystemConfig {
	pool int
}

pub struct System {
	width  int
	height int
mut:
	pool []&Particle
	bin  []&Particle
}

pub fn (mut s System) init(sc SystemConfig) {
	unsafe { s.pool.flags.set(.noslices) }
	unsafe { s.bin.flags.set(.noslices) }
	for i := 0; i < sc.pool; i++ {
		p := new(vec2.Vec2{f32(s.width) * 0.5, f32(s.height) * 0.5})
		s.bin << p
	}
}

pub fn (mut s System) update(dt f64) {
	mut p := &Particle(0)
	for i := 0; i < s.pool.len; i++ {
		p = s.pool[i]
		p.update(dt)
		if p.is_dead() {
			s.bin << p
			s.pool.delete(i)
		}
	}
}

pub fn (s System) draw() {
	sgl.begin_quads()
	for p in s.pool {
		p.draw()
	}
	sgl.end()
}

pub fn (mut s System) reset() {
	for i in 0 .. s.pool.len {
		mut p := s.pool[i]
		p.reset()
		p.life_time = 0
	}
	for i in 0 .. s.bin.len {
		mut p := s.pool[i]
		p.reset()
		p.life_time = 0
	}
}

pub fn (mut s System) explode(x f32, y f32) {
	mut reserve := 500
	center := vec2.Vec2{x, y}
	mut p := &Particle(0)
	for i := 0; i < s.bin.len && reserve > 0; i++ {
		p = s.bin[i]
		p.reset()
		p.location.from(center)
		p.acceleration = vec2.Vec2{rand.f32_in_range(-0.5, 0.5) or { -0.5 }, rand.f32_in_range(-0.5,
			0.5) or { -0.5 }}
		p.velocity = vec2.Vec2{rand.f32_in_range(-0.5, 0.5) or { -0.5 }, rand.f32_in_range(-0.5,
			0.5) or { -0.5 }}
		p.life_time = rand.f64_in_range(500, 2000) or { 500 }
		s.pool << p
		s.bin.delete(i)
		reserve--
	}
}

pub fn (mut s System) free() {
	for p in s.pool {
		if p == 0 {
			print(ptr_str(p) + ' ouch')
			continue
		}
		unsafe { free(p) }
	}
	s.pool.clear()
	for p in s.bin {
		if p == 0 {
			print(ptr_str(p) + ' ouch')
			continue
		}
		unsafe {
			// println('Freeing from bin')
			free(p)
		}
	}
	s.bin.clear()
}
