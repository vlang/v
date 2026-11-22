// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module physics

import gg
import math

// Body is a sphere rigid body with linear motion. Rotation is intentionally
// out of scope; this is a light broad-phase starter for games and demos.
@[heap]
pub struct Body {
pub mut:
	id          int
	position    gg.Vec3
	velocity    gg.Vec3
	radius      f32 = 0.5
	mass        f32 = 1
	restitution f32 = 0.5
	is_static   bool
}

// new_body returns a dynamic sphere body.
pub fn new_body(id int, position gg.Vec3, radius f32) &Body {
	return &Body{ id: id, position: position, radius: radius }
}

// World integrates bodies and resolves sphere-sphere and floor contacts.
pub struct World {
pub mut:
	bodies  []&Body
	gravity gg.Vec3 = gg.Vec3{0, -9.81, 0}
	floor_y f32     = -10
}

// new_world returns an empty world with earth gravity.
pub fn new_world() World {
	return World{}
}

// add registers a body.
pub fn (mut w World) add(body &Body) {
	w.bodies << body
}

fn resolve_pair(mut a &Body, mut b &Body) bool {
	delta := b.position - a.position
	dist2 := delta.dot(delta)
	minimum := a.radius + b.radius
	if dist2 <= 0 || dist2 >= minimum * minimum {
		return false
	}
	dist := f32(math.sqrt(f64(dist2)))
	normal := delta.scale(1.0 / dist)
	overlap := minimum - dist
	// A non-positive mass is treated as immovable: 1/mass would be Inf or NaN
	// and would poison positions and velocities with no useful physical meaning.
	inv_a := if a.is_static || a.mass <= 0 { 0 } else { 1.0 / a.mass }
	inv_b := if b.is_static || b.mass <= 0 { 0 } else { 1.0 / b.mass }
	total := inv_a + inv_b
	if total == 0 {
		return false
	}
	if inv_a > 0 {
		a.position = a.position - normal.scale(overlap * inv_a / total)
	}
	if inv_b > 0 {
		b.position = b.position + normal.scale(overlap * inv_b / total)
	}
	relative := b.velocity - a.velocity
	sep := relative.dot(normal)
	if sep >= 0 {
		return true
	}
	e := if a.restitution < b.restitution { a.restitution } else { b.restitution }
	impulse := -(1 + e) * sep / total
	if inv_a > 0 {
		a.velocity = a.velocity - normal.scale(impulse * inv_a)
	}
	if inv_b > 0 {
		b.velocity = b.velocity + normal.scale(impulse * inv_b)
	}
	return true
}

// step advances the world by dt seconds with simple semi-implicit Euler and
// runs a few collision relaxation passes.
pub fn (mut w World) step(dt f32) {
	if dt <= 0 {
		return
	}
	for mut body in w.bodies {
		if body.is_static {
			continue
		}
		body.velocity = body.velocity + w.gravity.scale(dt)
		body.position = body.position + body.velocity.scale(dt)
		if body.position.y - body.radius < w.floor_y {
			body.position.y = w.floor_y + body.radius
			if body.velocity.y < 0 {
				body.velocity.y = -body.velocity.y * body.restitution
			}
		}
	}
	// Sweep-and-prune on X: bodies are sorted once, then each body only tests
	// later bodies until their X intervals can no longer overlap. Spread-out
	// scenes avoid the quadratic all-pairs scan while dense scenes keep the
	// same result (every potentially overlapping pair is still tested).
	mut order := []int{len: w.bodies.len}
	for i in 0 .. order.len {
		order[i] = i
	}
	for i := 1; i < order.len; i++ {
		key := order[i]
		key_x := w.bodies[key].position.x
		mut j := i - 1
		for j >= 0 && w.bodies[order[j]].position.x > key_x {
			order[j + 1] = order[j]
			j--
		}
		order[j + 1] = key
	}
	// A large body later in the sorted order can still overlap `a` even when
	// the immediate neighbour cannot, so the break must use the largest radius
	// in the remaining suffix instead of the current body's radius.
	mut max_radius_from := []f32{len: order.len}
	mut suffix_max := f32(0)
	for i := order.len - 1; i >= 0; i-- {
		r := w.bodies[order[i]].radius
		if r > suffix_max {
			suffix_max = r
		}
		max_radius_from[i] = suffix_max
	}
	for _ in 0 .. 3 {
		for i in 0 .. order.len {
			ai := order[i]
			a := w.bodies[ai]
			for j in i + 1 .. order.len {
				bi := order[j]
				b := w.bodies[bi]
				if b.position.x - a.position.x > a.radius + max_radius_from[j] {
					break
				}
				resolve_pair(mut w.bodies[ai], mut w.bodies[bi])
			}
		}
	}
}
