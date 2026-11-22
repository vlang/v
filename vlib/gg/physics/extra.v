module physics

import gg
import math

// Candidate is a pair of collider indices whose bounds overlap on X.
pub struct Candidate {
pub:
	a int
	b int
}

// broadphase returns candidate pairs using sweep and prune on the X axis.
// It is conservative: every overlapping pair is included, non-overlapping
// pairs are skipped cheaply before the narrow phase.
pub fn broadphase(colliders []Collider) []Candidate {
	mut out := []Candidate{}
	if colliders.len < 2 {
		return out
	}
	mut order := []int{len: colliders.len}
	for i in 0 .. order.len {
		order[i] = i
	}
	for i := 1; i < order.len; i++ {
		key := order[i]
		key_min, _ := colliders[key].bounds()
		mut j := i - 1
		for j >= 0 {
			other_min, _ := colliders[order[j]].bounds()
			if other_min.x <= key_min.x {
				break
			}
			order[j + 1] = order[j]
			j--
		}
		order[j + 1] = key
	}
	for i in 0 .. order.len {
		_, a_max := colliders[order[i]].bounds()
		for j in i + 1 .. order.len {
			b_min, _ := colliders[order[j]].bounds()
			if b_min.x > a_max.x {
				break
			}
			if overlaps(colliders[order[i]], colliders[order[j]]) {
				out << Candidate{order[i], order[j]}
			}
		}
	}
	return out
}

// DistanceJoint keeps two bodies roughly rest units apart.
pub struct DistanceJoint {
pub mut:
	a         &Body
	b         &Body
	rest      f32
	stiffness f32 = 20
}

pub fn new_distance_joint(a &Body, b &Body, rest f32) DistanceJoint {
	return DistanceJoint{a, b, rest, 20}
}

// solve applies one spring step to both bodies.
pub fn (mut j DistanceJoint) solve(dt f32) {
	delta := j.b.position - j.a.position
	dist := f32(math.sqrt(f64(delta.dot(delta))))
	if dist < 1e-5 {
		return
	}
	error := dist - j.rest
	impulse := delta.scale(error * j.stiffness * dt / dist)
	// Non-positive masses are immovable rather than producing Inf/NaN impulses.
	inv_a := if j.a.is_static || j.a.mass <= 0 { 0 } else { 1 / j.a.mass }
	inv_b := if j.b.is_static || j.b.mass <= 0 { 0 } else { 1 / j.b.mass }
	j.a.velocity = j.a.velocity + impulse.scale(inv_a)
	j.b.velocity = j.b.velocity - impulse.scale(inv_b)
}

// SpringJoint pulls a body toward a fixed anchor.
pub struct SpringJoint {
pub mut:
	body      &Body
	anchor    gg.Vec3
	stiffness f32 = 20
	damping   f32 = 4
}

pub fn new_spring_joint(body &Body, anchor gg.Vec3) SpringJoint {
	return SpringJoint{ body: body, anchor: anchor }
}

pub fn (mut j SpringJoint) solve(dt f32) {
	force := (j.anchor - j.body.position).scale(j.stiffness) - j.body.velocity.scale(j.damping)
	j.body.velocity = j.body.velocity + force.scale(dt)
}

// CharacterController drives a sphere body with grounded movement and jumps.
pub struct CharacterController {
pub mut:
	body        &Body
	speed       f32 = 4
	jump_speed  f32 = 5
	floor_y     f32 = -10
	grounded    bool
	step_height f32 = 0.3
}

pub fn new_character_controller(body &Body) CharacterController {
	return CharacterController{ body: body }
}

// update applies horizontal velocity, jumping and a simple floor clamp.
pub fn (mut c CharacterController) update(dt f32, move_x f32, move_z f32, jump bool) {
	mut dir := gg.Vec3{move_x, 0, move_z}
	len2 := dir.dot(dir)
	if len2 > 1 {
		dir = dir.scale(1 / f32(math.sqrt(f64(len2))))
	}
	c.body.velocity.x = dir.x * c.speed
	c.body.velocity.z = dir.z * c.speed
	if jump && c.grounded {
		c.body.velocity.y = c.jump_speed
		c.grounded = false
	}
	floor := c.floor_y + c.body.radius
	if c.body.position.y <= floor + c.step_height && c.body.velocity.y <= 0 {
		c.body.position.y = floor
		c.body.velocity.y = 0
		c.grounded = true
	} else {
		c.grounded = false
	}
}
