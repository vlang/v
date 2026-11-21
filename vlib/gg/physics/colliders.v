module physics

import gg
import math

// ColliderKind selects the narrow-phase test for a shape.
pub enum ColliderKind {
	ball
	box
	capsule
}

// Collider is a shapes-only descriptor used by broadphase and joints. It does
// not own a body; bodies pair it with mass and velocity elsewhere.
pub struct Collider {
pub mut:
	kind     ColliderKind
	center   gg.Vec3
	half     gg.Vec3 = gg.Vec3{0.5, 0.5, 0.5}
	radius   f32     = 0.5
	height   f32     = 1 // capsule segment length along Y
	rotation gg.Vec3
}

pub fn ball(center gg.Vec3, radius f32) Collider {
	return Collider{ kind: .ball, center: center, radius: radius }
}

pub fn box(center gg.Vec3, half gg.Vec3) Collider {
	return Collider{ kind: .box, center: center, half: half }
}

pub fn capsule(center gg.Vec3, radius f32, height f32) Collider {
	return Collider{ kind: .capsule, center: center, radius: radius, height: height }
}

pub fn (c &Collider) bounds() (gg.Vec3, gg.Vec3) {
	mut extent := c.half
	if c.kind == .ball {
		extent = gg.Vec3{c.radius, c.radius, c.radius}
	} else if c.kind == .capsule {
		extent = gg.Vec3{c.radius, c.height / 2 + c.radius, c.radius}
	}
	return c.center - extent, c.center + extent
}

fn segment_distance(a gg.Vec3, b gg.Vec3, p gg.Vec3) gg.Vec3 {
	ab := b - a
	len2 := ab.dot(ab)
	if len2 <= 0 {
		return p - a
	}
	t := math.clip(f32((p - a).dot(ab) / len2), 0, 1)
	return p - (a + ab.scale(t))
}

// overlaps reports whether two colliders touch.
pub fn overlaps(a Collider, b Collider) bool {
	match a.kind {
		.ball {
			match b.kind {
				.ball {
					d := a.center - b.center
					r := a.radius + b.radius
					return d.dot(d) <= r * r
				}
				.box {
					q := gg.Vec3{math.clip(f64(a.center.x), f64(b.center.x - b.half.x), f64(b.center.x + b.half.x)), math.clip(f64(a.center.y), f64(b.center.y - b.half.y), f64(b.center.y + b.half.y)), math.clip(f64(a.center.z), f64(b.center.z - b.half.z), f64(b.center.z + b.half.z))}
					d := q - a.center
					return d.dot(d) <= a.radius * a.radius
				}
				.capsule {
					hy := b.height / 2
					d := segment_distance(b.center - gg.Vec3{0, hy, 0}, b.center + gg.Vec3{0, hy, 0}, a.center)
					r := a.radius + b.radius
					return d.dot(d) <= r * r
				}
			}
		}
		.box {
			match b.kind {
				.ball {
					return overlaps(b, a)
				}
				.box {
					return a.center.x - a.half.x <= b.center.x + b.half.x
						&& a.center.x + a.half.x >= b.center.x - b.half.x
						&& a.center.y - a.half.y <= b.center.y + b.half.y
						&& a.center.y + a.half.y >= b.center.y - b.half.y
						&& a.center.z - a.half.z <= b.center.z + b.half.z
						&& a.center.z + a.half.z >= b.center.z - b.half.z
				}
				.capsule {
					hy := b.height / 2
					lo := b.center - gg.Vec3{0, hy, 0}
					hi := b.center + gg.Vec3{0, hy, 0}
					q1 := gg.Vec3{math.clip(f64(lo.x), f64(a.center.x - a.half.x), f64(a.center.x + a.half.x)), math.clip(f64(lo.y), f64(a.center.y - a.half.y), f64(a.center.y + a.half.y)), math.clip(f64(lo.z), f64(a.center.z - a.half.z), f64(a.center.z + a.half.z))}
					q2 := gg.Vec3{math.clip(f64(hi.x), f64(a.center.x - a.half.x), f64(a.center.x + a.half.x)), math.clip(f64(hi.y), f64(a.center.y - a.half.y), f64(a.center.y + a.half.y)), math.clip(f64(hi.z), f64(a.center.z - a.half.z), f64(a.center.z + a.half.z))}
					mid := q1 + (q2 - q1).scale(0.5)
					d := segment_distance(lo, hi, mid)
					return d.dot(d) <= b.radius * b.radius
				}
			}
		}
		.capsule {
			match b.kind {
				.ball, .box {
					return overlaps(b, a)
				}
				.capsule {
					ah := a.height / 2
					bh := b.height / 2
					mut d := segment_distance(a.center - gg.Vec3{0, ah, 0}, a.center + gg.Vec3{0, ah, 0},
						b.center)
					r := a.radius + b.radius
					if d.dot(d) > r * r {
						d = segment_distance(b.center - gg.Vec3{0, bh, 0}, b.center + gg.Vec3{0, bh, 0},
							a.center)
						if d.dot(d) > r * r {
							return false
						}
					}
					return true
				}
			}
		}
	}
}

// contact_normal returns an approximate separation normal from a to b.
pub fn contact_normal(a Collider, b Collider) gg.Vec3 {
	d := b.center - a.center
	len := f32(math.sqrt(f64(d.dot(d))))
	if len == 0 {
		return gg.Vec3{0, 1, 0}
	}
	return d.scale(1 / len)
}
