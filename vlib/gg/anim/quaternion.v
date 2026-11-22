module anim

import gg
import math

// Quat is a unit quaternion for rotation interpolation.
pub struct Quat {
pub mut:
	x f32
	y f32
	z f32
	w f32 = 1
}

// quat_identity returns the no-rotation quaternion.
pub fn quat_identity() Quat {
	return Quat{}
}

// quat_from_axis_angle builds a quaternion from an axis and an angle in
// degrees. A zero axis returns the identity.
pub fn quat_from_axis_angle(axis gg.Vec3, angle_deg f32) Quat {
	n := axis.normalized()
	if n.x == 0 && n.y == 0 && n.z == 0 {
		return Quat{}
	}
	half := f32(math.radians(f64(angle_deg))) / 2
	s := f32(math.sin(f64(half)))
	return Quat{ x: n.x * s, y: n.y * s, z: n.z * s, w: f32(math.cos(f64(half))) }
}

// quat_from_euler converts an Euler angle triplet in degrees (XYZ order).
pub fn quat_from_euler(euler gg.Vec3) Quat {
	rx := quat_from_axis_angle(gg.Vec3{1, 0, 0}, euler.x)
	ry := quat_from_axis_angle(gg.Vec3{0, 1, 0}, euler.y)
	rz := quat_from_axis_angle(gg.Vec3{0, 0, 1}, euler.z)
	// quat_to_euler extracts the extrinsic XYZ (intrinsic ZYX) convention, so
	// compose in reverse: the x rotation is applied first, then y, then z.
	return quat_mul(quat_mul(rz, ry), rx)
}

// quat_mul composes two rotations.
pub fn quat_mul(a Quat, b Quat) Quat {
	return Quat{
		x: a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y
		y: a.w * b.y - a.x * b.z + a.y * b.w + a.z * b.x
		z: a.w * b.z + a.x * b.y - a.y * b.x + a.z * b.w
		w: a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z
	}
}

// quat_dot returns the inner product of two quaternions.
pub fn quat_dot(a Quat, b Quat) f32 {
	return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
}

// quat_normalize returns a unit-length copy.
pub fn quat_normalize(q Quat) Quat {
	len := f32(math.sqrt(f64(q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w)))
	if len == 0 {
		return Quat{}
	}
	return Quat{ x: q.x / len, y: q.y / len, z: q.z / len, w: q.w / len }
}

// quat_slerp interpolates between two orientations, taking the shortest arc.
pub fn quat_slerp(a Quat, b Quat, t f32) Quat {
	mut q1 := quat_normalize(a)
	q2 := quat_normalize(b)
	mut dot := quat_dot(q1, q2)
	if dot < 0 {
		q1 = Quat{ x: -q1.x, y: -q1.y, z: -q1.z, w: -q1.w }
		dot = -dot
	}
	if dot > 0.9995 {
		return quat_normalize(Quat{
			x: q1.x + (q2.x - q1.x) * t
			y: q1.y + (q2.y - q1.y) * t
			z: q1.z + (q2.z - q1.z) * t
			w: q1.w + (q2.w - q1.w) * t
		})
	}
	theta_0 := f32(math.acos(f64(math.clip(dot, -1.0, 1.0))))
	sin_theta_0 := f32(math.sin(f64(theta_0)))
	theta := theta_0 * t
	sin_theta := f32(math.sin(f64(theta)))
	s0 := f32(math.sin(f64(theta_0 - theta))) / sin_theta_0
	s1 := sin_theta / sin_theta_0
	return Quat{
		x: q1.x * s0 + q2.x * s1
		y: q1.y * s0 + q2.y * s1
		z: q1.z * s0 + q2.z * s1
		w: q1.w * s0 + q2.w * s1
	}
}

// quat_to_euler converts a quaternion to XYZ Euler angles in degrees.
pub fn quat_to_euler(q Quat) gg.Vec3 {
	unit := quat_normalize(q)
	sinr_cosp := 2 * (unit.w * unit.x + unit.y * unit.z)
	cosr_cosp := 1 - 2 * (unit.x * unit.x + unit.y * unit.y)
	x := f32(math.degrees(f64(math.atan2(f64(sinr_cosp), f64(cosr_cosp)))))
	sinp := 2 * (unit.w * unit.y - unit.z * unit.x)
	p := if sinp > 1.0 {
		1.0
	} else if sinp < -1.0 {
		-1.0
	} else {
		sinp
	}
	y := f32(math.degrees(f64(math.asin(f64(p)))))
	siny_cosp := 2 * (unit.w * unit.z + unit.x * unit.y)
	cosy_cosp := 1 - 2 * (unit.y * unit.y + unit.z * unit.z)
	z := f32(math.degrees(f64(math.atan2(f64(siny_cosp), f64(cosy_cosp)))))
	return gg.Vec3{x, y, z}
}
