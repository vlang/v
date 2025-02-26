import math { close, radians, veryclose }
import math.vec

fn test_vec2_int() {
	mut v1 := vec.vec2(0, 0)
	mut v2 := vec.vec2(0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec2[int]'
	assert v3.x == 2
	assert v3.y == 2
}

fn test_vec2_f32() {
	mut v1 := vec.vec2(f32(0), 0)
	mut v2 := vec.vec2(f32(0), 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec2[f32]'
	assert v3.x == 2
	assert v3.y == 2
}

fn test_vec2_f64() {
	mut v1 := vec.vec2(0.0, 0)
	mut v2 := vec.vec2(0.0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec2[f64]'
	assert v3.x == 2
	assert v3.y == 2
}

fn test_vec2_f64_utils_1() {
	mut v1 := vec.vec2(2.0, 3.0)
	mut v2 := vec.vec2(1.0, 4.0)

	mut zv := vec.vec2(5.0, 5.0)
	zv.zero()

	v3 := v1 + v2
	assert v3.x == 3
	assert v3.y == 7

	assert v1.dot(v2) == 14
	assert v1.cross(v2) == 5

	v1l := vec.vec2(40.0, 9.0)
	assert v1l.magnitude() == 41

	mut ctv1 := vec.vec2(0.000001, 0.000001)
	ctv1.clean_tolerance(0.00001)
	assert ctv1 == zv
}

fn test_vec2_f64_utils_2() {
	mut v1 := vec.vec2(4.0, 4.0)
	assert veryclose(v1.unit().magnitude(), 1)
	v2 := v1.mul_scalar(0.5)
	assert v2.x == 2
	assert v2.y == 2
	assert veryclose(v2.unit().magnitude(), 1)

	invv2 := v2.inv()
	assert invv2.x == 0.5
	assert invv2.y == 0.5
}

fn fcorrect_angles() []f64 {
	return [
		math.pi * 1.0 / 4.0,
		math.pi * 2.0 / 4.0,
		math.pi * 3.0 / 4.0,
		math.pi,
		math.pi * 5.0 / 4.0 - 2 * math.pi,
		math.pi * 6.0 / 4.0 - 2 * math.pi,
		math.pi * 7.0 / 4.0 - 2 * math.pi,
	]
}

fn fsurround() []vec.Vec2[f64] {
	return [
		vec.vec2(1.0, 1.0),
		vec.vec2(0.0, 1.0),
		vec.vec2(-1.0, 1.0),
		vec.vec2(-1.0, 0.0),
		vec.vec2(-1.0, -1.0),
		vec.vec2(0.0, -1.0),
		vec.vec2(1.0, -1.0),
	]
}

fn test_vec2_angle_between() {
	surround := fsurround()
	correct_angles := fcorrect_angles()
	v1 := vec.vec2(1.0, 0.0)
	for i in 0 .. 7 {
		assert v1.angle_between(surround[i]) == correct_angles[i]
		// check the angle between non-normalised, scaled vectors too:
		assert v1.mul_scalar(5.0).angle_between(surround[i].mul_scalar(2.0)) == correct_angles[i]
		offset := f64(i) + 123.123456
		assert v1.mul_scalar(offset).angle_between(surround[i].mul_scalar(offset)) == correct_angles[i]
		// check f32 version:
		v1_f32 := vec.vec2(f32(v1.x), f32(v1.y))
		v2_f32 := vec.vec2(f32(surround[i].x), f32(surround[i].y))
		assert v1_f32.angle_between(v2_f32) == f32(correct_angles[i])
		// check int version:
		v1_int := vec.vec2(int(v1.x), int(v1.y))
		v2_int := vec.vec2(int(surround[i].x), int(surround[i].y))
		assert v1_int.angle_between(v2_int) == int(correct_angles[i])
	}
}

fn test_vec2_angle_towards() {
	surround := fsurround()
	correct_angles := fcorrect_angles()
	// basic case, let p0 be the coordinate origin point:
	p0 := vec.vec2(0.0, 0.0)
	for i in 0 .. 7 {
		assert p0.angle_towards(surround[i]) == correct_angles[i]
		assert (surround[i] - p0).angle() == p0.angle_towards(surround[i])
		// check f32 version:
		p0_f32 := vec.vec2(f32(0), f32(0))
		s_f32 := vec.vec2(f32(surround[i].x), f32(surround[i].y))
		assert p0_f32.angle_towards(s_f32) == f32(correct_angles[i])
		// check int version:
		p0_int := vec.vec2(int(0), int(0))
		s_int := vec.vec2(int(surround[i].x), int(surround[i].y))
		assert p0_int.angle_towards(s_int) == int(correct_angles[i])
		// check invariants, when the 2 points are moved and translated:
		for x := -0.5; x <= 0.5; x += 0.1 {
			for y := -0.5; y <= 0.5; y += 0.1 {
				p1 := vec.vec2(x, y)
				p2 := p1.add(surround[i])
				assert veryclose(p1.angle_towards(p2), correct_angles[i]), 'i: ${i}, p1: ${p1} | p2: ${p2}'
				offset := f64(i) + 123.123456
				p1_o := p1.add_scalar(offset)
				p2_o := p2.add_scalar(offset)
				assert (p2_o - p1_o).angle() == p1_o.angle_towards(p2_o)
				assert close(p1_o.angle_towards(p2_o), correct_angles[i]), 'i: ${i}, p1_o: ${p1_o} | p2_o: ${p2_o}'
			}
		}
	}
}

fn test_vec2_rotate_around_cw() {
	origin := vec.vec2(0.0, 0.0)
	mut v := vec.vec2(0.0, 1.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 1.0)
	assert close(v.y, 0.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 0.0)
	assert close(v.y, -1.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, -1.0)
	assert close(v.y, 0.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 0.0)
	assert close(v.y, 1.0)
}

fn test_vec2_rotate_around_ccw() {
	origin := vec.vec2(0.0, 0.0)
	mut v := vec.vec2(0.0, 1.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, -1.0)
	assert close(v.y, 0.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, 0.0)
	assert close(v.y, -1.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, 1.0)
	assert close(v.y, 0.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, 0.0)
	assert close(v.y, 1.0)
}

fn test_vec2_rotate_around_cw_2() {
	origin := vec.vec2(1.0, 1.0)
	mut v := vec.vec2(1.0, 2.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 2.0)
	assert close(v.y, 1.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 1.0)
	assert close(v.y, 0.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 0.0)
	assert close(v.y, 1.0)
	v = v.rotate_around_cw(origin, radians(90))
	assert close(v.x, 1.0)
	assert close(v.y, 2.0)
}

fn test_vec2_rotate_around_ccw_2() {
	origin := vec.vec2(-1.0, 1.0)
	mut v := vec.vec2(-1.0, -1.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, 1.0)
	assert close(v.y, 1.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, -1.0)
	assert close(v.y, 3.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, -3.0)
	assert close(v.y, 1.0)
	v = v.rotate_around_ccw(origin, radians(90))
	assert close(v.x, -1.0)
	assert close(v.y, -1.0)
}
