module main

import math.vec

pub struct Bezier {
pub mut:
	points [4]vec.Vec2[f32]
}

pub fn (b Bezier) h3() vec.Vec2[f32] {
	return b.points[2]
}

pub fn (b Bezier) h4() vec.Vec2[f32] {
	return b.points[3]
}

pub fn (b Bezier) end() vec.Vec2[f32] {
	return b.h4()
}

pub struct Path {
pub mut:
	segments [1024]Bezier
	len      int // should be read-only
}

pub fn (mut p Path) add(b Bezier) {
	assert p.len + 1 < 1024
	p.segments[p.len] = b
	p.len++
}

fn test_main() {
	b1 := Bezier{
		points: [vec.Vec2[f32]{100, 100}, vec.Vec2[f32]{120, 80},
			vec.Vec2[f32]{200, 70}, vec.Vec2[f32]{400, 200}]!
	}
	b2 := Bezier{
		points: [b1.end(), b1.h3() + vec.Vec2[f32]{0, 2 * 80}, vec.Vec2[f32]{200, 70},
			b1.end() + vec.Vec2[f32]{400, 200}]!
	}

	mut path := Path{}

	path.add(b1)
	path.add(b2)

	assert sizeof(path) == 32772
}
