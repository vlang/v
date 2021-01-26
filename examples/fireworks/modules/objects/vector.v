module objects

import math
import rand

pub struct Vector {
pub mut:
	x f32
	y f32
}

pub fn (a Vector) + (b Vector) Vector {
	return Vector{a.x + b.x, a.y + b.y}
}

pub fn (vector Vector) mult(scalar f32) Vector {
	return Vector{vector.x * scalar, vector.y * scalar}
}

pub fn random_vector_in_circle() Vector {
	theta := rand.f32n(2 * math.pi)
	y := rand.f32()

	return {
		x: f32(y * math.sin(theta))
		y: f32(y * math.cos(theta))
	}
}
