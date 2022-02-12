module sim

import math

// Vector3D is a 3D vector
pub struct Vector3D {
	x f64
	y f64
	z f64
}

// vector creates a Vector3D passing x,y,z as parameteres
pub fn vector(data Vector3D) Vector3D {
	return Vector3D{
		...data
	}
}

// addition
pub fn (v Vector3D) + (v2 Vector3D) Vector3D {
	return Vector3D{
		x: v.x + v2.x
		y: v.y + v2.y
		z: v.z + v2.z
	}
}

// dot product
pub fn (v Vector3D) * (v2 Vector3D) f64 {
	return (v.x * v2.x) + (v.y * v2.y) + (v.z * v2.z)
}

// scale gets a scaled vector
pub fn (v Vector3D) scale(scalar f64) Vector3D {
	return Vector3D{
		x: v.x * scalar
		y: v.y * scalar
		z: v.z * scalar
	}
}

// norm_squared returns the square of the norm of the vector
pub fn (v Vector3D) norm_squared() f64 {
	return v * v
}

// norm returns the norm of the vector
pub fn (v Vector3D) norm() f64 {
	return math.sqrt(v.norm_squared())
}
