module generics_type_inference_bug

pub struct Vec3[T] {
pub mut:
	x T
	y T
	z T
}

pub fn vec3[T](x T, y T, z T) Vec3[T] {
	return Vec3[T]{
		x: x
		y: y
		z: z
	}
}

pub fn (v Vec3[T]) dot(u Vec3[T]) T {
	return T((v.x * u.x) + (v.y * u.y) + (v.z * u.z))
}

pub fn (v Vec3[T]) multiply_test(u Vec3[T]) T {
	// This is the critical test: multiplying two T values should give T, not some other type
	dot_result := v.dot(u)
	norm := T(0.5)
	result := dot_result * norm
	return result
}
