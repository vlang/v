import math

struct Vec5 {
	x f64
	y f64
	z f64
	a f64
	b f64
}

fn vec5_from(value f64) Vec5 {
	return Vec5{value, value, value, value, value}
}

fn (v Vec5) abs_oldstyle() Vec5 {
	return Vec5{math.abs(v.x), math.abs(v.y), math.abs(v.z), math.abs(v.a), math.abs(v.b)}
}

fn (v Vec5) generic_new(f fn (f64) f64) Vec5 {
	return Vec5{f(v.x), f(v.y), f(v.z), f(v.a), f(v.b)}
}

fn test_generic_function_argument_inference() {
	v := vec5_from(-1.0)
	assert v.generic_new(math.abs) == v.abs_oldstyle()
}
