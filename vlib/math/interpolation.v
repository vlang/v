module math

// mix performs a linear interpolation (LERP) mix between `start` and `end`, using `t` to weight between them.
// `t` should be in the closed interval [0, 1].
// For `t` == 0, the output is `x`.
// Note: mix is calculated in such a way, that the output *will* be `y`, when `t` == 1.0 .
// See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/mix.xhtml
// Also: https://en.wikipedia.org/wiki/Linear_interpolation .
@[inline]
pub fn mix[T](start T, end T, t T) T {
	return start * (1 - t) + end * t
}

// exp_decay returns a frame independent exponential decay value between `a` and `b` using `delta_time_seconds`.
// `decay` is supposed to be useful in the range 1.0 to 25.0. From slow to fast.
// The function is a frame rate independent (approximation) of the well-known `lerp` or `mix` (linear interpolation) function.
// It is ported to V from the pseudo code shown towards the end of the video https://youtu.be/LSNQuFEDOyQ?t=2977
// NOTE: Thanks to Freya Holmér for the function and the work done in this field.
@[inline]
pub fn exp_decay[T](a T, b T, decay f64, delta_time_seconds f64) T {
	return T(f64(b) + (f64(a) - f64(b)) * exp(-decay * delta_time_seconds))
}

// clip constrain the given value `x`, to lie between two further values `min_value` and `max_value`.
// See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/clamp.xhtml
// Also: https://en.wikipedia.org/wiki/Clamp_(function)
@[inline]
pub fn clip[T](x T, min_value T, max_value T) T {
	return if x > max_value {
		max_value
	} else if x < min_value {
		min_value
	} else {
		x
	}
}

// remap the input `x`, from the range [`a`,`b`] to [`c`,`d`] .
// Example: math.remap(20, 1, 100, 50, 5000) == 1000
// Note: `a` should be != `b`.
@[inline]
pub fn remap[T](x T, a T, b T, c T, d T) T {
	return c + (d - c) * (x - a) / (b - a)
}

// smoothstep smoothly maps a value between `edge0` and `edge1`. It returns:
// 0 if `x` is less than or equal to the left `edge0`,
// 1 if `x` is greater than or equal to the right `edge`,
// and smoothly interpolates, using a Hermite polynomial, between 0 and 1 otherwise.
// The gradient of the smoothstep function is zero at both edges. This is convenient
// for creating a sequence of transitions using smoothstep to interpolate each segment
// as an alternative to using more sophisticated or expensive interpolation techniques.
// `smoothstep` is a 1st order smoothing function, using a 3rd order polynomial.
// See also `smootherstep`, which is slower, but nicer looking.
// See also https://en.wikipedia.org/wiki/Smoothstep
@[inline]
pub fn smoothstep[T](edge0 T, edge1 T, x T) T {
	v := clip((x - edge0) / (edge1 - edge0), 0, 1)
	return v * v * (3 - 2 * v)
}

// smootherstep smoothly maps a value between `edge0` and `edge1`.
// smootherstep is a 2nd order smoothing function, using a 5th order polynomial.
// The 1st and 2nd order derivatives of the smootherstep function are 0 at both edges.
// See also `smoothstep`, which is faster, but has just its gradient being 0 at both edges.
// See also https://en.wikipedia.org/wiki/Smoothstep
@[inline]
pub fn smootherstep[T](edge0 T, edge1 T, x T) T {
	v := clip((x - edge0) / (edge1 - edge0), 0, 1)
	return v * v * v * (x * (6 * x - 15) + 10)
}
