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
