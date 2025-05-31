module easing

// Easing functions specify the value of a property, given a level of completeness from 0.0 to 1.0 .
// The names of the easing functions here, are based on the ones from https://easings.net/ and
// https://nicmulvaney.com/easing .
import math { cos, pow, sin, sqrt }

const pi = math.pi
const pi_half = pi / 2.0
const pi_double = pi * 2.0
const c1 = 1.70158
const c2 = c1 * 1.525
const c3 = c1 + 1.0
const c4 = pi_double / 3.0
const c5 = pi_double / 4.5
const n1 = 7.5625
const d1 = 2.75

// EasingFN defines the common shape of an easing function, so that you can define your own, and pass them everywhere,
// that expects easing.EasingFN . It is just a function that accepts a floating point number and returns one too.
pub type EasingFN = fn (x f64) f64

// linear just returns the parameter x, without changes .
@[inline]
pub fn linear(x f64) f64 {
	return x
}

// in_sine returns the equivalent of https://easings.net/#easeInSine .
@[inline]
pub fn in_sine(x f64) f64 {
	return 1.0 - cos(x * pi_half)
}

// out_sine returns the equivalent of https://easings.net/#easeOutSine .
@[inline]
pub fn out_sine(x f64) f64 {
	return sin(x * pi_half)
}

// in_out_sine returns the equivalent of https://easings.net/#easeInOutSine .
@[inline]
pub fn in_out_sine(x f64) f64 {
	return -(cos(pi * x) - 1.0) / 2.0
}

// in_quad returns the equivalent of https://easings.net/#easeInQuad .
@[inline]
pub fn in_quad(x f64) f64 {
	return x * x
}

// out_quad returns the equivalent of https://easings.net/#easeOutQuad .
@[inline]
pub fn out_quad(x f64) f64 {
	return 1.0 - (1.0 - x) * (1.0 - x)
}

// in_out_quad returns the equivalent of https://easings.net/#easeInOutQuad .
@[inline]
pub fn in_out_quad(x f64) f64 {
	return if x < 0.5 { 2.0 * x * x } else { 1.0 - (2.0 - 2.0 * x) * (2.0 - 2.0 * x) / 2.0 }
}

// in_cubic returns the equivalent of https://easings.net/#easeInCubic .
@[inline]
pub fn in_cubic(x f64) f64 {
	return x * x * x
}

// out_cubic returns the equivalent of https://easings.net/#easeOutCubic .
@[inline]
pub fn out_cubic(x f64) f64 {
	return 1.0 - (1.0 - x) * (1.0 - x) * (1.0 - x)
}

// in_out_cubic returns the equivalent of https://easings.net/#easeInOutCubic .
@[inline]
pub fn in_out_cubic(x f64) f64 {
	return if x < .5 {
		4.0 * x * x * x
	} else {
		1.0 - (2.0 - 2.0 * x) * (2.0 - 2.0 * x) * (2.0 - 2.0 * x) / 2.0
	}
}

// in_quart returns the equivalent of https://easings.net/#easeInQuart .
@[inline]
pub fn in_quart(x f64) f64 {
	return x * x * x * x
}

// out_quart returns the equivalent of https://easings.net/#easeOutQuart .
@[inline]
pub fn out_quart(x f64) f64 {
	return 1.0 - (1.0 - x) * (1.0 - x) * (1.0 - x) * (1.0 - x)
}

// in_out_quart returns the equivalent of https://easings.net/#easeInOutQuart .
@[inline]
pub fn in_out_quart(x f64) f64 {
	return if x < 0.5 {
		8.0 * x * x * x * x
	} else {
		1.0 - (2.0 - 2.0 * x) * (2.0 - 2.0 * x) * (2.0 - 2.0 * x) * (2.0 - 2.0 * x) / 2.0
	}
}

// in_quint returns the equivalent of https://easings.net/#easeInQuint .
@[inline]
pub fn in_quint(x f64) f64 {
	return x * x * x * x * x
}

// out_quint returns the equivalent of https://easings.net/#easeOutQuint .
@[inline]
pub fn out_quint(x f64) f64 {
	return 1.0 - (1.0 - x) * (1.0 - x) * (1.0 - x) * (1.0 - x) * (1.0 - x)
}

// in_out_quint returns the equivalent of https://easings.net/#easeInOutQuint .
@[inline]
pub fn in_out_quint(x f64) f64 {
	return if x < 0.5 {
		16.0 * x * x * x * x * x
	} else {
		1.0 - (2.0 - 2.0 * x) * (2.0 - 2.0 * x) * (2.0 - 2.0 * x) * (2.0 - 2.0 * x) * (2.0 - 2.0 * x) / 2.0
	}
}

// in_expo returns the equivalent of https://easings.net/#easeInExpo .
@[inline]
pub fn in_expo(x f64) f64 {
	return if math.close(x, 0.0) { 0.0 } else { pow(2.0, 10.0 * x - 10.0) }
}

// out_expo returns the equivalent of https://easings.net/#easeOutExpo .
@[inline]
pub fn out_expo(x f64) f64 {
	return if math.close(x, 1.0) { 1.0 } else { 1.0 - pow(2, -10.0 * x) }
}

// in_out_expo returns the equivalent of https://easings.net/#easeInOutExpo .
@[inline]
pub fn in_out_expo(x f64) f64 {
	return if math.close(x, 0.0) {
		0.0
	} else if math.close(x, 1.0) {
		1.0
	} else if x < 0.5 {
		pow(2.0, 20.0 * x - 10.0) / 2.0
	} else {
		2.0 - pow(2.0, 10.0 - 20.0 * x) / 2.0
	}
}

// in_circ returns the equivalent of https://easings.net/#easeInCirc .
@[inline]
pub fn in_circ(x f64) f64 {
	return 1.0 - sqrt(1.0 - x * x)
}

// out_circ returns the equivalent of https://easings.net/#easeOutCirc .
@[inline]
pub fn out_circ(x f64) f64 {
	return sqrt(1.0 - pow(x - 1.0, 2.0))
}

// in_out_circ returns the equivalent of https://easings.net/#easeInOutCirc .
@[inline]
pub fn in_out_circ(x f64) f64 {
	return if x < 0.5 {
		(1.0 - sqrt(1.0 - pow(2.0 * x, 2.0))) / 2.0
	} else {
		(sqrt(1.0 - pow(-2.0 * x + 2.0, 2.0)) + 1.0) / 2.0
	}
}

// in_back returns the equivalent of https://easings.net/#easeInBack .
@[inline]
pub fn in_back(x f64) f64 {
	return c3 * x * x * x - c1 * x * x
}

// out_back returns the equivalent of https://easings.net/#easeOutBack .
@[inline]
pub fn out_back(x f64) f64 {
	return 1.0 + c3 * pow(x - 1.0, 3.0) + c1 * pow(x - 1.0, 2.0)
}

// in_out_back returns the equivalent of https://easings.net/#easeInOutBack .
@[inline]
pub fn in_out_back(x f64) f64 {
	return if x < 0.5 {
		(pow(2.0 * x, x) * ((c2 + 1) * 2.0 * x - c2)) / 2.0
	} else {
		(pow(2.0 * x - 2.0, 2.0) * ((c2 + 1.0) * (x * 2.0 - 2.0) + c2) + 2.0) / 2.0
	}
}

// in_elastic returns the equivalent of https://easings.net/#easeInElastic .
@[inline]
pub fn in_elastic(x f64) f64 {
	return if math.close(x, 0.0) {
		0.0
	} else if math.close(x, 1.0) {
		1.0
	} else {
		-pow(2.0, 10.0 * x - 10.0) * sin((x * 10.0 - 10.75) * c4)
	}
}

// out_elastic returns the equivalent of https://easings.net/#easeOutElastic .
@[inline]
pub fn out_elastic(x f64) f64 {
	return if math.close(x, 0.0) {
		0.0
	} else if math.close(x, 1.0) {
		1.0
	} else {
		pow(2.0, -10.0 * x) * sin((x * 10.0 - 0.75) * c4) + 1.0
	}
}

// in_out_elastic returns the equivalent of https://easings.net/#easeInOutElastic .
@[inline]
pub fn in_out_elastic(x f64) f64 {
	return if math.close(x, 0.0) {
		0.0
	} else if math.close(x, 1.0) {
		1.0
	} else if x < 0.5 {
		-(pow(2.0, 20.0 * x - 10.0) * sin((20.0 * x - 11.125) * c5)) / 2.0
	} else {
		(pow(2.0, -20.0 * x + 10.0) * sin((20.0 * x - 11.125) * c5)) / 2.0 + 1.0
	}
}

// in_bounce returns the equivalent of https://easings.net/#easeInBounce .
@[inline]
pub fn in_bounce(x f64) f64 {
	return 1.0 - out_bounce(1.0 - x)
}

// out_bounce returns the equivalent of https://easings.net/#easeOutBounce .
pub fn out_bounce(x f64) f64 {
	if math.close(x, 1.0) {
		return 1.0
	}
	xd1 := x * d1
	return n1 * (if xd1 < 1.0 {
		x * x
	} else if xd1 < 2.0 {
		pow((x - 1.5 / d1), 2.0) + 0.75
	} else if xd1 < 2.5 {
		pow((x - 2.25 / d1), 2.0) + 0.9375
	} else {
		pow((x - 2.625 / d1), 2.0) + 0.984375
	})
}

// in_out_bounce returns the equivalent of https://easings.net/#easeInOutBounce .
pub fn in_out_bounce(x f64) f64 {
	if math.close(x, 1.0) {
		return 1.0
	}
	return if x < 0.5 {
		(1.0 - out_bounce(1.0 - 2.0 * x)) / 2.0
	} else {
		(1.0 + out_bounce(2.0 * x - 1.0)) / 2.0
	}
}
