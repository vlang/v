// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl

module easings

import arrays
import vsl.math

// linear_interpolation is a method of curve fitting using linear polynomials to construct new data points within the range of a discrete set of known data points
[inline]
pub fn linear_interpolation(p f64) f64 {
  	return p
}

// quadratic_ease_in eases in with a power of 2
[inline]
pub fn quadratic_ease_in(p f64) f64 {
  	return p * p
}

// quadratic_easing_eases out with a power of 2
[inline]
pub fn quadratic_ease_out(p f64) f64 {
  	return -(p * (p - 2))
}

// quadratic_easing_in_out speeds up function's growth in a power of 2, then slows down after a half at the same rate
[inline]
pub fn quadratic_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 2.0 * p * p
  	} else {
  	  	return (-2.0 * p * p) + (4.0 * p) - 1
  	}
}

// cubic_ease_in eases in with a power of 3
[inline]
pub fn cubic_ease_in(p f64) f64 {
  	return p * p * p
}

// cubic_ease_out eases out with a power of 3
[inline]
pub fn cubic_ease_out(p f64) f64 {
  	f := p - 1.0
  	return f * f * f + 1.0
}

// cubic_ease_in_out speeds up function's growth in a power of 3, then slows down after a half at the same rate
[inline]
pub fn cubic_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 4.0 * p * p * p
  	} else {
  	  	f := ((2.0 * p) - 2.0)
  	  	return 0.5 * f * f * f + 1.0
  	}
}

// quartic_ease_in eases in with a power of 4
[inline]
pub fn quartic_ease_in(p f64) f64 {
  	return p * p * p * p
}

// quartic_ease_out eases out with a power of 4
[inline]
pub fn quartic_ease_out(p f64) f64 {
  	f := (p - 1.0)
  	return f * f * f * (1.0 - p) + 1.0
}

// quartic_ease_in_out speeds up function's growth in a power of 4, then slows down after a half at the same rate
[inline]
pub fn quartic_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 8.0 * p * p * p * p
  	} else {
  	  	f := (p - 1.0)
  	  	return -8.0 * f * f * f * f + 1.0
  	}
}

// quintic_ease_in eases in with a power of 5
[inline]
pub fn quintic_ease_in(p f64) f64 {
  	return p * p * p * p * p
}

// quintic_ease_out eases out with a power of 5
[inline]
pub fn quintic_ease_out(p f64) f64 {
  	f := (p - 1.0)
  	return f * f * f * f * f + 1
}

// quintic_ease_in_out speeds up function's growth in a power of 5, then slows down after a half at the same rate
[inline]
pub fn quintic_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 16.0 * p * p * p * p * p
  	} else {
  	  	f := ((2.0 * p) - 2.0)
  	  	return  	0.5 * f * f * f * f * f + 1.0
  	}
}

// sine_ease_in accelerates using a sine formula
[inline]
pub fn sine_ease_in(p f64) f64 {
  	return math.sin((p - 1.0) * math.tau) + 1.0
}

// sine_ease_out decelerates using a sine formula
[inline]
pub fn sine_ease_out(p f64) f64 {
  	return math.sin(p * math.tau)
}

// sine_ease_in_out accelerates and decelerates using a sine formula
[inline]
pub fn sine_ease_in_out(p f64) f64 {
  	return 0.5 * (1.0 - math.cos(p * math.pi))
}

// circular_ease_in accelerates using a circular function
[inline]
pub fn circular_ease_in(p f64) f64 {
  	return 1.0 - math.sqrt(1.0 - (p * p))
}

// circular_ease_out decelerates using a circular function
[inline]
pub fn circular_ease_out(p f64) f64 {
  	return math.sqrt((2.0 - p) * p)
}

// circular_ease_in_out accelerates and decelerates using a circular function
[inline]
pub fn circular_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 0.5 * (1.0 - math.sqrt(1.0 - 4.0 * (p * p)))
  	} else {
  	  	return 0.5 * (math.sqrt(-((2.0 * p) - 3.0) * ((2.0 * p) - 1.0)) + 1.0)
  	}
}

// exponential_ease_in accelerates using an exponential formula
[inline]
pub fn exponential_ease_in(p f64) f64 {
  	return if p == 0.0 { p } else { math.pow(2, 10.0 * (p - 1.0)) }
}

// exponential_ease_out decelerates using an exponential formula
[inline]
pub fn exponential_ease_out(p f64) f64 {
  	return if p == 1.0 { p } else { 1.0 - math.pow(2, -10.0 * p) }
}

// exponential_ease_in_out accelerates and decelerates using an exponential formula
[inline]
pub fn exponential_ease_in_out(p f64) f64 {
  	if (p == 0.0 || p == 1.0) {
  	  	return p
  	}

  	if (p < 0.5) {
  	  	return 0.5 * math.pow(2, (20.0 * p) - 10.0)
  	} else {
  	  	return -0.5 * math.pow(2, (-20.0 * p) + 10.0) + 1
  	}
}

// elastic_ease_in resembles a spring oscillating back and forth, then accelerates
[inline]
pub fn elastic_ease_in(p f64) f64 {
  	return math.sin(13.0 * math.tau * p) * math.pow(2, 10.0 * (p - 1.0))
}

// elastic_ease_out resembles a spring oscillating back and forth, then decelerates
[inline]
pub fn elastic_ease_out(p f64) f64 {
  	return math.sin(-13.0 * math.tau * (p + 1.0)) * math.pow(2, -10.0 * p) + 1.0
}

// elastic_ease_in_out resembles a spring oscillating back and forth before it begins to accelerate, then resembles a spring oscillating back and forth before it begins to decelerate afer a half
[inline]
pub fn elastic_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 0.5 * math.sin(13.0 * math.tau * (2.0 * p)) * math.pow(2, 10.0 * ((2.0 * p) - 1.0))
  	} else {
  	  	return 0.5 * (math.sin(-13.0 * math.tau * ((2.0 * p - 1.0) + 1.0)) * math.pow(2, -10.0 * (2.0 * p - 1.0)) + 2.0)
  	}
}

// back_ease_in retracts the motion slightly before it begins to accelerate
[inline]
pub fn back_ease_in(p f64) f64 {
  	return p * p * p - p * math.sin(p * math.pi)
}

// back_ease_out retracts the motion slightly before it begins to decelerate
[inline]
pub fn back_ease_out(p f64) f64 {
  	f := (1.0 - p)
  	return 1.0 - (f * f * f - f * math.sin(f * math.pi))
}

// back_ease_in_out retracts the motion slightly before it begins to accelerate, then retracts the motion slightly before it begins to decelerate afer a half
[inline]
pub fn back_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	f := 2.0 * p
  	  	return 0.5 * (f * f * f - f * math.sin(f * math.pi))
  	} else {
  	  	f := (1.0 - (2.0 * p - 1.0 ))
  	  	return 0.5 * (1.0 - (f * f * f - f * math.sin(f * math.pi))) + 0.5
  	}
}

// bounce_ease_in creates a bouncing effect, then accelerates
[inline]
pub fn bounce_ease_in(p f64) f64 {
  	return 1.0 - bounce_ease_out(1.0 - p)
}

// bounce_ease_out creates a bouncing effect, then decelerates
[inline]
pub fn bounce_ease_out(p f64) f64 {
  	if (p < 4.0 / 11.0) {
  	  	return (121.0 * p * p) / 16.0
  	} else if (p < 8.0 / 11.0) {
  	  	return (363.0 / 40.0 * p * p) - (99.0 / 10.0 * p) + 17.0 / 5.0
  	} else if (p < 9.0 / 10.0) {
  	  	return (4356.0 / 361.0 * p * p) - (35442.0 / 1805.0 * p) + 16061.0 / 1805.0
  	} else {
  	  	return (54.0 / 5.0 * p * p) - (513.0 / 25.0 * p) + 268.0 / 25.0
  	}
}

// bounce_ease_in_out creates a bouncing effect before it begins to accelerate, then it creates a bouncing effects again before it begins to decelerate
[inline]
pub fn bounce_ease_in_out(p f64) f64 {
  	if (p < 0.5) {
  	  	return 0.5 * bounce_ease_in(p * 2.0)
  	} else {
  	  	return 0.5 * bounce_ease_out(p * 2.0 - 1.0) + 0.5
  	}
}

// animate returns []f64 of length "frames" using the easing function provided with lower and upper bounds as "from" and "to"
[inline]
pub fn animate(easing fn(f64) f64, from, to f64, frames int) []f64 {
  	len := int(math.max(frames, 0.00))
  	dt := f64(1.00 / (len - 1))
	animation := arrays.range<f64>(0.00, len)

  	return animation.map(from + easing(it*dt) * (to - from)) // t := it*dt
}
