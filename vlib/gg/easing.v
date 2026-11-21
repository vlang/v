// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Easing selects the interpolation curve used by ease and by Tween.
// The names follow the Robert Penner easing family used by most 2D toolkits.
pub enum Easing {
	linear
	ease_in_quad
	ease_out_quad
	ease_in_out_quad
	ease_in_cubic
	ease_out_cubic
	ease_in_out_cubic
	ease_in_quart
	ease_out_quart
	ease_in_out_quart
	ease_in_quint
	ease_out_quint
	ease_in_out_quint
	ease_in_sine
	ease_out_sine
	ease_in_out_sine
	ease_in_expo
	ease_out_expo
	ease_in_out_expo
	ease_in_circ
	ease_out_circ
	ease_in_out_circ
	ease_in_back
	ease_out_back
	ease_in_out_back
	ease_in_elastic
	ease_out_elastic
	ease_in_out_elastic
	ease_in_bounce
	ease_out_bounce
	ease_in_out_bounce
}

// ease maps a normalized time t in [0, 1] through the curve kind.
// Inputs outside the range are clamped, so ease never overshoots its input.
pub fn ease(t f32, kind Easing) f32 {
	return f32(ease_f64(f64(math.clip(t, 0.0, 1.0)), kind))
}

fn ease_out_bounce_f64(x f64) f64 {
	n1 := 7.5625
	d1 := 2.75
	if x < 1.0 / d1 {
		return n1 * x * x
	}
	if x < 2.0 / d1 {
		y := x - 1.5 / d1
		return n1 * y * y + 0.75
	}
	if x < 2.5 / d1 {
		y := x - 2.25 / d1
		return n1 * y * y + 0.9375
	}
	y := x - 2.625 / d1
	return n1 * y * y + 0.984375
}

fn ease_f64(x f64, kind Easing) f64 {
	if x == 0 || x == 1 {
		return x
	}
	c1 := 1.70158
	c2 := c1 * 1.525
	c3 := c1 + 1.0
	c4 := 2.0 * math.pi / 3.0
	c5 := 2.0 * math.pi / 4.5
	match kind {
		.linear {
			return x
		}
		.ease_in_quad {
			return x * x
		}
		.ease_out_quad {
			return 1 - (1 - x) * (1 - x)
		}
		.ease_in_out_quad {
			return if x < 0.5 { 2 * x * x } else { 1 - math.pow(-2 * x + 2, 2) / 2 }
		}
		.ease_in_cubic {
			return x * x * x
		}
		.ease_out_cubic {
			return 1 - math.pow(1 - x, 3)
		}
		.ease_in_out_cubic {
			return if x < 0.5 { 4 * x * x * x } else { 1 - math.pow(-2 * x + 2, 3) / 2 }
		}
		.ease_in_quart {
			return x * x * x * x
		}
		.ease_out_quart {
			return 1 - math.pow(1 - x, 4)
		}
		.ease_in_out_quart {
			return if x < 0.5 { 8 * x * x * x * x } else { 1 - math.pow(-2 * x + 2, 4) / 2 }
		}
		.ease_in_quint {
			return math.pow(x, 5)
		}
		.ease_out_quint {
			return 1 - math.pow(1 - x, 5)
		}
		.ease_in_out_quint {
			return if x < 0.5 { 16 * math.pow(x, 5) } else { 1 - math.pow(-2 * x + 2, 5) / 2 }
		}
		.ease_in_sine {
			return 1 - math.cos(x * math.pi / 2)
		}
		.ease_out_sine {
			return math.sin(x * math.pi / 2)
		}
		.ease_in_out_sine {
			return -(math.cos(math.pi * x) - 1) / 2
		}
		.ease_in_expo {
			return math.pow(2, 10 * x - 10)
		}
		.ease_out_expo {
			return 1 - math.pow(2, -10 * x)
		}
		.ease_in_out_expo {
			return if x < 0.5 {
				math.pow(2, 20 * x - 10) / 2
			} else {
				(2 - math.pow(2, -20 * x + 10)) / 2
			}
		}
		.ease_in_circ {
			return 1 - math.sqrt(1 - x * x)
		}
		.ease_out_circ {
			return math.sqrt(1 - (x - 1) * (x - 1))
		}
		.ease_in_out_circ {
			return if x < 0.5 {
				(1 - math.sqrt(1 - math.pow(2 * x, 2))) / 2
			} else {
				(math.sqrt(1 - math.pow(-2 * x + 2, 2)) + 1) / 2
			}
		}
		.ease_in_back {
			return c3 * x * x * x - c1 * x * x
		}
		.ease_out_back {
			return 1 + c3 * math.pow(x - 1, 3) + c1 * math.pow(x - 1, 2)
		}
		.ease_in_out_back {
			return if x < 0.5 {
				math.pow(2 * x, 2) * ((c2 + 1) * 2 * x - c2) / 2
			} else {
				(math.pow(2 * x - 2, 2) * ((c2 + 1) * (x * 2 - 2) + c2) + 2) / 2
			}
		}
		.ease_in_elastic {
			return -math.pow(2, 10 * x - 10) * math.sin((x * 10 - 10.75) * c4)
		}
		.ease_out_elastic {
			return math.pow(2, -10 * x) * math.sin((x * 10 - 0.75) * c4) + 1
		}
		.ease_in_out_elastic {
			return if x < 0.5 {
				-(math.pow(2, 20 * x - 10) * math.sin((20 * x - 11.125) * c5)) / 2
			} else {
				(math.pow(2, -20 * x + 10) * math.sin((20 * x - 11.125) * c5)) / 2 + 1
			}
		}
		.ease_in_bounce {
			return 1 - ease_out_bounce_f64(1 - x)
		}
		.ease_out_bounce {
			return ease_out_bounce_f64(x)
		}
		.ease_in_out_bounce {
			return if x < 0.5 {
				(1 - ease_out_bounce_f64(1 - 2 * x)) / 2
			} else {
				(1 + ease_out_bounce_f64(2 * x - 1)) / 2
			}
		}
	}
}
