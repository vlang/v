// Copyright (c) 2020-2022 Leah Lundqvist. All rights reserved.
// Use of this source code is governed by a GPL license
// that can be found in the LICENSE file.
module math

pub type EasingFunction = fn (arg_1 f64) f64

pub enum EasingType {
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
}

fn linear(x f64) f64 {
	return x
}

fn ease_in_quad(x f64) f64 {
	return x * x
}

fn ease_out_quad(x f64) f64 {
	return x * (2.0 - x)
}

fn ease_in_out_quad(x f64) f64 {
	return if x < .5 { 2.0 * x * x } else { -1.0 + (4.0 - 2.0 * x) * x }
}

fn ease_in_cubic(x f64) f64 {
	return x * x * x
}

fn ease_out_cubic(x f64) f64 {
	return (x - 1.0) * (x - 1.0) * (x - 1.0) + 1
}

fn ease_in_out_cubic(x f64) f64 {
	return if x < .5 { 4.0 * x * x * x } else { (x - 1.0) * (2.0 * x - 2.0) * (2.0 * x - 2.0) + 1.0 }
}

fn ease_in_quart(x f64) f64 {
	return x * x * x * x
}

fn ease_out_quart(x f64) f64 {
	return 1.0 - (x - 1.0) * (x - 1.0) * (x - 1.0) * (x - 1.0)
}

fn ease_in_out_quart(x f64) f64 {
	return if x < 0.5 {
		8.0 * x * x * x * x
	} else {
		1.0 - 8.0 * (x - 1.0) * (x - 1.0) * (x - 1.0) * (x - 1.0)
	}
}

fn ease_in_quint(x f64) f64 {
	return x * x * x * x * x
}

fn ease_out_quint(x f64) f64 {
	return 1.0 + (x - 1.0) * (x - 1.0) * (x - 1.0) * (x - 1.0) * (x - 1.0)
}

fn ease_in_out_quint(x f64) f64 {
	return if x < 0.5 {
		16.0 * x * x * x * x * x
	} else {
		1.0 + 16.0 * (x - 1.0) * (x - 1.0) * (x - 1.0) * (x - 1.0) * (x - 1.0)
	}
}

pub fn easing(easingtype EasingType) EasingFunction {
	match easingtype {
		.linear { return linear }
		.ease_in_quad { return ease_in_quad }
		.ease_out_quad { return ease_out_quad }
		.ease_in_out_quad { return ease_in_out_quad }
		.ease_in_cubic { return ease_in_cubic }
		.ease_out_cubic { return ease_out_cubic }
		.ease_in_out_cubic { return ease_in_out_cubic }
		.ease_in_quart { return ease_in_quart }
		.ease_out_quart { return ease_out_quart }
		.ease_in_out_quart { return ease_in_out_quart }
		.ease_in_quint { return ease_in_quint }
		.ease_out_quint { return ease_out_quint }
		.ease_in_out_quint { return ease_in_out_quint }
	}
}
