// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module math

const (
	PI = 3.14159265358979323846264338327950288419716939937510582097494459
)

fn abs(a f64) f64 {
	if a < 0 {
		return -a
	}
	return a
}

fn cos(a f64) f64 {
	return C.cos(a)
}

fn max(a, b f64) f64 {
	if a > b {
		return a
	}
	return b
}

fn min(a, b f64) f64 {
	if a < b {
		return a
	}
	return b
}

fn pow(a, b f64) f64 {
	return C.pow(a, b)
}

fn radians(degrees f64) f64 {
	return degrees * (PI / 180.0)
}

fn round(f f64) f64 {
	return C.round(f)
}

fn sin(a f64) f64 {
	return C.sin(a)
}

fn sqrt(a f64) f64 {
	return C.sqrt(a)
}

