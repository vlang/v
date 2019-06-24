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

fn acos(a f64) f64 {
	return C.acos(a)
}

fn asin(a f64) f64 {
	return C.asin(a)
}

fn atan(a f64) f64 {
	return C.atan(a)
}

fn atan2(a, b f64) f64 {
	return C.atan2(a, b)
}

fn ceil(a f64) f64 {
	return C.ceil(a)
}

fn cos(a f64) f64 {
	return C.cos(a)
}

fn cosh(a f64) f64 {
	return C.cosh(a)
}

fn exp(a f64) f64 {
	return C.exp(a)
}

fn floor(a f64) f64 {
	return C.floor(a)
}

fn fmod(a, b f64) f64 {
	return C.fmod(a, b)
}

fn log(a f64) f64 {
	return C.log(a)
}

fn log10(a f64) f64 {
	return C.log10(a)
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

fn degrees(radians f64) f64 {
	return radians * (180.0 / PI)
}

fn round(f f64) f64 {
	return C.round(f)
}

fn sin(a f64) f64 {
	return C.sin(a)
}

fn sinh(a f64) f64 {
	return C.sinh(a)
}

fn sqrt(a f64) f64 {
	return C.sqrt(a)
}

fn tan(a f64) f64 {
	return C.tan(a)
}

fn tanh(a f64) f64 {
	return C.tanh(a)
}

fn trunc(a f64) f64 {
	return C.trunc(a)
}
