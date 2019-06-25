// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module math

const (
	E   = 2.71828182845904523536028747135266249775724709369995957496696763
	Pi  = 3.14159265358979323846264338327950288419716939937510582097494459
	Phi = 1.61803398874989484820458683436563811772030917980576286213544862

	Sqrt2   = 1.41421356237309504880168872420969807856967187537694807317667974
	SqrtE   = 1.64872127070012814684865078781416357165377610071014801157507931
	SqrtPi  = 1.77245385090551602729816748334114518279754945612238712821380779
	SqrtPhi = 1.27201964951406896425242246173749149171560804184009624861664038

	Ln2    = 0.693147180559945309417232121458176568075500134360255254120680009
	Log2E  = 1.0 / Ln2
	Ln10   = 2.30258509299404568401799145468436420760110148862877297603332790
	Log10E = 1.0 / Ln10
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
	return degrees * (Pi / 180.0)
}

fn degrees(radians f64) f64 {
	return radians * (180.0 / Pi)
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
