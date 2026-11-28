// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module anim

import math

// Noise is a deterministic, seedable generator for 2D value and gradient
// noise. It is pure V, so it behaves identically on native and web backends.
pub struct Noise {
pub mut:
	seed u32
}

// new_noise returns a noise generator with the given integer seed.
pub fn new_noise(seed u32) Noise {
	return Noise{ seed: seed }
}

fn noise_hash2(x int, y int, seed u32) f32 {
	mut h := i64(x) * 374761393 + i64(y) * 668265263 + i64(seed) * 1442695041
	h = (h ^ (h >> 13)) * 1274126177
	h ^= h >> 16
	return f32(u32(h & i64(0x7fffffff))) / f32(0x7fffffff)
}

fn noise_fade(t f32) f32 {
	return f32(math.smoothstep(0.0, 1.0, f64(math.clip(t, 0.0, 1.0))))
}

// value2d returns smooth value noise in [-1, 1] at (x, y).
pub fn (n &Noise) value2d(x f32, y f32) f32 {
	ix := int(math.floor(f64(x)))
	iy := int(math.floor(f64(y)))
	fx := x - f32(ix)
	fy := y - f32(iy)
	u := noise_fade(fx)
	v := noise_fade(fy)
	a := noise_hash2(ix, iy, n.seed)
	b := noise_hash2(ix + 1, iy, n.seed)
	c := noise_hash2(ix, iy + 1, n.seed)
	d := noise_hash2(ix + 1, iy + 1, n.seed)
	return math.mix(math.mix(a, b, u), math.mix(c, d, u), v) * 2 - 1
}

fn noise_grad2(ix int, iy int, seed u32, x f32, y f32) f32 {
	angle := noise_hash2(ix, iy, seed) * 2 * f32(math.pi)
	return f32(math.cos(f64(angle))) * x + f32(math.sin(f64(angle))) * y
}

// perlin2d returns gradient (Perlin-style) noise at (x, y). Values are
// approximately in [-1, 1] but may slightly exceed it between lattice points.
pub fn (n &Noise) perlin2d(x f32, y f32) f32 {
	ix := int(math.floor(f64(x)))
	iy := int(math.floor(f64(y)))
	fx := x - f32(ix)
	fy := y - f32(iy)
	u := noise_fade(fx)
	v := noise_fade(fy)
	a := noise_grad2(ix, iy, n.seed, fx, fy)
	b := noise_grad2(ix + 1, iy, n.seed, fx - 1, fy)
	c := noise_grad2(ix, iy + 1, n.seed, fx, fy - 1)
	d := noise_grad2(ix + 1, iy + 1, n.seed, fx - 1, fy - 1)
	return math.mix(math.mix(a, b, u), math.mix(c, d, u), v)
}

// fbm2d returns fractal Brownian motion: octaves layers of perlin2d at
// increasing frequency and decreasing amplitude, normalized to [-1, 1].
pub fn (n &Noise) fbm2d(x f32, y f32, octaves int, lacunarity f32, gain f32) f32 {
	if octaves <= 0 {
		return 0
	}
	mut amplitude := f32(1)
	mut frequency := f32(1)
	mut sum := f32(0)
	mut norm := f32(0)
	for _ in 0 .. octaves {
		sum += n.perlin2d(x * frequency, y * frequency) * amplitude
		norm += amplitude
		amplitude *= gain
		frequency *= lacunarity
	}
	if norm == 0 {
		return 0
	}
	return sum / norm
}
