module lsh

import math

const pi = 3.14159265358979323846

// Prng is a small deterministic xorshift64* generator. It exists so signatures
// are reproducible across backends and never depend on global random state.
struct Prng {
mut:
	state     u64
	spare     f64
	has_spare bool
}

fn new_prng(seed u64) Prng {
	mut s := seed
	if s == 0 {
		s = 0x9e3779b97f4a7c15
	}
	return Prng{ state: s }
}

@[ignore_overflow]
fn (mut p Prng) next_u64() u64 {
	mut x := p.state
	x ^= x >> 12
	x ^= x << 25
	x ^= x >> 27
	p.state = x
	return x * 2685821657736338717
}

fn (mut p Prng) below(max u64) u64 {
	if max == 0 {
		return 0
	}
	return p.next_u64() % max
}

fn (mut p Prng) unit() f64 {
	return f64(p.next_u64() >> 11) / 9007199254740992.0
}

fn (mut p Prng) normal() f64 {
	if p.has_spare {
		p.has_spare = false
		return p.spare
	}
	mut u := p.unit()
	if u <= 0.0 {
		u = 1.0 / 9007199254740992.0
	}
	v := p.unit()
	mag := math.sqrt(-2.0 * math.log(u))
	angle := 2.0 * pi * v
	p.spare = mag * math.sin(angle)
	p.has_spare = true
	return mag * math.cos(angle)
}

// sign returns a Rademacher variate: -1.0 or 1.0 with equal probability.
// It is used for SimHash hyperplanes, where only the sign of the dot product
// matters and drawing a Gaussian coefficient per dimension is unnecessary.
@[inline]
fn (mut p Prng) sign() f64 {
	return if p.next_u64() & 1 == 0 { -1.0 } else { 1.0 }
}

fn (mut p Prng) cauchy() f64 {
	mut u := p.unit()
	if u <= 0.0 {
		u = 1e-15
	}
	if u >= 1.0 {
		u = 1.0 - 1e-15
	}
	a := pi * (u - 0.5)
	return math.sin(a) / math.cos(a)
}
