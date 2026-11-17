module quick

import math

// Rng is a small deterministic xorshift64* generator. Tests stay
// reproducible: the same seed always yields the same values.
pub struct Rng {
mut:
	state u64
}

pub fn new_rng(seed u64) Rng {
	mut s := seed
	if s == 0 {
		s = 0x9e3779b97f4a7c15
	}
	return Rng{ state: s }
}

pub fn (mut r Rng) next_u64() u64 {
	mut x := r.state
	x ^= x >> 12
	x ^= x << 25
	x ^= x >> 27
	r.state = x
	return x * 0x2545f4914f6cdd1d
}

pub fn (mut r Rng) next_int_range(lo int, hi int) int {
	if hi <= lo {
		return lo
	}
	span := u64(hi - lo)
	return lo + int(r.next_u64() % span)
}

// Generator produces random values of type T for property tests.
pub interface Generator[T] {
mut:
	generate(mut rng Rng) T
	render(v T) string
}

// Config tunes a property check run.
@[params]
pub struct Config {
pub:
	max_count int = 100
	seed      u64 = 1
}

// check runs `prop` against up to `cfg.max_count` values from `gen`,
// returning the number of cases passed. The first counterexample
// fails the check with its rendered value.
pub fn check[T](mut gen Generator[T], prop fn (v T) bool, cfg Config) !int {
	max := if cfg.max_count > 0 { cfg.max_count } else { 100 }
	mut rng := new_rng(cfg.seed)
	for i in 0 .. max {
		v := gen.generate(mut rng)
		if !prop(v) {
			return error('quick: counterexample after ${i} cases: ${gen.render(v)}')
		}
	}
	return max
}

// BoolGenerator generates booleans.
pub struct BoolGenerator {}

pub fn (mut g BoolGenerator) generate(mut rng Rng) bool {
	return rng.next_u64() & 1 == 1
}

pub fn (mut g BoolGenerator) render(v bool) string {
	return v.str()
}

// IntGenerator generates ints in [lo, hi).
pub struct IntGenerator {
pub:
	lo int
	hi int
}

pub fn (mut g IntGenerator) generate(mut rng Rng) int {
	return rng.next_int_range(g.lo, g.hi)
}

pub fn (mut g IntGenerator) render(v int) string {
	return v.str()
}

// F64Generator generates f64 values in [lo, hi).
pub struct F64Generator {
pub:
	lo f64
	hi f64
}

pub fn (mut g F64Generator) generate(mut rng Rng) f64 {
	if !(g.hi > g.lo) {
		return g.lo
	}
	frac := f64(rng.next_u64() >> 11) / f64(u64(1) << 53)
	// Clamp infinite endpoints to the finite extremes. The clamped
	// interval is a subset of [lo, hi), so every value stays in contract
	// while remaining finite.
	lo := if math.is_inf(g.lo, -1) { -math.max_f64 } else { g.lo }
	hi := if math.is_inf(g.hi, 1) { math.max_f64 } else { g.hi }
	if !(hi > lo) {
		return g.lo
	}
	// Overflow-safe interpolation: each term is bounded by
	// max(|lo|, |hi|), so the sum cannot overflow to inf for finite
	// endpoints, unlike lo + frac * (hi - lo).
	v := lo * (1.0 - frac) + hi * frac
	// Enforce the exclusive upper bound: rounding can push v to hi,
	// notably when lo and hi are adjacent representable values.
	if v >= hi {
		return math.nextafter(hi, lo)
	}
	if v < lo {
		return lo
	}
	return v
}

pub fn (mut g F64Generator) render(v f64) string {
	return v.str()
}

const gen_alphabet = 'abcdefghijklmnopqrstuvwxyz0123456789'

// StringGenerator generates strings up to max_len runes from an
// alphanumeric alphabet.
pub struct StringGenerator {
pub:
	max_len int = 16
}

pub fn (mut g StringGenerator) generate(mut rng Rng) string {
	n := rng.next_int_range(0, g.max_len + 1)
	mut out := []u8{cap: n}
	for _ in 0 .. n {
		out << gen_alphabet[rng.next_int_range(0, gen_alphabet.len)]
	}
	return out.bytestr()
}

pub fn (mut g StringGenerator) render(v string) string {
	return '"${v}"'
}
