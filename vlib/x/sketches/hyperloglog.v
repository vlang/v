module sketches

import hash
import math

// HyperLogLog estimates the number of distinct strings added to it.
pub struct HyperLogLog {
pub:
	precision int
	seed      u64
mut:
	registers []u8
}

pub fn new_hyperloglog(precision int, seed u64) !HyperLogLog {
	if precision < 4 || precision > 20 {
		return error('precision must be in 4..20')
	}
	size := 1 << precision
	return HyperLogLog{
		precision: precision
		seed:      seed
		registers: []u8{len: size, init: 0}
	}
}

fn leading_zeros64(x u64) int {
	if x == 0 {
		return 64
	}
	mut n := 0
	mut v := x
	for (v & 0x8000000000000000) == 0 {
		n++
		v = v << 1
	}
	return n
}

fn pow2_neg(n int) f64 {
	mut v := 1.0
	for _ in 0 .. n {
		v /= 2.0
	}
	return v
}

pub fn (mut h HyperLogLog) add(value string) {
	h.add_hash(hash.sum64_string(value, h.seed))
}

// add_hash folds an already computed 64-bit hash into the registers.
pub fn (mut h HyperLogLog) add_hash(value u64) {
	idx := int(value >> u64(64 - h.precision))
	w := value << u64(h.precision)
	mut rank := leading_zeros64(w) + 1
	max_rank := 64 - h.precision + 1
	if rank > max_rank {
		rank = max_rank
	}
	if u8(rank) > h.registers[idx] {
		h.registers[idx] = u8(rank)
	}
}

// count returns the estimated number of distinct values.
pub fn (h &HyperLogLog) count() f64 {
	m := f64(h.registers.len)
	mut sum := 0.0
	mut zeros := 0
	for r in h.registers {
		if r == 0 {
			zeros++
		}
		sum += pow2_neg(int(r))
	}
	alpha := 0.7213 / (1.0 + 1.079 / m)
	mut estimate := alpha * m * m / sum
	if estimate <= 2.5 * m && zeros > 0 {
		estimate = m * math.log(m / f64(zeros))
	}
	return estimate
}

// merge folds another HyperLogLog with the same precision into this one.
pub fn (mut h HyperLogLog) merge(other &HyperLogLog) ! {
	if h.precision != other.precision {
		return error('precision must match')
	}
	for i in 0 .. h.registers.len {
		if other.registers[i] > h.registers[i] {
			h.registers[i] = other.registers[i]
		}
	}
}
