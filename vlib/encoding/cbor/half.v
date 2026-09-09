module cbor

import math

// Half-precision (binary16) <-> f32/f64 conversion, integer-only.
// CBOR major type 7, additional info 25 carries an IEEE 754 binary16 value.
// V has no f16 type, so we synthesise the conversion via bit manipulation.
//
// Layout reminder (big-endian on the wire):
//   binary16: 1 sign bit | 5 exponent bits (bias 15) | 10 mantissa bits
//   binary32: 1 sign bit | 8 exponent bits (bias 127) | 23 mantissa bits
//   binary64: 1 sign bit | 11 exponent bits (bias 1023) | 52 mantissa bits

// IEEE 754 binary16 special-value bit patterns — used by the encoder when
// emitting NaN / ±Inf, and recognised by the decoder. The CBOR canonical
// quiet-NaN payload is 0x7e00 (RFC 8949 §3.3 / §4.2.2).
const half_qnan_bits = u16(0x7e00)
const half_pos_inf_bits = u16(0x7c00)
const half_neg_inf_bits = u16(0xfc00)

// half_to_f64 expands a 16-bit IEEE 754 value (as a u16) to an f64. Inf and
// NaN are preserved as the corresponding f64 representations; subnormals
// are converted exactly.
@[inline]
fn half_to_f64(h u16) f64 {
	sign := u64(h & 0x8000) << 48
	exp := int((h >> 10) & 0x1f)
	mant := u64(h & 0x3ff)
	if exp == 0 {
		if mant == 0 {
			return math.f64_from_bits(sign) // ±0
		}
		// Subnormal binary16: value = mant * 2^-24. Renormalize for f64.
		mut m := mant
		mut e := 1
		for m & 0x400 == 0 {
			m <<= 1
			e++
		}
		m &= 0x3ff
		// Unbiased exp16 = 1 - 15 - (e - 1) = -14 - (e - 1) = -13 - e ; biased f64 = exp + 1023
		f64_exp := u64(1023 - 14 - (e - 1)) << 52
		return math.f64_from_bits(sign | f64_exp | (m << 42))
	}
	if exp == 0x1f {
		// Inf or NaN.
		f64_exp := u64(0x7ff) << 52
		return math.f64_from_bits(sign | f64_exp | (mant << 42))
	}
	// Normal binary16.
	f64_exp := u64(exp - 15 + 1023) << 52
	return math.f64_from_bits(sign | f64_exp | (mant << 42))
}

// f32_to_half tries to round-trip a binary32 value into binary16. Returns
// (bits, true) when the conversion is exact (lossless), otherwise the
// boolean is false. NaN is mapped to the canonical quiet NaN 0x7e00 and
// reported as exact, since the CBOR preferred-serialisation rule
// (RFC 8949 §4.2.2) authorises that mapping for any NaN payload.
@[inline]
fn f32_to_half(v f32) (u16, bool) {
	bits := math.f32_bits(v)
	sign := u16((bits >> 16) & 0x8000)
	exp32 := int((bits >> 23) & 0xff)
	mant32 := bits & 0x7fffff
	// Zero.
	if exp32 == 0 && mant32 == 0 {
		return sign, true
	}
	// Inf.
	if exp32 == 0xff {
		if mant32 == 0 {
			return sign | 0x7c00, true
		}
		// NaN: collapse to canonical quiet NaN.
		return sign | 0x7e00, true
	}
	// Real value with unbiased exponent.
	exp_real := exp32 - 127
	if exp_real > 15 {
		return 0, false // would overflow to ±inf, not lossless
	}
	if exp_real >= -14 {
		// Normal range in binary16: low 13 bits of f32 mantissa must be zero.
		if mant32 & 0x1fff != 0 {
			return 0, false
		}
		half_exp := u16(u32(exp_real + 15) << 10)
		return sign | half_exp | u16(mant32 >> 13), true
	}
	if exp_real >= -24 {
		// Subnormal in binary16. Build the implicit-leading-1 mantissa and
		// check the dropped low bits are all zero.
		shift := u32(-exp_real - 1)
		full := u32(mant32 | (u32(1) << 23))
		mask := (u32(1) << shift) - 1
		if full & mask != 0 {
			return 0, false
		}
		return sign | u16(full >> shift), true
	}
	return 0, false
}

// f64_to_half_via_f32 returns half bits for an f64 only when both
// f64 -> f32 and f32 -> f16 are lossless.
@[inline]
fn f64_to_half(v f64) (u16, bool) {
	f := f32(v)
	if f64(f) != v {
		return 0, false
	}
	return f32_to_half(f)
}
