module constants

// Commonly used constants across RNGs - some taken from "Numerical Recipes".
pub const lower_mask = u64(0x00000000FFFFFFFF)
pub const max_u32 = u32(0xFFFFFFFF)
pub const max_u64 = u64(0xFFFFFFFFFFFFFFFF)
pub const u31_mask = u32(0x7FFFFFFF)
pub const u63_mask = u64(0x7FFFFFFFFFFFFFFF)
// 23 bits for f32
pub const ieee754_mantissa_f32_mask = (u32(1) << 23) - 1
// 52 bits for f64
pub const ieee754_mantissa_f64_mask = (u64(1) << 52) - 1
// smallest mantissa with exponent 0 (un normalized)
pub const reciprocal_2_23rd = 1.0 / f64(u32(1) << 23)
pub const reciprocal_2_52nd = 1.0 / f64(u64(1) << 52)
