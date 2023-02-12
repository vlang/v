module constants

// Commonly used constants across RNGs - some taken from "Numerical Recipes".
pub const (
	lower_mask                = u64(0x00000000FFFFFFFF)
	max_u32                   = u32(0xFFFFFFFF)
	max_u64                   = u64(0xFFFFFFFFFFFFFFFF)
	u31_mask                  = u32(0x7FFFFFFF)
	u63_mask                  = u64(0x7FFFFFFFFFFFFFFF)
	// 23 bits for f32
	ieee754_mantissa_f32_mask = (u32(1) << 23) - 1
	// 52 bits for f64
	ieee754_mantissa_f64_mask = (u64(1) << 52) - 1
	// smallest mantissa with exponent 0 (un normalized)
	reciprocal_2_23rd         = 1.0 / f64(u32(1) << 23)
	reciprocal_2_52nd         = 1.0 / f64(u64(1) << 52)
)
