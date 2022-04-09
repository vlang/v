module constants

// Commonly used constants across RNGs - some taken from "Numerical Recipes".
pub const (
	lower_mask     = u64(0x00000000FFFFFFFF)
	max_u32        = u32(0xFFFFFFFF)
	max_u64        = u64(0xFFFFFFFFFFFFFFFF)
	max_u32_as_f32 = f32(max_u32) + 1
	max_u64_as_f64 = f64(max_u64) + 1
	u31_mask       = u32(0x7FFFFFFF)
	u63_mask       = u64(0x7FFFFFFFFFFFFFFF)
)
