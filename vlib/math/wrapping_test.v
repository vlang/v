import math

fn test_wrapping_u8() {
	assert (math.wrapping[u8](200) + math.wrapping[u8](100)).value == 44
	assert (math.wrapping[u8](255) + math.wrapping[u8](1)).value == 0
	assert (math.wrapping[u8](0) - math.wrapping[u8](1)).value == 255
	assert (math.wrapping[u8](16) * math.wrapping[u8](16)).value == 0
	assert (math.wrapping[u8](200) % math.wrapping[u8](7)).value == 4
	assert math.wrapping[u8](1).neg().value == 255
}

fn test_wrapping_i8() {
	assert (math.wrapping[i8](100) + math.wrapping[i8](100)).value == -56
	assert (math.wrapping[i8](127) + math.wrapping[i8](1)).value == -128
	assert (math.wrapping[i8](-128) - math.wrapping[i8](1)).value == 127
	assert (math.wrapping[i8](-128) / math.wrapping[i8](-1)).value == -128
	assert math.wrapping[i8](-128).neg().value == -128
	assert (math.wrapping[i8](-128) % math.wrapping[i8](-1)).value == 0
}

fn test_wrapping_u64_i64() {
	max_u64 := math.maxof[u64]()
	min_i64 := math.minof[i64]()
	max_i64 := math.maxof[i64]()
	assert (math.wrapping[u64](max_u64) + math.wrapping[u64](1)).value == 0
	assert (math.wrapping[u64](max_u64) * math.wrapping[u64](2)).value == max_u64 - 1
	assert (math.wrapping[i64](min_i64) - math.wrapping[i64](1)).value == max_i64
	assert (math.wrapping[i64](min_i64) / math.wrapping[i64](-1)).value == min_i64
	assert (math.wrapping[i64](max_i64) + math.wrapping[i64](1)).value == min_i64
}

fn test_wrapping_other_widths() {
	assert (math.wrapping[u32](4000000000) + math.wrapping[u32](4000000000)).value == 3705032704
	assert (math.wrapping[i16](20000) + math.wrapping[i16](20000)).value == -25536
	assert (math.wrapping[u16](60000) * math.wrapping[u16](2)).value == 54464
	assert (math.wrapping[i32](-2147483648) - math.wrapping[i32](1)).value == 2147483647
}

fn test_wrapping_helpers() {
	assert math.wrapping(3) == math.wrapping(1) + math.wrapping(2)
	assert math.wrapping[u8](44).str() == '44'
	assert math.saturating(7).value == 7
}

fn test_saturating_u8() {
	assert (math.saturating[u8](200) + math.saturating[u8](100)).value == 255
	assert (math.saturating[u8](255) + math.saturating[u8](1)).value == 255
	assert (math.saturating[u8](5) - math.saturating[u8](10)).value == 0
	assert (math.saturating[u8](16) * math.saturating[u8](16)).value == 255
	assert (math.saturating[u8](7) / math.saturating[u8](2)).value == 3
	assert math.saturating[u8](5).neg().value == 0
	assert math.saturating[u8](0).neg().value == 0
}

fn test_saturating_i8() {
	assert (math.saturating[i8](100) + math.saturating[i8](100)).value == 127
	assert (math.saturating[i8](-100) + math.saturating[i8](-100)).value == -128
	assert (math.saturating[i8](-120) - math.saturating[i8](20)).value == -128
	assert (math.saturating[i8](-128) / math.saturating[i8](-1)).value == 127
	assert math.saturating[i8](-128).neg().value == 127
	assert math.saturating[i8](5).neg().value == -5
	assert (math.saturating[i8](5) - math.saturating[i8](10)).value == -5
}

fn test_saturating_u64_i64() {
	max_u64 := math.maxof[u64]()
	min_i64 := math.minof[i64]()
	max_i64 := math.maxof[i64]()
	assert (math.saturating[u64](max_u64) + math.saturating[u64](1)).value == max_u64
	assert (math.saturating[u64](max_u64) * math.saturating[u64](2)).value == max_u64
	assert (math.saturating[u64](0) - math.saturating[u64](1)).value == 0
	assert (math.saturating[i64](min_i64) - math.saturating[i64](1)).value == min_i64
	assert (math.saturating[i64](max_i64) + math.saturating[i64](1)).value == max_i64
	assert (math.saturating[i64](min_i64) / math.saturating[i64](-1)).value == max_i64
	assert (math.saturating[i64](min_i64) * math.saturating[i64](min_i64)).value == max_i64
	assert (math.saturating[i64](min_i64) * math.saturating[i64](0)).value == 0
}

fn test_saturating_other_widths() {
	assert (math.saturating[u16](60000) + math.saturating[u16](60000)).value == 65535
	assert (math.saturating[u16](100) - math.saturating[u16](200)).value == 0
	assert (math.saturating[i32](2000000000) * math.saturating[i32](2)).value == 2147483647
	assert (math.saturating[i32](-2147483648) / math.saturating[i32](-1)).value == 2147483647
	assert math.saturating[i32](-3).str() == '-3'
}

fn test_minimum_remainder_negative_one() {
	min_i64 := math.minof[i64]()
	assert (math.wrapping[i64](min_i64) % math.wrapping[i64](-1)).value == 0
	assert (math.saturating[i64](min_i64) % math.saturating[i64](-1)).value == 0
	assert (math.wrapping[i32](math.minof[i32]()) % math.wrapping[i32](-1)).value == 0
	assert (math.saturating[i32](math.minof[i32]()) % math.saturating[i32](-1)).value == 0
	assert (math.saturating[i8](-128) % math.saturating[i8](-1)).value == 0
}
