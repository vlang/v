pub const fixed_sized_comptime_array_const = $if amd64 {
	[u8(0x01), 0x02]!
} $else $if i386 {
	[u8(0x01), 0x02, 0x03]!
} $else {
	[u8(0)]!
}

pub const fixed_sized_comptime_array_const2 = $if i386 {
	[u8(0x01), 0x02, 0x03]!
} $else $if amd64 {
	[u8(0x01), 0x02]!
} $else {
	[u8(0)]!
}

fn test_main() {
	assert fixed_sized_comptime_array_const == [u8(1), 2]!
	assert fixed_sized_comptime_array_const2 == [u8(1), 2]!
}
