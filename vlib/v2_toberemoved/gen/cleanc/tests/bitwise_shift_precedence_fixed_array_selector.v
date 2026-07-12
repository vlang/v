struct QuaternionKey {
	values [3]u16
}

fn unpack(key QuaternionKey) u32 {
	packed := u32(key.values[0]) >> 3 | u32(key.values[1]) << 13 | u32(key.values[2]) << 29
	return packed
}

fn main() {
	key := QuaternionKey{
		values: [u16(0x1234), u16(0x56), u16(0x79)]!
	}
	println(unpack(key))
}
