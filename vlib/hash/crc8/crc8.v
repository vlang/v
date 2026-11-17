module crc8

// smbus computes the CRC-8/SMBUS checksum of `data`.
pub fn smbus(data []u8) u8 {
	mut crc := u8(0)
	for b in data {
		crc ^= b
		for _ in 0 .. 8 {
			if crc & 0x80 != 0 {
				crc = (crc << 1) ^ 0x07
			} else {
				crc <<= 1
			}
		}
	}
	return crc
}
