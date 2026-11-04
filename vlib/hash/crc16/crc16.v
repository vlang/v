module crc16

// ccitt computes the CRC-16/CCITT-FALSE checksum of `data`.
pub fn ccitt(data []u8) u16 {
	mut crc := u16(0xffff)
	for b in data {
		crc ^= u16(b) << 8
		for _ in 0 .. 8 {
			if crc & 0x8000 != 0 {
				crc = (crc << 1) ^ 0x1021
			} else {
				crc <<= 1
			}
		}
	}
	return crc
}

// modbus computes the CRC-16/MODBUS checksum of `data`.
pub fn modbus(data []u8) u16 {
	mut crc := u16(0xffff)
	for b in data {
		crc ^= u16(b)
		for _ in 0 .. 8 {
			if crc & 1 != 0 {
				crc = (crc >> 1) ^ 0xa001
			} else {
				crc >>= 1
			}
		}
	}
	return crc
}
