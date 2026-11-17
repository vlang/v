module crc16

fn test_ccitt_known() {
	// CRC-16/CCITT-FALSE of "123456789" is 0x29B1
	assert ccitt('123456789'.bytes()) == 0x29b1
}

fn test_modbus_known() {
	// CRC-16/MODBUS of "123456789" is 0x4B37
	assert modbus('123456789'.bytes()) == 0x4b37
}
