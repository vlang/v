module crc8

fn test_smbus_known() {
	// CRC-8/SMBUS of "123456789" is 0xF4
	assert smbus('123456789'.bytes()) == 0xf4
}
