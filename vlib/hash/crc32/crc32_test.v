import hash.crc32

fn test_hash_crc32() {
	assert crc32.sum('testing crc32') == u32(1212124400)
	assert crc32.sum('testing crc32').hex() == '0x483f8cf0'

	c := crc32.new(crc32.IEEE)
	assert c.checksum('testing crc32 again') == u32(1420327025)
	assert c.checksum('testing crc32 again').hex() == '0x54a87871'
}