import hash.crc32

fn test_hash_crc32() {
	b1 := 'testing crc32'.bytes()
	sum1 := crc32.sum(b1)
	assert sum1 == u32(1212124400)
	assert sum1.hex() == '483f8cf0'

	c := crc32.new(int(crc32.ieee))
	b2 := 'testing crc32 again'.bytes()
	sum2 := c.checksum(b2)
	assert sum2 == u32(1420327025)
	assert sum2.hex() == '54a87871'
}
