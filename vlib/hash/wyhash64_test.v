import hash

fn test_wyhash64_c_full_width_product_vectors() {
	assert hash.wyhash64_c(0, 0) == u64(0xbc956e8ecb2e6e19)
	assert hash.wyhash64_c(1, 0) == u64(0x030f406543ebc24d)
	assert hash.wyhash64_c(u64(1) << 32, 0) == u64(0x25d2c2d96b9881fd)
	assert hash.wyhash64_c(u64(1) << 44, 0) == u64(0x8a487489aaf8d7a5)
	assert hash.wyhash64_c(u64(1) << 63, 0) == u64(0x4c52593c8f813c32)
	assert hash.wyhash64_c(u64(0xffffffffffffffff), 0) == u64(0xbbadee89d92e665d)
	assert hash.wyhash64_c(u64(0x2d358dccaa6c78a5), 0) == u64(0xd08e4d32cfdd8d08)
	assert hash.wyhash64_c(u64(0x2d358dccaa6c78a5), 1) == u64(0xb982ce5d6c11b5e2)
	assert hash.wyhash64_c(0, u64(0x8bb84b93962eacc9)) == u64(0x6c1b23bc04f3e311)
	assert hash.wyhash64_c(1, u64(0x8bb84b93962eacc9)) == u64(0xd3810d578c364f45)
}
