import hash

fn test_wyhash64_c_full_width_product_vectors() {
	assert hash.wyhash64_c(0, 0) == u64(0xca813bf4c7abf0a9)
	assert hash.wyhash64_c(1, 0) == u64(0x5ed9c758b9c48de0)
	assert hash.wyhash64_c(u64(1) << 32, 0) == u64(0x70f0cf3c53f32535)
	assert hash.wyhash64_c(u64(1) << 44, 0) == u64(0xa4569cb05af20b0a)
	assert hash.wyhash64_c(u64(1) << 63, 0) == u64(0x0ca561be9c542e04)
	assert hash.wyhash64_c(u64(0xffffffffffffffff), 0) == u64(0xd111bbf2944bfa09)
}
