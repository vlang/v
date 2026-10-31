module fnv

fn test_empty() {
	assert sum32a([]u8{}) == u32(0x811c9dc5)
	assert sum64a([]u8{}) == u64(0xcbf29ce484222325)
}

fn test_known() {
	assert sum32a('a'.bytes()) == u32(0xe40c292c)
}

fn test_incremental() {
	mut h := new32a()
	h = write32a(h, 'ab'.bytes())
	assert h == sum32a('ab'.bytes())
}

fn test_64() {
	assert sum64a('a'.bytes()) == u64(0xaf63dc4c8601ec8c)
}
