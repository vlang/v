module fnv

import hash.fnv1a

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

fn test_all_variants_known_answers() {
	// Golden values from Go's src/hash/fnv/fnv_test.go.
	inputs := ['', 'a', 'ab', 'abc']
	fnv32 := [u32(0x811c9dc5), 0x050c5d7e, 0x70772d38, 0x439c2f4b]
	fnv32a := [u32(0x811c9dc5), 0xe40c292c, 0x4d2505ca, 0x1a47e90b]
	fnv64 := [u64(0xcbf29ce484222325), 0xaf63bd4c8601b7be, 0x08326707b4eb37b8, 0xd8dcca186bafadcb]
	fnv64a := [u64(0xcbf29ce484222325), 0xaf63dc4c8601ec8c, 0x089c4407b545986a, 0xe71fa2190541574b]
	for i, text in inputs {
		data := text.bytes()
		assert sum32(data) == fnv32[i]
		assert sum32a(data) == fnv32a[i]
		assert sum64(data) == fnv64[i]
		assert sum64a(data) == fnv64a[i]
	}
}

fn test_all_variants_incremental_at_every_split() {
	data := [u8(0), 1, 127, 128, 254, 255, 0]
	for split in 0 .. data.len + 1 {
		assert write32(write32(new32(), data[..split]), data[split..]) == sum32(data)
		assert write32a(write32a(new32a(), data[..split]), data[split..]) == sum32a(data)
		assert write64(write64(new64(), data[..split]), data[split..]) == sum64(data)
		assert write64a(write64a(new64a(), data[..split]), data[split..]) == sum64a(data)
	}
}

fn test_empty_chunks_preserve_state() {
	empty := []u8{}
	assert write32(u32(0), empty) == 0
	assert write32a(u32(0xffffffff), empty) == u32(0xffffffff)
	assert write64(u64(0), empty) == 0
	assert write64a(u64(0xffffffffffffffff), empty) == u64(0xffffffffffffffff)
}

fn test_fnv1a_matches_existing_module_for_binary_input() {
	data := []u8{len: 256, init: u8(index)}
	assert sum32a(data) == fnv1a.sum32(data)
	assert sum64a(data) == fnv1a.sum64(data)
}
