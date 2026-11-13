module maphash

import rand

fn test_deterministic_with_same_seed() {
	seed := make_seed()
	mut h1 := new_hasher(seed)
	mut h2 := new_hasher(seed)
	h1.write_string('hello')
	h2.write_string('hello')
	assert h1.sum64() == h2.sum64()
}

fn test_different_data_differs() {
	seed := make_seed()
	mut h1 := new_hasher(seed)
	mut h2 := new_hasher(seed)
	h1.write_string('hello')
	h2.write_string('world')
	assert h1.sum64() != h2.sum64()
}

fn test_reset() {
	seed := make_seed()
	mut h := new_hasher(seed)
	h.write_string('hello')
	before := h.sum64()
	h.reset()
	h.write_string('hello')
	assert h.sum64() == before
}

fn test_write_u64() {
	seed := make_seed()
	mut h := new_hasher(seed)
	h.write_u64(42)
	assert h.sum64() != 0
}

fn test_zero_hasher_reset_preserves_initial_state() {
	mut h := Hasher{}
	initial := h.sum64()
	h.reset()
	assert h.sum64() == initial
	mut explicit := new_hasher(Seed{})
	assert h.sum64() == explicit.sum64()
	h.write_string('zero seed')
	explicit.write_string('zero seed')
	assert h.sum64() == explicit.sum64()
	h.reset()
	h.write_string('zero seed')
	assert h.sum64() == explicit.sum64()
}

fn test_make_seed_does_not_advance_global_rand() {
	rand.seed([u32(123), 456])
	expected := rand.u64()
	rand.seed([u32(123), 456])
	_ := make_seed()
	mut workers := []thread Seed{}
	for _ in 0 .. 8 {
		workers << spawn make_seed()
	}
	seeds := workers.wait()
	assert seeds.len == 8
	assert rand.u64() == expected
}

fn test_chunked_writes_and_sum_do_not_change_stream() {
	seed := make_seed()
	data := [u8(0), 0xff, 0xc3, 0xa9, 1, 2, 3, 4, 5]
	mut whole := new_hasher(seed)
	whole.write(data)
	for split in 0 .. data.len + 1 {
		mut h := new_hasher(seed)
		h.write(data[..split])
		before := h.sum64()
		assert h.sum64() == before
		h.write([]u8{})
		assert h.sum64() == before
		h.write_string(data[split..].bytestr())
		assert h.sum64() == whole.sum64()
	}
}

fn test_write_u64_uses_big_endian_bytes() {
	seed := make_seed()
	mut integers := new_hasher(seed)
	mut bytes := new_hasher(seed)
	integers.write_u64(0x0123456789abcdef)
	bytes.write([u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef])
	assert integers.sum64() == bytes.sum64()
	integers.write_u64(0)
	bytes.write([]u8{len: 8})
	assert integers.sum64() == bytes.sum64()
}

fn test_hasher_copy_can_diverge_without_changing_original() {
	seed := make_seed()
	mut original := new_hasher(seed)
	original.write_string('prefix')
	before := original.sum64()
	mut copy := original
	copy.write_string('suffix')
	mut expected := new_hasher(seed)
	expected.write_string('prefixsuffix')
	assert copy.sum64() == expected.sum64()
	assert original.sum64() == before
}
