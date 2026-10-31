module maphash

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
