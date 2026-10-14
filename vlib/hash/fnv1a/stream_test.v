module fnv1a

import hash

// digest_through_interface exercises Fnv1a64 purely as a hash.Hasher,
// proving interface conformance at compile time.
fn digest_through_interface(mut h hash.Hasher, chunks [][]u8) !u64 {
	for chunk in chunks {
		h.write(chunk)!
	}
	return h.finish()
}

fn test_fnv1a64_known_answers() {
	mut h := new_fnv1a64()
	h.write(''.bytes())!
	assert h.finish() == 0xcbf29ce484222325
	h.write('a'.bytes())!
	assert h.finish() == 0xaf63dc4c8601ec8c
	mut f := new_fnv1a64()
	f.write('foobar'.bytes())!
	assert f.finish() == 0x85944171f73967e8
}

fn test_fnv1a64_chunked_equals_oneshot() {
	data := 'The quick brown fox jumps over the lazy dog'.bytes()
	expected := sum64(data)
	mut h := new_fnv1a64()
	// odd chunk sizes to cross all alignment paths
	for i := 0; i < data.len; i += 7 {
		end := if i + 7 < data.len { i + 7 } else { data.len }
		h.write(data[i..end])!
	}
	assert h.finish() == expected
	assert h.sum(data) == expected
}

fn test_fnv1a64_finish_is_repeatable() {
	mut h := new_fnv1a64()
	h.write('abc'.bytes())!
	first := h.finish()
	h.write('def'.bytes())!
	second := h.finish()
	assert first != second, 'state must advance'
	assert h.finish() == second, 'finish must not consume state'
}

fn test_fnv1a64_implements_hash_hasher() {
	mut h := new_fnv1a64()
	got := digest_through_interface(mut h, ['foo'.bytes(), 'bar'.bytes()]) or {
		assert false, 'write must not fail'
		u64(0)
	}
	mut direct := new_fnv1a64()
	direct.write('foobar'.bytes())!
	assert got == direct.finish()
}
