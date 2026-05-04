module sha3

fn test_shake256_streaming_matches_oneshot() {
	data := 'hello world'.bytes()
	// oneshot
	expected := shake256(data, 64)

	// streaming
	mut s := new_shake256()
	s.write(data)
	result := s.read(64)

	assert result == expected, 'streaming SHAKE-256 output differs from one-shot'
}

fn test_shake128_streaming_matches_oneshot() {
	data := 'hello world'.bytes()
	expected := shake128(data, 64)

	mut s := new_shake128()
	s.write(data)
	result := s.read(64)

	assert result == expected, 'streaming SHAKE-128 output differs from one-shot'
}

fn test_shake256_incremental_write() {
	data := 'the quick brown fox jumps over the lazy dog'.bytes()
	expected := shake256(data, 128)

	mut s := new_shake256()
	s.write(data[..10])
	s.write(data[10..25])
	s.write(data[25..])
	result := s.read(128)

	assert result == expected, 'incremental write produced different output'
}

fn test_shake256_incremental_read() {
	data := 'test data for incremental reads'.bytes()

	// all at once
	mut s1 := new_shake256()
	s1.write(data)
	all_at_once := s1.read(200)

	// in chunks
	mut s2 := new_shake256()
	s2.write(data)
	mut chunked := []u8{}
	chunked << s2.read(50)
	chunked << s2.read(80)
	chunked << s2.read(70)

	assert chunked == all_at_once, 'incremental read produced different output'
}

fn test_shake128_large_output() {
	data := 'large output test'.bytes()
	mut s := new_shake128()
	s.write(data)
	// more than one block (168 bytes in shake128)
	result := s.read(500)
	assert result.len == 500
}

fn test_shake_reset() {
	data := 'reset test'.bytes()

	mut s := new_shake256()
	s.write(data)
	first := s.read(32)

	s.reset()
	s.write(data)
	second := s.read(32)

	assert first == second, 'reset did not restore initial state'
}
