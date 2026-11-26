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

fn test_shake128_small_reads_across_blocks() {
	data := []u8{len: 34, init: u8(index)}
	expected := shake128(data, 1000)

	mut s := new_shake128()
	s.write(data)
	mut chunked := []u8{}
	for chunked.len < 1000 {
		chunked << s.read(int_min(3, 1000 - chunked.len))
	}

	assert chunked == expected, 'reading 3 bytes at a time produced different output'
}

fn test_shake128_writes_across_blocks() {
	data := []u8{len: 700, init: u8(index * 7)}
	expected := shake128(data, 64)

	for step in [1, 13, 167, 168, 169] {
		mut s := new_shake128()
		mut off := 0
		for off < data.len {
			end := int_min(off + step, data.len)
			s.write(data[off..end])
			off = end
		}
		assert s.read(64) == expected, 'writing ${step} bytes at a time produced different output'
	}
}

fn test_shake_copy_is_independent() {
	mut s := new_shake128()
	s.write('0123456789abcde'.bytes())
	mut t := *s
	s.write('A'.bytes())
	t.write('B'.bytes())

	assert s.read(32) == shake128('0123456789abcdeA'.bytes(), 32)
	assert t.read(32) == shake128('0123456789abcdeB'.bytes(), 32)
}
