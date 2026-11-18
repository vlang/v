module hex

fn test_hexdump() {
	d := hexdump('Hello'.bytes())
	assert d.starts_with('00000000')
	assert d.contains('48 65 6c 6c 6f')
	assert d.contains('|Hello|')
	assert d.ends_with('\n')
}

fn test_hexdump_empty() {
	assert hexdump([]u8{}) == ''
}

fn test_hexdump_multiline() {
	mut data := []u8{len: 17}
	for i in 0 .. 17 {
		data[i] = u8(i)
	}
	d := hexdump(data)
	assert d.starts_with('00000000')
	assert d.contains('00000010')
}

fn test_hexdump_exact_partial_row() {
	// Exact Go Dump-compatible output for a 5-byte partial row:
	// full-width 49-char hex field (incl. the extra gap after byte 8),
	// space-padded, then the ASCII column.
	assert hexdump('Hello'.bytes()) == '00000000  48 65 6c 6c 6f                                    |Hello|\n'
}

fn test_hexdump_exact_full_row() {
	// A complete 16-byte row is 79 bytes and carries the double-space
	// separator after the 8th byte.
	mut data := []u8{len: 16}
	for i in 0 .. 16 {
		data[i] = u8(i)
	}
	d := hexdump(data)
	assert d == '00000000  00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f  |................|\n'
	assert d.len == 79
}
