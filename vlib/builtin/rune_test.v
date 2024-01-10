fn test_repeat() {
	r1 := `V`
	r2 := `👋`

	assert r1.repeat(5) == 'VVVVV'
	assert r2.repeat(5) == '👋👋👋👋👋'

	assert r1.repeat(1) == r1.str()
	assert r2.repeat(1) == r2.str()

	assert r1.repeat(0) == ''
	assert r2.repeat(0) == ''
}

fn test_length_in_bytes() {
	assert rune(0x0).length_in_bytes() == 1
	assert `A`.length_in_bytes() == 1 // latin letter
	assert rune(0x7F).length_in_bytes() == 1
	//
	assert rune(0x80).length_in_bytes() == 2
	assert `Д`.length_in_bytes() == 2 // cyrillic letter
	assert rune(0x7FF).length_in_bytes() == 2
	//
	assert rune(0x800).length_in_bytes() == 3
	assert `喂`.length_in_bytes() == 3 // hey
	assert rune(0xFFFF).length_in_bytes() == 3
	//
	assert rune(0xD800).length_in_bytes() == -1 // min for surrogates
	assert rune(0xD866).length_in_bytes() == -1 // invalid
	assert rune(0xDFFF).length_in_bytes() == -1 // max for surrogates
	//
	assert rune(0x100000).length_in_bytes() == 4
	assert rune(0x10FFD7).length_in_bytes() == 4 // "Supplementary Private Use Area-B" ¯\_(ツ)_/¯
	assert rune(0x10FFFF).length_in_bytes() == 4
	//
	assert rune(0x110000).length_in_bytes() == -1
}

fn test_bytes() {
	r1 := `★`
	assert r1.bytes() == [u8(0xe2), 0x98, 0x85]
}
