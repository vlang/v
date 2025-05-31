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

	assert rune(0x80).length_in_bytes() == 2
	assert `Д`.length_in_bytes() == 2 // cyrillic letter
	assert rune(0x7FF).length_in_bytes() == 2

	assert rune(0x800).length_in_bytes() == 3
	assert `喂`.length_in_bytes() == 3 // hey
	assert rune(0xFFFF).length_in_bytes() == 3

	assert rune(0xD800).length_in_bytes() == -1 // min for surrogates
	assert rune(0xD866).length_in_bytes() == -1 // invalid
	assert rune(0xDFFF).length_in_bytes() == -1 // max for surrogates

	assert rune(0x100000).length_in_bytes() == 4
	assert rune(0x10FFD7).length_in_bytes() == 4 // "Supplementary Private Use Area-B" ¯\_(ツ)_/¯
	assert rune(0x10FFFF).length_in_bytes() == 4

	assert rune(0x110000).length_in_bytes() == -1
}

fn test_bytes() {
	r1 := `★`
	assert r1.bytes() == [u8(0xe2), 0x98, 0x85]
}

fn test_to_upper() {
	assert `c`.to_upper() == `C`
	assert `C`.to_upper() == `C`
	assert `δ`.to_upper() == `Δ`
	assert `Δ`.to_upper() == `Δ`
	assert `ā`.to_upper() == `Ā`
	assert `Ā`.to_upper() == `Ā`
	assert `Я`.to_upper() == `Я`
	assert `я`.to_upper() == `Я`
	assert `ǅ`.to_upper() == `Ǆ`
	assert `ǆ`.to_upper() == `Ǆ`
	assert `Ǆ`.to_upper() == `Ǆ`
}

fn test_to_lower() {
	assert `C`.to_lower() == `c`
	assert `c`.to_lower() == `c`
	assert `Δ`.to_lower() == `δ`
	assert `δ`.to_lower() == `δ`
	assert `Ā`.to_lower() == `ā`
	assert `ā`.to_lower() == `ā`
	assert `я`.to_lower() == `я`
	assert `Я`.to_lower() == `я`
	assert `ǅ`.to_lower() == `ǆ`
	assert `Ǆ`.to_lower() == `ǆ`
	assert `ǆ`.to_lower() == `ǆ`
}

fn test_to_title() {
	assert `c`.to_title() == `C`
	assert `C`.to_title() == `C`
	assert `δ`.to_title() == `Δ`
	assert `Δ`.to_title() == `Δ`
	assert `ā`.to_title() == `Ā`
	assert `Ā`.to_title() == `Ā`
	assert `я`.to_title() == `Я`
	assert `Я`.to_title() == `Я`
	assert `Ǆ`.to_title() == `ǅ`
	assert `ǆ`.to_title() == `ǅ`
	assert `ǅ`.to_title() == `ǅ`
}
