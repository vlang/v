import encoding.utf8

fn test_validate_str_accepts_well_formed_utf8() {
	assert utf8.validate_str('')
	assert utf8.validate_str('plain ascii')
	assert utf8.validate_str('ascii + accents: ăâ êñ 中文 日本語 😀')
	assert utf8.validate_str('mixed 漢字 and latin')
}

fn test_validate_str_rejects_broken_utf8() {
	// truncated continuation byte
	assert !utf8.validate_str('\xc3')
	// lone continuation byte
	assert !utf8.validate_str('\x80')
	// overlong 2-byte encoding of '/'
	assert !utf8.validate_str('\xc0\xaf')
	// 4-byte sequence with a first byte that promises fewer
	assert !utf8.validate_str('\xf0\x9f')
	// continuation after a complete sequence
	assert !utf8.validate_str('a\x80')
}

fn test_validate_agrees_with_validate_str() {
	// validate reads raw bytes, validate_str reads a string; both must agree
	assert utf8.validate(unsafe { nil }, 0) == utf8.validate_str('')
	for s in ['ok', '中文', 'ăâ êñ', 'broken \xff byte', 'truncated \xe2\x82'] {
		assert utf8.validate(unsafe { s.str }, s.len) == utf8.validate_str(s)
	}
	// an explicit length shorter than the buffer must be judged on that prefix alone
	s := 'ok\xffxy'
	assert !utf8.validate(unsafe { s.str }, s.len)
	assert utf8.validate(unsafe { s.str }, 2)
	assert utf8.validate_str('ok')
}

// NOTE: is_rune_punct and is_rune_global_punct currently share one table and one
// implementation body, so they agree on every input. This is the observed behaviour.
fn test_is_rune_global_punct_matches_is_rune_punct() {
	for r in [rune(`!`), `.`, `,`, `;`, `'`, `/`, `*`, `¿`, `।`, `⸘`] {
		assert utf8.is_rune_global_punct(r) == utf8.is_rune_punct(r)
	}
}

fn test_is_rune_global_punct_identifies_global_punctuation() {
	for r in [rune(`!`), `?`, `#`, `%`, `(`, `)`, `-`, `[`, `]`, `{`, `}`, `¿`, `¡`] {
		assert utf8.is_rune_global_punct(r)
	}
}

fn test_is_rune_global_punct_rejects_letters_and_digits() {
	for r in [rune(`a`), `Z`, `5`, `\n`, ` `, `\t`] {
		assert !utf8.is_rune_global_punct(r)
	}
}

// is_global_punct is is_rune_global_punct applied at a byte index
fn test_is_global_punct_agrees_with_the_rune_form() {
	for s in ['!x', '.x', 'あ!', 'x!', ''] {
		for i in 0 .. s.len {
			if utf8.get_rune(s, i) != rune(-1) {
				assert utf8.is_global_punct(s, i) == utf8.is_rune_global_punct(utf8.get_rune(s,
					i))
			}
		}
	}
}
