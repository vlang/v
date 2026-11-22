module fastc

fn test_source_text_search_respects_slices_nuls_and_start_offsets() {
	nul := []u8{len: 1}.bytestr()
	for text in ['', 'const', '_const constable const', 'fn f[T]() {}', 'a${nul}const${nul}b'] {
		for length in 0 .. text.len + 1 {
			bounded := text[..length]
			for needle in ['', 'const', 'fn', nul, 'b', 'missing'] {
				for start in 0 .. bounded.len + 2 {
					expected := if needle.len == 0 || start >= bounded.len {
						-1
					} else {
						bounded.index_after_(needle, start)
					}
					assert fastc_source_text_index(bounded, needle, start) == expected
				}
			}
		}
	}
}

fn test_source_word_search_keeps_keyword_boundaries() {
	for word in ['import', 'chan', 'const', 'interface', '__global', 'fn', 'type', 'select'] {
		for text in ['', 'pre${word}post', '${word}_ ${word}2', '// ${word}',
			'pre${word} ${word} post${word}', '${word} ${word}'] {
			mut expected := -1
			for offset in 0 .. text.len {
				if fastc_source_word_at(text, offset, word) { expected = offset }
			}
			assert fastc_source_last_word_index(text, word) == expected
			assert fastc_source_contains_word(text, word) == (expected >= 0)
		}
	}
}
