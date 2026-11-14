module strings

fn test_split_after() {
	assert split_after('a.b.c', '.') == ['a.', 'b.', 'c']
	assert split_after('a.b.', '.') == ['a.', 'b.', ''], 'trailing separator yields a final empty piece, like Go'
	assert split_after('abc', '.') == ['abc']
	assert split_after('', '.') == ['']
	assert split_after('a::b::c', '::') == ['a::', 'b::', 'c']
	assert split_after('a-b', 'xyz') == ['a-b']
	assert split_after('abc', '') == ['abc']
	assert split_after('...', '.') == ['.', '.', '.', '']
}

fn test_equal_fold() {
	assert equal_fold('Go', 'GO')
	assert equal_fold('go', 'GO')
	assert equal_fold('', '')
	assert equal_fold('Abc123!', 'aBC123!')
	assert !equal_fold('Go', 'Go!')
	assert !equal_fold('abc', 'abd')
	assert !equal_fold('Ä', 'ä'), 'non-ASCII is not folded'
	assert equal_fold('Ä', 'Ä')
}

fn test_last_index_any() {
	assert last_index_any('abcabc', 'bc') == 5
	assert last_index_any('hello', 'lo') == 4
	assert last_index_any('hello', 'z') == -1
	assert last_index_any('', 'a') == -1
	assert last_index_any('abc', '') == -1
	assert last_index_any('aaa', 'a') == 2
	assert last_index_any('a.b.c', '.') == 3
}

fn test_last_index_any_all_byte_values() {
	mut bytes := []u8{len: 256}
	for i in 0 .. bytes.len {
		bytes[i] = u8(i)
	}
	source := bytes.bytestr()
	for count in [1, 2, 4, 5, 8, 16, 31, 64, 256] {
		assert last_index_any(source, bytes[..count].bytestr()) == count - 1
		assert last_index_any(source, bytes[256 - count..].bytestr()) == 255
	}
	assert last_index_any(bytes[..64].bytestr(), bytes[64..128].bytestr()) == -1
	assert last_index_any('abcabc', 'zzzzy') == -1
	assert last_index_any('abcabc', 'zzzcb') == 5
}

fn test_contains_rune() {
	assert contains_rune('héllo', `é`)
	assert !contains_rune('hello', `é`)
	assert contains_rune('abc', `b`)
	assert !contains_rune('', `a`)
	assert contains_rune('日本語', `本`)
}

fn test_text_ops_boundaries_and_unicode_contracts() {
	assert split_after('é::文::', '::') == ['é::', '文::', '']
	assert split_after('', '') == ['']
	assert split_after('aba', 'aba') == ['aba', '']
	assert last_index_any('aéz', 'é') == 2
	assert last_index_any('é', 'ø') == 0, 'this API intentionally searches bytes'
	assert contains_rune('x😀y', `😀`)
	assert !contains_rune('é', `ø`)
	assert equal_fold('Vé', 'vé')
	assert !equal_fold('VÉ', 'vé')
}
