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

fn test_contains_rune() {
	assert contains_rune('héllo', `é`)
	assert !contains_rune('hello', `é`)
	assert contains_rune('abc', `b`)
	assert !contains_rune('', `a`)
	assert contains_rune('日本語', `本`)
}
