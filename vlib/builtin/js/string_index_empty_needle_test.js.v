fn test_index_empty_needle() {
	for s in ['abc', '', 'é🙂'] {
		assert s.index('')? == 0
		assert s.last_index('')? == s.len
	}
	assert 'abc'.index('missing') == none
	assert ''.index('a') == none
	assert ''.last_index('a') == none
}

fn test_after_delimiters() {
	for s in ['abc', '', 'é🙂'] {
		assert s.after('') == s
		assert s.after('missing') == s
	}
	assert '23:34:45.234'.after(':') == '45.234'
	assert 'abc--def--ghi'.after('--') == 'ghi'
	assert 'α🙂β🙂γ'.after('🙂') == 'γ'
}
