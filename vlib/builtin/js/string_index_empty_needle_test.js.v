fn test_index_empty_needle() {
	for s in ['abc', '', 'é🙂'] {
		assert s.index('')? == 0
		assert s.last_index('')? == s.len
	}
	assert 'abc'.index('missing') == none
	assert ''.index('a') == none
	assert ''.last_index('a') == none
}
