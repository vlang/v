fn test_tmpl_with_single_quotes() {
	a := 'foo'
	b := 'bar'
	result := $tmpl('tmpl/include.txt')

	assert result.trim_space() == 'foo
bar'
}
