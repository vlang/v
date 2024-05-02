// Add more examples of potentially buggy patterns in vlib/v/parser/templates/index.html
fn test_tmpl_comptime() {
	index := $tmpl('templates/index.html').trim_space()
	// dump(index)
	assert index.contains('<br>Line ending with percent %\n')
	assert index.contains('<br>Line ending with at $\n')
	assert index.contains('<br>Line ending with ampersand &\n')
	assert index.contains('<br>Line ending with hash #\n')
	assert index.contains('<br>Line ending with slash /\n')
	assert index.contains('<br>Line ending with dollar $\n')
	assert index.contains('<br>Line ending with caret ^\n')
}
