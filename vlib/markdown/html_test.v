module markdown

fn test_to_html() {
	text := '# Hello World!'
	result := to_html(text)
	assert result == '<h1>Hello World!</h1>'
}