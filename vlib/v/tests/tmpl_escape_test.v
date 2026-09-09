fn test_escape() {
	world := 'world!'
	res := $tmpl('tmpl/escape.txt')

	assert res == 'Hello @world
Hello world!
Hello @ world! @
'
}

fn test_keeps_at_in_html_urls() {
	title := 'template ok'
	res := $tmpl('at_in_url_template.html')
	assert res.contains('https://cdn.jsdelivr.net/npm/jquery@3.6.0/dist/jquery.slim.min.js')
	assert res.contains('https://unpkg.com/htmx.org@1.5.0')
	assert res.contains('https://unpkg.com/hyperscript.org@0.8.1')
	assert res.contains('<div>template ok</div>')
}

fn test_escape_double_at_before_complex_at_expression() {
	res := $tmpl('tmpl/at_before_complex.txt')
	// @@ before ident(...) must yield the literal `@ident(...)`, not `@{ident(...)}`.
	// Regression for `@@get('/x')` being rewritten to `@{get('/x')}`.
	assert res.contains("@get('/x')")
	assert res.contains("@post('/save')")
	assert !res.contains('@{get(')
	assert !res.contains('@{post(')
}
