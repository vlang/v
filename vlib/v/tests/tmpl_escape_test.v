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
