fn test_escape() {
	world := 'world!'
	res := $tmpl('tmpl/escape.txt')

	assert res == 'Hello @world
Hello world!
Hello $ world! $
'
}
