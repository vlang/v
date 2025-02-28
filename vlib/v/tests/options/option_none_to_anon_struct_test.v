fn opt(params ?struct { name string surname string }) {
	if params == none {
		assert '${params}' == 'Option(none)'
	}
	if params != none {
		assert '${params}' == "struct {
    name: 'foo'
    surname: 'bar'
}"
	}
}

fn test_main() {
	opt(none)
	opt(struct { name: 'foo', surname: 'bar' })
}
