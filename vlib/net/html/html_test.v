module html

fn test_parse() {
	doc := parse('<html><body><h1 class="title">Hello world!</h1></body></html>')
	tags := doc.get_tag('h1')
	assert tags.len == 1
	h1_tag := tags[0] // <h1>Hello world!</h1>
	assert h1_tag.name == 'h1'
	assert h1_tag.content == 'Hello world!'
	assert h1_tag.attributes.len == 2
	// TODO: do not remove. Attributes must not have an empty attr.
	// assert h1_tag.attributes.len == 1
	assert h1_tag.str() == '<h1 class="title" >Hello world!</h1>'
	// assert h1_tag.str() == '<h1 class="title">Hello world!</h1>'
}
