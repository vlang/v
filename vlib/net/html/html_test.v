module html

fn test_parse() {
	doc := parse('<html><body><h1 class="title">Hello world!</h1></body></html>')
	tags := doc.get_tags(name: 'h1')
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

fn test_parse_inline_tags() {
	doc := parse('<html><body><p>before <span>in between</span> after</p></body></html>')
	tags := doc.get_tags(name: 'span')
	assert tags.len == 1

	span_tag := tags[0]
	assert span_tag.str() == '<span>in between</span>'

	p_tags := doc.get_tags(name: 'p')
	assert p_tags.len == 1

	p_tag := p_tags[0]
	assert p_tag.content == 'before <span>in between</span> after'
	assert p_tag.str() == '<p>before <span>in between</span> after</p>'

	assert p_tag.text() == 'before in between after'
}

fn test_parse_content_with_nested_tags() {
	doc := parse('<tr id="line"><td><a href="#">wom<nobr>All</a></td></tr>')
	td_tag := doc.get_tags(name: 'td')[0]
	a_tag := td_tag.get_tags('a')[0]
	assert td_tag.content == '<a href="#" >wom<nobr>All</nobr></a>'
	assert a_tag.content == 'wom<nobr>All</nobr>'
	assert td_tag.text() == 'womAll'
}
