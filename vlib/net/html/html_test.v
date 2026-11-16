module html

import os
import strings

fn nested_element_markup(name string, depth int, content string) string {
	mut markup := strings.new_builder(depth * (name.len * 2 + 5) + content.len)
	for _ in 0 .. depth {
		markup.write_string('<${name}>')
	}
	markup.write_string(content)
	for _ in 0 .. depth {
		markup.write_string('</${name}>')
	}
	return markup.str()
}

fn nested_markup(depth int, content string) string {
	return nested_element_markup('x', depth, content)
}

fn test_parse() {
	doc := parse('<html><body><h1 class="title">Hello world!</h1></body></html>')
	tags := doc.get_tags(name: 'h1')
	assert tags.len == 1
	h1_tag := tags[0] // <h1>Hello world!</h1>
	assert h1_tag.name == 'h1'
	assert h1_tag.content == 'Hello world!'
	assert h1_tag.attributes.len == 1
	assert h1_tag.str() == '<h1 class="title">Hello world!</h1>'
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
	assert td_tag.content == '<a href="#">wom<nobr>All</nobr></a>'
	assert a_tag.content == 'wom<nobr>All</nobr>'
	assert td_tag.text() == 'womAll'
}

fn test_synthetic_text_and_real_text_elements_keep_distinct_semantics() {
	synthetic := parse('plain text')
	assert synthetic.get_root().name == 'text'
	assert synthetic.get_root().text() == 'plain text'
	assert synthetic.get_root().str() == 'plain text'

	actual_markup := nested_element_markup('text', 256, 'body')
	actual := parse_checked(actual_markup) or { panic(err) }
	assert actual.get_root().str() == actual_markup
	assert actual.get_root().text() == 'body'
	too_deep_text := nested_element_markup('text', 257, '')
	if _ := parse_checked(too_deep_text) {
		assert false, 'actual text elements must count toward the nesting limit'
	} else {
		assert err.msg().contains('nesting')
	}
}

fn test_parser_text_nodes_do_not_become_containers_when_text_closes_exist() {
	markup := '<div>' + '<span>x</span>tail'.repeat(300) + '</text></div>'
	dom := parse_checked(markup) or {
		panic(err)
	}
	root := dom.get_root()
	text_nodes := root.get_tags('text')
	assert root.children.len == 600
	assert text_nodes.len == 300
	assert root.get_tags('span').len == 300
	for text_node in text_nodes {
		assert text_node.is_text_node
		assert text_node.children.len == 0
		assert text_node.parent == root
	}
}

fn test_parse_nesting_limit_and_atomic_fallbacks() {
	valid := parse_checked(nested_markup(256, '')) or { panic(err) }
	assert valid.get_tags().len == 256
	assert valid.parse_error() == none
	doctype_valid := parse_checked('<!doctype html>' + nested_markup(256, '')) or { panic(err) }
	assert doctype_valid.get_tags(name: 'x').len == 256

	too_deep := nested_markup(257, '')
	if _ := parse_checked(too_deep) {
		assert false, 'parse_checked must reject depth 257'
	} else {
		assert err.msg().contains('nesting')
	}
	legacy := parse(too_deep)
	assert legacy.parse_error() != none
	assert legacy.get_tags().len == 0
	assert legacy.get_root().name == ''

	mut checked_parser := Parser{}
	if _ := checked_parser.parse_html_checked(too_deep) {
		assert false, 'parse_html_checked must reject depth 257'
	} else {
		assert err.msg().contains('nesting')
	}
	assert checked_parser.get_dom().get_tags().len == 0

	mut legacy_parser := Parser{}
	legacy_parser.parse_html(too_deep)
	legacy_parser_dom := legacy_parser.get_dom()
	assert legacy_parser_dom.parse_error() != none
	assert legacy_parser_dom.get_tags().len == 0

	mut split_parser := Parser{}
	split_parser.split_parse(too_deep)
	split_dom := split_parser.get_dom()
	assert split_dom.parse_error() != none
	assert split_dom.get_tags().len == 0

	mut siblings := strings.new_builder(20_000)
	siblings.write_string('<div>')
	for _ in 0 .. 1024 {
		siblings.write_string('<span></span>')
	}
	siblings.write_string('</div>')
	sibling_dom := parse_checked(siblings.str()) or { panic(err) }
	assert sibling_dom.parse_error() == none
	assert sibling_dom.get_tags(name: 'span').len == 1024

	wide := parse_checked('<div>' + '<i></i>'.repeat(32_768) + '</div>') or { panic(err) }
	assert wide.get_root().content.len == '<i></i>'.len * 32_768
	assert wide.get_tags(name: 'i').len == 32_768

	code_body := nested_markup(257, '')
	for name in ['script', 'style'] {
		raw_dom := parse_checked('<${name}>${code_body}</${name}>') or { panic(err) }
		assert raw_dom.parse_error() == none
		assert raw_dom.get_root().name == name
	}
}

fn test_parse_normalized_content_budget_preflights_expansion() {
	large_nested := nested_markup(256, 'x'.repeat(512 * 1024))
	assert large_nested.len < 1024 * 1024
	if _ := parse_checked(large_nested) {
		assert false, 'nested content expansion must be rejected'
	} else {
		assert err.msg().contains('normalized content')
	}
	large_text_nested := nested_element_markup('text', 256, 'x'.repeat(512 * 1024))
	if _ := parse_checked(large_text_nested) {
		assert false, 'nested text elements must be included in the content budget'
	} else {
		assert err.msg().contains('normalized content')
	}
	legacy := parse(large_nested)
	assert legacy.parse_error() != none
	assert legacy.get_tags().len == 0

	flat_payload := 'x'.repeat(1024 * 1024)
	flat := parse_checked('<div>${flat_payload}</div>') or { panic(err) }
	assert flat.parse_error() == none
	assert flat.get_root().content.len == flat_payload.len
}

fn test_parse_file_checked_and_legacy_error_are_visible() {
	path := os.join_path(os.temp_dir(), 'html_depth_limit_${os.getpid()}.html')
	os.write_file(path, nested_markup(257, '')) or { panic(err) }
	if _ := parse_file_checked(path) {
		assert false, 'parse_file_checked must reject depth 257'
	} else {
		assert err.msg().contains('nesting')
	}
	legacy := parse_file(path)
	assert legacy.parse_error() != none
	assert legacy.get_tags().len == 0
	os.rm(path) or {}
}
