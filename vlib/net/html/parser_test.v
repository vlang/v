module html

import strings

fn test_parse_empty_string() {
	mut parser := Parser{}

	parser.parse_html('')

	assert parser.tags.len == 0
}

fn test_parse_text() {
	mut parser := Parser{}
	text_content := 'test\nparse\ntext'

	parser.parse_html(text_content)

	assert parser.tags.len == 1
	assert parser.tags.first().text() == text_content
}

fn test_parse_one_tag_with_text() {
	mut parser := Parser{}
	text_content := 'tag\nwith\ntext'
	p_tag := '<p>${text_content}</p>'

	parser.parse_html(p_tag)

	assert parser.tags.first().text() == text_content
}

fn test_parse_html_reuse_starts_a_fresh_document() {
	mut parser := Parser{}
	first_input := '<div>'.repeat(6) + '</div>'.repeat(6)
	second_input := '<p>ok</p>'
	assert first_input.len == 66
	assert second_input.len == 9

	parser.parse_html(first_input)
	first_dom := parser.get_dom()
	assert first_dom.get_root().children.len == 1
	assert first_dom.source_bytes == first_input.len

	parser.parse_html(second_input)
	second_dom := parser.get_dom()
	second_root := second_dom.get_root()
	assert parser.tags.len == 2
	assert parser.tags[0].name == 'p'
	assert parser.tags[1].name == '/p'
	assert second_dom.parse_error() == none
	assert second_dom.source_bytes == second_input.len
	assert second_root.name == 'p'
	assert second_root.content == 'ok'
	assert second_root.children.len == 0
	assert second_root.str() == second_input
	assert second_dom.get_tags(name: 'div').len == 0
	assert second_dom.get_tags(name: 'p').len == 1
	mut normalized_content_bytes := i64(0)
	for tag in second_dom.all_tags {
		if tag.children.len > 0 {
			normalized_content_bytes += i64(tag.content.len)
		}
	}
	assert normalized_content_bytes <= i64(second_input.len) * html_max_content_expansion_ratio
}

fn test_parse_html_reuse_after_limit_error_resets_error_state() {
	mut parser := Parser{}
	depth := html_max_nesting_depth + 1
	parser.parse_html('<div>'.repeat(depth) + '</div>'.repeat(depth))
	assert parser.get_dom().parse_error() != none

	parser.parse_html_checked('<p>ok</p>') or { panic(err) }
	dom := parser.get_dom()
	assert dom.parse_error() == none
	assert dom.source_bytes == 9
	assert dom.get_root().name == 'p'
	assert dom.get_root().content == 'ok'
}

fn test_parse_html_reuse_preserves_custom_code_tags() {
	mut parser := Parser{}
	parser.add_code_tag('sample-code')
	first_input := '<sample-code>first <b>raw</sample-code>'
	parser.parse_html(first_input)
	first_root := parser.get_dom().get_root()
	assert first_root.content == 'first <b>raw'
	assert first_root.str() == first_input

	second_input := '<sample-code>second <i>raw</sample-code>'
	parser.parse_html(second_input)
	second_dom := parser.get_dom()
	second_root := second_dom.get_root()
	assert second_dom.parse_error() == none
	assert second_root.name == 'sample-code'
	assert second_root.children.len == 0
	assert second_root.content == 'second <i>raw'
	assert second_root.str() == second_input
}

fn test_split_parse() {
	mut parser := Parser{}
	parser.init()
	parser.split_parse('<!doctype htm')
	parser.split_parse('l public')
	parser.split_parse('><html><he')
	parser.split_parse('ad><t')
	parser.split_parse('itle> Hum... ')
	parser.split_parse('A Tit')
	parser.split_parse('\nle</ti\ntle>')
	parser.split_parse('</\nhead><body>\t\t\t<h3>')
	parser.split_parse('Nice Test!</h3>')
	parser.split_parse('</bo\n\n\ndy></html>')
	parser.finalize()
	assert parser.tags.len == 11
	assert parser.tags[3].content == ' Hum... A Tit\nle'
}

fn test_giant_string() {
	mut temp_html := strings.new_builder(200)
	mut parser := Parser{}
	temp_html.write_string('<!doctype html><html><head><title>Giant String</title></head><body>')
	for counter := 0; counter < 2000; counter++ {
		temp_html.write_string("<div id='name_${counter}' class='several-${counter}'>Look at ${counter}</div>")
	}
	temp_html.write_string('</body></html>')
	parser.parse_html(temp_html.str())
	assert parser.tags.len == 4009
}

fn test_script_tag() {
	mut parser := Parser{}
	script_content := '\nvar googletag = googletag || {};\ngoogletag.cmd = googletag.cmd || [];if(3 > 5) {console.log("Quoted \'message\'");}\n'
	temp_html := '<html><body><script>${script_content}</script></body></html>'
	parser.parse_html(temp_html)
	assert parser.tags[2].content.len == script_content.len
}

fn test_parse_script_and_style_keep_their_body() {
	for sample in ['<script>var x = 1;</script>', '<style>body { color: red; }</style>'] {
		mut parser := Parser{}
		parser.parse_html(sample)
		assert parser.tags.len == 1, sample
		tag := parser.tags.first()
		assert tag.content != '', sample
		assert tag.str() == sample, tag.str()
	}
	// the normal text path is unchanged
	mut parser := Parser{}
	parser.parse_html('<textarea>keep me</textarea>')
	assert parser.tags.first().content == 'keep me'
	assert parser.tags.first().str() == '<textarea>keep me</textarea>'
}

fn test_code_body_stays_separate_from_following_text() {
	for sample in ['<div><script>let x = 1;</script>after<p>end</p></div>',
		'<div><style>p { color: red; }</style>after</div>'] {
		mut dom := parse(sample)
		root := dom.get_root()
		assert root.str() == sample
		assert root.children[0].content.ends_with('after') == false
		assert root.children[1].name == 'text'
		assert root.children[1].content == 'after'
	}
	mut parser := Parser{}
	parser.split_parse('<script>a();</script>after')
	parser.finalize()
	assert parser.tags.len == 2
	assert parser.tags[0].content == 'a();'
	assert parser.tags[1].name == 'text'
	assert parser.tags[1].content == 'after'
}

fn test_split_code_body_is_preserved() {
	mut parser := Parser{}
	parser.split_parse('<script>a\n')
	parser.split_parse('b</scr')
	parser.split_parse('ipt><style>x</style>')
	parser.finalize()
	assert parser.tags.len == 2
	assert parser.tags[0].content == 'a\nb'
	assert parser.tags[1].content == 'x'
}

fn test_unterminated_code_body_is_preserved_at_eof() {
	for name in ['script', 'style'] {
		mut parser := Parser{}
		parser.parse_html('<${name}>raw <text>\nkept')
		assert parser.tags.len == 1
		assert parser.tags[0].content == 'raw <text>\nkept'
	}
	mut streaming := Parser{}
	streaming.split_parse('<script>first')
	streaming.split_parse(' second')
	dom := streaming.get_dom()
	assert dom.get_root().name == 'script'
	assert dom.get_root().content == 'first second'
}

fn test_parse_quoted_attribute_adds_no_empty_key() {
	// a quoted attribute must not leave an empty-key attribute behind, which
	// also corrupted str() round-trips with a stray space.
	// See https://github.com/vlang/v/issues/30139
	dom := parse('<div><span class="a b">x</span></div>')
	span := dom.get_tags(name: 'span')[0]
	assert span.attributes == {
		'class': 'a b'
	}
	assert span.str() == '<span class="a b">x</span>'
}

fn test_parse_unquoted_attribute_values() {
	// unquoted values belong to the preceding `name=`, they are not
	// attributes of their own.
	// See https://github.com/vlang/v/issues/30139
	dom := parse('<div><input type=text name=q></div>')
	input := dom.get_tags(name: 'input')[0]
	assert input.attributes == {
		'type': 'text'
		'name': 'q'
	}
}

fn test_parse_tag_split_across_lines() {
	// a newline inside a tag separates like a space: without this the tag
	// name glues to the first attribute (`divclass`).
	// See https://github.com/vlang/v/issues/30423
	dom := parse('<div>\n<span\nclass="x">t</span>\n</div>')
	span := dom.get_tags(name: 'span')[0]
	assert span.attributes['class'] == 'x'
	assert span.text() == 't'
}

fn test_parse_newline_before_quoted_attribute_value() {
	dom := parse('<span class=\n"x" id="y">t</span>')
	span := dom.get_tags(name: 'span')[0]
	assert span.attributes['class'] == 'x'
	assert span.attributes['id'] == 'y'
	assert span.text() == 't'
	assert '' !in span.attributes
}

fn test_parse_repeated_newlines_inside_tag() {
	dom := parse('<span\n\nclass="x"\n\nid="y">t</span>')
	span := dom.get_tags(name: 'span')[0]
	assert span.attributes['class'] == 'x'
	assert span.attributes['id'] == 'y'
	assert '' !in span.attributes
}

fn test_parse_trailing_text_after_last_tag() {
	// text after the last tag must survive parsing instead of being
	// dropped: it becomes a text node following the tags.
	// See https://github.com/vlang/v/issues/30161
	mut dom := parse('<p>a</p>trailing')
	mut found := false
	for t in dom.get_tags() {
		if t.name == 'text' && t.text_content == 'trailing' {
			found = true
		}
	}
	assert found, 'trailing text was dropped'
}

fn test_parse_unclosed_tag_keeps_text() {
	// text inside an element that is never closed belongs to it.
	// See https://github.com/vlang/v/issues/30161
	mut dom := parse('<div><p>unclosed')
	mut p := dom.get_tags(name: 'p')[0]
	assert p.text_content == 'unclosed'
}

fn test_parse_trailing_text_belongs_to_the_surrounding_element() {
	dom := parse('<div><p>a</p>trailing')
	p := dom.get_tags(name: 'p')[0]
	assert p.text() == 'a'
	assert p.children.len == 0
	texts := dom.get_tags(name: 'text')
	assert texts.len == 1
	assert texts[0].text() == 'trailing'
	assert texts[0].parent.name == 'div'
	assert dom.get_root().text() == 'atrailing'
}

fn test_parse_unquoted_values_preserve_case_and_class_lookup() {
	dom := parse('<div><span CLASS = Hero ID=Mixed\nDATA-X = A=B checked>t</span></div>')
	span := dom.get_tags(name: 'span')[0]
	assert span.attributes == {
		'class':   'Hero'
		'id':      'Mixed'
		'data-x':  'A=B'
		'checked': ''
	}
	assert dom.get_tags_by_class_name('Hero')[0].attributes['id'] == 'Mixed'
}

fn test_parse_unquoted_values_separated_by_html_whitespace() {
	for separator in [' ', '\t', '\n', '\r', '\f'] {
		dom := parse('<div><input value=Mixed${separator}name=Q></div>')
		input := dom.get_tags(name: 'input')[0]
		assert input.attributes['value'] == 'Mixed'
		assert input.attributes['name'] == 'Q'
		assert '' !in input.attributes
	}
}
