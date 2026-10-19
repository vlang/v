module html

struct CommentTextCase {
	input    string
	expected string
}

fn test_text_after_comments() {
	cases := [
		CommentTextCase{'<div><p><!--[-->hello<!--]--></p></div>', 'hello'},
		CommentTextCase{'<div><p>a<!--[-->hello<!--]-->b</p></div>', 'ahellob'},
		CommentTextCase{'<div><!-- a comment -->text</div>', 'text'},
		CommentTextCase{'<div><p>x</p><!--[-->text<!--]--></div>', 'xtext'},
		CommentTextCase{'<div><p>a<!--\ncomment\n-->b</p></div>', 'ab'},
		CommentTextCase{'<div>a<!-- one -->b<!-- two -->c<span>d</span>e<!-- three -->f</div>', 'abcdef'},
		CommentTextCase{'<!-- before root --><div>text<!-- after text --></div>', 'text'},
		CommentTextCase{'<div><!-- one --><!-- two --><span>text</span><!-- three --></div>', 'text'},
	]
	for c in cases {
		doc := parse(c.input)
		assert doc.get_root().text() == c.expected, c.input
	}
}

fn test_comment_text_keeps_nested_markup() {
	doc := parse('<div>a<!-- marker -->b<span>c<!-- marker -->d</span>e<!-- marker -->f</div>')
	root := doc.get_root()
	assert root.text() == 'abcdef'
	assert root.content == 'ab<span>cd</span>ef'
	span := root.get_tag('span') or { panic('missing span') }
	assert span.text() == 'cd'
	assert span.content == 'cd'
	assert doc.get_tags(name: 'span').len == 1
}

fn test_empty_comments_do_not_create_text_nodes() {
	doc := parse('<!-- --><div><!-- --><span>text</span><!-- --></div>')
	assert doc.get_root().name == 'div'
	assert doc.get_root().text() == 'text'
	assert doc.get_tags(name: 'text').len == 0
}

fn test_comments_preserve_empty_element_named_text() {
	doc := parse('<div><text></text><!-- marker -->hello</div>')
	root := doc.get_root()
	assert root.text() == 'hello'
	assert root.children.len == 2
	assert root.children[0].name == 'text'
	assert root.children[0].content == ''
}

fn test_comment_text_split_at_each_byte() {
	input := '<div>a<!--[-->hello<!--]-->b<span>c<!--\ncomment\n-->d</span>e</div>'
	for boundary in 1 .. input.len {
		mut parser := Parser{}
		parser.split_parse(input[..boundary])
		parser.split_parse(input[boundary..])
		parser.finalize()
		doc := parser.get_dom()
		assert doc.get_root().text() == 'ahellobcde', 'boundary ${boundary}'
	}
	mut parser := Parser{}
	for byte in input.bytes() {
		parser.split_parse(byte.ascii_str())
	}
	parser.finalize()
	doc := parser.get_dom()
	assert doc.get_root().text() == 'ahellobcde'
}

fn test_comment_followed_by_text_at_end_of_input() {
	for input in ['<!-- comment -->text', '<!-- one --><!-- two -->text'] {
		doc := parse(input)
		assert doc.get_root().text() == 'text'
	}
}

fn test_comment_syntax_inside_script_remains_content() {
	script := 'const marker = "<!-- marker -->";'
	doc := parse('<div><script>${script}</script><!-- marker -->text</div>')
	root := doc.get_root()
	script_tag := root.get_tag('script') or { panic('missing script') }
	assert script_tag.content == script
	assert root.text() == script + 'text'
}
