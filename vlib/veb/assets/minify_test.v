import veb.assets

fn test_minify_css_preserves_selector_whitespace() {
	assert assets.minify_css('div\n span {\n color: red;\n }\n') == 'div span { color: red; }'
	assert assets.minify_css('a{}\n\n b{}\n') == 'a{} b{}'
	assert assets.minify_css('\n\t \n') == ''
}

fn test_minify_css_preserves_string_line_continuations() {
	for newline in ['\n', '\r\n', '\r', '\f'] {
		for quote in ['"', "'"] {
			css := 'a::before {content: ${quote}hello\\${newline}world${quote};}'
			// Keeping the escape and newline preserves the string value "helloworld".
			assert assets.minify_css(css) == css
			indented := 'a::before {content: ${quote}hello\\${newline} world${quote};}'
			assert assets.minify_css(indented) == indented
		}
	}
	// Ordinary line breaks in the same stylesheet retain selector whitespace.
	css := 'div\n span {content: "hello\\\nworld";}\na {color: red;}'
	assert assets.minify_css('  ${css}\n ') == css
}

fn test_minify_js_has_no_trailing_whitespace() {
	assert assets.minify_js('  const x = 1;\n\nconst y = 2;\n') == 'const x = 1; const y = 2;'
	assert assets.minify_js('const x = 1;') == 'const x = 1;'
	assert assets.minify_js('\n\t \n') == ''
}
