import encoding.html

fn test_unescape_all_unterminated_entities() {
	for input in ['&', 'plain &', '&foo', 'a&b', 'café &unknown', '&#', '&#x', '&unknown; &'] {
		assert html.unescape(input, all: true) == input
	}
	assert html.unescape('&amp; plain &', all: true) == '& plain &'
	assert html.unescape('&unknown; &lt; café &foo', all: true) == '&unknown; < café &foo'
	assert html.unescape('&amp', all: true) == '&'
}
