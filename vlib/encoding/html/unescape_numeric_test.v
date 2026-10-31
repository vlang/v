import encoding.html

fn test_unescape_numeric_unicode_scalars() {
	assert html.unescape('&#x1F600;&#X1f600;&#128512;', all: true) == '😀😀😀'
	assert html.unescape('&#65;&#x41;&#x9;&#10;', all: true) == 'AA\t\n'
	assert html.unescape('&#55295;&#xD7FF;&#57344;&#xE000;', all: true).runes() == [
		rune(0xd7ff),
		rune(0xd7ff),
		rune(0xe000),
		rune(0xe000),
	]
	assert html.unescape('&#1114111;&#x10FFFF;', all: true).runes() == [
		rune(0x10ffff),
		rune(0x10ffff),
	]
	assert html.unescape('&#0000000000000000000000000000128512;&#x000000000000000000000000000001f600;',
		all: true
	) == '😀😀'
}

fn test_unescape_numeric_replaces_invalid_scalars() {
	for input in ['&#0;', '&#x0;', '&#55296;', '&#xD800;', '&#57343;', '&#xDFFF;', '&#1114112;',
		'&#x110000;', '&#4294967296;', '&#x100000000;', '&#999999999999999999999999999999999999;',
		'&#xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;'] {
		assert html.unescape(input, all: true) == '�'
	}
	assert html.unescape('before &#0; &#x110000; after', all: true) == 'before � � after'
}

fn test_unescape_numeric_preserves_malformed_references() {
	for input in ['&#;', '&#x;', '&#X;', '&#-1;', '&#+65;', '&#xG;', '&#12q;',
		'&#xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFg;'] {
		assert html.unescape(input, all: true) == input
	}
	assert html.unescape('&#xG;&amp;&#65;', all: true) == '&#xG;&A'
}
