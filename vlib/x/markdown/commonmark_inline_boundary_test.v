// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

fn test_numeric_entities_replace_invalid_unicode_values() {
	for source in ['&#0;', '&#x0;', '&#55296;', '&#xDFFF;', '&#1114112;', '&#9999999;', '&#x110000;',
		'&#xFFFFFF;'] {
		assert to_html(source) == '<p>�</p>\n', source
	}
	assert to_html('&#1114111; &#x10FFFF;') == '<p>${rune(0x10ffff)} ${rune(0x10ffff)}</p>\n'
	assert to_html('&#0000065; &#x000041; &#X000041;') == '<p>A A A</p>\n'
}

fn test_numeric_entity_digit_limits_and_malformed_controls() {
	for source in ['&#00000065;', '&#11141110;', '&#x0000041;', '&#x1000000;', '&#;', '&#x;', '&#-1;',
		'&#+65;', '&#xG;', '&#12z;', '&# 65;', '&#65', '&#x41'] {
		assert to_html(source) == '<p>${source.replace('&', '&amp;')}</p>\n', source
	}
}

fn test_uri_autolinks_allow_empty_payload_and_validate_schemes() {
	for source in ['ab:', 'http:', 'a+.-:', 'abcdefghijklmnopqrstuvwxyzabcdef:'] {
		assert to_html('<${source}>') == '<p><a href="${source}">${source}</a></p>\n', source
	}
	for source in ['a:', 'abcdefghijklmnopqrstuvwxyzabcdefg:', '1a:', 'a_:', ':'] {
		assert to_html('<${source}>') == '<p>&lt;${source}&gt;</p>\n', source
	}
}

fn test_uri_autolinks_reject_ascii_controls() {
	for c in 0 .. 33 {
		assert try_autolink('<ab:${u8(c).ascii_str()}payload>') == -1, c.str()
	}
	assert try_autolink('<ab:${u8(127).ascii_str()}payload>') == -1
	for c in [u8(1), u8(11), u8(31), u8(127)] {
		control := c.ascii_str()
		assert to_html('<ab:${control}payload>') == '<p>&lt;ab:${control}payload&gt;</p>\n'
	}
	assert to_html('<ab:foo>') == '<p><a href="ab:foo">ab:foo</a></p>\n'
}
