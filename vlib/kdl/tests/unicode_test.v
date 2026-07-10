module main

import kdl

fn test_unicode_nbsp() {
	doc := kdl.parse('node\xc2\xa0"val"')!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_ideographic_space() {
	doc := kdl.parse('node\xe3\x80\x80"val"')!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_thin_space() {
	doc := kdl.parse('node\xe2\x80\x89"val"')!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_em_space() {
	doc := kdl.parse('node\xe2\x80\x83"val"')!
	assert doc.nodes[0].name == 'node'
}

fn test_unicode_bom() {
	doc := kdl.parse('\xef\xbb\xbfnode "val"')!
	assert doc.nodes[0].name == 'node'
}
