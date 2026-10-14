module xml

const sample_doc = '
<root>
	<c id="c1"/>
	<c id="c2">
		Sample Text
	</c>
	<empty/>
	<c id="c3"/>
	<abc id="c4"/>
	<xyz id="c5"/>
	<c id="c6"/>
	<cx id="c7"/>
	<cd id="c8"/>
	<child id="c9">
		More Sample Text
	</child>
	<cz id="c10"/>
</root>'

const xml_elements = [
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c1'
		}
	},
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c2'
		}
		children:   [
			'Sample Text',
		]
	},
	XMLNode{
		name:       'empty'
		attributes: {}
	},
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c3'
		}
	},
	XMLNode{
		name:       'abc'
		attributes: {
			'id': 'c4'
		}
	},
	XMLNode{
		name:       'xyz'
		attributes: {
			'id': 'c5'
		}
	},
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c6'
		}
	},
	XMLNode{
		name:       'cx'
		attributes: {
			'id': 'c7'
		}
	},
	XMLNode{
		name:       'cd'
		attributes: {
			'id': 'c8'
		}
	},
	XMLNode{
		name:       'child'
		attributes: {
			'id': 'c9'
		}
		children:   [
			'More Sample Text',
		]
	},
	XMLNode{
		name:       'cz'
		attributes: {
			'id': 'c10'
		}
	},
]

fn test_single_element_parsing() ! {
	mut reader := FullBufferReader{
		contents: sample_doc.bytes()
	}
	// Skip the "<root>" tag
	mut skip := []u8{len: 6}
	reader.read(mut skip)!

	mut local_buf := [u8(0)]
	mut ch := next_char(mut reader, mut local_buf)!

	mut count := 0

	for count < xml_elements.len {
		match ch {
			`<` {
				next_ch := next_char(mut reader, mut local_buf)!
				match next_ch {
					`/` {}
					else {
						parsed_element := parse_single_node(next_ch, mut reader)!
						assert xml_elements[count] == parsed_element
						count++
					}
				}

				ch = next_char(mut reader, mut local_buf)!
			}
			else {
				for ch != `<` {
					ch = next_char(mut reader, mut local_buf)!
				}
			}
		}
	}
}

fn test_parser_decodes_entities_in_text_and_attributes() {
	doc := XMLDocument.from_string('<r a="&lt;&gt;&amp;&quot;&apos;">&lt;&gt;&amp;&quot;&apos;</r>')!
	assert doc.root.attributes['a'] == '<>&"\''
	assert doc.root.children == [XMLNodeContents('<>&"\'')]
	assert XMLDocument.from_string(doc.str())!.root == doc.root
}

fn test_parser_decodes_numeric_character_references() {
	doc := XMLDocument.from_string('<r a="&#65;&#x42;&#x1F600;">&#65;&#x42;&#128512;&#32;</r>')!
	assert doc.root.attributes['a'] == 'AB😀'
	assert doc.root.children == [XMLNodeContents('AB😀 ')]
	space := XMLDocument.from_string('<r>&#32;<c/>&#x20;</r>')!
	assert space.root.children == [XMLNodeContents(' '), XMLNode{ name: 'c' }, ' ']
}

fn test_parser_rejects_invalid_entity_references() {
	for reference in ['&unknown;', '&amp', '&#0;', '&#xD800;', '&#xFFFE;', '&#x110000;',
		'&#999999999999999999999;', '&#;', '&#x;', '&#-1;', '&#xG;', '&#1;'] {
		for input in ['<r>${reference}</r>', '<r a="${reference}"/>'] {
			if doc := XMLDocument.from_string(input) {
				assert false, 'accepted ${input}: ${doc}'
			} else {
				assert err.msg().len > 0
			}
		}
	}
}

fn test_parser_preserves_cdata_and_decodes_separate_text_runs() {
	doc := XMLDocument.from_string('<r>a&amp;<!--c-->b&lt;<![CDATA[&amp;]]>c&gt;</r>')!
	assert doc.root.children == [XMLNodeContents('a&'), XMLComment{ text: 'c' }, 'b<',
		XMLCData{ text: '&amp;' }, 'c>']
}

fn test_parser_preserves_whitespace_inside_text_runs_split_by_comments_and_cdata() {
	for input in ['<r>a <!--c-->b</r>', '<r>a<!--c--> b</r>', '<r>a <!--c--> b</r>',
		'<r>a<!--c--> <!--d-->b</r>', '<r> a <!--c--> b </r>'] {
		doc := XMLDocument.from_string(input)!
		mut text := ''
		for child in doc.root.children {
			if child is string {
				text += child
			}
		}
		assert text == if input.contains('a <!--c--> b') { 'a  b' } else { 'a b' }
		assert XMLDocument.from_string(doc.str())!.root == doc.root
		assert XMLDocument.from_string(doc.pretty_str('\t'))!.root == doc.root
	}
	for input in ['<r>a <![CDATA[b]]> c</r>', '<r>a <!--c--><![CDATA[b]]> c</r>',
		'<r>a <![CDATA[]]><!--c--> b</r>'] {
		doc := XMLDocument.from_string(input)!
		assert doc.root.children[0] == XMLNodeContents('a ')
		assert doc.root.children.last() == XMLNodeContents(' c')
			|| doc.root.children.last() == XMLNodeContents(' b')
		assert XMLDocument.from_string(doc.str())!.root == doc.root
		assert XMLDocument.from_string(doc.pretty_str('\t'))!.root == doc.root
	}
}

fn test_parser_still_trims_text_run_edges_and_formatting_whitespace() {
	for input in ['<r> \n<!--c--> \nhello \n<!--d--> \n</r>', '<r><!--c--> hello <!--d--></r>'] {
		doc := XMLDocument.from_string(input)!
		assert doc.root.children == [XMLNodeContents(XMLComment{ text: 'c' }), 'hello',
			XMLComment{ text: 'd' }]
	}
	doc := XMLDocument.from_string('<r> \n<!--c--> \n<c/> \n<![CDATA[x]]> \n</r>')!
	assert doc.root.children == [XMLNodeContents(XMLComment{ text: 'c' }), XMLNode{ name: 'c' },
		XMLCData{ text: 'x' }]
}

fn test_serialization_preserves_numeric_whitespace_in_text() {
	for input in ['<r>&#32;<c/>&#x20;</r>', '<r>&#32;hello&#32;</r>', '<r>&#9;&#10;&#13;</r>',
		'<r>&#x9;&#xA;&#xD;hello&#xD;&#xA;&#x9;</r>', '<r>a&#13;&#10;b</r>',
		'<r>&#32;<!--c-->&#9;<![CDATA[x]]>&#10;&#13;</r>',
		'<r a="&#9;&#10;&#13;">&#32;&amp;&lt;😀&#32;</r>'] {
		doc := XMLDocument.from_string(input)!
		for rendered in [doc.str(), doc.pretty_str('\t'), doc.root.pretty_str('  ', 0,
			default_entities_reverse)] {
			reparsed := XMLDocument.from_string(rendered)!
			assert reparsed.root == doc.root
			assert XMLDocument.from_string(reparsed.str())!.root == doc.root
		}
	}
}

fn test_validation_does_not_decode_entities_twice() {
	doc := XMLDocument.from_string('<?xml version="1.0"?><!DOCTYPE r [<!ELEMENT r (#PCDATA)>]><r>&amp;amp;</r>')!
	assert doc.root.children == [XMLNodeContents('&amp;')]
	assert doc.validate()!.root == doc.root
}

fn test_parser_decodes_declared_entities() {
	doc := XMLDocument.from_string('<?xml version="1.0"?><!DOCTYPE r [<!ENTITY warning "hello">]><r a="&warning;">&warning;</r>')!
	assert doc.root.attributes['a'] == 'hello'
	assert doc.root.children == [XMLNodeContents('hello')]
	assert doc.validate()!.root == doc.root
}

fn test_doctype_external_identifiers() {
	for prefix in ['', '<?xml version="1.0"?>'] {
		for identifier in ['SYSTEM "root.dtd"', "SYSTEM 'root.dtd'",
			'PUBLIC "-//Example//DTD Root//EN" "root.dtd"', 'SYSTEM "a>b[1].dtd"', 'SYSTEM ""',
			'PUBLIC "" ""'] {
			doc := XMLDocument.from_string('${prefix}<!DOCTYPE root ${identifier}><root/>')!
			assert doc.root.name == 'root'
			assert doc.doctype.name == 'root'
			assert doc.doctype.external_id == identifier
			assert doc.str().contains('<!DOCTYPE root ${identifier}>')
			assert XMLDocument.from_string(doc.str())!.doctype == doc.doctype
		}
	}
}

fn test_doctype_external_identifier_with_internal_subset() {
	doc := XMLDocument.from_string('<!DOCTYPE root SYSTEM "root.dtd" [<!ELEMENT root (child)>]><root><child/></root>')!
	assert doc.doctype.name == 'root'
	assert doc.doctype.external_id == 'SYSTEM "root.dtd"'
	if doc.doctype.dtd is DocumentTypeDefinition {
		assert doc.doctype.dtd.list.len == 1
	} else {
		assert false
	}
	assert doc.validate()!.root == doc.root
	assert doc.str().contains('SYSTEM "root.dtd" [')
	assert XMLDocument.from_string(doc.str())!.doctype == doc.doctype
}

fn test_doctype_malformed_external_identifiers() {
	for input in ['<!DOCTYPE root SYSTEM><root/>', '<!DOCTYPE root SYSTEM root.dtd><root/>',
		'<!DOCTYPE root PUBLIC "id"><root/>', '<!DOCTYPE root PUBLIC "id""root.dtd"><root/>',
		'<!DOCTYPE root SYSTEM "root.dtd" extra><root/>', '<!DOCTYPE root SYSTEM "root.dtd',
		'<!DOCTYPE root SYSTEM "root.dtd"', '<!DOCTYPE root OTHER "root.dtd"><root/>'] {
		if doc := XMLDocument.from_string(input) {
			assert false, 'accepted ${input}: ${doc}'
		} else {
			assert err.msg().len > 0
			assert err.msg().contains('DOCTYPE')
		}
	}
}

fn test_duplicate_attributes_are_rejected() {
	for input in ['<r a="1" a="2"/>', '<r a="1" a="2"></r>', '<r><child a="1" a="1"/></r>',
		'<?xml version="1.0" version="1.0"?><r/>', '<r x:a="1" x:a="2"/>'] {
		if doc := XMLDocument.from_string(input) {
			assert false, 'accepted duplicate attribute: ${doc}'
		} else {
			assert err.msg().starts_with('Duplicate XML attribute: ')
		}
	}
}

fn test_attributes_remain_case_sensitive() {
	doc := XMLDocument.from_string('<r a="1" A="2" x:a="3"/>')!
	assert doc.root.attributes == {
		'a':   '1'
		'A':   '2'
		'x:a': '3'
	}
}

fn test_doctype_entities_and_significant_whitespace_roundtrip_together() {
	doc := XMLDocument.from_string('<!DOCTYPE r SYSTEM "r.dtd" [<!ENTITY sep " ">]><r a="&amp;">&sep;a <!--c-->&#32;<c/>&#9;&#10;&#13;</r>')!
	assert doc.doctype.external_id == 'SYSTEM "r.dtd"'
	assert doc.root.attributes['a'] == '&'
	assert doc.root.children == [XMLNodeContents(' a '), XMLComment{ text: 'c' }, ' ',
		XMLNode{ name: 'c' }, '\t\n\r']
	for rendered in [doc.str(), doc.pretty_str('\t')] {
		reparsed := XMLDocument.from_string(rendered)!
		assert reparsed.doctype == doc.doctype
		assert reparsed.root == doc.root
	}
}
