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
