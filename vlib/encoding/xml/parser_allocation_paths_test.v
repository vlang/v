import encoding.xml

fn test_plain_attributes_and_text_preserve_owned_values() ! {
	mut source := '<r a="plain" empty="">plain<child x="value"/></r>'.clone()
	doc := xml.XMLDocument.from_string(source)!
	unsafe { source.free() }
	assert doc.root.name == 'r'
	assert doc.root.attributes == {
		'a':     'plain'
		'empty': ''
	}
	assert doc.root.children[0] as string == 'plain'
	assert (doc.root.children[1] as xml.XMLNode).attributes['x'] == 'value'
}

fn test_dtd_entity_overrides_do_not_modify_default_maps() ! {
	first := xml.XMLDocument.from_string('<!DOCTYPE r [<!ENTITY amp "overridden"><!ENTITY own "custom">]><r a="&amp;">&own;</r>')!
	assert first.root.attributes['a'] == 'overridden'
	assert first.root.children[0] as string == 'custom'
	assert xml.default_entities['amp'] == '&'
	second := xml.XMLDocument.from_string('<r a="&amp;">&amp;</r>')!
	assert second.root.attributes['a'] == '&'
	assert second.root.children[0] as string == '&'
	if _ := xml.XMLDocument.from_string('<r>&own;</r>') {
		assert false, 'custom DTD entity leaked into a subsequent document'
	}
}

fn test_node_name_scan_preserves_attribute_whitespace_and_unicode_values() ! {
	doc := xml.XMLDocument.from_string('<root\t a="one two"\r\n b="Привет 文 😀"><empty/><child></child></root>')!
	assert doc.root.name == 'root'
	assert doc.root.attributes == {
		'a': 'one two'
		'b': 'Привет 文 😀'
	}
	assert doc.root.children.len == 2
	assert (doc.root.children[0] as xml.XMLNode).name == 'empty'
	assert (doc.root.children[1] as xml.XMLNode).name == 'child'
}
