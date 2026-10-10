import encoding.xml

// parse_error returns the message of the error that parsing `source` fails with.
fn parse_error(source string) string {
	xml.XMLDocument.from_string(source) or { return err.msg() }
	return 'parsed without an error'
}

fn attributes_of(source string) !map[string]string {
	return xml.XMLDocument.from_string(source)!.root.attributes
}

fn children_of(source string) ![]xml.XMLNodeContents {
	return xml.XMLDocument.from_string(source)!.root.children
}

fn test_attribute_without_a_value_is_an_error() {
	// It was merged into the key of the next attribute: {'disabled k': 'v'}
	assert parse_error('<a disabled k="v"></a>') == 'Attribute "disabled" has no value in attribute string: "disabled k="v""'
	assert parse_error('<a disabled k="v"/>') == 'Attribute "disabled" has no value in attribute string: "disabled k="v""'
	assert parse_error('<a k="v" disabled j="w"/>') == 'Attribute "disabled" has no value in attribute string: "k="v" disabled j="w""'
	assert parse_error('<a x y z="1"/>') == 'Attribute "x" has no value in attribute string: "x y z="1""'
	assert parse_error('<a x\ty="1"/>').starts_with('Attribute "x" has no value')
	assert parse_error('<a x\r\ny="1"/>').starts_with('Attribute "x" has no value')
	// After the last value, or alone, it was dropped.
	assert parse_error('<a k="v" disabled></a>') == 'Attribute "disabled" has no value in attribute string: "k="v" disabled"'
	assert parse_error('<a k="v" disabled/>') == 'Attribute "disabled" has no value in attribute string: "k="v" disabled"'
	assert parse_error('<a disabled></a>') == 'Attribute "disabled" has no value in attribute string: "disabled"'
	assert parse_error('<a disabled/>') == 'Attribute "disabled" has no value in attribute string: "disabled"'
	assert parse_error('<?xml version="1.0" standalone?><r/>').starts_with('Attribute "standalone" has no value')
}

fn test_whitespace_around_attributes() ! {
	expected := {
		'k': 'v'
		'j': 'w'
	}
	assert attributes_of('<a k="v" j="w"/>')! == expected
	assert attributes_of("<a k='v' j='w'></a>")! == expected
	assert attributes_of('<a k="v"  j="w" />')! == expected
	assert attributes_of('<a\tk="v"\n\tj=\'w\'/>')! == expected
	assert attributes_of('<a\r\n  k="v"\r\n  j="w"\r\n/>')! == expected
	assert attributes_of('<a\n  k="v"\n  j="w"\n>text</a>')! == expected
	assert attributes_of('<a k ="v" j\t="w"/>')! == expected
	assert attributes_of('<a k\n="v" j\r\n="w"/>')! == expected
	assert attributes_of('<a k="v"j="w"/>')! == expected
	assert attributes_of('<a k="a b  c" j=" pad " e="" eq="x=y"/>')! == {
		'k':  'a b  c'
		'j':  ' pad '
		'e':  ''
		'eq': 'x=y'
	}
	assert attributes_of('<a ns:k="v" k-2="w" k.3="x" _k="y"/>')! == {
		'ns:k': 'v'
		'k-2':  'w'
		'k.3':  'x'
		'_k':   'y'
	}
}

fn test_attribute_value_keeps_the_other_kind_of_quote() ! {
	// The value ended at the first quote of any kind, which left `s"` behind as a name without a value.
	assert attributes_of('<a k="it\'s" j=\'say "hi"\' l="x"/>')! == {
		'k': "it's"
		'j': 'say "hi"'
		'l': 'x'
	}
}

fn test_text_before_a_comment_or_cdata_keeps_the_document_order() ! {
	comment := xml.XMLNodeContents(xml.XMLComment{
		text: ' c '
	})
	cdata := xml.XMLNodeContents(xml.XMLCData{
		text: 'q'
	})
	x, y, z := xml.XMLNodeContents('x'), xml.XMLNodeContents('y'), xml.XMLNodeContents('z')
	node := xml.XMLNodeContents(xml.XMLNode{
		name: 'a'
	})

	// The text was added after the comment or CDATA, as one merged 'xy'.
	assert children_of('<r>x<!-- c -->y</r>')! == [x, comment, y]
	assert children_of('<r>x<![CDATA[q]]>y</r>')! == [x, cdata, y]
	assert children_of('<r>x<!-- c --></r>')! == [x, comment]
	assert children_of('<r>x<![CDATA[q]]></r>')! == [x, cdata]
	assert children_of('<r>x<!-- c -->y<![CDATA[q]]>z</r>')! == [x, comment, y, cdata, z]
	assert children_of('<r>x<a/>y<!-- c -->z</r>')! == [x, node, y, comment, z]
	assert children_of('<r>l1\r\nl2<!-- c -->l3\r\nl4</r>')! == [
		xml.XMLNodeContents('l1\nl2'),
		comment,
		xml.XMLNodeContents('l3\nl4'),
	]
	// Each text is trimmed, like the text around a child node.
	assert children_of('<r> x <!-- c --> y </r>')! == [x, comment, y]
	assert children_of('<r> x <a/> y </r>')! == [x, node, y]
}

fn test_whitespace_around_a_comment_or_cdata_is_not_a_child() ! {
	comment := xml.XMLNodeContents(xml.XMLComment{
		text: ' c '
	})
	cdata := xml.XMLNodeContents(xml.XMLCData{
		text: 'q'
	})
	node := xml.XMLNodeContents(xml.XMLNode{
		name: 'a'
	})
	assert children_of('<r>\n  <!-- c -->\n  <a/>\n</r>')! == [comment, node]
	assert children_of('<r>\n  <a/>\n  <!-- c -->\n</r>')! == [node, comment]
	assert children_of('<r>\r\n\t<![CDATA[q]]>\r\n\t<a/>\r\n</r>')! == [cdata, node]
	assert children_of('<r>\n  <!-- c -->\n  <!-- c -->\n  text\n  <!-- c -->\n</r>')! == [
		comment,
		comment,
		xml.XMLNodeContents('text'),
		comment,
	]
	assert children_of('<r>y<!-- c --></r>')! == children_of('<r>\n  y\n  <!-- c -->\n</r>')!
}

fn test_comments_before_the_root_without_an_xml_declaration() ! {
	// The comment was read as the tag of the root node.
	doc := xml.XMLDocument.from_string('<!-- hello --><r/>')!
	assert doc.comments == [xml.XMLComment{
		text: ' hello '
	}]
	assert doc.root == xml.XMLNode{
		name: 'r'
	}
	assert doc.version == '1.0'
	assert doc.encoding == 'UTF-8'

	// The document has the same structure with and without the declaration.
	declaration := '<?xml version="1.0" encoding="UTF-8"?>'
	for rest in [
		'<!-- hello --><r/>',
		'<!-- a --><!-- b --><r k="v">text</r>',
		'\n<!-- a -->\r\n\t<!-- b -->\n<r>\n  <!-- c -->\n  <a/>\n</r>\n',
	] {
		assert xml.XMLDocument.from_string(rest)! == xml.XMLDocument.from_string(declaration + rest)!
	}
	two := xml.XMLDocument.from_string('\xEF\xBB\xBF<!-- a -->\n<!-- b -->\n<r/>')!
	assert two.comments.map(it.text) == [' a ', ' b ']
}

fn test_doctype_before_the_root_without_an_xml_declaration() ! {
	declaration := '<?xml version="1.0" encoding="UTF-8"?>'
	for rest in [
		'<!DOCTYPE note [<!ELEMENT note (#PCDATA)>]><note/>',
		'<!-- a -->\n<!DOCTYPE note [\n  <!ENTITY w "x">\n]>\n<!-- b -->\n<note>text</note>',
	] {
		doc := xml.XMLDocument.from_string(rest)!
		assert doc.doctype.name == 'note'
		assert doc.root.name == 'note'
		assert doc == xml.XMLDocument.from_string(declaration + rest)!
	}
	// The errors are the ones that the same prolog has after a declaration.
	for rest in [
		'<!DOCTYPE a [<!ELEMENT a (#PCDATA)>]><!DOCTYPE a [<!ELEMENT a (#PCDATA)>]><a/>',
		'<!x><r/>',
		'<!-x --><r/>',
		'<!DOCTYPX a><r/>',
	] {
		assert parse_error(rest) != ''
		assert parse_error(rest) == parse_error(declaration + rest)
	}
}

fn test_unterminated_document_error_has_a_message() {
	// All of these returned an `io.Eof`, which has no message.
	assert parse_error('<r>') == 'XML node <r> not closed.'
	assert parse_error('<r>text') == 'XML node <r> not closed.'
	assert parse_error('<r><a>') == 'XML node <a> not closed.'
	assert parse_error('<r><a></a>') == 'XML node <r> not closed.'
	assert parse_error('<r><a/>') == 'XML node <r> not closed.'
	assert parse_error('<r></') == 'XML node <r> not closed.'
	assert parse_error('<r><!') == 'XML node <r> not closed.'
	assert parse_error('<?xml version="1.0"?><r>') == 'XML node <r> not closed.'
	assert parse_error('<r') == 'XML tag not closed. Expected ">".'
	assert parse_error('<r k="v"') == 'XML tag not closed. Expected ">".'
	assert parse_error('<r><a') == 'XML tag not closed. Expected ">".'
	assert parse_error('<r><!-- c') == 'XML Comment not closed.'
	assert parse_error('<r><!-- c --') == 'XML Comment not closed.'
	assert parse_error('<r><![CDATA[x') == 'CDATA section not closed.'
	assert parse_error('<r><![CDATA[x]]') == 'CDATA section not closed.'
	assert parse_error('<!-- hello') == 'XML Comment not closed.'
	assert parse_error('<?xml version="1.0"?><!-- hello') == 'XML Comment not closed.'
	assert parse_error('<!DOCTYPE a [') == 'DOCTYPE declaration not closed.'
	assert parse_error('<?xml version="1.0"?><!DOCTYPE a [') == 'DOCTYPE declaration not closed.'
	// A document without a root node keeps the message that it had.
	assert parse_error('') == 'XML document is empty.'
	assert parse_error('<?xml version="1.0"?>\n') == 'XML document is empty.'
	assert parse_error('<!-- hello -->') == 'XML document is empty.'
}

fn test_every_truncated_document_has_an_error_message() {
	body := '<!-- a -->\n<!DOCTYPE r [\n  <!ELEMENT r (e)>\n  <!ENTITY w "x">\n]>\n<r k="v" j=\'w\'>\n  text\n  <!-- c -->\n  <![CDATA[d]]>\n  <e k="v"/>\n  <e>&w;</e>\n</r>'
	for document in [body, '<?xml version="1.0" encoding="UTF-8"?>\n' + body, '\xEF\xBB\xBF' + body] {
		xml.XMLDocument.from_string(document) or {
			assert false, 'the complete document has to parse: ${err.msg()}'
			return
		}
		for end in 0 .. document.len {
			truncated := document[..end]
			if _ := xml.XMLDocument.from_string(truncated) {
				assert false, 'parsed a truncated document: `${truncated}`'
			} else {
				assert err.msg() != '', 'no error message for: `${truncated}`'
			}
		}
	}
}

fn test_processing_instruction_is_reported_as_unsupported() {
	// In content it was read as a node named `?pi`, which gave `Invalid XML. Incomplete node end.`
	unsupported := 'XML processing instructions are not supported.'
	assert parse_error('<r><?pi x?></r>') == unsupported
	assert parse_error('<r>a<?pi?>b</r>') == unsupported
	assert parse_error('<r><a/><?pi k="v"?></r>') == unsupported
	// Before the root node it gave an error without a message.
	assert parse_error('<?xml version="1.0"?><?pi x?><r/>') == unsupported
	assert parse_error('<?xml version="1.0"?><?xml-stylesheet type="text/xsl" href="s.xsl"?><r/>') == unsupported
}

fn test_empty_element_name_is_an_error() {
	missing := 'XML node is missing name.'
	// It was accepted as a node named `><`.
	assert parse_error('<></>') == missing
	assert parse_error('<>x</>') == missing
	assert parse_error('<r><></></r>') == missing
	assert parse_error('<>') == missing
	// It was accepted as a node with an empty name.
	assert parse_error('</>') == missing
	assert parse_error('< />') == missing
	assert parse_error('<r>< /></r>') == missing
	// It was a panic: `array.get: index out of range`.
	assert parse_error('< >') == missing
	assert parse_error('<r>< ></ ></r>') == missing
	assert parse_error('<\n>') == missing
}
