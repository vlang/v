module xml

import strings

// pretty_str returns a pretty-printed version of the XML node. It requires the current indentation
// the node is at, the depth of the node in the tree, and a map of reverse entities to use when
// escaping text.
pub fn (node XMLNode) pretty_str(original_indent string, depth int, reverse_entities map[string]string) string {
	// Create the proper indentation first
	mut indent_builder := strings.new_builder(original_indent.len * depth)
	for _ in 0 .. depth {
		indent_builder.write_string(original_indent)
	}
	indent := indent_builder.str()

	// Now we can stringify the node
	mut builder := strings.new_builder(1024)
	builder.write_string(indent)
	builder.write_u8(`<`)
	builder.write_string(node.name)

	for key, value in node.attributes {
		builder.write_u8(` `)
		builder.write_string(key)
		builder.write_string('="')
		builder.write_string(value)
		builder.write_u8(`"`)
	}
	builder.write_string('>\n')
	for child in node.children {
		match child {
			string {
				builder.write_string(indent)
				builder.write_string(original_indent)
				builder.write_string(escape_text(content: child, reverse_entities: reverse_entities))
			}
			XMLNode {
				builder.write_string(child.pretty_str(original_indent, depth + 1, reverse_entities))
			}
			XMLComment {
				builder.write_string(indent)
				builder.write_string(original_indent)
				builder.write_string('<!--')
				builder.write_string(child.text)
				builder.write_string('-->')
			}
			XMLCData {
				builder.write_string(indent)
				builder.write_string(original_indent)
				builder.write_string('<![CDATA[')
				builder.write_string(child.text)
				builder.write_string(']]>')
			}
		}
		builder.write_u8(`\n`)
	}
	builder.write_string(indent)
	builder.write_string('</')
	builder.write_string(node.name)
	builder.write_u8(`>`)
	return builder.str()
}

fn (list []DTDListItem) pretty_str(indent string) string {
	if list.len == 0 {
		return ''
	}

	mut builder := strings.new_builder(1024)
	builder.write_u8(`[`)
	builder.write_u8(`\n`)

	for item in list {
		match item {
			DTDEntity {
				builder.write_string('${indent}<!ENTITY ${item.name} "${item.value}">')
			}
			DTDElement {
				builder.write_string('${indent}<!ELEMENT ${item.name} ${item.definition}>')
			}
		}
		builder.write_u8(`\n`)
	}
	builder.write_u8(`]`)
	return builder.str()
}

fn (doctype DocumentType) pretty_str(indent string) string {
	match doctype.dtd {
		string {
			content := doctype.dtd
			return if content.len > 0 {
				'<!DOCTYPE ${doctype.name} SYSTEM "${content}">'
			} else {
				''
			}
		}
		DocumentTypeDefinition {
			if doctype.dtd.list.len == 0 {
				return ''
			}

			mut builder := strings.new_builder(1024)
			builder.write_string('<!DOCTYPE ')
			builder.write_string(doctype.name)
			builder.write_string(' ')
			builder.write_string(doctype.dtd.list.pretty_str(indent))
			builder.write_string('>')
			builder.write_u8(`\n`)
			return builder.str()
		}
	}
}

// pretty_str returns a pretty-printed version of the XML document. It requires the string used to
// indent each level of the document.
pub fn (doc XMLDocument) pretty_str(indent string) string {
	mut document_builder := strings.new_builder(1024)

	prolog := '<?xml version="${doc.version}" encoding="${doc.encoding}"?>'
	comments := if doc.comments.len > 0 {
		mut comments_buffer := strings.new_builder(512)
		for comment in doc.comments {
			comments_buffer.write_string('<!--')
			comments_buffer.write_string(comment.text)
			comments_buffer.write_string('-->')
			comments_buffer.write_u8(`\n`)
		}
		comments_buffer.str()
	} else {
		''
	}

	document_builder.write_string(prolog)
	document_builder.write_u8(`\n`)
	document_builder.write_string(doc.doctype.pretty_str(indent))
	document_builder.write_u8(`\n`)
	document_builder.write_string(comments)
	document_builder.write_string(doc.root.pretty_str(indent, 0, doc.parsed_reverse_entities))

	return document_builder.str()
}

// str returns a string representation of the XML document. It uses a 2-space indentation
// to pretty-print the document.
pub fn (doc XMLDocument) str() string {
	return doc.pretty_str('  ')
}
