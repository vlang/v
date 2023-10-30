module xml

import strings

// pretty_str returns a pretty-printed version of the XML node. It requires the current indentation
// the node is at, the depth of the node in the tree, and a map of reverse entities to use when
// escaping text.
pub fn (node XMLNode) pretty_str(original_indent string, depth int, reverse_entities map[string]string) string {
	mut builder := strings.new_builder(1024)
	indent := original_indent.repeat(depth)
	builder.write_string('${indent}<${node.name}')
	for key, value in node.attributes {
		builder.write_string(' ${key}="${value}"')
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
	builder.write_string('${indent}</${node.name}>')
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
	prolog := '<?xml version="${doc.version}" encoding="${doc.encoding}"?>'
	comments := if doc.comments.len > 0 {
		mut buffer := strings.new_builder(512)
		for comment in doc.comments {
			buffer.write_string('<!--')
			buffer.write_string(comment.text)
			buffer.write_string('-->')
			buffer.write_u8(`\n`)
		}
		buffer.str()
	} else {
		''
	}
	return '${prolog}\n${doc.doctype.pretty_str(indent)}${comments}${doc.root.pretty_str(indent,
		0, doc.parsed_reverse_entities)}'
}

// str returns a string representation of the XML document. It uses a 2-space indentation
// to pretty-print the document.
pub fn (doc XMLDocument) str() string {
	return doc.pretty_str('  ')
}
