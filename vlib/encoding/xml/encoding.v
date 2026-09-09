module xml

import strings

fn write_indent(mut builder strings.Builder, indent string, depth int) {
	for _ in 0 .. depth {
		builder.write_string(indent)
	}
}

fn write_pretty_xml_node(mut builder strings.Builder, node XMLNode, original_indent string, depth int, reverse_entities map[string]string) {
	write_indent(mut builder, original_indent, depth)
	builder.write_u8(`<`)
	builder.write_string(node.name)

	for key, value in node.attributes {
		builder.write_u8(` `)
		builder.write_string(key)
		builder.write_string('="')
		builder.write_string(value)
		builder.write_u8(`"`)
	}
	if node.children.len > 0 {
		builder.write_string('>\n')
		for child in node.children {
			match child {
				string {
					write_indent(mut builder, original_indent, depth + 1)
					builder.write_string(escape_text(child, reverse_entities: reverse_entities))
				}
				XMLNode {
					write_pretty_xml_node(mut builder, child, original_indent, depth + 1,
						reverse_entities)
				}
				XMLComment {
					write_indent(mut builder, original_indent, depth + 1)
					builder.write_string('<!--')
					builder.write_string(child.text)
					builder.write_string('-->')
				}
				XMLCData {
					write_indent(mut builder, original_indent, depth + 1)
					builder.write_string('<![CDATA[')
					builder.write_string(child.text)
					builder.write_string(']]>')
				}
			}

			builder.write_u8(`\n`)
		}
		write_indent(mut builder, original_indent, depth)
		builder.write_string('</')
		builder.write_string(node.name)
		builder.write_u8(`>`)
	} else {
		builder.write_string('/>')
	}
}

fn write_pretty_dtd_list(mut builder strings.Builder, list []DTDListItem, indent string) bool {
	if list.len == 0 {
		return false
	}

	builder.write_u8(`[`)
	builder.write_u8(`\n`)

	for item in list {
		match item {
			DTDEntity {
				builder.write_string(indent)
				builder.write_string('<!ENTITY ')
				builder.write_string(item.name)
				builder.write_string(' "')
				builder.write_string(item.value)
				builder.write_string('">')
			}
			DTDElement {
				builder.write_string(indent)
				builder.write_string('<!ELEMENT ')
				builder.write_string(item.name)
				builder.write_string(' [')
				builder.write_string(item.definition.join(', '))
				builder.write_string(']>')
			}
		}

		builder.write_u8(`\n`)
	}
	builder.write_u8(`]`)
	return true
}

fn write_pretty_doctype(mut builder strings.Builder, doctype DocumentType, indent string) bool {
	match doctype.dtd {
		string {
			content := doctype.dtd
			if content.len == 0 {
				return false
			}
			builder.write_string('<!DOCTYPE ')
			builder.write_string(doctype.name)
			builder.write_string(' SYSTEM ')
			builder.write_string(content)
			builder.write_string('>\n')
			return true
		}
		DocumentTypeDefinition {
			if doctype.dtd.list.len == 0 {
				return false
			}

			builder.write_string('<!DOCTYPE ')
			builder.write_string(doctype.name)
			builder.write_string(' ')
			write_pretty_dtd_list(mut builder, doctype.dtd.list, indent)
			builder.write_string('>\n')
			return true
		}
	}
}

// pretty_str returns a pretty-printed version of the XML node. It requires the current indentation
// the node is at, the depth of the node in the tree, and a map of reverse entities to use when
// escaping text.
pub fn (node XMLNode) pretty_str(original_indent string, depth int, reverse_entities map[string]string) string {
	mut builder := strings.new_builder(1024)
	write_pretty_xml_node(mut builder, node, original_indent, depth, reverse_entities)
	return builder.str()
}

fn (list []DTDListItem) pretty_str(indent string) string {
	mut builder := strings.new_builder(1024)
	if !write_pretty_dtd_list(mut builder, list, indent) {
		return ''
	}
	return builder.str()
}

fn (doctype DocumentType) pretty_str(indent string) string {
	mut builder := strings.new_builder(1024)
	if !write_pretty_doctype(mut builder, doctype, indent) {
		return ''
	}
	return builder.str()
}

// pretty_str returns a pretty-printed version of the XML document. It requires the string used to
// indent each level of the document.
pub fn (doc XMLDocument) pretty_str(indent string) string {
	mut document_builder := strings.new_builder(1024)

	document_builder.write_string('<?xml version="')
	document_builder.write_string(doc.version)
	document_builder.write_string('" encoding="')
	document_builder.write_string(doc.encoding)
	document_builder.write_string('"?>\n')

	if write_pretty_doctype(mut document_builder, doc.doctype, indent) {
		document_builder.write_u8(`\n`)
	}
	for comment in doc.comments {
		document_builder.write_string('<!--')
		document_builder.write_string(comment.text)
		document_builder.write_string('-->')
		document_builder.write_u8(`\n`)
	}
	write_pretty_xml_node(mut document_builder, doc.root, indent, 0, doc.parsed_reverse_entities)

	return document_builder.str()
}

// str returns a string representation of the XML document. It uses a 2-space indentation
// to pretty-print the document.
pub fn (doc XMLDocument) str() string {
	return doc.pretty_str('  ')
}
