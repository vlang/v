module xml

import io
import os
import strings

const (
	default_prolog_attributes = {
		'version':  '1.0'
		'encoding': 'UTF-8'
	}
	default_string_builder_cap = 32
)

fn parse_attributes(all_attributes string) !map[string]string {
	if all_attributes.contains_u8(`<`) {
		return error('Malformed XML. Found "<" in attribute string: "${all_attributes}"')
	}
	mut attributes := map[string]string{}

	mut key_buf := strings.new_builder(xml.default_string_builder_cap)
	mut value_buf := strings.new_builder(xml.default_string_builder_cap)

	mut reading_key := true
	mut reading_eq := false
	mut reading_value := false

	mut found_quote := u8(0)

	for ch in all_attributes {
		if reading_key {
			match ch {
				`=` {
					reading_key = false
					reading_eq = true
				}
				else {
					key_buf.write_u8(ch)
				}
			}
		} else if reading_eq {
			match ch {
				`=` {
					return error('Duplicate "=" in attribute string: "${all_attributes}"')
				}
				`'`, `"` {
					found_quote = ch
					reading_eq = false
					reading_value = true
				}
				else {
					reading_eq = false
					reading_value = true
					found_quote = 0
				}
			}
		} else if reading_value {
			match ch {
				`'`, `"` {
					if found_quote == ch {
						// We have reached the end of the value
						reading_value = false
						reading_key = true
						attributes[key_buf.str().trim_space()] = value_buf.str().trim_space()

						found_quote = 0
					} else {
						if found_quote == 0 {
							found_quote = ch
						} else {
							value_buf.write_u8(ch)
						}
					}
				}
				else {
					value_buf.write_u8(ch)
				}
			}
		} else {
			return error('Invalid state while parsing attributes: ${all_attributes}')
		}
	}

	return attributes
}

fn parse_comment(mut reader io.Reader) !XMLComment {
	mut comment_buffer := strings.new_builder(xml.default_string_builder_cap)

	for {
		ch := next_char(mut reader)!
		match ch {
			`-` {
				if next_char(mut reader)! == `-` {
					if next_char(mut reader)! == `>` {
						break
					}
					return error('XML Comment not closed. Expected ">".')
				} else {
					comment_buffer.write_u8(ch)
				}
			}
			else {
				comment_buffer.write_u8(ch)
			}
		}
	}

	comment_contents := comment_buffer.str()
	return XMLComment{comment_contents}
}

fn parse_cdata(mut reader io.Reader) !XMLCData {
	mut contents_buf := strings.new_builder(xml.default_string_builder_cap)
	mut found_bracket := false
	mut found_double_bracket := false
	for {
		ch := next_char(mut reader)!
		contents_buf.write_u8(ch)
		match ch {
			`]` {
				if found_double_bracket {
					// Another ] after the ]] for some reason. Keep the state
					found_double_bracket = true
				} else if found_bracket {
					found_double_bracket = true
				} else {
					found_bracket = true
				}
			}
			`>` {
				if found_double_bracket {
					break
				} else {
					found_bracket = false
					found_double_bracket = false
				}
			}
			else {
				found_bracket = false
				found_double_bracket = false
			}
		}
	}
	contents := contents_buf.str().trim_space()
	if !contents.ends_with(']]>') {
		return error('CDATA section not closed.')
	}
	return XMLCData{contents[1..contents.len - 3]}
}

fn parse_entity(contents string) !(DTDEntity, string) {
	// We find the nearest '>' to the start of the ENTITY
	entity_end := contents.index('>') or { return error('Entity declaration not closed.') }
	entity_contents := contents[9..entity_end]

	name := entity_contents.trim_left(' \t\n').all_before(' ')
	value := entity_contents.all_after_first(name).trim_space().trim('"\'')

	// TODO: Add support for SYSTEM and PUBLIC entities

	return DTDEntity{name, value}, contents[entity_end + 1..]
}

fn parse_element(contents string) !(DTDElement, string) {
	// We find the nearest '>' to the start of the ELEMENT
	element_end := contents.index('>') or { return error('Element declaration not closed.') }
	element_contents := contents[9..element_end]

	name := element_contents.trim_left(' \t\n').all_before(' ')
	definition_string := element_contents.all_after_first(name).trim_space().trim('"\'')

	definition := if definition_string.starts_with('(') {
		// We have a list of possible children

		// Ensure that both ( and ) are present
		if !definition_string.ends_with(')') {
			return error('Element declaration not closed.')
		}

		definition_string.trim('()').split(',')
	} else {
		// Invalid definition
		return error('Invalid element definition: ${definition_string}')
	}

	// TODO: Add support for SYSTEM and PUBLIC entities

	return DTDElement{name, definition}, contents[element_end + 1..]
}

fn parse_doctype(mut reader io.Reader) !DocumentType {
	// We may have more < in the doctype so keep count
	mut depth := 1
	mut doctype_buffer := strings.new_builder(xml.default_string_builder_cap)

	for {
		ch := next_char(mut reader)!
		doctype_buffer.write_u8(ch)
		match ch {
			`<` {
				depth++
			}
			`>` {
				depth--
				if depth == 0 {
					break
				}
			}
			else {}
		}
	}

	doctype_contents := doctype_buffer.str().trim_space()

	name := doctype_contents.all_before(' ').trim_space()

	mut list_contents := doctype_contents.all_after(' [').all_before(']').trim_space()
	mut items := []DTDListItem{}

	for list_contents.len > 0 {
		if list_contents.starts_with('<!ENTITY') {
			entity, remaining := parse_entity(list_contents)!
			items << entity
			list_contents = remaining.trim_space()
		} else if list_contents.starts_with('<!ELEMENT') {
			element, remaining := parse_element(list_contents)!
			items << element
			list_contents = remaining.trim_space()
		} else {
			return error('Unknown DOCTYPE list item: ${list_contents}')
		}
	}

	return DocumentType{
		name: name
		dtd: DocumentTypeDefinition{
			name: ''
			list: items
		}
	}
}

fn parse_prolog(mut reader io.Reader) !(Prolog, u8) {
	// Trim trailing whitespace
	mut ch := next_char(mut reader)!
	for {
		match ch {
			` `, `\t`, `\n` {
				ch = next_char(mut reader)!
				continue
			}
			`<` {
				break
			}
			else {
				return error('Expecting a prolog or root node starting with "<".')
			}
		}
	}

	ch = next_char(mut reader)!
	if ch != `?` {
		return Prolog{}, ch
	}

	ch = next_char(mut reader)!
	if ch != `x` {
		return error('Expecting a prolog starting with "<?x".')
	}

	ch = next_char(mut reader)!
	if ch != `m` {
		return error('Expecting a prolog starting with "<?xm".')
	}

	ch = next_char(mut reader)!
	if ch != `l` {
		return error('Expecting a prolog starting with "<?xml".')
	}

	mut prolog_buffer := strings.new_builder(xml.default_string_builder_cap)

	// Keep reading character by character until we find the end of the prolog
	mut found_question_mark := false

	for {
		ch = next_char(mut reader)!
		match ch {
			`?` {
				if found_question_mark {
					return error('Invalid prolog: Two question marks found in a row.')
				}
				found_question_mark = true
			}
			`>` {
				if found_question_mark {
					break
				}
				return error('Invalid prolog: Found ">" before "?".')
			}
			else {
				if found_question_mark {
					found_question_mark = false
					prolog_buffer.write_u8(`?`)
				}
				prolog_buffer.write_u8(ch)
			}
		}
	}

	prolog_attributes := prolog_buffer.str().trim_space()

	attributes := if prolog_attributes.len == 0 {
		xml.default_prolog_attributes
	} else {
		parse_attributes(prolog_attributes)!
	}

	version := attributes['version'] or { return error('XML declaration missing version.') }
	encoding := attributes['encoding'] or { 'UTF-8' }

	mut comments := []XMLComment{}
	mut doctype := DocumentType{
		name: ''
		dtd: ''
	}
	mut found_doctype := false

	for {
		ch = next_char(mut reader)!
		match ch {
			` `, `\t`, `\n` {
				continue
			}
			`<` {
				// We have a comment, DOCTYPE, or root node
				ch = next_char(mut reader)!
				match ch {
					`!` {
						// A comment or DOCTYPE
						match next_char(mut reader)! {
							`-` {
								// A comment
								if next_char(mut reader)! != `-` {
									return error('Invalid comment.')
								}
								comments << parse_comment(mut reader)!
							}
							`D` {
								if found_doctype {
									return error('Duplicate DOCTYPE declaration.')
								}
								// <!D -> OCTYPE
								mut doc_buf := []u8{len: 6}
								if reader.read(mut doc_buf)! != 6 {
									return error('Invalid DOCTYPE.')
								}
								if doc_buf.bytestr() != 'OCTYPE' {
									return error('Invalid DOCTYPE.')
								}
								found_doctype = true
								doctype = parse_doctype(mut reader)!
							}
							else {
								return error('Unsupported control sequence found in prolog.')
							}
						}
					}
					else {
						// We have found the start of the root node
						break
					}
				}
			}
			else {}
		}
	}

	return Prolog{
		version: version
		encoding: encoding
		doctype: doctype
		comments: comments
	}, ch
}

fn parse_children(name string, attributes map[string]string, mut reader io.Reader) !XMLNode {
	mut inner_contents := strings.new_builder(xml.default_string_builder_cap)

	mut children := []XMLNodeContents{}

	for {
		ch := next_char(mut reader)!
		match ch {
			`<` {
				second_char := next_char(mut reader)!
				match second_char {
					`!` {
						// Comment, CDATA
						mut next_two := [u8(0), 0]
						if reader.read(mut next_two)! != 2 {
							return error('Invalid XML. Incomplete comment or CDATA declaration.')
						}
						prefix := next_two.bytestr()
						if prefix == '--' {
							// Comment
							comment := parse_comment(mut reader)!
							children << comment
						} else if prefix == '[C' {
							// <![CDATA -> DATA
							mut cdata_buf := []u8{len: 4}
							if reader.read(mut cdata_buf)! != 4 {
								return error('Invalid XML. Incomplete CDATA declaration.')
							}
							if cdata_buf.bytestr() != 'DATA' {
								return error('Invalid XML. Expected "CDATA" after "<![C".')
							}
							cdata := parse_cdata(mut reader)!
							children << cdata
						} else {
							return error('Invalid XML. Unknown control sequence: ${prefix}')
						}
					}
					`/` {
						// End of node
						mut node_end_buffer := []u8{len: name.len + 1}
						if reader.read(mut node_end_buffer)! != name.len + 1 {
							return error('Invalid XML. Incomplete node end.')
						}
						if node_end_buffer.bytestr() != '${name}>' {
							return error('XML node <${name}> not closed.')
						}

						collected_contents := inner_contents.str().trim_space()
						if collected_contents.len > 0 {
							// We have some inner text
							children << collected_contents
						}
						return XMLNode{
							name: name
							attributes: attributes
							children: children
						}
					}
					else {
						// Start of child node
						child := parse_single_node(second_char, mut reader) or {
							if err.msg() == 'XML node cannot start with "</".' {
								return error('XML node <${name}> not closed.')
							} else {
								return err
							}
						}
						text := inner_contents.str().trim_space()
						if text.len > 0 {
							children << text
						}
						children << child
					}
				}
			}
			else {
				inner_contents.write_u8(ch)
			}
		}
	}
	return error('XML node <${name}> not closed.')
}

fn parse_single_node(first_char u8, mut reader io.Reader) !XMLNode {
	mut ch := next_char(mut reader)!
	mut contents := strings.new_builder(xml.default_string_builder_cap)
	// We're expecting an opening tag
	if ch == `/` {
		return error('XML node cannot start with "</".')
	}
	contents.write_u8(ch)

	for {
		ch = next_char(mut reader)!
		if ch == `>` {
			break
		}
		contents.write_u8(ch)
	}

	tag_contents := contents.str().trim_space()

	parts := tag_contents.split_any(' \t\n')
	name := first_char.ascii_str() + parts[0]

	// Check if it is a self-closing tag
	if tag_contents.ends_with('/') {
		// We're not looking for children and inner text
		return XMLNode{
			name: name
			attributes: parse_attributes(tag_contents[name.len - 1..tag_contents.len].trim_space())!
		}
	}

	attribute_string := tag_contents[name.len - 1..].trim_space()
	attributes := parse_attributes(attribute_string)!

	return parse_children(name, attributes, mut reader)
}

// XMLDocument.from_string parses an XML document from a string.
pub fn XMLDocument.from_string(raw_contents string) !XMLDocument {
	mut reader := FullBufferReader{
		contents: raw_contents.bytes()
	}
	return XMLDocument.from_reader(mut reader)!
}

// XMLDocument.from_file parses an XML document from a file. Note that the file is read in its entirety
// and then parsed. If the file is too large, try using the XMLDocument.from_reader function instead.
pub fn XMLDocument.from_file(path string) !XMLDocument {
	mut reader := FullBufferReader{
		contents: os.read_bytes(path)!
	}
	return XMLDocument.from_reader(mut reader)!
}

// XMLDocument.from_reader parses an XML document from a reader. This is the most generic way to parse
// an XML document from any arbitrary source that implements that io.Reader interface.
pub fn XMLDocument.from_reader(mut reader io.Reader) !XMLDocument {
	prolog, first_char := parse_prolog(mut reader) or {
		if err is os.Eof || err is io.Eof || err.msg() == 'Unexpected End Of File.' {
			return error('XML document is empty.')
		} else {
			return err
		}
	}

	root := parse_single_node(first_char, mut reader)!

	return XMLDocument{
		version: prolog.version
		encoding: prolog.encoding
		comments: prolog.comments
		doctype: prolog.doctype
		root: root
	}
}
