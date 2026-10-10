module xml

import io
import os
import strings

const default_prolog_attributes = {
	'version':  '1.0'
	'encoding': 'UTF-8'
}
const default_string_builder_cap = 32

const element_len = '<!ELEMENT'.len
const entity_len = '<!ENTITY'.len

const doctype_chars = 'OCTYPE'.bytes()
const double_dash = '--'.bytes()
const c_tag = '[C'.bytes()
const data_chars = 'DATA'.bytes()

const byte_order_marking_first = u8(0xEF)
const byte_order_marking_bytes = [u8(0xBB), 0xBF]

// Helper types to assist in parsing

struct TextSpan {
mut:
	start int
	end   int
}

enum AttributeParserState {
	key
	eq
	value
}

fn parse_attributes(attribute_contents string) !map[string]string {
	if attribute_contents.contains_u8(`<`) {
		return error('Malformed XML. Found "<" in attribute string: "${attribute_contents}"')
	}
	mut attributes := map[string]string{}

	mut state := AttributeParserState.key
	mut key_span, mut value_span := TextSpan{}, TextSpan{}
	mut key := ''
	// The quote that opened the current value. Only the same one closes it: the other kind is a
	// part of the value.
	mut quote := u8(0)

	for index, ch in attribute_contents {
		match state {
			.key {
				match ch {
					`=` {
						// Only one name can stand in front of a "=". Another one before it has no value.
						key = attribute_contents[key_span.start..key_span.end].trim_space()
						if key.contains_any(' \t\r\n') {
							return valueless_attribute_error(key, attribute_contents)
						}
						state = AttributeParserState.eq
					}
					else {
						key_span.end++
					}
				}
			}
			.eq {
				match ch {
					`=` {
						return error('Duplicate "=" in attribute string: "${attribute_contents}"')
					}
					`'`, `"` {
						state = AttributeParserState.value
						value_span.start = index + 1
						quote = ch
					}
					else {
						return error('Invalid character in attribute string: "${attribute_contents}"')
					}
				}
			}
			.value {
				match ch {
					quote {
						state = AttributeParserState.key
						value_span.end = index
						attributes[key] = attribute_contents[value_span.start..value_span.end]

						key_span.start = index + 1
						key_span.end = index + 1
					}
					else {
						state = AttributeParserState.value
						value_span.end++
					}
				}
			}
		}
	}

	if state == .key && key_span.end > key_span.start {
		// A name after the last value has no value either.
		key = attribute_contents[key_span.start..key_span.end].trim_space()
		if key != '' {
			return valueless_attribute_error(key, attribute_contents)
		}
	}

	return attributes
}

// valueless_attribute_error names the first attribute in `names`, which is not followed by a "=".
// XML has no attributes without a value, and such a name would otherwise be merged into the key of
// the next attribute.
fn valueless_attribute_error(names string, attribute_contents string) IError {
	name := names.split_any(' \t\r\n')[0]
	return error('Attribute "${name}" has no value in attribute string: "${attribute_contents}"')
}

fn parse_comment(mut reader io.Reader) !XMLComment {
	mut comment_buffer := strings.new_builder(default_string_builder_cap)

	mut local_buf := [u8(0)]
	for {
		ch := next_char(mut reader, mut local_buf)!
		match ch {
			`-` {
				after_ch := next_char(mut reader, mut local_buf)!
				if after_ch == `-` {
					if next_char(mut reader, mut local_buf)! == `>` {
						break
					}
					return error('XML Comment not closed. Expected ">".')
				} else {
					comment_buffer.write_u8(ch)
					comment_buffer.write_u8(after_ch)
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

enum CDATAParserState {
	normal
	single
	double
}

fn parse_cdata(mut reader io.Reader) !XMLCData {
	mut contents_buf := strings.new_builder(default_string_builder_cap)

	mut state := CDATAParserState.normal
	mut local_buf := [u8(0)]

	for {
		ch := next_char(mut reader, mut local_buf)!
		contents_buf.write_u8(ch)
		match ch {
			`]` {
				match state {
					.double {
						// Another ] after the ]] for some reason. Keep the state
					}
					.single {
						state = .double
					}
					.normal {
						state = .single
					}
				}
			}
			`>` {
				match state {
					.double {
						break
					}
					else {
						state = .normal
					}
				}
			}
			else {
				state = .normal
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
	entity_contents := contents[entity_len..entity_end]

	name := entity_contents.trim_left(' \t\n').all_before(' ')
	if name == '' {
		return error('Entity is missing name.')
	}
	value := entity_contents.all_after_first(name).trim_space().trim('"\'')
	if value.len == 0 {
		return error('Entity is missing value.')
	}

	// TODO: Add support for SYSTEM and PUBLIC entities

	return DTDEntity{name, value}, contents[entity_end + 1..]
}

fn parse_element(contents string) !(DTDElement, string) {
	// We find the nearest '>' to the start of the ELEMENT
	element_end := contents.index('>') or { return error('Element declaration not closed.') }
	element_contents := contents[element_len..element_end].trim_left(' \t\r\n')

	mut name_span := TextSpan{}

	for ch in element_contents {
		match ch {
			` `, `\t`, `\n` {
				break
			}
			// Valid characters in an entity name are:
			// 1. Lowercase alphabet - a-z
			// 2. Uppercase alphabet - A-Z
			// 3. Numbers - 0-9
			// 4. Underscore - _
			// 5. Colon - :
			// 6. Period - .
			`a`...`z`, `A`...`Z`, `0`...`9`, `_`, `:`, `.` {
				name_span.end++
			}
			else {
				return error('Invalid character in element name: "${ch}"')
			}
		}
	}

	name := element_contents[name_span.start..name_span.end].trim_left(' \t\n')
	if name == '' {
		return error('Element is missing name.')
	}
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
	mut doctype_buffer := strings.new_builder(default_string_builder_cap)
	mut local_buf := [u8(0)]
	for {
		ch := next_char(mut reader, mut local_buf)!
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

	name := doctype_contents.all_before('[').trim_space()

	mut list_contents := doctype_contents.all_after('[').all_before(']').trim_space()
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
		dtd:  DocumentTypeDefinition{
			list: items
		}
	}
}

fn parse_prolog(mut reader io.Reader) !(Prolog, u8) {
	// Skip trailing whitespace and invalid characters
	mut local_buf := [u8(0)]
	mut ch := next_char(mut reader, mut local_buf)!
	for {
		match ch {
			` `, `\t`, `\r`, `\n` {
				ch = next_char(mut reader, mut local_buf)!
				continue
			}
			`<` {
				break
			}
			byte_order_marking_first {
				// UTF-8 BOM
				mut bom_buf := [u8(0), 0]
				if reader.read(mut bom_buf)! != 2 {
					return error('Invalid UTF-8 BOM.')
				}
				if bom_buf != byte_order_marking_bytes {
					return error('Invalid UTF-8 BOM.')
				}
				ch = next_char(mut reader, mut local_buf)!
				continue
			}
			else {
				return error('Expecting a prolog or root node starting with "<".')
			}
		}
	}

	ch = next_char(mut reader, mut local_buf)!
	if ch != `?` {
		// There is no XML declaration. Comments and a DOCTYPE can still come before the root node.
		defaults := Prolog{}
		return parse_prolog_misc(defaults.version, defaults.encoding, ch, mut reader)
	}

	ch = next_char(mut reader, mut local_buf)!
	if ch != `x` {
		return error('Expecting a prolog starting with "<?x".')
	}

	ch = next_char(mut reader, mut local_buf)!
	if ch != `m` {
		return error('Expecting a prolog starting with "<?xm".')
	}

	ch = next_char(mut reader, mut local_buf)!
	if ch != `l` {
		return error('Expecting a prolog starting with "<?xml".')
	}

	mut prolog_buffer := strings.new_builder(default_string_builder_cap)

	// Keep reading character by character until we find the end of the prolog
	mut found_question_mark := false

	for {
		ch = next_char(mut reader, mut local_buf)!
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
		default_prolog_attributes
	} else {
		parse_attributes(prolog_attributes)!
	}

	version := attributes['version'] or { return error('XML declaration missing version.') }
	encoding := attributes['encoding'] or { 'UTF-8' }

	ch = next_tag_char(mut reader, mut local_buf)!
	return parse_prolog_misc(version, encoding, ch, mut reader)
}

// next_tag_char skips to the next "<" and returns the character after it.
fn next_tag_char(mut reader io.Reader, mut buf []u8) !u8 {
	mut ch := next_char(mut reader, mut buf)!
	for ch != `<` {
		ch = next_char(mut reader, mut buf)!
	}
	return next_char(mut reader, mut buf)!
}

// parse_prolog_misc parses the comments and the DOCTYPE that can stand before the root node, with or
// without an XML declaration in front of them. `first_char` is the character after the last "<"
// that was read. The character returned with the prolog is the first one of the root node's tag.
fn parse_prolog_misc(version string, encoding string, first_char u8, mut reader io.Reader) !(Prolog, u8) {
	mut local_buf := [u8(0)]
	mut ch := first_char
	mut comments := []XMLComment{}
	mut doctype := DocumentType{
		name: ''
		dtd:  ''
	}
	mut found_doctype := false
	// "<!" starts a comment or a DOCTYPE. Anything else is the start of the root node.
	for ch == `!` {
		match next_char(mut reader, mut local_buf)! {
			`-` {
				// A comment
				if next_char(mut reader, mut local_buf)! != `-` {
					return error('Invalid comment.')
				}
				comments << parse_comment(mut reader) or {
					return eof_error(err, 'XML Comment not closed.')
				}
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
				if doc_buf != doctype_chars {
					return error('Invalid DOCTYPE.')
				}
				found_doctype = true
				doctype = parse_doctype(mut reader) or {
					return eof_error(err, 'DOCTYPE declaration not closed.')
				}
			}
			else {
				return error('Unsupported control sequence found in prolog.')
			}
		}
		ch = next_tag_char(mut reader, mut local_buf)!
	}

	return Prolog{
		version:  version
		encoding: encoding
		doctype:  doctype
		comments: comments
	}, ch
}

fn parse_children(name string, attributes map[string]string, mut reader io.Reader) !XMLNode {
	mut inner_contents := strings.new_builder(default_string_builder_cap)

	mut children := []XMLNodeContents{}
	mut local_buf := [u8(0)]

	for {
		ch := next_char(mut reader, mut local_buf)!
		match ch {
			`<` {
				// Any markup ends the text in front of it. Adding that text now keeps the children
				// in document order, whether a node, a comment or a CDATA section follows.
				text := inner_contents.str().trim_space()
				if text.len > 0 {
					children << text.replace('\r\n', '\n')
				}
				second_char := next_char(mut reader, mut local_buf)!
				match second_char {
					`!` {
						// Comment, CDATA
						mut next_two := [u8(0), 0]
						if reader.read(mut next_two)! != 2 {
							return error('Invalid XML. Incomplete comment or CDATA declaration.')
						}
						if next_two == double_dash {
							// Comment
							comment := parse_comment(mut reader) or {
								return eof_error(err, 'XML Comment not closed.')
							}
							children << comment
						} else if next_two == c_tag {
							// <![CDATA -> DATA
							mut cdata_buf := []u8{len: 4}
							if reader.read(mut cdata_buf)! != 4 {
								return error('Invalid XML. Incomplete CDATA declaration.')
							}
							if cdata_buf != data_chars {
								return error('Invalid XML. Expected "CDATA" after "<![C".')
							}
							cdata := parse_cdata(mut reader) or {
								return eof_error(err, 'CDATA section not closed.')
							}
							children << cdata
						} else {
							return error('Invalid XML. Unknown control sequence: ${next_two.bytestr()}')
						}
					}
					`/` {
						// End of node
						mut node_end_buffer := []u8{len: name.len + 1}
						if reader.read(mut node_end_buffer)! != name.len + 1 {
							return error('Invalid XML. Incomplete node end.')
						}

						mut ending_chars := name.bytes()
						ending_chars << `>`

						if node_end_buffer != ending_chars {
							return error('XML node <${name}> not closed.')
						}

						return XMLNode{
							name:       name
							attributes: attributes
							children:   children
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

// parse_single_node parses a single XML node from the reader. The first character of the tag is passed
// in as the first_char parameter.
// This function is meant to assist in parsing nested nodes one at a time. Using this function as
// opposed to the recommended static functions makes it easier to parse smaller nodes in extremely large
// XML documents without running out of memory.
pub fn parse_single_node(first_char u8, mut reader io.Reader) !XMLNode {
	if first_char == `?` {
		// "<?" starts a processing instruction, which the document tree has no node for.
		return error('XML processing instructions are not supported.')
	}
	mut contents := strings.new_builder(default_string_builder_cap)

	mut local_buf := [u8(0)]
	// The first character can already be the one that closes the tag, as in "<>".
	mut ch := first_char
	for ch != `>` {
		contents.write_u8(ch)
		ch = next_char(mut reader, mut local_buf) or {
			return eof_error(err, 'XML tag not closed. Expected ">".')
		}
	}

	tag_contents := contents.str().trim_space()

	parts := tag_contents.split_any(' \t\r\n')
	// An empty tag, like "<>" or "< >", has no parts.
	name := if parts.len > 0 { parts[0].trim_right('/') } else { '' }
	if name == '' {
		return error('XML node is missing name.')
	}

	// Check if it is a self-closing tag
	if tag_contents.ends_with('/') {
		// We're not looking for children and inner text
		return XMLNode{
			name:       name
			attributes: parse_attributes(tag_contents[name.len..tag_contents.len - 1].trim_space())!
		}
	}

	attribute_string := tag_contents[name.len..].trim_space()
	attributes := parse_attributes(attribute_string)!

	return parse_children(name, attributes, mut reader) or {
		return eof_error(err, 'XML node <${name}> not closed.')
	}
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
		return eof_error(err, 'XML document is empty.')
	}

	root := parse_single_node(first_char, mut reader)!

	return XMLDocument{
		version:  prolog.version
		encoding: prolog.encoding
		comments: prolog.comments
		doctype:  prolog.doctype
		root:     root
	}
}
