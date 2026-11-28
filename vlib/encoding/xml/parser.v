module xml

import io
import os
import strings

const default_prolog_attributes = {
	'version':  '1.0'
	'encoding': 'UTF-8'
}
const default_string_builder_cap = 32

// Leave stack headroom on V's 2 MiB 32-bit default spawned-thread stack.
const xml_max_nesting_depth = 256

const element_len = '<!ELEMENT'.len
const entity_len = '<!ENTITY'.len

const doctype_chars = 'OCTYPE'.bytes()
const double_dash = '--'.bytes()
const c_tag = '[C'.bytes()
const data_chars = 'DATA'.bytes()

const byte_order_marking_first = u8(0xEF)
const byte_order_marking_bytes = [u8(0xBB), 0xBF]

struct TextSpan {
mut:
	start int
	end   int
}

fn parse_attributes(attribute_contents string) !map[string]string {
	return parse_attributes_with_entities(attribute_contents, default_entities, false)
}

// Borrow the trimmed span only until the current XML field is decoded.
@[direct_array_access]
fn xml_trim_view(value string) string {
	mut start := 0
	mut end := value.len
	for start < end && (value[start] == ` ` || (value[start] >= `\t` && value[start] <= `\r`)) {
		start++
	}
	for end > start && (value[end - 1] == ` ` || (value[end - 1] >= `\t` && value[end - 1] <= `\r`)) {
		end--
	}
	return value.substr_unsafe(start, end)
}

fn parse_attributes_with_entities(attribute_contents string, entities map[string]string, borrow_text bool) !map[string]string {
	if attribute_contents.contains_u8(`<`) {
		return error('Malformed XML. Found "<" in attribute string: "${attribute_contents}"')
	}
	mut attributes := map[string]string{}
	mut position := 0
	for position < attribute_contents.len {
		remaining := attribute_contents.substr_unsafe(position, attribute_contents.len)
		equals := remaining.index_u8(`=`)
		if equals < 0 {
			extra := remaining.trim_space()
			if extra != '' {
				return valueless_attribute_error(extra, attribute_contents)
			}
			break
		}
		key := xml_trim_view(remaining.substr_unsafe(0, equals))
		if key == '' {
			return error('XML attribute is missing name.')
		}
		for byte in key {
			if byte in [` `, `\t`, `\r`, `\n`] {
				return valueless_attribute_error(key, attribute_contents)
			}
		}
		quote_index := position + equals + 1
		if quote_index == attribute_contents.len {
			return valueless_attribute_error(key, attribute_contents)
		}
		quote := attribute_contents[quote_index]
		if quote == `=` {
			return error('Duplicate "=" in attribute string: "${attribute_contents}"')
		}
		if quote != `'` && quote != `"` {
			return error('Invalid character in attribute string: "${attribute_contents}"')
		}
		value_start := quote_index + 1
		value_end := attribute_contents.substr_unsafe(value_start, attribute_contents.len).index_u8(quote)
		if value_end < 0 {
			return error('Unterminated value of XML attribute "${key}".')
		}
		if key in attributes {
			return error('Duplicate XML attribute: ${key}')
		}
		value := attribute_contents.substr_unsafe(value_start, value_start + value_end)
		if !value.contains_u8(`&`) && value.is_pure_ascii() {
			attributes[key] = if borrow_text { value } else { value.clone() }
		} else {
			attributes[key] = unescape_text(value, entities: entities)!
		}
		position = value_start + value_end + 1
	}

	return attributes
}

// valueless_attribute_error reports the first name with no attribute value.
fn valueless_attribute_error(names string, contents string) IError {
	name := names.split_any(' \t\r\n')[0]
	return error('Attribute "${name}" has no value in attribute string: "${contents}"')
}

fn parse_comment(mut reader XMLInput) !XMLComment {
	if reader.stream == unsafe { nil } {
		start := reader.position
		end := reader.source.index_after('--', start) or { return io.Eof{} }
		if end + 2 == reader.source.len {
			return io.Eof{}
		}
		if reader.source[end + 2] != `>` {
			return error('XML Comment not closed. Expected ">".')
		}
		reader.position = end + 3
		return XMLComment{reader.source[start..end]}
	}
	mut comment_buffer := strings.new_builder(default_string_builder_cap)

	mut local_buf := [u8(0)]
	for {
		ch := next_input_byte(mut reader, mut local_buf)!
		match ch {
			`-` {
				after_ch := next_input_byte(mut reader, mut local_buf)!
				if after_ch == `-` {
					if next_input_byte(mut reader, mut local_buf)! == `>` {
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

fn parse_cdata(mut reader XMLInput) !XMLCData {
	mut contents_buf := strings.new_builder(default_string_builder_cap)

	mut state := CDATAParserState.normal
	mut local_buf := [u8(0)]

	for {
		ch := next_input_byte(mut reader, mut local_buf)!
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

fn parse_doctype_literal(contents string) !(string, string) {
	text := contents.trim_left(' \t\r\n')
	if text.len == 0 || text[0] !in [`'`, `"`] {
		return error('Invalid DOCTYPE: expected a quoted external identifier.')
	}
	end := text[1..].index_u8(text[0])
	if end < 0 {
		return error('Invalid DOCTYPE: external identifier not closed.')
	}
	return text[..end + 2], text[end + 2..]
}

fn parse_doctype_external_id(contents string) !string {
	text := contents.trim_space()
	if text == '' {
		return ''
	}
	kind := if text.starts_with('SYSTEM') {
		'SYSTEM'
	} else if text.starts_with('PUBLIC') {
		'PUBLIC'
	} else {
		return error('Invalid DOCTYPE: expected SYSTEM or PUBLIC.')
	}
	if text.len <= kind.len || text[kind.len] !in [` `, `\t`, `\r`, `\n`] {
		return error('Invalid DOCTYPE: missing external identifier.')
	}
	_, remaining := parse_doctype_literal(text[kind.len..])!
	mut trailing := remaining
	if kind == 'PUBLIC' {
		if remaining.len == 0 || remaining[0] !in [` `, `\t`, `\r`, `\n`] {
			return error('Invalid DOCTYPE: PUBLIC requires a system identifier.')
		}
		_, rest := parse_doctype_literal(remaining)!
		trailing = rest
	}
	if trailing.trim_space() != '' {
		return error('Invalid DOCTYPE: unexpected text after external identifier.')
	}
	return text
}

fn parse_doctype(mut reader XMLInput) !DocumentType {
	mut doctype_buffer := strings.new_builder(default_string_builder_cap)
	mut local_buf := [u8(0)]
	mut quote := u8(0)
	mut subset_depth := 0
	mut subset_start := -1
	mut subset_end := -1
	for {
		ch := next_input_byte(mut reader, mut local_buf) or {
			if err is io.Eof || err is os.Eof || err.msg() == 'Unexpected End Of File.' {
				return error('Invalid DOCTYPE: declaration not closed.')
			}
			return err
		}
		if quote != 0 {
			if ch == quote {
				quote = 0
			}
		} else {
			match ch {
				`'`, `"` { quote = ch }
				`[` {
					if subset_depth == 0 {
						subset_start = doctype_buffer.len
					}
					subset_depth++
				}
				`]` {
					subset_depth--
					if subset_depth < 0 {
						return error('Invalid DOCTYPE: unexpected closing bracket.')
					}
					if subset_depth == 0 {
						subset_end = doctype_buffer.len
					}
				}
				`>` {
					if subset_depth == 0 {
						break
					}
				}
				else {}
			}
		}
		doctype_buffer.write_u8(ch)
	}
	contents := doctype_buffer.str()
	header := if subset_start >= 0 { contents[..subset_start] } else { contents }
	text := header.trim_space()
	mut name_end := 0
	for ch in text {
		if ch in [` `, `\t`, `\r`, `\n`] {
			break
		}
		name_end++
	}
	if name_end == 0 {
		return error('Invalid DOCTYPE: missing root name.')
	}
	name := text[..name_end]
	external_id := parse_doctype_external_id(text[name_end..])!
	mut list_contents := ''
	if subset_start >= 0 {
		if subset_end <= subset_start || contents[subset_end + 1..].trim_space() != '' {
			return error('Invalid DOCTYPE: malformed internal subset.')
		}
		list_contents = contents[subset_start + 1..subset_end].trim_space()
	}
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
		name:        name
		external_id: external_id
		dtd:         DocumentTypeDefinition{ list: items }
	}
}

fn parse_document_start(first_char u8, mut reader XMLInput) !(DocumentType, []XMLComment, u8) {
	mut ch := first_char
	mut local_buf := [u8(0)]
	mut comments := []XMLComment{}
	mut doctype := DocumentType{ name: '', dtd: '' }
	mut found_doctype := false
	for ch == `!` {
		match next_input_byte(mut reader, mut local_buf)! {
			`-` {
				if next_input_byte(mut reader, mut local_buf)! != `-` {
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
				mut doc_buf := []u8{len: 6}
				if reader.read(mut doc_buf)! != 6 || doc_buf != doctype_chars {
					return error('Invalid DOCTYPE.')
				}
				doctype = parse_doctype(mut reader) or {
					return eof_error(err, 'DOCTYPE declaration not closed.')
				}
				found_doctype = true
			}
			else {
				return error('Unsupported control sequence found in prolog.')
			}
		}
		ch = next_input_byte(mut reader, mut local_buf)!
		for (ch in [` `, `\t`, `\r`, `\n`]) {
			ch = next_input_byte(mut reader, mut local_buf)!
		}
		if ch != `<` {
			return error('Expected root element after XML declaration or DOCTYPE.')
		}
		ch = next_input_byte(mut reader, mut local_buf)!
	}
	return doctype, comments, ch
}

fn parse_prolog(mut reader XMLInput) !(Prolog, u8) {
	// Skip trailing whitespace and invalid characters
	mut local_buf := [u8(0)]
	mut ch := next_input_byte(mut reader, mut local_buf)!
	for {
		match ch {
			` `, `\t`, `\r`, `\n` {
				ch = next_input_byte(mut reader, mut local_buf)!
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
				ch = next_input_byte(mut reader, mut local_buf)!
				continue
			}
			else {
				return error('Expecting a prolog or root node starting with "<".')
			}
		}
	}

	ch = next_input_byte(mut reader, mut local_buf)!
	if ch != `?` {
		doctype, comments, root_char := parse_document_start(ch, mut reader)!
		return Prolog{ doctype: doctype, comments: comments }, root_char
	}

	ch = next_input_byte(mut reader, mut local_buf)!
	if ch != `x` {
		return error('Expecting a prolog starting with "<?x".')
	}

	ch = next_input_byte(mut reader, mut local_buf)!
	if ch != `m` {
		return error('Expecting a prolog starting with "<?xm".')
	}

	ch = next_input_byte(mut reader, mut local_buf)!
	if ch != `l` {
		return error('Expecting a prolog starting with "<?xml".')
	}

	mut prolog_buffer := strings.new_builder(default_string_builder_cap)

	// Keep reading character by character until we find the end of the prolog
	mut found_question_mark := false

	for {
		ch = next_input_byte(mut reader, mut local_buf)!
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

	ch = next_input_byte(mut reader, mut local_buf)!
	for (ch in [` `, `\t`, `\r`, `\n`]) {
		ch = next_input_byte(mut reader, mut local_buf)!
	}
	if ch != `<` {
		return error('Expected root element after XML declaration.')
	}
	ch = next_input_byte(mut reader, mut local_buf)!
	doctype, comments, root_char := parse_document_start(ch, mut reader)!

	return Prolog{
		version:  version
		encoding: encoding
		doctype:  doctype
		comments: comments
	}, root_char
}

fn append_text_run(mut children []XMLNodeContents, mut pending []XMLNodeContents, mut contents strings.Builder, entities map[string]string) ! {
	if contents.len > 0 {
		pending << contents.str()
	}
	if pending.len == 0 {
		return
	}
	// Comments split text fragments, but only element boundaries trim the whole text run.
	for i in 0 .. pending.len {
		item := pending[i]
		match item {
			string {
				trimmed := item.trim_space_left()
				pending[i] = trimmed
				if trimmed.len > 0 {
					break
				}
			}
			XMLCData {
				if item.text.len > 0 {
					break
				}
			}
			else {}
		}
	}
	for i := pending.len - 1; i >= 0; i-- {
		item := pending[i]
		match item {
			string {
				trimmed := item.trim_space_right()
				pending[i] = trimmed
				if trimmed.len > 0 {
					break
				}
			}
			XMLCData {
				if item.text.len > 0 {
					break
				}
			}
			else {}
		}
	}
	for item in pending {
		if item is string {
			if item.len > 0 {
				children << decode_owned_text(item.replace('\r\n', '\n'), entities)!
			}
		} else {
			children << item
		}
	}
	pending.clear()
}

// The caller already owns content. Plain ASCII does not need entity decoding
// or a second copy; non-ASCII retains unescape_text's UTF-8 handling.
fn decode_owned_text(content string, entities map[string]string) !string {
	if !content.contains_u8(`&`) && content.is_pure_ascii() {
		return content
	}
	return unescape_text(content, entities: entities)
}

fn parse_control_node(mut reader XMLInput) !XMLNodeContents {
	mut next_two := [u8(0), 0]
	if reader.read(mut next_two)! != 2 {
		return error('Invalid XML. Incomplete comment or CDATA declaration.')
	}
	if next_two == double_dash {
		comment := parse_comment(mut reader) or { return eof_error(err, 'XML Comment not closed.') }
		return XMLNodeContents(comment)
	}
	if next_two == c_tag {
		mut cdata_buf := []u8{len: 4}
		if reader.read(mut cdata_buf)! != 4 {
			return error('Invalid XML. Incomplete CDATA declaration.')
		}
		if cdata_buf != data_chars {
			return error('Invalid XML. Expected "CDATA" after "<![C".')
		}
		cdata := parse_cdata(mut reader) or { return eof_error(err, 'CDATA section not closed.') }
		return XMLNodeContents(cdata)
	}
	return error('Invalid XML. Unknown control sequence: ${next_two.bytestr()}')
}

fn parse_preserved_children(name string, attributes map[string]string, entities map[string]string,
	nesting_depth int, mut reader XMLInput) !XMLNode {
	// A preserved run can contain several adjacent text and markup nodes.
	mut children := []XMLNodeContents{cap: 6}
	for reader.position < reader.source.len {
		start := reader.position
		if reader.source[start] != `<` {
			value, terminated := reader.advance_until(`<`)
			if !terminated {
				break
			}
			if reader.options.strict_markup && value.contains_u8(`]`) && value.contains(']]>') {
				return error('CDATA terminator in XML text')
			}
			children << if value.contains_u8(`&`) {
				unescape_text(value, entities: entities)!
			} else if reader.options.borrow_text {
				value
			} else {
				value.clone()
			}
			reader.position--
			continue
		}
		if start + 1 >= reader.source.len {
			break
		}
		kind := reader.source[start + 1]
		reader.position += 2
		match kind {
			`/` {
				read_node_end(name, mut reader)!
				return XMLNode{ name: name, attributes: attributes, children: children }
			}
			`!`, `?` {
				if handler := reader.options.inline_markup {
					entry := handler(reader.source, start)!
					if entry.end < reader.position || entry.end > reader.source.len {
						return error('Invalid inline XML markup extent.')
					}
					children << entry.node
					reader.position = entry.end
				} else if kind == `!` {
					children << parse_control_node(mut reader)!
				} else {
					return error('XML processing instructions are not supported.')
				}
			}
			else {
				children << parse_node_with_entities(kind, entities, nesting_depth + 1, mut reader)!
			}
		}
	}
	return error('XML node <${name}> not closed.')
}

fn parse_children(
	name string,
	attributes map[string]string,
	entities map[string]string,
	nesting_depth int,
	mut reader XMLInput
) !XMLNode {
	mut inner_contents := strings.new_builder(0)

	mut children := []XMLNodeContents{}
	mut pending := []XMLNodeContents{}
	mut local_buf := [u8(0)]

	for {
		if reader.stream == unsafe { nil } && reader.position < reader.source.len && reader.source[reader.position] != `<` {
			text, terminated := reader.advance_until(`<`)
			if !terminated {
				return error('XML node <${name}> not closed.')
			}
			inner_contents.write_string(text)
			reader.position--
		}
		ch := next_input_byte(mut reader, mut local_buf)!
		match ch {
			`<` {
				second_char := next_input_byte(mut reader, mut local_buf)!
				match second_char {
					`!` {
						if inner_contents.len > 0 {
							pending << inner_contents.str()
						}
						pending << parse_control_node(mut reader)!
					}
					`/` {
						// End of node
						read_node_end(name, mut reader)!

						append_text_run(mut children, mut pending, mut inner_contents, entities)!
						return XMLNode{
							name:       name
							attributes: attributes
							children:   children
						}
					}
					else {
						// Start of child node
						child := parse_node_with_entities(second_char,
							entities,
							nesting_depth + 1,
							mut reader) or {
							if err.msg() == 'XML node cannot start with "</".' {
								return error('XML node <${name}> not closed.')
							} else {
								return err
							}
						}
						append_text_run(mut children, mut pending, mut inner_contents, entities)!
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

fn read_node_end(name string, mut reader XMLInput) ! {
	if reader.stream == unsafe { nil } {
		start := reader.position
		if start == reader.source.len {
			return io.Eof{}
		}
		reader.position += name.len + 1
		if reader.position > reader.source.len {
			return error('Invalid XML. Incomplete node end.')
		}
		if reader.source[reader.position - 1] != `>`
			|| reader.source.substr_unsafe(start, reader.position - 1) != name {
			return error('XML node <${name}> not closed.')
		}
		return
	}
	mut buffer := []u8{len: name.len + 1}
	if reader.read(mut buffer)! != name.len + 1 {
		return error('Invalid XML. Incomplete node end.')
	}
	if buffer[name.len] != `>` {
		return error('XML node <${name}> not closed.')
	}
	for i, byte in name {
		if buffer[i] != byte {
			return error('XML node <${name}> not closed.')
		}
	}
}

// parse_single_node parses a single XML node from the reader. The first character of the tag is passed
// in as the first_char parameter. Predefined entities and character references are decoded.
// This function is meant to assist in parsing nested nodes one at a time. Using this function as
// opposed to the recommended static functions makes it easier to parse smaller nodes in extremely large
// XML documents without running out of memory.
pub fn parse_single_node(first_char u8, mut reader io.Reader) !XMLNode {
	mut input := XMLInput{ stream: reader }
	return parse_node_with_entities(first_char, default_entities, 1, mut input)
}

fn parse_node_with_entities(
	first_char u8,
	entities map[string]string,
	nesting_depth int,
	mut reader XMLInput
) !XMLNode {
	if nesting_depth > xml_max_nesting_depth {
		return error('XML element nesting exceeds the maximum depth of ${xml_max_nesting_depth}.')
	}
	if first_char == `?` {
		return error('XML processing instructions are not supported.')
	}
	raw_tag := read_tag_contents(first_char, mut reader)!
	tag_contents := xml_trim_view(raw_tag)
	if reader.options.strict_markup {
		if first_char in [` `, `\t`, `\r`, `\n`] {
			return error('invalid XML opening tag')
		}
		if raw_tag.len != tag_contents.len && tag_contents.ends_with('/') {
			return error('whitespace in XML empty-element delimiter')
		}
	}
	mut name_end := 0
	for name_end < tag_contents.len && tag_contents[name_end] !in [` `, `\t`, `\r`, `\n`] {
		name_end++
	}
	for name_end > 0 && tag_contents[name_end - 1] == `/` {
		name_end--
	}
	name := tag_contents[..name_end]
	if name == '' {
		return error('XML node is missing name.')
	}

	// Check if it is a self-closing tag
	if tag_contents.ends_with('/') {
		// We're not looking for children and inner text
		return XMLNode{
			name:       name
			attributes: parse_attributes_with_entities(xml_trim_view(tag_contents.substr_unsafe(name.len, tag_contents.len - 1)), entities, reader.options.borrow_text)!
		}
	}

	attribute_string := xml_trim_view(tag_contents.substr_unsafe(name.len, tag_contents.len))
	attributes := parse_attributes_with_entities(attribute_string, entities, reader.options.borrow_text)!

	if reader.options.preserve_text {
		return parse_preserved_children(name, attributes, entities, nesting_depth, mut reader) or {
			return eof_error(err, 'XML node <${name}> not closed.')
		}
	}
	return parse_children(name, attributes, entities, nesting_depth, mut reader) or {
		return eof_error(err, 'XML node <${name}> not closed.')
	}
}

fn read_tag_contents(first_char u8, mut reader XMLInput) !string {
	if first_char == `>` {
		return ''
	}
	if reader.stream == unsafe { nil } {
		start := reader.position - 1
		_, terminated := reader.advance_until(`>`)
		if !terminated {
			return error('XML tag not closed. Expected ">".')
		}
		return reader.source.substr_unsafe(start, reader.position - 1)
	}
	mut contents := strings.new_builder(default_string_builder_cap)
	mut local_buf := [u8(0)]
	mut ch := first_char
	for ch != `>` {
		contents.write_u8(ch)
		ch = next_input_byte(mut reader, mut local_buf) or {
			return eof_error(err, 'XML tag not closed. Expected ">".')
		}
	}
	return contents.str()
}

// XMLDocument.from_string parses an XML document and decodes entities in text and attributes.
pub fn XMLDocument.from_string(raw_contents string) !XMLDocument {
	document, _ := XMLDocument.from_string_with_consumed(raw_contents, XMLStringOptions{})!
	return document
}

// XMLInlineMarkup parses one inline declaration or processing instruction.
pub type XMLInlineMarkup = fn (string, int) !XMLInlineMarkupResult

pub struct XMLInlineMarkupResult {
pub:
	node XMLNodeContents @[required]
	end  int
}

@[params]
pub struct XMLStringOptions {
pub:
	// Keep leading and trailing whitespace and raw line endings in character data.
	preserve_text bool
	// Accept only exact start-tag and empty-element delimiters.
	strict_markup bool
	// Override inline <! and <? parsing while preserving character data.
	inline_markup ?XMLInlineMarkup
	// Borrow entity-free character data and plain ASCII attribute values.
	// Keep the input alive until consumers finish reading or copy those values.
	borrow_text bool
}

// XMLDocument.from_string_with_consumed also returns the byte offset after one XML document.
pub fn XMLDocument.from_string_with_consumed(raw_contents string, options XMLStringOptions) !(XMLDocument, int) {
	if options.inline_markup != none && !options.preserve_text {
		return error('Inline XML markup requires preserved text.')
	}
	mut input := XMLInput{ source: raw_contents, options: options }
	// A document that starts with the root element uses the default prolog
	// and entities. Enter the same node parser directly in that case.
	if raw_contents.len >= 2 && raw_contents[0] == `<` && raw_contents[1] !in [`?`, `!`] {
		input.position = 2
		root := parse_node_with_entities(raw_contents[1], default_entities, 1, mut input)!
		return XMLDocument{ root: root }, input.position
	}
	document := xml_document_from_input(mut input)!
	return document, input.position
}

// XMLDocument.from_file parses an XML document from a file. Note that the file is read in its entirety
// and then parsed. If the file is too large, try using the XMLDocument.from_reader function instead.
pub fn XMLDocument.from_file(path string) !XMLDocument {
	mut reader := FullBufferReader{
		contents: os.read_bytes(path)!
	}
	return XMLDocument.from_reader(mut reader)!
}

// XMLDocument.from_reader parses an XML document and decodes entities in text and attributes. This is the most generic way to parse
// an XML document from any arbitrary source that implements that io.Reader interface.
pub fn XMLDocument.from_reader(mut reader io.Reader) !XMLDocument {
	mut input := XMLInput{ stream: reader }
	return xml_document_from_input(mut input)
}

fn xml_document_from_input(mut reader XMLInput) !XMLDocument {
	prolog, first_char := parse_prolog(mut reader) or {
		if err is os.Eof || err is io.Eof || err.msg() == 'Unexpected End Of File.' {
			return error('XML document is empty.')
		} else {
			return err
		}
	}

	// Borrow the read-only defaults until a DTD entity requires a private map.
	mut entities := unsafe { default_entities }
	mut owns_entities := false
	if prolog.doctype.dtd is DocumentTypeDefinition {
		for item in prolog.doctype.dtd.list {
			if item is DTDEntity {
				if !owns_entities {
					entities = default_entities.clone()
					owns_entities = true
				}
				entities[item.name] = item.value
			}
		}
	}
	root := parse_node_with_entities(first_char, entities, 1, mut reader)!

	return XMLDocument{
		version:  prolog.version
		encoding: prolog.encoding
		comments: prolog.comments
		doctype:  prolog.doctype
		root:     root
	}
}
