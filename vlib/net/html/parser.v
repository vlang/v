module html

import os
import strings

struct LexicalAttributes {
mut:
	current_tag        &Tag = unsafe { nil }
	open_tag           bool
	open_code          bool
	open_string        int
	open_comment       bool
	is_attribute       bool
	pending_attribute  string
	opened_code_type   string
	line_count         int
	outside_tag        bool
	text_after_tag     bool
	text_after_comment bool
	lexeme_builder     strings.Builder = strings.new_builder(100)
	code_tags          map[string]bool = {
		'script': true
		'style':  true
	}
}

// Parser is responsible for reading the HTML strings and converting them into a `DocumentObjectModel`.
pub struct Parser {
mut:
	dom                DocumentObjectModel
	lexical_attributes LexicalAttributes = LexicalAttributes{
		current_tag: &Tag{}
	}
	filename           string = 'direct-parse'
	initialized        bool
	tags               []&Tag
	debug_file         os.File
}

// This function is used to add a tag for the parser ignore it's content.
// For example, if you have an html or XML with a custom tag, like `<script>`, using this function,
// like `add_code_tag('script')` will make all `script` tags content be jumped,
// so you still have its content, but will not confuse the parser with it's `>` or `<`.
pub fn (mut parser Parser) add_code_tag(name string) {
	if name.len <= 0 {
		return
	}
	parser.lexical_attributes.code_tags[name] = true
}

@[inline]
fn (parser Parser) builder_str() string {
	return parser.lexical_attributes.lexeme_builder.after(0)
}

@[if debug_html ?]
fn (mut parser Parser) print_debug(data string) {
	if data.len > 0 {
		parser.debug_file.writeln(data) or { panic(err) }
	}
}

fn (mut parser Parser) verify_end_comment(remove bool) bool {
	lexeme := parser.builder_str()
	last := lexeme[lexeme.len - 1]
	penultimate := lexeme[lexeme.len - 2]
	is_end_comment := last == `-` && penultimate == `-`
	if is_end_comment && remove {
		parser.lexical_attributes.lexeme_builder.go_back(2)
	}
	return is_end_comment
}

fn blank_string(data string) bool {
	mut count := 0
	for chr in data {
		if chr == 10 || chr == 9 || chr == 32 {
			count++
		}
	}
	return count == data.len
}

// init initializes the parser.
fn (mut parser Parser) init() {
	if parser.initialized {
		return
	}
	parser.dom = DocumentObjectModel{
		debug_file: parser.debug_file
		root:       &Tag{}
	}
	parser.add_code_tag('')
	parser.tags = []&Tag{}
	parser.dom.close_tags['/!document'] = true
	parser.lexical_attributes.current_tag = &Tag{}
	parser.lexical_attributes.outside_tag = true
	parser.initialized = true
}

fn (mut parser Parser) generate_tag() {
	if parser.lexical_attributes.open_tag {
		return
	}
	if parser.lexical_attributes.text_after_comment
		&& parser.lexical_attributes.lexeme_builder.len > 0 {
		parser.lexical_attributes.current_tag.content = parser.builder_str()
		parser.lexical_attributes.lexeme_builder.go_back_to(0)
	}
	if (parser.lexical_attributes.current_tag.name != ''
		&& !parser.lexical_attributes.text_after_comment)
		|| parser.lexical_attributes.current_tag.content.len > 0 {
		parser.tags << parser.lexical_attributes.current_tag
	}
	parser.lexical_attributes.text_after_comment = false
	parser.lexical_attributes.current_tag = &Tag{}
}

fn (mut parser Parser) set_attribute_value(value string) {
	name := parser.lexical_attributes.current_tag.last_attribute
	parser.lexical_attributes.current_tag.attributes[name] = value
	if name == 'class' {
		for class_name in value.split_any('\t\r\n \f') {
			if class_name != '' {
				parser.lexical_attributes.current_tag.class_set.add(class_name)
			}
		}
	}
	parser.lexical_attributes.current_tag.last_attribute = ''
	parser.lexical_attributes.pending_attribute = ''
}

// split_parse parses the HTML fragment
pub fn (mut parser Parser) split_parse(data string) {
	parser.init()
	for chr in data {
		is_quote := chr == `"` || chr == `'`
		string_code := match chr {
			`"` { 1 }
			`'` { 2 }
			else { 0 }
		}

		if parser.lexical_attributes.open_code { // verify if open_code is complete and handle string code
			parser.lexical_attributes.lexeme_builder.write_u8(chr)
			if parser.lexical_attributes.open_string > 0
				&& parser.lexical_attributes.open_string == string_code {
				parser.lexical_attributes.open_string = 0
			} else if chr == `>` { // code tag is finished
				name_close_tag := '</${parser.lexical_attributes.opened_code_type}>'
				if parser.builder_str().to_lower().ends_with(name_close_tag) {
					parser.lexical_attributes.open_code = false
					// modify lexeme_builder to include script text as content in the next loop
					parser.lexical_attributes.lexeme_builder.go_back(name_close_tag.len)
					// the builder now holds exactly the body, since it was reset
					// when the opening tag closed; keep it as the tag content
					parser.lexical_attributes.current_tag.content = parser.builder_str()
					parser.lexical_attributes.current_tag.closed = true
					parser.lexical_attributes.current_tag.close_type = .new_tag
					parser.lexical_attributes.lexeme_builder.go_back_to(0)
					parser.generate_tag()
				}
			}
		} else if parser.lexical_attributes.open_comment {
			if chr == `>` && parser.verify_end_comment(false) {
				// parser.print_debug(parser.builder_str() + " >> " + parser.lexical_attributes.line_count.str())
				parser.lexical_attributes.lexeme_builder.go_back_to(0)
				parser.lexical_attributes.open_comment = false
				parser.lexical_attributes.open_tag = false
				parser.lexical_attributes.current_tag = &Tag{
					name:   'text'
					closed: true
				}
				parser.lexical_attributes.text_after_comment = true
			} else {
				parser.lexical_attributes.lexeme_builder.write_u8(chr)
			}
		} else if parser.lexical_attributes.open_string > 0 {
			if parser.lexical_attributes.open_string == string_code {
				parser.lexical_attributes.open_string = 0
				parser.lexical_attributes.lexeme_builder.write_u8(chr)
				temp_lexeme := parser.builder_str()
				if parser.lexical_attributes.current_tag.last_attribute != '' {
					parser.set_attribute_value(temp_lexeme.substr(1, temp_lexeme.len - 1))
				} else {
					parser.lexical_attributes.current_tag.attributes[temp_lexeme.to_lower()] = ''
					// parser.print_debug(temp_lexeme)
				}
				parser.lexical_attributes.lexeme_builder.go_back_to(0)
			} else {
				parser.lexical_attributes.lexeme_builder.write_u8(chr)
			}
		} else if parser.lexical_attributes.open_tag {
			if parser.lexical_attributes.lexeme_builder.len == 0 && is_quote {
				parser.lexical_attributes.open_string = string_code
				parser.lexical_attributes.lexeme_builder.write_u8(chr)
			} else if chr == `>` {
				complete_lexeme := parser.builder_str()
				parser.lexical_attributes.current_tag.closed = (parser.lexical_attributes.current_tag.last_attribute == '' && complete_lexeme.len > 0
					&& complete_lexeme[complete_lexeme.len - 1] == `/`)
				if complete_lexeme.len > 0 && complete_lexeme[0] == `/` {
					parser.dom.close_tags[complete_lexeme.to_lower()] = true
				}
				/*
				else if complete_lexeme.len > 0 && complete_lexeme[complete_lexeme.len - 1] == 47 { // if end tag like "/>"
					parser.lexical_attributes.current_tag.closed = true
				}
				*/
				if parser.lexical_attributes.current_tag.name == '' {
					parser.lexical_attributes.current_tag.name = complete_lexeme.to_lower()
				} else if parser.lexical_attributes.current_tag.last_attribute != '' {
					parser.set_attribute_value(complete_lexeme)
				} else if complete_lexeme != '' && complete_lexeme != '/' {
					parser.lexical_attributes.current_tag.attributes[complete_lexeme.to_lower()] = ''
				}
				parser.lexical_attributes.pending_attribute = ''
				parser.lexical_attributes.open_tag = false
				parser.lexical_attributes.lexeme_builder.go_back_to(0) // if tag name is code
				if parser.lexical_attributes.current_tag.name in parser.lexical_attributes.code_tags {
					parser.lexical_attributes.open_code = true
					parser.lexical_attributes.opened_code_type = parser.lexical_attributes.current_tag.name
				}
				// parser.print_debug(parser.lexical_attributes.current_tag.name)
			} else if chr == `=` && parser.lexical_attributes.current_tag.last_attribute != ''
				&& parser.lexical_attributes.lexeme_builder.len > 0 {
				parser.lexical_attributes.lexeme_builder.write_u8(chr)
			} else if chr !in [u8(9), ` `, `=`, `\n`, `\r`, `\f`] {
				parser.lexical_attributes.lexeme_builder.write_u8(chr)
			} else {
				complete_lexeme := parser.builder_str()
				if parser.lexical_attributes.current_tag.name == '' {
					parser.lexical_attributes.current_tag.name = complete_lexeme.to_lower()
				} else if parser.lexical_attributes.current_tag.last_attribute != '' {
					if complete_lexeme != '' {
						parser.set_attribute_value(complete_lexeme)
					}
				} else if chr == `=` {
					name := if complete_lexeme != '' {
						complete_lexeme.to_lower()
					} else {
						parser.lexical_attributes.pending_attribute
					}
					if name != '' {
						parser.lexical_attributes.current_tag.attributes[name] = ''
						parser.lexical_attributes.current_tag.last_attribute = name
					}
					parser.lexical_attributes.pending_attribute = ''
				} else if complete_lexeme != '' {
					name := complete_lexeme.to_lower()
					parser.lexical_attributes.current_tag.attributes[name] = ''
					parser.lexical_attributes.pending_attribute = name
				}
				parser.lexical_attributes.lexeme_builder.go_back_to(0)
			}
			if parser.builder_str() == '!--' {
				parser.lexical_attributes.open_comment = true
			}
		} else if chr == `<` {
			temp_string := parser.builder_str()
			if parser.lexical_attributes.lexeme_builder.len >= 1 {
				if parser.lexical_attributes.current_tag.name.len > 1
					&& parser.lexical_attributes.current_tag.name[0] == 47
					&& !blank_string(temp_string) {
					parser.lexical_attributes.text_after_tag = true
				} else {
					if parser.lexical_attributes.current_tag.name == '' {
						parser.lexical_attributes.current_tag.name = 'text'
					}
					parser.lexical_attributes.current_tag.content = temp_string // verify later who has this content
				}
			}
			// parser.print_debug(parser.lexical_attributes.current_tag.str())
			parser.lexical_attributes.lexeme_builder.go_back_to(0)
			parser.generate_tag()
			parser.lexical_attributes.open_tag = true
			parser.lexical_attributes.outside_tag = false

			if parser.lexical_attributes.text_after_tag == true {
				parser.tags << &Tag{
					name:    'text'
					content: temp_string
				}
				parser.lexical_attributes.text_after_tag = false
			}
		} else {
			parser.lexical_attributes.lexeme_builder.write_u8(chr)
		}
	}

	// If `data` has not tags but has only text.
	if parser.lexical_attributes.outside_tag {
		temp_string := parser.lexical_attributes.lexeme_builder.str()

		if parser.tags.len == 0 {
			parser.tags << &Tag{
				name:    'text'
				content: temp_string
			}
		} else if parser.tags.len == 1 {
			mut tag := parser.tags.first()

			if tag.name == 'text' {
				tag.content += temp_string
			}
		}
	}
}

// parse_html parses the given HTML string
pub fn (mut parser Parser) parse_html(data string) {
	parser.init()
	mut lines := data.split_into_lines()
	for index, line in lines {
		parser.lexical_attributes.line_count++
		// Parser shouldn't replace `\n`, because it may break JS code or text which sticks together.
		// After `split_into_lines()` we need to add `\n` again.
		parser.split_parse(if index < lines.len - 1 { '${line}\n' } else { line })
	}
	parser.finalize()
	parser.dom.debug_file = parser.debug_file
	parser.dom.construct(parser.tags)
}

// finalize flushes pending text and finishes the parsing stage.
@[inline]
pub fn (mut parser Parser) finalize() {
	if parser.lexical_attributes.open_code {
		parser.lexical_attributes.current_tag.content = parser.builder_str()
		parser.lexical_attributes.lexeme_builder.go_back_to(0)
		parser.lexical_attributes.open_code = false
	}
	if !parser.lexical_attributes.open_code && !parser.lexical_attributes.open_tag
		&& parser.lexical_attributes.lexeme_builder.len > 0 {
		if parser.lexical_attributes.current_tag.name == ''
			|| parser.lexical_attributes.current_tag.name[0] == `/` {
			// trailing text after the last tag (or with no tags at all):
			// it belongs to a new text node. Flush the closing tag first
			// so construction attaches the text to the surrounding element.
			parser.generate_tag()
			parser.tags << &Tag{
				name:         'text'
				content:      parser.builder_str()
				text_content: parser.builder_str()
			}
			parser.lexical_attributes.lexeme_builder.go_back_to(0)
		} else {
			// text inside an element that was never closed belongs to it
			parser.lexical_attributes.current_tag.content += parser.builder_str()
			parser.lexical_attributes.lexeme_builder.go_back_to(0)
		}
	}
	parser.generate_tag()
}

// get_dom returns the parser's current DOM representation.
pub fn (mut parser Parser) get_dom() DocumentObjectModel {
	if !parser.dom.constructed {
		parser.finalize()
		parser.dom.construct(parser.tags)
	}
	return parser.dom
}
