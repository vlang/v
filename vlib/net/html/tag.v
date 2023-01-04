module html

import strings

enum CloseTagType {
	in_name
	new_tag
}

// Tag holds the information of an HTML tag.
[heap]
pub struct Tag {
pub mut:
	name               string
	content            string
	children           []&Tag
	attributes         map[string]string // attributes will be like map[name]value
	last_attribute     string
	parent             &Tag = unsafe { nil }
	position_in_parent int
	closed             bool
	close_type         CloseTagType = .in_name
}

fn (mut tag Tag) add_parent(t &Tag, position int) {
	tag.position_in_parent = position
	tag.parent = t
}

fn (mut tag Tag) add_child(t &Tag) int {
	tag.children << t
	return tag.children.len
}

// text returns the text contents of the tag.
pub fn (tag Tag) text() string {
	if tag.name.len >= 2 && tag.name[..2] == 'br' {
		return '\n'
	}
	mut text_str := strings.new_builder(200)
	text_str.write_string(tag.content.replace('\n', ''))
	for child in tag.children {
		text_str.write_string(child.text())
	}
	return text_str.str()
}

pub fn (tag &Tag) str() string {
	mut html_str := strings.new_builder(200)
	html_str.write_string('<${tag.name}')
	for key, value in tag.attributes {
		html_str.write_string(' ${key}')
		if value.len > 0 {
			html_str.write_string('="${value}"')
		}
	}
	html_str.write_string(if tag.closed && tag.close_type == .in_name { '/>' } else { '>' })
	html_str.write_string(tag.content)
	if tag.children.len > 0 {
		for child in tag.children {
			html_str.write_string(child.str())
		}
	}
	if !tag.closed || tag.close_type == .new_tag {
		html_str.write_string('</${tag.name}>')
	}
	return html_str.str()
}

// get_tags retrieves all the child tags recursively in the tag that has the given tag name.
pub fn (tag &Tag) get_tags(name string) []&Tag {
	mut res := []&Tag{}
	for child in tag.children {
		if child.name == name {
			res << child
		}
		res << child.get_tags(name)
	}
	return res
}

// get_tags_by_attribute retrieves all the child tags recursively in the tag that has the given attribute name.
pub fn (tag &Tag) get_tags_by_attribute(name string) []&Tag {
	mut res := []&Tag{}
	for child in tag.children {
		if child.attributes[name] != '' {
			res << child
		}
		res << child.get_tags_by_attribute(name)
	}
	return res
}

// get_tags_by_attribute_value retrieves all the child tags recursively in the tag that has the given attribute name and value.
pub fn (tag &Tag) get_tags_by_attribute_value(name string, value string) []&Tag {
	mut res := []&Tag{}
	for child in tag.children {
		if child.attributes[name] == value {
			res << child
		}
		res << child.get_tags_by_attribute_value(name, value)
	}
	return res
}
