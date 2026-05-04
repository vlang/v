module html

import strings
import datatypes

pub enum CloseTagType {
	in_name
	new_tag
}

// Tag holds the information of an HTML tag.
@[heap]
pub struct Tag {
pub mut:
	name               string
	content            string
	children           []&Tag
	attributes         map[string]string // attributes will be like map[name]value
	last_attribute     string
	class_set          datatypes.Set[string]
	parent             &Tag = unsafe { nil }
	position_in_parent int
	closed             bool
	close_type         CloseTagType = .in_name
mut:
	text_content          string
	content_is_inner_html bool
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
pub fn (tag &Tag) text() string {
	if tag.name == 'text' {
		return tag.leading_text()
	}
	if tag.name.len >= 2 && tag.name[..2] == 'br' {
		return '\n'
	}
	mut text_str := strings.new_builder(200)
	text_str.write_string(tag.leading_text())
	for child in tag.children {
		text_str.write_string(child.text())
	}
	return text_str.str()
}

fn (tag &Tag) leading_text() string {
	if tag.text_content.len > 0 {
		return tag.text_content
	}
	if !tag.content_is_inner_html || tag.children.len == 0 || tag.name == 'text' {
		return tag.content
	}
	return ''
}

pub fn (tag &Tag) str() string {
	if tag.name == 'text' {
		return tag.leading_text()
	}
	mut html_str := strings.new_builder(200)
	html_str.write_string('<${tag.name}')
	for key, value in tag.attributes {
		html_str.write_string(' ${key}')
		if value != '' {
			html_str.write_string('="${value}"')
		}
	}
	html_str.write_string(if tag.closed && tag.close_type == .in_name { '/>' } else { '>' })
	html_str.write_string(tag.content)
	if !tag.content_is_inner_html && tag.children.len > 0 {
		for child in tag.children {
			html_str.write_string(child.str())
		}
	}
	if !tag.closed || tag.close_type == .new_tag {
		html_str.write_string('</${tag.name}>')
	}
	return html_str.str()
}

// get_tag retrieves the first found child tag in the tag that has the given tag name.
pub fn (tag &Tag) get_tag(name string) ?&Tag {
	for child in tag.children {
		if child.name == name {
			return child
		}
		if c := child.get_tag(name) {
			return c
		}
	}
	return none
}

// get_tags retrieves all child tags recursively in the tag that have the given tag name.
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

// get_tag_by_attribute retrieves the first found child tag in the tag that has the given attribute name.
pub fn (tag &Tag) get_tag_by_attribute(name string) ?&Tag {
	for child in tag.children {
		if child.attributes[name] != '' {
			return child
		}
		if c := child.get_tag_by_attribute(name) {
			return c
		}
	}
	return none
}

// get_tags_by_attribute retrieves all child tags recursively in the tag that have the given attribute name.
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

// get_tag_by_attribute_value retrieves the first found child tag in the tag that has the given attribute name and value.
pub fn (tag &Tag) get_tag_by_attribute_value(name string, value string) ?&Tag {
	for child in tag.children {
		if child.attributes[name] == value {
			return child
		}
		if c := child.get_tag_by_attribute_value(name, value) {
			return c
		}
	}
	return none
}

// get_tags_by_attribute_value retrieves all child tags recursively in the tag that have the given attribute name and value.
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

// get_tag_by_class_name retrieves the first found child tag in the tag that has the given class name(s).
pub fn (tag &Tag) get_tag_by_class_name(names ...string) ?&Tag {
	for child in tag.children {
		mut matched := true
		for name in names {
			matched = child.class_set.exists(name)
			if !matched {
				break
			}
		}
		if matched {
			return child
		}
		if c := child.get_tag_by_class_name(...names) {
			return c
		}
	}
	return none
}

// get_tags_by_class_name retrieves all child tags recursively in the tag that have the given class name(s).
pub fn (tag &Tag) get_tags_by_class_name(names ...string) []&Tag {
	mut res := []&Tag{}
	for child in tag.children {
		mut matched := true
		for name in names {
			matched = child.class_set.exists(name)
			if !matched {
				break
			}
		}
		if matched {
			res << child
		}
		res << child.get_tags_by_class_name(...names)
	}
	return res
}
