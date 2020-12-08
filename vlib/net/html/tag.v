module html

import strings

enum CloseTagType {
	in_name
	new_tag
}

// An object that holds tags information, such as `name`, `attributes`, `children`.
[ref_only]
pub struct Tag {
pub mut:
	name               string
	content            string
	children           []&Tag
	attributes         map[string]string // attributes will be like map[name]value
	last_attribute     string
	parent             &Tag = 0
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

// Returns the content of the tag and all tags inside it.
// Also, any `<br>` tag will be converted into `\n`.
pub fn (tag Tag) text() string {
	if tag.name.len >= 2 && tag.name[0..2] == 'br' {
		return '\n'
	}
	mut to_return := tag.content.replace('\n', '')
	for index := 0; index < tag.children.len; index++ {
		to_return += tag.children[index].text()
	}
	return to_return
}

pub fn (tag &Tag) str() string {
	mut html_str := strings.new_builder(200)
	html_str.write('<$tag.name')
	for key, value in tag.attributes {
		html_str.write(' $key')
		if value.len > 0 {
			html_str.write('="$value"')
		}
	}
	html_str.write(if tag.closed && tag.close_type == .in_name { '/>' } else { '>' })
	html_str.write(tag.content)
	if tag.children.len > 0 {
		for child in tag.children {
			html_str.write(child.str())
		}
	}
	if !tag.closed || tag.close_type == .new_tag {
		html_str.write('</$tag.name>')
	}
	return html_str.str()
}
