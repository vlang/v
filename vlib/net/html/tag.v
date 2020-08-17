module html

enum CloseTagType {
	in_name
	new_tag
}

[ref_only]
pub struct Tag {
pub mut:
	name               string
	content            string
	children           []&Tag
mut:
	attributes         map[string]string // attributes will be like map[name]value
	last_attribute     string
	parent             &Tag = C.NULL
	position_in_parent int = 0
	closed             bool = false
	close_type         CloseTagType = .in_name
}

fn (mut tag Tag) add_parent(t &Tag, position int) {
	tag.position_in_parent = position
	tag.parent = t
}

fn (mut tag Tag) add_child(t &Tag) int {
	mut children := tag.children
	children << t
	tag.children = children
	return tag.children.len
}

pub fn (tag Tag) get_children() []Tag_ptr {
	return tag.children
}

pub fn (tag Tag) get_parent() &Tag {
	return tag.parent
}

pub fn (tag Tag) get_name() string {
	return tag.name
}

pub fn (tag Tag) get_content() string {
	return tag.content
}

pub fn (tag Tag) get_attributes() map[string]string {
	return tag.attributes
}

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
	mut to_return := '<$tag.name'
	for key in tag.attributes.keys() {
		to_return += ' $key'
		value := tag.attributes[key]
		if value.len > 0 {
			to_return += '=' + '"${tag.attributes[key]}"'
		}
	}
	to_return += if tag.closed && tag.close_type == .in_name { '/>' } else { '>' }
	to_return += '$tag.content'
	if tag.children.len > 0 {
		// println('${tag.name} have ${tag.children.len} childrens')
		for index := 0; index < tag.children.len; index++ {
			to_return += tag.get_children()[index].str()
		}
	}
	if !tag.closed || tag.close_type == .new_tag {
		to_return += '</$tag.name>'
	}
	return to_return
}
