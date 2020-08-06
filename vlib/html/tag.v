module html

[ref_only]
pub struct Tag {
mut:
	name               string = ''
	attributes         map[string]string // attributes will be like map[name]value
	last_attribute     string = ''
	content            string = ''
	children           []&Tag
	parent             &Tag = C.NULL
	position_in_parent int = 0
	closed             bool = false
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

pub fn (tag &Tag) str() string { // add text method to generate a tag text from it content and childs content
	mut to_return := '<$tag.name'
	for key in tag.attributes.keys() {
		to_return += ' $key'
		value := tag.attributes[key]
		if value.len > 0 {
			to_return += '=' + '"${tag.attributes[key]}"'
		}
	}
	to_return += if tag.closed { '/>' } else { '>' }
	to_return += '$tag.content'
	if tag.children.len > 0 {
		// println('${tag.name} have ${tag.children.len} childrens')
		for index := 0; index < tag.children.len; index++ {
			to_return += tag.get_children()[index].str()
		}
	}
	if !tag.closed {
		to_return += '</$tag.name>'
	}
	return to_return
}
