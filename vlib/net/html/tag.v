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
	is_element_node       bool
	is_text_node          bool
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

fn (tag &Tag) is_synthetic_text() bool {
	return tag.is_text_node || (tag.name == 'text' && !tag.is_element_node)
}

struct TagWalkFrame {
	tag &Tag
mut:
	next_child int
}

struct TagWalker {
mut:
	frames []TagWalkFrame
	active map[&Tag]bool
}

fn new_tag_walker(root &Tag) TagWalker {
	mut active := map[&Tag]bool{}
	active[root] = true
	return TagWalker{
		frames: [TagWalkFrame{
			tag: root
		}]
		active: active
	}
}

fn (mut walker TagWalker) next() ?&Tag {
	for walker.frames.len > 0 {
		frame_index := walker.frames.len - 1
		if walker.frames[frame_index].next_child >= walker.frames[frame_index].tag.children.len {
			finished := walker.frames[frame_index].tag
			walker.active.delete(finished)
			_ = walker.frames.pop()
			continue
		}
		child := walker.frames[frame_index].tag.children[walker.frames[frame_index].next_child]
		walker.frames[frame_index].next_child++
		if child in walker.active {
			continue
		}
		walker.active[child] = true
		walker.frames << TagWalkFrame{
			tag: child
		}
		return child
	}
	return none
}

fn (mut walker TagWalker) skip_current_children() {
	if walker.frames.len == 0 {
		return
	}
	frame_index := walker.frames.len - 1
	walker.frames[frame_index].next_child = walker.frames[frame_index].tag.children.len
}

struct TagSerializationEvent {
	tag        &Tag
	is_closing bool
}

// text returns the text contents of the tag and its children.
// A br element (including the parser's br/ form) contributes a newline.
pub fn (tag &Tag) text() string {
	if tag.is_synthetic_text() {
		return tag.leading_text()
	}
	// Only the line-break element maps to a newline: a prefix match would
	// also catch custom elements like `bracket` and drop their content.
	// `br/` is the name the parser currently keeps for a self-closing
	// `<br/>` (see #30139); accept it until that is normalized to `br`.
	if tag.name == 'br' || tag.name == 'br/' {
		return '\n'
	}
	mut text_str := strings.new_builder(200)
	text_str.write_string(tag.leading_text())
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if child.is_synthetic_text() {
			text_str.write_string(child.leading_text())
			walker.skip_current_children()
			continue
		}
		if child.name == 'br' || child.name == 'br/' {
			text_str.write_string('\n')
			walker.skip_current_children()
			continue
		}
		text_str.write_string(child.leading_text())
	}
	return text_str.str()
}

fn (tag &Tag) leading_text() string {
	if tag.text_content.len > 0 {
		return tag.text_content
	}
	if !tag.content_is_inner_html || tag.children.len == 0 || tag.is_synthetic_text() {
		return tag.content
	}
	return ''
}

// str serializes the tag and its children without recursive calls.
pub fn (tag &Tag) str() string {
	mut html_str := strings.new_builder(200)
	tag.write_str(mut html_str)
	return html_str.str()
}

fn (tag &Tag) write_str(mut html_str strings.Builder) {
	if tag.is_synthetic_text() {
		html_str.write_string(tag.leading_text())
		return
	}
	if tag.content_is_inner_html || tag.children.len == 0 {
		tag.write_opening(mut html_str)
		tag.write_closing(mut html_str)
		return
	}
	mut events := [TagSerializationEvent{
		tag: tag
	}]
	mut active := map[&Tag]bool{}
	for events.len > 0 {
		event := events.pop()
		if event.is_closing {
			event.tag.write_closing(mut html_str)
			active.delete(event.tag)
			continue
		}
		current := event.tag
		if current in active {
			continue
		}
		if current.is_synthetic_text() {
			html_str.write_string(current.leading_text())
			continue
		}
		active[current] = true
		current.write_opening(mut html_str)
		if !current.content_is_inner_html && current.children.len > 0 {
			events << TagSerializationEvent{
				tag:        current
				is_closing: true
			}
			for index := current.children.len - 1; index >= 0; index-- {
				child := current.children[index]
				if child !in active {
					events << TagSerializationEvent{
						tag: child
					}
				}
			}
		} else {
			current.write_closing(mut html_str)
			active.delete(current)
		}
	}
}

fn (tag &Tag) write_opening(mut html_str strings.Builder) {
	html_str.write_string('<${tag.name}')
	for key, value in tag.attributes {
		html_str.write_string(' ${key}')
		if value != '' {
			html_str.write_string('="${value}"')
		}
	}
	html_str.write_string(if tag.closed && tag.close_type == .in_name { '/>' } else { '>' })
	html_str.write_string(tag.content)
}

fn (tag &Tag) write_closing(mut html_str strings.Builder) {
	if !tag.closed || tag.close_type == .new_tag {
		html_str.write_string('</${tag.name}>')
	}
}

// get_tag retrieves the first found child tag in the tag that has the given tag name.
pub fn (tag &Tag) get_tag(name string) ?&Tag {
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if child.name == name {
			return child
		}
	}
	return none
}

// get_tags retrieves all child tags recursively in the tag that have the given tag name.
pub fn (tag &Tag) get_tags(name string) []&Tag {
	mut res := []&Tag{}
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if child.name == name {
			res << child
		}
	}
	return res
}

// get_tag_by_attribute retrieves the first found child tag in the tag that has the given attribute name.
pub fn (tag &Tag) get_tag_by_attribute(name string) ?&Tag {
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if name in child.attributes {
			return child
		}
	}
	return none
}

// get_tags_by_attribute retrieves all child tags recursively in the tag that have the given attribute name.
pub fn (tag &Tag) get_tags_by_attribute(name string) []&Tag {
	mut res := []&Tag{}
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if name in child.attributes {
			res << child
		}
	}
	return res
}

// get_tag_by_attribute_value retrieves the first found child tag in the tag that has the given attribute name and value.
pub fn (tag &Tag) get_tag_by_attribute_value(name string, value string) ?&Tag {
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if name in child.attributes && child.attributes[name] == value {
			return child
		}
	}
	return none
}

// get_tags_by_attribute_value retrieves all child tags recursively in the tag that have the given attribute name and value.
pub fn (tag &Tag) get_tags_by_attribute_value(name string, value string) []&Tag {
	mut res := []&Tag{}
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
		if name in child.attributes && child.attributes[name] == value {
			res << child
		}
	}
	return res
}

// get_tag_by_class_name retrieves the first found child tag in the tag that has the given class name(s).
pub fn (tag &Tag) get_tag_by_class_name(names ...string) ?&Tag {
	if names.len == 0 {
		// no class name to match, no tag can match it
		return none
	}
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
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
	}
	return none
}

// get_tags_by_class_name retrieves all child tags recursively in the tag that have the given class name(s).
pub fn (tag &Tag) get_tags_by_class_name(names ...string) []&Tag {
	if names.len == 0 {
		// no class name to match, no tag can match it
		return []&Tag{}
	}
	mut res := []&Tag{}
	mut walker := new_tag_walker(tag)
	for {
		child := walker.next() or { break }
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
	}
	return res
}
