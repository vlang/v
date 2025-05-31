module html

import os

// The W3C Document Object Model (DOM) is a platform and language-neutral
// interface that allows programs and scripts to dynamically access and
// update the content, structure, and style of a document.
//
// https://www.w3.org/TR/WD-DOM/introduction.html
pub struct DocumentObjectModel {
mut:
	root           &Tag = unsafe { nil }
	constructed    bool
	btree          BTree
	all_tags       []&Tag
	all_attributes map[string][]&Tag
	close_tags     map[string]bool // add a counter to see count how many times is closed and parse correctly
	attributes     map[string][]string
	tag_attributes map[string][][]&Tag
	tag_type       map[string][]&Tag
	debug_file     os.File
}

@[params]
pub struct GetTagsOptions {
pub:
	name string
}

@[if debug_html ?]
fn (mut dom DocumentObjectModel) print_debug(data string) {
	if data.len > 0 {
		dom.debug_file.writeln(data) or { eprintln(err) }
	}
}

@[inline]
fn is_close_tag(tag &Tag) bool {
	return tag.name != '' && tag.name[0] == `/`
}

fn (mut dom DocumentObjectModel) where_is(item_name string, attribute_name string) int {
	if attribute_name !in dom.attributes {
		dom.attributes[attribute_name] = []string{}
	}
	mut string_array := dom.attributes[attribute_name]
	mut counter := 0
	for value in string_array {
		if value == item_name {
			return counter
		}
		counter++
	}
	string_array << item_name
	dom.attributes[attribute_name] = string_array
	return string_array.len - 1
}

fn (mut dom DocumentObjectModel) add_tag_attribute(tag &Tag) {
	for attribute_name, _ in tag.attributes {
		attribute_value := tag.attributes[attribute_name]
		location := dom.where_is(attribute_value, attribute_name)
		if attribute_name !in dom.tag_attributes {
			dom.tag_attributes[attribute_name] = []
		}
		for {
			mut temp_array := unsafe { dom.tag_attributes[attribute_name] }
			temp_array << []&Tag{}
			dom.tag_attributes[attribute_name] = temp_array
			if location < unsafe { dom.tag_attributes[attribute_name].len } + 1 {
				break
			}
		}
		mut temp_array := unsafe { dom.tag_attributes[attribute_name][location] }
		temp_array << tag
		dom.tag_attributes[attribute_name][location] = temp_array
	}
}

fn (mut dom DocumentObjectModel) add_tag_by_type(tag &Tag) {
	tag_name := tag.name
	if tag_name !in dom.tag_type {
		dom.tag_type[tag_name] = [tag]
	} else {
		mut temp_array := unsafe { dom.tag_type[tag_name] }
		temp_array << tag
		dom.tag_type[tag_name] = temp_array
	}
}

fn (mut dom DocumentObjectModel) add_tag_by_attribute(tag &Tag) {
	for attribute_name in tag.attributes.keys() {
		if attribute_name !in dom.all_attributes {
			dom.all_attributes[attribute_name] = [tag]
		} else {
			mut temp_array := unsafe { dom.all_attributes[attribute_name] }
			temp_array << tag
			dom.all_attributes[attribute_name] = temp_array
		}
	}
}

fn (mut dom DocumentObjectModel) construct(tag_list []&Tag) {
	dom.constructed = true

	// If there are no tags, accessing `tag_list` below does panic.
	if tag_list.len == 0 {
		return
	}

	mut temp_map := map[string]int{}
	mut temp_int := null_element
	mut temp_string := ''
	mut stack := Stack{}
	dom.btree = BTree{}
	dom.root = tag_list[0]
	dom.all_tags = [tag_list[0]]
	temp_map['0'] = dom.btree.add_children(tag_list[0])
	stack.push(0)
	root_index := 0

	for index := 1; index < tag_list.len; index++ {
		mut tag := tag_list[index]
		dom.print_debug(tag.str())
		if is_close_tag(tag) {
			temp_int = stack.peek()
			temp_string = tag.name[1..]
			for !is_null(temp_int) && temp_string != tag_list[temp_int].name
				&& !tag_list[temp_int].closed {
				dom.print_debug(temp_string + ' >> ' + tag_list[temp_int].name + ' ' +
					(temp_string == tag_list[temp_int].name).str())
				stack.pop()
				temp_int = stack.peek()
			}
			temp_int = stack.peek()
			temp_int = if !is_null(temp_int) { stack.pop() } else { root_index }
			if is_null(temp_int) {
				stack.push(root_index)
			}
			dom.print_debug('Removed ' + temp_string + ' -- ' + tag_list[temp_int].name)
		} else if tag.name != '' {
			dom.add_tag_attribute(tag) // error here
			dom.add_tag_by_attribute(tag)
			dom.add_tag_by_type(tag)
			dom.all_tags << tag
			temp_int = stack.peek()
			if !is_null(temp_int) {
				dom.btree.move_pointer(temp_map[temp_int.str()])
				temp_map[index.str()] = dom.btree.add_children(tag)
				mut temp_tag := tag_list[temp_int]
				position_in_parent := temp_tag.add_child(tag) // tag_list[temp_int] = temp_tag
				tag.add_parent(temp_tag, position_in_parent)
				/*
				dom.print_debug("Added ${tag.name} as child of '" + tag_list[temp_int].name +
					"' which now has ${dom.btree.get_children().len} childrens")
				*/
				dom.print_debug("Added ${tag.name} as child of '" + temp_tag.name +
					"' which now has ${temp_tag.children.len} childrens")
			} else { // dom.new_root(tag)
				stack.push(root_index)
			}
			temp_string = '/' + tag.name
			if temp_string in dom.close_tags && !tag.closed { // if tag ends with />
				dom.print_debug('Pushed ' + temp_string)
				stack.push(index)
			}
		}
	} // println(tag_list[root_index]) for debug purposes
	dom.root = tag_list[0]
}

// get_root returns the root of the document.
pub fn (dom &DocumentObjectModel) get_root() &Tag {
	return dom.root
}

// get_tags returns all tags stored in the document.
pub fn (dom &DocumentObjectModel) get_tags(options GetTagsOptions) []&Tag {
	if options.name != '' {
		return if options.name in dom.tag_type {
			unsafe { dom.tag_type[options.name] }
		} else {
			[]&Tag{}
		}
	}
	return dom.all_tags
}

// get_tags_by_class_name retrieves all tags recursively in the document root that have the given class name(s).
pub fn (dom &DocumentObjectModel) get_tags_by_class_name(names ...string) []&Tag {
	return dom.root.get_tags_by_class_name(...names)
}

// get_tags_by_attribute retrieves all tags in the document that have the given attribute name.
pub fn (dom &DocumentObjectModel) get_tags_by_attribute(name string) []&Tag {
	return if name in dom.all_attributes { unsafe { dom.all_attributes[name] } } else { []&Tag{} }
}

// get_tags_by_attribute_value retrieves all tags in the document that have the given attribute name and value.
pub fn (mut dom DocumentObjectModel) get_tags_by_attribute_value(name string, value string) []&Tag {
	location := dom.where_is(value, name)
	attributes := unsafe { dom.tag_attributes[name] }
	if attributes.len > location {
		return attributes[location]
	}
	return []
}
