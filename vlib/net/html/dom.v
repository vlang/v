module html

import os
import strings
import datatypes

// Keep eager inner HTML semantics while bounding parser work and nested-content copies.
// Prevents recursive normalization from exhausting the stack.
const html_max_nesting_depth = 256
// Caps the total content rebuilt across the tree.
const html_max_normalized_content_bytes = 64 * 1024 * 1024
// Bounds expansion relative to the source, including small inputs.
const html_max_content_expansion_ratio = 256

struct NormalizedContentSize {
	inner_html i64
	serialized i64
}

// The W3C Document Object Model (DOM) is a platform and language-neutral
// interface that allows programs and scripts to dynamically access and
// update the content, structure, and style of a document.
//
// https://www.w3.org/TR/WD-DOM/introduction.html
pub struct DocumentObjectModel {
mut:
	root                    &Tag = unsafe { nil }
	constructed             bool
	btree                   BTree
	all_tags                []&Tag
	all_attributes          map[string][]&Tag
	close_tags              map[string]bool // add a counter to see count how many times is closed and parse correctly
	attribute_value_indexes map[string]&datatypes.BTree[string, int]
	tag_attributes          map[string][][]&Tag
	tag_type                map[string][]&Tag
	debug_file              os.File
	parse_error_msg         string
	source_bytes            i64
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

fn is_element_tag(tag &Tag) bool {
	return tag.name != '' && !is_close_tag(tag) && !tag.is_synthetic_text() && !is_doctype(tag)
}

fn is_doctype(tag &Tag) bool {
	return tag.name == '!doctype' || tag.name.starts_with('!doctype ')
}

fn (mut dom DocumentObjectModel) intern_attribute_value(item_name string, attribute_name string) int {
	mut value_indexes := dom.attribute_value_indexes[attribute_name] or {
		datatypes.new_btree[string, int]()
	}
	if location := value_indexes.get(item_name) {
		return location
	}
	location := value_indexes.len()
	value_indexes.set(item_name, location)
	dom.attribute_value_indexes[attribute_name] = value_indexes
	return location
}

fn (dom &DocumentObjectModel) lookup_attribute_value(item_name string, attribute_name string) ?int {
	value_indexes := dom.attribute_value_indexes[attribute_name] or { return none }
	return value_indexes.get(item_name)
}

fn (mut dom DocumentObjectModel) reject_parse(message string) {
	dom = DocumentObjectModel{
		root:            &Tag{}
		constructed:     true
		parse_error_msg: message
		source_bytes:    dom.source_bytes
	}
}

fn (dom &DocumentObjectModel) normalized_content_within_limit(mut sizes map[&Tag]NormalizedContentSize) bool {
	max_content_bytes := i64(html_max_normalized_content_bytes)
	content_budget := if dom.source_bytes >= max_content_bytes / html_max_content_expansion_ratio {
		max_content_bytes
	} else {
		dom.source_bytes * html_max_content_expansion_ratio
	}
	mut total_content_bytes := i64(0)
	for index := dom.all_tags.len - 1; index >= 0; index-- {
		tag := dom.all_tags[index]
		mut inner_html_size := i64(tag.content.len)
		if tag.children.len > 0 && inner_html_size > content_budget {
			return false
		}
		for child in tag.children {
			child_size := sizes[child] or { return false }
			if child_size.serialized > content_budget - inner_html_size {
				return false
			}
			inner_html_size += child_size.serialized
		}
		if tag.children.len > 0 {
			if inner_html_size > content_budget - total_content_bytes {
				return false
			}
			total_content_bytes += inner_html_size
		}
		mut serialized_size := inner_html_size
		if tag.is_synthetic_text() {
			if serialized_size > content_budget {
				serialized_size = content_budget + 1
			}
			sizes[tag] = NormalizedContentSize{
				inner_html: inner_html_size
				serialized: serialized_size
			}
			continue
		}
		if serialized_size > content_budget {
			sizes[tag] = NormalizedContentSize{
				inner_html: inner_html_size
				serialized: content_budget + 1
			}
			continue
		}
		open_size := tag_open_size(tag)
		if open_size > content_budget - serialized_size {
			serialized_size = content_budget + 1
		} else {
			serialized_size += open_size
			if !tag.closed || tag.close_type == .new_tag {
				close_size := i64(tag.name.len) + 3
				if close_size > content_budget - serialized_size {
					serialized_size = content_budget + 1
				} else {
					serialized_size += close_size
				}
			}
		}
		sizes[tag] = NormalizedContentSize{
			inner_html: inner_html_size
			serialized: serialized_size
		}
	}
	return true
}

fn tag_open_size(tag &Tag) i64 {
	mut size := i64(tag.name.len) + 1
	for key, value in tag.attributes {
		size += i64(key.len) + 1
		if value != '' {
			size += i64(value.len) + 3
		}
	}
	if tag.closed && tag.close_type == .in_name {
		size += 2
	} else {
		size++
	}
	return size
}

fn (mut dom DocumentObjectModel) add_tag_attribute(tag &Tag) {
	for attribute_name, attribute_value in tag.attributes {
		location := dom.intern_attribute_value(attribute_value, attribute_name)
		mut attribute_tags := dom.tag_attributes[attribute_name] or { [][]&Tag{} }
		for attribute_tags.len <= location {
			attribute_tags << []&Tag{}
		}
		attribute_tags[location] << tag
		dom.tag_attributes[attribute_name] = attribute_tags
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
	// the root never passes through the loop below, so index it like
	// every other tag, or no lookup can ever find it
	if !is_close_tag(tag_list[0]) && tag_list[0].name != '' {
		dom.add_tag_attribute(tag_list[0])
		dom.add_tag_by_attribute(tag_list[0])
		dom.add_tag_by_type(tag_list[0])
	}

	for index := 1; index < tag_list.len; index++ {
		mut tag := tag_list[index]
		dom.print_debug(tag.str())
		if is_close_tag(tag) {
			temp_int = stack.peek()
			temp_string = tag.name[1..]
			old_stack_size := stack.size
			for !is_null(temp_int) && temp_string != tag_list[temp_int].name
				&& !tag_list[temp_int].closed {
				dom.print_debug(temp_string + ' >> ' + tag_list[temp_int].name + ' ' +
					(temp_string == tag_list[temp_int].name).str())
				stack.pop()
				temp_int = stack.peek()
			}
			if is_null(temp_int) || temp_string != tag_list[temp_int].name {
				stack.size = old_stack_size
				continue
			}
			temp_int = stack.peek()
			temp_int = if !is_null(temp_int) { stack.pop() } else { root_index }
			if is_null(temp_int) {
				stack.push(root_index)
			}
			dom.print_debug('Removed ' + temp_string + ' -- ' + tag_list[temp_int].name)
		} else if tag.name != '' {
			if is_element_tag(tag) {
				mut ancestor_element_depth := 0
				for stack_index in 0 .. stack.size {
					ancestor := tag_list[stack.elements[stack_index]]
					if is_element_tag(ancestor) {
						ancestor_element_depth++
					}
				}
				if ancestor_element_depth >= html_max_nesting_depth {
					dom.reject_parse('HTML element nesting exceeds the maximum depth of ${html_max_nesting_depth}.')
					return
				}
			}
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
			if temp_string in dom.close_tags && !tag.closed && is_element_tag(tag) {
				dom.print_debug('Pushed ' + temp_string)
				stack.push(index)
			}
		}
	} // println(tag_list[root_index]) for debug purposes
	dom.root = tag_list[0]
	mut normalized_sizes := map[&Tag]NormalizedContentSize{}
	if !dom.normalized_content_within_limit(mut normalized_sizes) {
		dom.reject_parse('HTML normalized content exceeds the maximum size allowed for this input.')
		return
	}
	mut root := dom.root
	dom.normalize_tag_content(mut root, normalized_sizes)
}

fn (mut dom DocumentObjectModel) normalize_tag_content(mut tag &Tag, sizes map[&Tag]NormalizedContentSize) {
	tag.text_content = tag.content
	tag.content_is_inner_html = true
	if tag.children.len == 0 {
		return
	}
	content_size := sizes[tag] or { return }
	mut inner_html := strings.new_builder(int(content_size.inner_html))
	inner_html.write_string(tag.text_content)
	for idx := 0; idx < tag.children.len; idx++ {
		mut child := tag.children[idx]
		dom.normalize_tag_content(mut child, sizes)
		child.write_str(mut inner_html)
	}
	tag.content = inner_html.str()
}

// get_root returns the root of the document.
pub fn (dom &DocumentObjectModel) get_root() &Tag {
	return dom.root
}

// parse_error returns the reason the DOM could not be parsed, or none on success.
pub fn (dom &DocumentObjectModel) parse_error() ?string {
	if dom.parse_error_msg == '' {
		return none
	}
	return dom.parse_error_msg
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
	if names.len == 0 || dom.all_tags.len == 0 {
		return []&Tag{}
	}
	mut res := []&Tag{}
	// the tag level search only looks at children, so consider the root itself here
	mut matched := true
	for name in names {
		matched = dom.root.class_set.exists(name)
		if !matched {
			break
		}
	}
	if matched {
		res << dom.root
	}
	res << dom.root.get_tags_by_class_name(...names)
	return res
}

// get_tags_by_attribute retrieves all tags in the document that have the given attribute name.
pub fn (dom &DocumentObjectModel) get_tags_by_attribute(name string) []&Tag {
	return if name in dom.all_attributes { unsafe { dom.all_attributes[name] } } else { []&Tag{} }
}

// get_tags_by_attribute_value retrieves all tags in the document that have the given attribute name and value.
pub fn (mut dom DocumentObjectModel) get_tags_by_attribute_value(name string, value string) []&Tag {
	location := dom.lookup_attribute_value(value, name) or { return []&Tag{} }
	attributes := dom.tag_attributes[name] or { return []&Tag{} }
	if attributes.len > location {
		return attributes[location]
	}
	return []
}
