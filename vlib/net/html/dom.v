module html

import os

pub struct DocumentObjectModel {
mut:
	root           &Tag
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

[if debug]
fn (mut dom DocumentObjectModel) print_debug(data string) {
	$if debug {
		if data.len > 0 {
			dom.debug_file.writeln(data)
		}
	}
}

/*
fn (dom mut DocumentObjectModel) new_root(tag &Tag) {
	mut new_tag := &Tag{} new_tag.name = "div"
	new_tag.add_child(dom.root) new_tag.add_child(tag)
	dom.root = new_tag
}
*/
fn is_close_tag(tag &Tag) bool {
	if tag.name.len > 0 {
		return tag.name[0] == 47 // return if equals to /
	}
	return false
}

fn (mut dom DocumentObjectModel) where_is(item_name, attribute_name string) int {
	if !(attribute_name in dom.attributes) {
		temp_array := []string{}
		dom.attributes[attribute_name] = temp_array
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
	for attribute_name in tag.attributes.keys() {
		attribute_value := tag.attributes[attribute_name]
		location := dom.where_is(attribute_value, attribute_name)
		if !(attribute_name in dom.tag_attributes) {
			dom.tag_attributes[attribute_name] = []
		}
		for {
			mut temp_array := dom.tag_attributes[attribute_name]
			temp_array << []&Tag{}
			dom.tag_attributes[attribute_name] = temp_array
			if location < dom.tag_attributes[attribute_name].len + 1 {
				break
			}
		}
		mut temp_array := dom.tag_attributes[attribute_name][location]
		temp_array << tag
		dom.tag_attributes[attribute_name][location] = temp_array
	}
}

fn (mut dom DocumentObjectModel) add_tag_by_type(tag &Tag) {
	tag_name := tag.name
	if !(tag_name in dom.tag_type) {
		dom.tag_type[tag_name] = [tag]
	} else {
		mut temp_array := dom.tag_type[tag_name]
		temp_array << tag
		dom.tag_type[tag_name] = temp_array
	}
}

fn (mut dom DocumentObjectModel) add_tag_by_attribute(tag &Tag) {
	for attribute_name in tag.attributes.keys() {
		if !(attribute_name in dom.all_attributes) {
			dom.all_attributes[attribute_name] = [tag]
		} else {
			mut temp_array := dom.all_attributes[attribute_name]
			temp_array << tag
			dom.all_attributes[attribute_name] = temp_array
		}
	}
}

fn compare_string(a, b string) bool { // for some reason == doesn't work
	if a.len != b.len {
		return false
	}
	for i := 0; i < a.len; i++ {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

fn (mut dom DocumentObjectModel) construct(tag_list []Tag_ptr) {
	dom.constructed = true
	mut temp_map := map[string]int{}
	mut temp_int := C.INT_MIN
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
			temp_string = tag.name[1..tag.name.len] // print(temp_string + " != " + tag_list[temp_int].name + " >> ") // println(temp_string != tag_list[temp_int].name)
			for !stack.is_null(temp_int) &&
				!compare_string(temp_string, tag_list[temp_int].name) && !tag_list[temp_int].closed {
				dom.print_debug(temp_string + ' >> ' + tag_list[temp_int].name + ' ' +
					compare_string(temp_string, tag_list[temp_int].name).str())
				stack.pop()
				temp_int = stack.peek()
			}
			temp_int = stack.peek()
			if !stack.is_null(temp_int) {
				temp_int = stack.pop()
			} else {
				temp_int = root_index
			}
			if stack.is_null(temp_int) {
				stack.push(root_index)
			}
			dom.print_debug('Removed ' + temp_string + ' -- ' + tag_list[temp_int].name)
		} else if tag.name.len > 0 {
			dom.add_tag_attribute(tag) // error here
			dom.add_tag_by_attribute(tag)
			dom.add_tag_by_type(tag)
			dom.all_tags << tag
			temp_int = stack.peek()
			if !stack.is_null(temp_int) {
				dom.btree.move_pointer(temp_map[temp_int.str()])
				temp_map[index.str()] = dom.btree.add_children(tag)
				mut temp_tag := tag_list[temp_int]
				position_in_parent := temp_tag.add_child(tag) // tag_list[temp_int] = temp_tag
				tag.add_parent(temp_tag, position_in_parent)
				/*
				dom.print_debug("Added ${tag.name} as child of '" + tag_list[temp_int].name +
					"' which now has ${dom.btree.get_children().len} childrens")
				*/
				dom.print_debug("Added $tag.name as child of '" + temp_tag.name +
					"' which now has $temp_tag.get_children().len childrens")
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

pub fn (mut dom DocumentObjectModel) get_by_attribute_value(name, value string) []Tag_ptr {
	location := dom.where_is(value, name)
	if dom.tag_attributes[name].len > location {
		return dom.tag_attributes[name][location]
	}
	return []&Tag{}
}

pub fn (dom DocumentObjectModel) get_by_tag(name string) []Tag_ptr {
	if name in dom.tag_type {
		return dom.tag_type[name]
	}
	return []&Tag{}
}

pub fn (dom DocumentObjectModel) get_by_attribute(name string) []Tag_ptr {
	if name in dom.all_attributes {
		return dom.all_attributes[name]
	}
	return []&Tag{}
}

pub fn (dom DocumentObjectModel) get_root() &Tag {
	return dom.root
}

pub fn (dom DocumentObjectModel) get_all_tags() []Tag_ptr {
	return dom.all_tags
}

/*pub fn (dom DocumentObjectModel) get_xpath() XPath {
	return XPath{
		dom: dom
	}
}*/
