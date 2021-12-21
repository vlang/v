module main

pub interface IObject {
mut:
	do_stuff()
	do_something_with_int(int)
	do_something_with_string(string)
}

pub struct BaseObject {
mut:
	some_attr int
}

pub fn (mut base BaseObject) do_stuff() {}

pub fn (mut base BaseObject) do_something_with_int(n int) {}

pub fn (mut base BaseObject) do_something_with_string(s string) {}

pub fn (mut base BaseObject) method_thats_available_to_all_object() {}

pub struct GameObject {
	BaseObject
pub mut:
	some_attr_for_game int
}

fn test_interface_method_using_struct_embed() {
	mut common_object := []IObject{}
	common_object << GameObject{}
	println(common_object)
	assert common_object.len == 1
}
