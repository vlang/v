module main

import custombuilder
import strings

fn test_imported_builder_keeps_its_own_methods() {
	returned := custombuilder.new()
	explicit := &custombuilder.Builder{}
	assert returned.write_string('x') == 43
	assert explicit.write_string('xyz') == 45
	assert custombuilder.from_pointer(voidptr(returned)) == 44
	assert returned.str() == 'custom-builder'
}

fn test_standard_strings_builder_control() {
	mut standard := strings.new_builder(10)
	standard.write_string('hello')
	standard.write_string(' world')
	assert standard.str() == 'hello world'
}
