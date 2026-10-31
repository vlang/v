module main

import strings

struct Builder {}

fn (b Builder) write_string(s string) int {
	return 7 + s.len
}

fn (b Builder) str() string {
	return 'main-builder'
}

fn test_main_builder_keeps_its_own_methods() {
	local := Builder{}
	assert local.write_string('x') == 8
	assert local.str() == 'main-builder'
	mut standard := strings.new_builder(10)
	standard.write_string('standard')
	assert standard.str() == 'standard'
}
