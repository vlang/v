module main

fn test_trim_doc_node_description() {
	mod := 'foo'
	mut readme := '## Description

`foo` is a module that provides tools and utility functions to assist in working with bar.
It also assists with composing and testing baz.'
	expected := 'is a module that provides tools and utility functions to assist in working with'
	res := trim_doc_node_description(mod, readme).trim_space()
	assert res == expected

	readme = '# Foo
`foo` is a module that provides tools and utility functions to assist in working with bar.
It also assists with composing and testing baz.'
	res2 := trim_doc_node_description(mod, readme).trim_space()
	assert res2 == res
}
