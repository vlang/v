module main

import rowreader
import streamreader

// Cloning an interface value (`streamreader.Reader`) must not copy the fields
// of a struct with the same short name in another module (`rowreader.Reader`).
fn test_interface_value_clone_ignores_fields_of_a_struct_homonym() {
	mut rows := rowreader.new_reader(['a'])
	assert rows.read_line()! == 'a'
	mut readers := map[string]streamreader.Reader{}
	readers['k'] = streamreader.Reader(streamreader.new_byte_source('abc'))
	mut kept := []streamreader.Reader{}
	if mut r := readers['k'] {
		kept << r
	}
	assert streamreader.read_all_through(mut kept[0]) == 'abc'
}
