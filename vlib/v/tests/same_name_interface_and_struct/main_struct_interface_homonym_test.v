module main

import streamreader

struct Reader {
	claim string
}

fn test_main_struct_does_not_replace_imported_interface_box_fields() {
	_ := Reader{'local struct'}
	mut reader := streamreader.Reader(streamreader.new_byte_source('hello world'))
	assert reader is streamreader.ByteSource
	mut buf := []u8{len: 5}
	assert reader.read(mut buf)! == 5
	assert buf.bytestr() == 'hello'
	assert streamreader.read_all_through(mut reader) == ' world'
}
