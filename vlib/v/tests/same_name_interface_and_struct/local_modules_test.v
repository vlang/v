module main

import rowreader
import streamreader

// https://github.com/vlang/v/issues/28834
// A struct and an interface with the same short name (`Reader`) in different
// modules must not share method signatures: the interface `read` dispatcher
// has to keep its buffer parameter, and the struct `read` stays a plain method.
fn test_interface_and_struct_with_same_short_name_in_different_modules() {
	mut src := streamreader.new_byte_source('hello world')
	assert streamreader.read_all_through(mut src) == 'hello world'
	mut r := rowreader.new_reader(['a,b', '1,2'])
	mut got := [][]string{}
	for {
		row := r.read() or { break }
		got << row
	}
	assert got == [['a', 'b'], ['1', '2']]
	mut lines := rowreader.new_reader(['x', 'y'])
	assert lines.read_line()! == 'x'
	assert lines.read()! == ['y']
}

fn implements_stream_reader[T]() bool {
	$if T is streamreader.Reader {
		return true
	}
	return false
}

fn test_struct_with_interface_short_name_is_not_an_implementer() {
	assert implements_stream_reader[streamreader.ByteSource]()
	assert !implements_stream_reader[rowreader.Reader]()
	mut r := streamreader.Reader(streamreader.new_byte_source('abc'))
	assert r is streamreader.ByteSource
	mut buf := []u8{len: 8}
	assert r.read(mut buf)! == 3
	assert buf[..3].bytestr() == 'abc'
}
