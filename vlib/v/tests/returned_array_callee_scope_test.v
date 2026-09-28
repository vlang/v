import os

fn identity_text(path string) string { return path }

fn split_text(path string) []string { return identity_text(path).split_into_lines() }

fn read_text(path string) string { return os.read_file(path) or { panic(err) } }

fn read_lines(path string) []string { return read_text(path).split_into_lines() }

fn test_returned_array_uses_callee_scope() {
	path := 42
	mut values := split_text('one\ntwo')
	values << 'three'
	assert values == ['one', 'two', 'three']
	assert path == 42
	mut source := read_lines(@FILE)
	source << 'extra line'
	assert source.len > 2
}
