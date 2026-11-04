module strings

import io

fn test_reader() {
	mut r := new_reader('hello')
	mut buf := []u8{len: 3}
	n := r.read(mut buf) or { 0 }
	assert n == 3
	assert buf.bytestr() == 'hel'
	assert r.len() == 2
	r.reset('world')
	assert r.len() == 5
}
