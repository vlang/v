module bytes

fn test_reader() {
	mut r := new_reader('hello'.bytes())
	mut buf := []u8{len: 3}
	n := r.read(mut buf)
	assert n == 3
	assert buf.bytestr() == 'hel'
	assert r.len() == 2
	r.reset('world'.bytes())
	assert r.len() == 5
}
