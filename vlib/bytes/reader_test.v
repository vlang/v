module bytes

import io

fn test_reader() {
	mut r := new_reader('hello'.bytes())
	mut buf := []u8{len: 3}
	n := r.read(mut buf)!
	assert n == 3
	assert buf.bytestr() == 'hel'
	assert r.len() == 2
	r.reset('world'.bytes())
	assert r.len() == 5
}

fn test_reader_reports_eof_when_exhausted() {
	mut r := new_reader('ab'.bytes())
	mut buf := []u8{len: 1}
	assert r.read(mut buf)! == 1
	assert buf[0] == `a`
	assert r.read(mut buf)! == 1
	assert buf[0] == `b`
	r.read(mut buf) or {
		assert err is io.Eof
		return
	}
	assert false, 'expected io.Eof after Reader is exhausted'
}

fn test_reader_composes_with_multi_reader_and_read_all() {
	mut m := io.multi_reader([io.Reader(new_reader('a'.bytes())), io.Reader(new_reader('b'.bytes()))])
	data := io.read_all(io.ReadAllConfig{ reader: m })!
	assert data.bytestr() == 'ab'
}
