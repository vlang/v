module main

import io as csv
import csvrows

// Here `csv.Reader` is the `io.Reader` interface, reached through an import
// alias, while `encoding.csv.Reader` (a struct) is loaded by `csvrows`. The
// alias must still resolve to the interface.
interface ReadCloser {
	csv.Reader
mut:
	close()
}

struct Src {
mut:
	data   []u8
	pos    int
	closed bool
}

fn (mut s Src) read(mut buf []u8) !int {
	if s.pos >= s.data.len {
		return csv.Eof{}
	}
	n := copy(mut buf, s.data[s.pos..])
	s.pos += n
	return n
}

fn (mut s Src) close() {
	s.closed = true
}

fn drain(mut r csv.Reader) string {
	mut out := []u8{}
	mut buf := []u8{len: 3}
	for {
		n := r.read(mut buf) or { break }
		out << buf[..n]
	}
	return out.bytestr()
}

fn as_reader(r ReadCloser) csv.Reader {
	return r
}

fn test_io_alias_named_like_a_loaded_csv_module() {
	assert csvrows.first_row('a,b\n') == ['a', 'b']
	mut s := Src{
		data: 'hello'.bytes()
	}
	assert drain(mut s) == 'hello'
	mut src := &Src{
		data: 'xy'.bytes()
	}
	mut rc := ReadCloser(src)
	mut r := as_reader(rc)
	assert drain(mut r) == 'xy'
	rc.close()
	assert src.closed
	mut br := csv.new_buffered_reader(
		reader: Src{
			data: 'l1\nl2\n'.bytes()
		}
	)
	assert br.read_line()! == 'l1'
}
