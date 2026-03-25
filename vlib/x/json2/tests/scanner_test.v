import io
import x.json2 as json

struct ChunkedReader {
	data       []u8
	chunk_size int
mut:
	pos int
}

fn (mut r ChunkedReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	mut n := r.chunk_size
	remaining := r.data.len - r.pos
	if n > remaining {
		n = remaining
	}
	if n > buf.len {
		n = buf.len
	}
	read := copy(mut buf[..n], r.data[r.pos..r.pos + n])
	r.pos += read
	return read
}

fn test_public_scanner_iterates_tokens() {
	mut scanner := json.new_scanner('{"items":[1,true,null,"x"]}')
	mut got := []string{}
	for {
		token := scanner.next()!
		got << '${token.kind}:${token.literal()}'
		if token.is_eof() {
			break
		}
	}
	assert got == [
		'lcbr:',
		'str:items',
		'colon:',
		'lsbr:',
		'int:1',
		'comma:',
		'bool:true',
		'comma:',
		'null:null',
		'comma:',
		'str:x',
		'rsbr:',
		'rcbr:',
		'eof:',
	]
}

fn test_reader_scanner_iterates_tokens() {
	mut reader := ChunkedReader{
		data:       '[10,true,"chunked",null]'.bytes()
		chunk_size: 2
	}
	mut scanner := json.new_reader_scanner(reader: reader, buffer_size: 3)
	defer {
		scanner.free()
	}
	mut got := []string{}
	for {
		token := scanner.next()!
		got << '${token.kind}:${token.literal()}'
		if token.is_eof() {
			break
		}
	}
	assert got == [
		'lsbr:',
		'int:10',
		'comma:',
		'bool:true',
		'comma:',
		'str:chunked',
		'comma:',
		'null:null',
		'rsbr:',
		'eof:',
	]
}

fn test_reader_scanner_reports_errors() {
	mut reader := ChunkedReader{
		data:       r'["\z"]'.bytes()
		chunk_size: 1
	}
	mut scanner := json.new_reader_scanner(reader: reader, buffer_size: 1)
	defer {
		scanner.free()
	}
	assert scanner.next()!.kind == .lsbr
	scanner.next() or {
		assert err.msg().contains('invalid backslash escape')
		return
	}
	assert false
}
