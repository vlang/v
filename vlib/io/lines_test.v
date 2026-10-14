module io

// ChunkReader serves its input in small fixed chunks, forcing the
// BufferedReader to refill mid-line.
struct ChunkReader {
mut:
	chunks [][]u8
}

fn (mut c ChunkReader) read(mut buf []u8) !int {
	if c.chunks.len == 0 {
		return Eof{}
	}
	chunk := c.chunks[0]
	c.chunks.delete(0)
	return copy(mut buf, chunk)
}

fn collect_lines(mut it LineIterator) []string {
	mut got := []string{}
	for {
		got << it.next() or { break }
	}
	return got
}

fn test_lines_iterator_across_buffer_refills() {
	mut cr := ChunkReader{
		chunks: ['ab\nc'.bytes(), 'd\nef'.bytes(), '\ngh\n'.bytes()]
	}
	mut br := new_buffered_reader(reader: cr, cap: 8)
	defer {
		br.free()
	}
	mut it := br.lines()
	assert collect_lines(mut it) == ['ab', 'cd', 'ef', 'gh']
}

fn test_lines_iterator_last_line_without_newline() {
	mut cr := ChunkReader{
		chunks: ['one\ntwo'.bytes()]
	}
	mut br := new_buffered_reader(reader: cr, cap: 8)
	defer {
		br.free()
	}
	mut it := br.lines()
	assert collect_lines(mut it) == ['one', 'two']
}

fn test_lines_iterator_crlf_and_empty_input() {
	mut cr := ChunkReader{
		chunks: ['a\r\nb\r\n'.bytes()]
	}
	mut br := new_buffered_reader(reader: cr, cap: 8)
	defer {
		br.free()
	}
	mut it := br.lines()
	assert collect_lines(mut it) == ['a', 'b']

	mut empty := ChunkReader{}
	mut bre := new_buffered_reader(reader: empty, cap: 8)
	defer {
		bre.free()
	}
	mut ite := bre.lines()
	assert collect_lines(mut ite) == []
}
