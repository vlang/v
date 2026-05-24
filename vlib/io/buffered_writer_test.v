import io
import rand

fn small_amount() int {
	return int(rand.u8()) + 1
}

struct ArrayWriter {
pub mut:
	result []u8
}

// implement io.Writer
fn (mut aw ArrayWriter) write(buf []u8) !int {
	len := buf.len
	mut res := 0
	for i := 0; i < len; i++ {
		aw.result << buf[i]
		res++
	}
	return res
}

struct ChunkedArrayWriter {
	chunk int
pub mut:
	result []u8
}

fn (mut cw ChunkedArrayWriter) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	n := if buf.len < cw.chunk { buf.len } else { cw.chunk }
	for i := 0; i < n; i++ {
		cw.result << buf[i]
	}
	return n
}

struct FailOnceChunkedWriter {
	chunk int
pub mut:
	result []u8
mut:
	writes int
}

fn (mut fw FailOnceChunkedWriter) write(buf []u8) !int {
	if fw.writes == 1 {
		fw.writes++
		return error('forced write failure')
	}
	fw.writes++
	n := if buf.len < fw.chunk { buf.len } else { fw.chunk }
	for i := 0; i < n; i++ {
		fw.result << buf[i]
	}
	return n
}

struct ZeroProgressWriter {}

fn (mut w ZeroProgressWriter) write(buf []u8) !int {
	return 0
}

// write less than max bytes, returns number of bytes written
// data is written in chunks.
fn write_random_data(mut aw ArrayWriter, mut bw io.BufferedWriter, max int) !int {
	less_than_max := max - 255 // guarantee 1 full u8 less than max
	mut total := 0
	for total < less_than_max {
		r := rand.u8()
		d := rand.bytes(r)!
		w := bw.write(d)!
		total += w
	}
	return total
}

fn test_flush() {
	mut aw := ArrayWriter{}
	max := 65536
	mut bw := io.new_buffered_writer(writer: &aw, cap: max)!

	// write less data than buffer capacity, the underlying writer should receive no data.
	written := write_random_data(mut aw, mut bw, 65536)!
	assert written == bw.buffered()
	assert aw.result.len == 0

	bw.flush()!
	assert aw.result.len == written
	assert bw.buffered() == 0
	assert bw.available() == max
}

fn test_write() {
	mut aw := ArrayWriter{}
	max := 65536
	mut bw := io.new_buffered_writer(writer: &aw, cap: max)!

	// write less data than buffer capacity, the underlying writer should receive no data.
	written := write_random_data(mut aw, mut bw, 65536)!

	// now exceed buffer capacity by a little
	little := small_amount()
	excess := bw.available() + little
	excess_data := rand.bytes(excess)!
	w := bw.write(excess_data)!
	assert bw.buffered() == little
	assert bw.available() == max - little
	assert aw.result.len == max
}

fn test_write_big() {
	mut aw := ArrayWriter{}
	max := 65536
	mut bw := io.new_buffered_writer(writer: &aw, cap: max)!

	more_than_max := max + small_amount()
	big_source := rand.bytes(more_than_max)!
	w := bw.write(big_source)!
	assert w == more_than_max
	assert bw.buffered() == 0
	assert bw.available() == max
	assert aw.result.len == more_than_max
}

// create_data returns an array with `n` elements plus `\n` as last character.
fn create_data(n int) []u8 {
	mut res := []u8{}
	for i := 0; i < n; i++ {
		res << `X`
	}
	res << `\n`
	return res
}

fn test_simple_write() {
	mut aw := ArrayWriter{}
	mut bw := io.new_buffered_writer(writer: &aw, cap: 10)!
	mut data := create_data(6)
	w1 := bw.write(data)!

	// add data to buffer, less than cap
	// data is written to buffer, not to underlying writer.
	assert w1 == 7 // 6*x + \n
	assert bw.buffered() == 7
	assert aw.result.len == 0

	// add more data, exceed cap
	// data is flushed to underlying writer when buffer is full, then more data is written to buffer.
	w2 := bw.write(data)!
	assert w2 == 7
	assert bw.buffered() == 4
	assert aw.result.len == 10

	// exceed cap immediately
	// all data is written without buffering
	aw.result = []u8{}
	data = create_data(33)
	bw.reset()
	w3 := bw.write(data)!
	assert bw.buffered() == 0
	assert aw.result.len == 34 // 33*x + \n
}

fn test_simple_flush() {
	mut aw := ArrayWriter{}
	mut bw := io.new_buffered_writer(writer: &aw, cap: 10)!
	data := create_data(6)
	w := bw.write(data)!

	assert w == 7 // 6*x + \n
	assert bw.buffered() == 7

	bw.flush()!
	assert bw.buffered() == 0
	assert aw.result.len == 7
}

fn test_flush_handles_partial_writes() {
	mut cw := ChunkedArrayWriter{
		chunk: 2
	}
	mut bw := io.new_buffered_writer(writer: &cw, cap: 8)!
	data := 'abcdefg'.bytes()

	written := bw.write(data)!
	assert written == data.len
	assert bw.buffered() == data.len
	assert cw.result.len == 0

	bw.flush()!
	assert bw.buffered() == 0
	assert cw.result == data
}

fn test_flush_preserves_unwritten_data_on_error() {
	mut fw := FailOnceChunkedWriter{
		chunk: 3
	}
	mut bw := io.new_buffered_writer(writer: &fw, cap: 8)!
	data := 'abcdefg'.bytes()
	_ = bw.write(data)!

	if _ := bw.flush() {
		assert false
	} else {
		assert err.msg() == 'forced write failure'
	}
	assert fw.result == 'abc'.bytes()
	assert bw.buffered() == 4
	assert bw.buf[..bw.buffered()] == 'defg'.bytes()

	bw.flush()!
	assert bw.buffered() == 0
	assert fw.result == data
}

fn test_write_returns_error_for_zero_progress_writes() {
	mut zw := ZeroProgressWriter{}
	mut bw := io.new_buffered_writer(writer: &zw, cap: 4)!

	if _ := bw.write('12345'.bytes()) {
		assert false
	} else {
		assert err.msg() == 'writer returned an invalid number of bytes while writing'
	}
	assert bw.buffered() == 0
}
