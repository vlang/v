import arrays
import io

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

// create_data returns an array with `n` elements plus `\n` as last character.
fn create_data(n int) []u8 {
	mut res := []u8{}
	for i := 0; i < n; i++ {
		res << `X`
	}
	res << `\n`
	return res
}

fn test_create_buffered_writer() {
	bw := io.new_buffered_writer(writer: ArrayWriter{}, cap: 10)!
}

fn test_write() {
	mut aw := ArrayWriter{}
	mut bw := io.new_buffered_writer(writer: aw, cap: 10)!
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

fn test_flush() {
	mut aw := ArrayWriter{}
	mut bw := io.new_buffered_writer(writer: aw, cap: 10)!
	data := create_data(6)
	w := bw.write(data)!

	assert w == 7 // 6*x + \n
	assert bw.buffered() == 7

	bw.flush()!
	assert bw.buffered() == 0
	assert aw.result.len == 7
}
