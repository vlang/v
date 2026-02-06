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
	mut bw := io.new_buffered_writer(writer: aw, cap: max)!

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
	mut bw := io.new_buffered_writer(writer: aw, cap: max)!

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
	mut bw := io.new_buffered_writer(writer: aw, cap: max)!

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

fn test_simple_flush() {
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
