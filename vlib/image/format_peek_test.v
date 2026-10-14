module image

import image.color
import io

// FixedSrc serves a fixed buffer and reports EOF once it is drained.
struct FixedSrc {
	data []u8
mut:
	pos int
}

fn (mut r FixedSrc) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	n := copy(mut buf, r.data[r.pos..])
	r.pos += n
	return n
}

// DripSrc hands out at most two bytes per call, so BufferedPeekReader has to
// refill from the underlying reader instead of getting the whole payload at once.
struct DripSrc {
	data []u8
mut:
	pos int
}

fn (mut r DripSrc) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	mut n := if buf.len < 2 {
		buf.len
	} else {
		2
	}
	if n > r.data.len - r.pos {
		n = r.data.len - r.pos
	}
	for i in 0 .. n {
		buf[i] = r.data[r.pos + i]
	}
	r.pos += n
	return n
}

fn payload_decode(mut r PeekReader) !Image {
	mut header := []u8{len: 4}
	n := r.read(mut header)!
	assert n == 4
	mut img := new_rgba(rect(0, 0, 1, 1))
	img.set_rgba(0, 0, color.RGBA{
		r: 10
		g: 20
		b: 30
		a: 255
	})
	return img
}

fn payload_decode_config(mut r PeekReader) !Config {
	mut header := []u8{len: 4}
	n := r.read(mut header)!
	assert n == 4
	return Config{
		color_model: color.rgba_model
		width:       1
		height:      1
	}
}

fn exploding_decode(mut _r PeekReader) !Image {
	return error('decoder exploded')
}

fn test_peek_leaves_the_reader_where_it_was() {
	mut r := as_reader(FixedSrc{
		data: 'HELLOWORLD'.bytes()
	})
	assert r.peek(5)! == 'HELLO'.bytes()
	mut buf := []u8{len: 5}
	assert r.read(mut buf)! == 5
	assert buf.bytestr() == 'HELLO'
	assert r.peek(5)! == 'WORLD'.bytes()
	mut rest := []u8{len: 5}
	assert r.read(mut rest)! == 5
	assert rest.bytestr() == 'WORLD'
	if _ := r.peek(1) {
		assert false, 'peek past the end succeeded'
	}
}

// The bytes a peek buffered stay buffered, so a read that stopped short of the
// buffer length is still visible to the next peek, and the peek that follows a
// read sees the unconsumed bytes rather than the whole stream again.
fn test_peek_and_read_share_one_buffered_cursor() {
	mut r := as_reader(FixedSrc{
		data: 'HELLOWORLD'.bytes()
	})
	mut head := []u8{len: 3}
	assert r.read(mut head)! == 3
	assert head.bytestr() == 'HEL'
	assert r.peek(7)! == 'LOWORLD'.bytes()
	assert r.peek(7)! == 'LOWORLD'.bytes()
	mut tail := []u8{len: 4}
	assert r.read(mut tail)! == 4
	assert tail.bytestr() == 'LOWO'
	assert r.peek(3)! == 'RLD'.bytes()
	if _ := r.peek(4) {
		assert false, 'peek past the end succeeded'
	}
}

fn test_read_refills_from_a_reader_that_returns_tiny_chunks() {
	mut r := as_reader(DripSrc{
		data: 'ABCDEFGH'.bytes()
	})
	assert r.peek(4)! == 'ABCD'.bytes()
	mut buf := []u8{len: 8}
	mut total := 0
	for total < 8 {
		n := r.read(mut buf[total..]) or { break }
		assert n > 0
		total += n
	}
	assert total == 8
	assert buf.bytestr() == 'ABCDEFGH'
}

fn test_peek_of_zero_bytes_returns_an_empty_slice() {
	mut r := as_reader(FixedSrc{
		data: 'AB'.bytes()
	})
	assert r.peek(0)!.len == 0
	assert r.peek(0)!.len == 0
	assert r.peek(1)! == 'A'.bytes()
}

fn test_peek_past_the_end_of_the_reader_errors() {
	mut r := as_reader(FixedSrc{
		data: 'AB'.bytes()
	})
	if _ := r.peek(3) {
		assert false, 'peek beyond EOF succeeded'
	} else {
		assert err is io.Eof
	}
	mut empty := as_reader(FixedSrc{
		data: []u8{}
	})
	if _ := empty.peek(1) {
		assert false, 'peek of an empty reader succeeded'
	} else {
		assert err is io.Eof
	}
}

fn test_read_with_an_empty_buffer_reads_nothing() {
	mut r := as_reader(FixedSrc{
		data: 'AB'.bytes()
	})
	mut buf := []u8{}
	assert r.read(mut buf)! == 0
	assert r.peek(2)! == 'AB'.bytes()
}

fn test_read_stops_at_the_end_of_the_underlying_reader() {
	mut r := as_reader(FixedSrc{
		data: 'AB'.bytes()
	})
	mut buf := []u8{len: 4}
	assert r.read(mut buf)! == 2
	if _ := r.read(mut buf) {
		assert false, 'read past EOF succeeded'
	} else {
		assert err is io.Eof
	}
}

fn test_match_magic_wildcards_and_lengths() {
	// NOTE: a zero length magic matches the zero bytes that peek(0) returns, so
	// an empty magic would match every payload.
	assert match_magic('', []u8{})
	assert match_magic('A', 'A'.bytes())
	assert match_magic('?', 'A'.bytes())
	assert match_magic('??', 'AB'.bytes())
	assert match_magic('A?', 'AB'.bytes())
	assert match_magic('A?C', 'ABC'.bytes())
	assert !match_magic('A?C', 'ABD'.bytes())
	assert !match_magic('A?C', 'AB'.bytes())
	assert !match_magic('A?C', 'ABCD'.bytes())
	assert !match_magic('A', 'B'.bytes())
	assert !match_magic('AB', [])
}

fn test_sniff_uses_the_magic_length_of_each_registered_format() {
	// 'short' is registered first and its magic is a prefix of the payload, so
	// a later, longer magic is never reached.
	register_format('short', 'SH', payload_decode, payload_decode_config)
	register_format('long', 'SHORTER!', payload_decode, payload_decode_config)

	mut reader := FixedSrc{
		data: 'SHORTLY'.bytes()
	}
	img, name := decode(reader)!
	assert name == 'short'
	assert img.bounds() == rect(0, 0, 1, 1)

	mut config_reader := FixedSrc{
		data: 'SHORTLY'.bytes()
	}
	config, config_name := decode_config(config_reader)!
	assert config_name == 'short'
	assert config.width == 1
	assert config.height == 1
	assert config.color_model == color.rgba_model
}

fn test_sniff_skips_registered_formats_that_do_not_match() {
	register_format('alpha', 'ZZZZ', payload_decode, payload_decode_config)
	register_format('beta', 'QR', payload_decode, payload_decode_config)

	mut reader := FixedSrc{
		data: 'QRSOMETHING'.bytes()
	}
	_, name := decode(reader)!
	assert name == 'beta'
}

fn test_decode_reports_an_unknown_format_for_an_empty_reader() {
	mut reader := FixedSrc{
		data: []u8{}
	}
	if _, _ := decode(reader) {
		assert false, 'decode accepted an empty reader'
	} else {
		assert err.msg() == err_format
	}
	if _, _ := decode_config(reader) {
		assert false, 'decode_config accepted an empty reader'
	} else {
		assert err.msg() == err_format
	}
}

fn test_decode_reports_an_unknown_format_for_a_short_reader() {
	mut reader := FixedSrc{
		data: 'AB'.bytes()
	}
	if _, _ := decode(reader) {
		assert false, 'decode accepted a reader shorter than every magic'
	} else {
		assert err.msg() == err_format
	}
}

fn test_decode_propagates_the_decoder_error() {
	register_format('boom', 'BOOM', exploding_decode, payload_decode_config)

	mut reader := FixedSrc{
		data: 'BOOM!'.bytes()
	}
	if _, _ := decode(reader) {
		assert false, 'decode swallowed the decoder error'
	} else {
		assert err.msg() == 'decoder exploded'
	}
}
