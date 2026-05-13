module binary

import io

struct ChunkedReader {
	data  []u8
	chunk int = 1
mut:
	offset int
}

fn (mut r ChunkedReader) read(mut buf []u8) !int {
	if r.offset >= r.data.len {
		return io.Eof{}
	}
	mut n := if r.chunk > 0 && r.chunk < buf.len { r.chunk } else { buf.len }
	remaining := r.data.len - r.offset
	if remaining < n {
		n = remaining
	}
	copy(mut buf[..n], r.data[r.offset..r.offset + n])
	r.offset += n
	return n
}

struct ChunkedWriter {
	chunk int = 1
mut:
	data []u8
}

fn (mut w ChunkedWriter) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	n := if w.chunk > 0 && w.chunk < buf.len { w.chunk } else { buf.len }
	w.data << buf[..n]
	return n
}

struct Packet {
	flag    bool
	count   u16
	values  [2]u32
	sample  f32
	trailer [3]u8
}

struct UnsupportedPacket {
	name string
}

fn test_stream_write_and_read_struct() {
	packet := Packet{
		flag:    true
		count:   0x1234
		values:  [u32(0x11223344), 0x55667788]!
		sample:  f32(12.5)
		trailer: [u8(1), 2, 3]!
	}
	mut writer := ChunkedWriter{
		chunk: 3
	}
	write(mut writer, big_endian, packet)!
	assert size(packet) == writer.data.len

	mut reader := ChunkedReader{
		data:  writer.data.clone()
		chunk: 2
	}
	mut decoded := Packet{}
	read(mut reader, big_endian, mut decoded)!
	assert decoded == packet
}

fn test_stream_read_and_write_slice() {
	values := [u16(0x0102), 0x0304, 0x0506]
	mut writer := ChunkedWriter{
		chunk: 2
	}
	write(mut writer, little_endian, values)!
	assert size(values) == 6

	mut reader := ChunkedReader{
		data:  writer.data.clone()
		chunk: 1
	}
	mut decoded := []u16{len: values.len}
	read(mut reader, little_endian, mut decoded)!
	assert decoded == values
}

fn test_stream_read_and_write_bytes() {
	bytes := [u8(9), 8, 7, 6, 5]
	mut writer := ChunkedWriter{
		chunk: 4
	}
	write(mut writer, little_endian, bytes)!

	mut reader := ChunkedReader{
		data:  writer.data.clone()
		chunk: 2
	}
	mut decoded := []u8{len: bytes.len}
	read(mut reader, little_endian, mut decoded)!
	assert decoded == bytes
}

fn test_stream_short_read_returns_eof() {
	mut reader := ChunkedReader{
		data:  [u8(1), 2]
		chunk: 1
	}
	mut value := u32(0)
	if _ := read(mut reader, little_endian, mut value) {
		assert false
	} else {
		assert err is io.Eof
	}
}

fn test_stream_reports_unsupported_types() {
	assert size('hello') == -1
	assert size(UnsupportedPacket{}) == -1

	mut writer := ChunkedWriter{}
	if _ := write(mut writer, little_endian, 'hello') {
		assert false
	} else {
		assert err.msg() == 'binary.write: unsupported type string'
	}
}
