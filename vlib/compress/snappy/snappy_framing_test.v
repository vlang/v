module snappy

import arrays
import io

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn stream_round_trip(label string, input []u8) ! {
	encoded := encode_stream(input)
	decoded := decode_stream(encoded)!

	assert decoded == input, '${label}: stream round-trip mismatch (len=${input.len})'
}

// drain_decoder closes dec and returns all decoded bytes accumulated so far.
fn drain_decoder(mut dec StreamDecoder) []u8 {
	dec.close() or { panic(err) }
	mut collected := []u8{}
	mut chunk := []u8{len: 4096}
	for {
		n := dec.read(mut chunk) or { break }
		if n == 0 {
			break
		}
		collected << chunk[..n]
	}
	return collected
}

// ---------------------------------------------------------------------------
// CRC32C tests
// ---------------------------------------------------------------------------

fn test_crc32c_empty() {
	// Known value: CRC32C("") = 0x00000000
	assert crc32c([]) == u32(0x00000000)
}

fn test_crc32c_known_vector() {
	// CRC32C("123456789") = 0xE3069283 — standard test vector.
	assert crc32c('123456789'.bytes()) == u32(0xe3069283)
}

fn test_mask_unmask_roundtrip() {
	for v in [u32(0), u32(1), u32(0xdeadbeef), u32(0xffffffff)] {
		assert unmask_crc(mask_crc(v)) == v, 'mask/unmask failed for 0x${v:08x}'
	}
}

// ---------------------------------------------------------------------------
// One-shot encode / decode
// ---------------------------------------------------------------------------

fn test_stream_empty() {
	stream_round_trip('empty', []u8{}) or { panic(err) }
}

fn test_stream_single_byte() {
	stream_round_trip('single byte', [u8(0x42)]) or { panic(err) }
}

fn test_stream_small() {
	stream_round_trip('hello world', 'Hello, World!'.bytes()) or { panic(err) }
}

fn test_stream_compressible() {
	input := []u8{len: 8192, init: u8(index % 7)}
	stream_round_trip('compressible', input) or { panic(err) }
}

fn test_stream_incompressible() {
	mut input := []u8{len: 2048}
	mut seed := u32(0xc0ffee42)
	for i in 0 .. input.len {
		seed = seed * 1664525 + 1013904223
		input[i] = u8(seed >> 24)
	}
	stream_round_trip('incompressible', input) or { panic(err) }
}

fn test_stream_exactly_one_block() {
	// Exactly max_chunk_data_size bytes — exercises the boundary condition.
	input := []u8{len: max_chunk_data_size, init: u8(index & 0xff)}
	stream_round_trip('one-full-block', input) or { panic(err) }
}

fn test_stream_multi_block() {
	// Spans multiple 64 KiB blocks.
	input := []u8{len: max_chunk_data_size * 3 + 100, init: u8(index & 0xff)}
	stream_round_trip('multi-block', input) or { panic(err) }
}

fn test_stream_identifier_present() {
	// The encoded stream must start with the identifier chunk.
	encoded := encode_stream('test'.bytes())
	assert encoded.len >= 10, 'stream too short to contain identifier'
	assert encoded[0] == chunk_type_stream_id, 'first byte must be stream identifier type'
	// The 6-byte magic must follow the 4-byte header.
	magic := encoded[4..10]
	assert magic == stream_identifier_magic, 'stream identifier magic mismatch'
}

fn test_stream_crc_mismatch_detected() {
	mut encoded := encode_stream('hello'.bytes())
	// Corrupt a byte in the CRC field of the first data chunk (after the
	// 10-byte identifier: 4-byte header + 4-byte CRC starts at byte 14).
	if encoded.len > 14 {
		encoded[14] ^= 0xff
	}
	decode_stream(encoded) or { return } // expected error
	panic('expected decode_stream to fail on CRC mismatch')
}

fn test_stream_missing_identifier() {
	// A stream that starts with a compressed chunk instead of the identifier.
	bad := [u8(chunk_type_compressed), 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
	decode_stream(bad) or { return }
	panic('expected decode_stream to fail without identifier chunk')
}

fn test_stream_unskippable_reserved_chunk() {
	mut s := encode_stream('hello'.bytes())
	// Inject a reserved unskippable chunk (type=0x02) before the data.
	injected := [u8(0x02), 0x01, 0x00, 0x00, u8(0x00)] // type + 3-byte len=1 + 1 payload byte
	// Insert after the 10-byte stream identifier.
	s = arrays.reduce([s[..10], injected, s[10..]], fn (acc []u8, x []u8) []u8 {
		mut out := acc.clone()
		out << x
		return out
	})!
	decode_stream(s) or { return }
	panic('expected decode_stream to fail on unskippable reserved chunk')
}

fn test_stream_padding_skipped() {
	// Build a stream manually: identifier + padding + compressed data.
	mut s := []u8{}
	// Stream identifier
	s << chunk_type_stream_id
	write_u24le(mut s, stream_identifier_magic.len)
	s << stream_identifier_magic
	// Padding chunk (type=0xfe, length=4, 4 zero bytes)
	s << chunk_type_padding
	write_u24le(mut s, 4)
	s << [u8(0), 0, 0, 0]
	// A real data chunk
	inner := encode_stream('after padding'.bytes())
	// Skip the inner identifier (first 10 bytes) and append the data chunk.
	s << inner[10..]

	decoded := decode_stream(s) or { panic(err) }
	assert decoded == 'after padding'.bytes(), 'padding chunk should be silently skipped'
}

// ---------------------------------------------------------------------------
// Incremental encoder
// ---------------------------------------------------------------------------

fn test_incremental_encoder_matches_oneshot() {
	input := []u8{len: max_chunk_data_size + 500, init: u8(index & 0xff)}

	// One-shot
	oneshot := encode_stream(input)

	// Incremental: feed in 1000-byte pieces via write/close/peek.
	mut enc := StreamEncoder{}
	mut pos := 0
	for pos < input.len {
		end := if pos + 1000 < input.len { pos + 1000 } else { input.len }
		enc.write(input[pos..end]) or { panic(err) }
		pos = end
	}
	enc.close()
	incremental := enc.peek()

	assert incremental == oneshot, 'incremental encoder output differs from one-shot'
}

fn test_incremental_encoder_decodable() {
	input := 'Streaming is fun!'.bytes().repeat(3000)

	mut enc := StreamEncoder{}
	enc.write(input[..input.len / 2]) or { panic(err) }
	enc.write(input[input.len / 2..]) or { panic(err) }
	enc.close()
	stream := enc.peek()

	decoded := decode_stream(stream) or { panic(err) }
	assert decoded == input, 'incremental encoder produced undecodable stream'
}

fn test_incremental_encoder_write_after_close_errors() {
	// write() on a closed encoder must return an error.
	mut enc := StreamEncoder{}
	enc.write('hello'.bytes()) or { panic(err) }
	enc.close()
	enc.write('world'.bytes()) or { return } // expected error
	panic('expected write on closed encoder to fail')
}

fn test_incremental_encoder_read_drains_output() {
	// read() should drain bytes from the encoded output incrementally.
	input := []u8{len: max_chunk_data_size + 100, init: u8(index & 0xff)}
	mut enc := StreamEncoder{}
	enc.write(input) or { panic(err) }
	enc.close()

	// Drain via read() in small pieces and reassemble.
	mut collected := []u8{}
	mut chunk := []u8{len: 256}
	for {
		n := enc.read(mut chunk) or { break }
		if n == 0 {
			break
		}
		collected << chunk[..n]
	}

	expected := encode_stream(input)
	assert collected == expected, 'read() did not produce the same bytes as encode_stream'
}

// ---------------------------------------------------------------------------
// Incremental decoder
// ---------------------------------------------------------------------------

fn test_incremental_decoder_byte_by_byte() {
	input := 'Byte by byte delivery'.bytes().repeat(500)
	stream := encode_stream(input)

	mut dec := StreamDecoder{}
	for b in stream {
		dec.write([b]) or { panic(err) }
	}
	assert drain_decoder(mut dec) == input, 'incremental decoder failed (byte-by-byte)'
}

fn test_incremental_decoder_large_chunks() {
	input := []u8{len: max_chunk_data_size * 2 + 99, init: u8(index % 13)}
	stream := encode_stream(input)

	mut dec := StreamDecoder{}
	mut pos := 0
	for pos < stream.len {
		end := if pos + 8192 < stream.len { pos + 8192 } else { stream.len }
		dec.write(stream[pos..end]) or { panic(err) }
		pos = end
	}
	assert drain_decoder(mut dec) == input, 'incremental decoder failed (large chunks)'
}

fn test_incremental_decoder_read_drains_output() {
	// A first read() call should return all decoded bytes; a second call on a
	// closed decoder with no remaining output should return io.Eof.
	stream := encode_stream('hello world'.bytes())
	mut dec := StreamDecoder{}
	dec.write(stream) or { panic(err) }
	dec.close() or { panic(err) }

	mut buf := []u8{len: 4096}

	// First read — should yield the decoded payload.
	n := dec.read(mut buf) or { panic(err) }
	assert buf[..n] == 'hello world'.bytes(), 'first read should return decoded bytes'

	// Second read — decoder is closed and output is empty; expect Eof.
	dec.read(mut buf) or {
		assert err is io.Eof, 'expected io.Eof after output is drained, got: ${err}'
		return
	}
	panic('expected io.Eof on second read after output is drained')
}

fn test_incremental_decoder_write_after_close_errors() {
	// write() on a closed decoder must return an error.
	stream := encode_stream('hello'.bytes())
	mut dec := StreamDecoder{}
	dec.write(stream) or { panic(err) }
	dec.close() or { panic(err) }
	dec.write(stream) or { return } // expected error
	panic('expected write on closed decoder to fail')
}
