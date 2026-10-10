module deflate

import encoding.binary
import encoding.hex
import hash.adler32
import hash.crc32

fn must_decode_hex(s string) []u8 {
	return hex.decode(s) or { panic(err) }
}

// zlib_stream wraps a raw DEFLATE payload that decodes to `plain` in a zlib container.
fn zlib_stream(payload []u8, plain []u8) []u8 {
	mut out := [u8(0x78), 0x9c]
	out << payload
	out << binary.big_endian_get_u32(adler32.sum(plain))
	return out
}

// gzip_stream wraps a raw DEFLATE payload that decodes to `plain` in a gzip container.
fn gzip_stream(payload []u8, plain []u8) []u8 {
	mut out := [u8(0x1f), 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff]
	out << payload
	out << binary.little_endian_get_u32(crc32.sum(plain))
	out << binary.little_endian_get_u32(u32(plain.len))
	return out
}

fn collect_chunk(chunk []u8, userdata voidptr) int {
	mut out := unsafe { &[]u8(userdata) }
	out << chunk
	return chunk.len
}

// decompress_chunked decodes `data` with decompress_with_callback() and joins the chunks.
fn decompress_chunked(data []u8) ![]u8 {
	mut out := []u8{}
	n := decompress_with_callback(data, collect_chunk, &out)!
	assert n == out.len
	return out
}

// decode_errors returns the error message of decompress() and of decompress_with_callback()
// for `data`; '' stands for no error.
fn decode_errors(data []u8) []string {
	mut res := []string{}
	if _ := decompress(data) {
		res << ''
	} else {
		res << err.msg()
	}
	if _ := decompress_chunked(data) {
		res << ''
	} else {
		res << err.msg()
	}
	return res
}

// bit_flips returns one copy of `data` per bit of the 4 bytes at `start`, with that bit flipped.
fn bit_flips(data []u8, start int) [][]u8 {
	mut res := [][]u8{cap: 32}
	for i in start .. start + 4 {
		for bit in 0 .. 8 {
			mut bad := data.clone()
			bad[i] ^= u8(1) << bit
			res << bad
		}
	}
	return res
}

// assert_checksum_errors checks that, with the payload intact, a wrong adler32, crc32 or isize
// is reported as exactly that, by the one-shot and by the callback decoder.
fn assert_checksum_errors(payload []u8, plain []u8) ! {
	z := zlib_stream(payload, plain)
	gz := gzip_stream(payload, plain)
	assert decompress(z)! == plain
	assert decompress(gz)! == plain
	assert decompress_chunked(z)! == plain
	assert decompress_chunked(gz)! == plain
	for bad in bit_flips(z, z.len - 4) {
		assert decode_errors(bad) == ['invalid zlib stream: adler32 mismatch'].repeat(2)
	}
	for bad in bit_flips(gz, gz.len - 8) {
		assert decode_errors(bad) == ['invalid gzip stream: crc32 mismatch'].repeat(2)
	}
	for bad in bit_flips(gz, gz.len - 4) {
		assert decode_errors(bad) == ['invalid gzip stream: size mismatch'].repeat(2)
	}
}

fn test_zlib_roundtrip() {
	data := 'Hello world!'.bytes()
	compressed := compress(data)!
	assert compressed[0] == 0x78 && compressed[1] == 0x9c // zlib header
	assert decompress(compressed)! == data
}

fn test_gzip_roundtrip() {
	data := 'Hello gzip!'.repeat(10).bytes()
	compressed := compress(data, format: .gzip)!
	assert compressed[0] == 0x1f && compressed[1] == 0x8b // gzip magic
	assert decompress(compressed)! == data
}

fn test_raw_deflate_roundtrip() {
	data := 'raw deflate'.repeat(20).bytes()
	raw := compress(data, format: .raw_deflate)!
	decoded := decompress(raw)! // auto-detected as raw
	assert decoded == data
}

fn test_decompress_auto_detects_all_formats() {
	data := 'multi-format detection test'.repeat(5).bytes()
	assert decompress(compress(data)!)! == data
	assert decompress(compress(data, format: .gzip)!)! == data
	assert decompress(compress(data, format: .raw_deflate)!)! == data
}

fn test_wrapper_helpers_match_unified_api() {
	data := 'wrapper compatibility'.repeat(8).bytes()
	assert compress(data)! == compress(data, format: .zlib)!
	assert compress_gzip(data)! == compress(data, format: .gzip)!
	assert compress_raw(data)! == compress(data, format: .raw_deflate)!
}

fn test_roundtrip_repeated() {
	data := 'abcabc'.repeat(100).bytes()
	compressed := compress(data)!
	assert compressed.len < data.len
	assert decompress(compressed)! == data
}

fn test_bad_compression_method_fails() {
	bad := [u8(0x79), 0x18, 0x00, 0x00, 0x00, 0x00]
	decompress(bad) or {
		assert err.msg().len > 0
		return
	}
	assert false
}

fn test_corrupt_checksum_fails() {
	mut enc := compress(('hello world').repeat(10).bytes())!
	// flip a byte in the adler32 footer
	enc[enc.len - 1] ^= 0xff
	decompress(enc) or {
		assert err.msg().contains('adler32')
		return
	}
	assert false
}

fn test_truncated_zlib_payload_fails() {
	decompress([u8(0x78), 0x9c, 0x03, 0x00, 0x00, 0x00, 0x01]) or {
		assert err.msg().contains('unexpected end of stream')
		return
	}
	assert false
}

fn test_zlib_inserted_bytes_before_adler_fails() {
	enc := compress('zlib injected trailer bytes'.repeat(4).bytes())!
	mut bad := []u8{cap: enc.len + 2}
	bad << enc[..enc.len - 4]
	bad << [u8(0xaa), 0x55]
	bad << enc[enc.len - 4..]
	decompress(bad) or {
		assert err.msg() == 'invalid zlib stream: trailing data before adler32'
		return
	}
	assert false
}

fn test_gzip_inserted_bytes_before_trailer_fails() {
	enc := compress('gzip injected trailer bytes'.repeat(4).bytes(), format: .gzip)!
	mut bad := []u8{cap: enc.len + 1}
	bad << enc[..enc.len - 8]
	bad << u8(0x42)
	bad << enc[enc.len - 8..]
	decompress(bad) or {
		assert err.msg() == 'invalid gzip stream: trailing data before trailer'
		return
	}
	assert false
}

fn test_wrong_checksum_alone_reports_the_checksum() {
	plain := 'abacabad'.repeat(16).bytes()
	// every block type in one payload, the Huffman blocks made by the reference zlib:
	// dynamic Huffman for plain[..64], followed by the empty stored block of a sync flush
	mut payload := must_decode_hex('04c10101000008c3a0acccf7cf20c8c9e464723239999c4c4e262793933d000000ffff')
	payload << [u8(0x00), 0x20, 0x00, 0xdf, 0xff] // stored, LEN=32: plain[64..96]
	payload << plain[64..96]
	payload << must_decode_hex('4b4c4a4c4e4c4a4c49c4410300') // final, fixed Huffman: plain[96..]
	assert_checksum_errors(payload, plain)!
	// compress() output: the empty input, and more than one callback chunk
	for data in [[]u8{}, 'abcdefghij'.repeat(4_000).bytes()] {
		assert_checksum_errors(compress_raw(data)!, data)!
	}
}

fn test_stored_block_after_huffman_block_with_short_end_of_block_code() {
	// A dynamic Huffman block whose end-of-block code is 1 bit long while its longest literal
	// code is 15 bits long, followed by a stored block. Decoding the end-of-block code loads
	// 15 bits ahead, so LEN and NLEN of the stored block are in the bit buffer by the time the
	// stored block starts. Both payloads are valid; the reference zlib decodes them.
	payloads := {
		'stored':      '04e1019224499224cb9e9558d43cb27af6fd7f7f00000044140600f9ff73746f726564'
		'aaaaastored': '04e1019224499224cb9e9558d43cb27af6fd7f7f00000044ac4a000600f9ff73746f726564'
	}
	for text, hex_payload in payloads {
		plain := text.bytes()
		payload := must_decode_hex(hex_payload)
		res := decompress_raw_with_consumed(payload)!
		assert res.decoded == plain
		assert res.consumed == payload.len
		assert_checksum_errors(payload, plain)!
	}
}
