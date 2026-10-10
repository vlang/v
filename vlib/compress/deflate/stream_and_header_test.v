module deflate

import encoding.binary
import hash.adler32
import hash.crc32

// canonical_code returns the MSB-first canonical Huffman code for `sym`, given
// the fixed RFC 1951 code lengths it reverses below to build a bitstream.
fn ll_code(sym int) (int, int) {
	mut lens := []int{len: 288}
	for i in 0 .. 144 {
		lens[i] = 8
	}
	for i in 144 .. 256 {
		lens[i] = 9
	}
	for i in 256 .. 280 {
		lens[i] = 7
	}
	for i in 280 .. 288 {
		lens[i] = 8
	}
	return canonical_code(lens, sym), lens[sym]
}

fn d_code(sym int) (int, int) {
	lens := []int{len: 32, init: 5}
	return canonical_code(lens, sym), lens[sym]
}

fn canonical_code(lengths []int, sym int) int {
	mut code := 0
	for bits in 1 .. 12 {
		for s in 0 .. lengths.len {
			if lengths[s] == bits {
				if s == sym {
					return code
				}
				code++
			}
		}
		code <<= 1
	}
	return 0
}

fn lsb_reverse(v int, n int) u32 {
	mut r := u32(0)
	mut x := u32(v)
	for _ in 0 .. n {
		r = (r << 1) | (x & 1)
		x >>= 1
	}
	return r
}

// StreamBitWriter emits the LSB-first bit-packed stream that RFC 1951 requires.
struct StreamBitWriter {
mut:
	out   []u8
	bits  u32
	nbits int
}

fn (mut w StreamBitWriter) write(value u32, n int) {
	if n == 0 {
		return
	}
	w.bits |= (value & ((u32(1) << n) - 1)) << w.nbits
	w.nbits += n
	for w.nbits >= 8 {
		w.out << u8(w.bits & 0xff)
		w.bits >>= 8
		w.nbits -= 8
	}
}

fn (mut w StreamBitWriter) finish() []u8 {
	if w.nbits > 0 {
		w.out << u8(w.bits & 0xff)
		w.bits = 0
		w.nbits = 0
	}
	return w.out
}

// fixed_huffman_stream builds a raw DEFLATE stream from fixed-Huffman
// lit/len symbols. Each entry is `[symbol, distance_symbol]`, with -1 meaning
// the symbol is a literal that carries no distance code.
fn fixed_huffman_stream(pairs [][]int) []u8 {
	mut w := StreamBitWriter{}
	w.write(1, 1) // BFINAL
	w.write(1, 2) // BTYPE=01, fixed Huffman
	for pair in pairs {
		code, len := ll_code(pair[0])
		w.write(lsb_reverse(code, len), len)
		if pair[1] >= 0 {
			dc, dl := d_code(pair[1])
			w.write(lsb_reverse(dc, dl), dl)
		}
	}
	code256, len256 := ll_code(256)
	w.write(lsb_reverse(code256, len256), len256) // end of block
	return w.finish()
}

// stored_block_stream builds a raw DEFLATE stream of type BTYPE=00 (stored).
fn stored_block_stream(payloads [][]u8, final_on_last bool) []u8 {
	mut out := []u8{}
	for i, payload in payloads {
		mut header_byte := u8(0x00) // BTYPE=00, BFINAL=0
		if final_on_last && i == payloads.len - 1 {
			header_byte = 0x01
		}
		out << header_byte
		out << u8(payload.len & 0xff)
		out << u8((payload.len >> 8) & 0xff)
		nlen := u16(~u16(payload.len))
		out << u8(nlen & 0xff)
		out << u8((nlen >> 8) & 0xff)
		out << payload
	}
	return out
}

// GzipAllFields is a RFC 1952 stream whose header sets FEXTRA, FNAME, FCOMMENT
// and FHCRC, so that the parsed fields can be read back.
struct GzipAllFields {
	bytes []u8
}

fn gzip_all_fields(body []u8) !GzipAllFields {
	raw := compress_raw(body)!
	mut hdr := []u8{}
	hdr << [u8(0x1f), 0x8b, 0x08]
	hdr << u8(0x04 | 0x08 | 0x10 | 0x02) // FEXTRA | FNAME | FCOMMENT | FHCRC
	hdr << u8(0x78) // MTIME, little endian
	hdr << u8(0x56)
	hdr << u8(0x34)
	hdr << u8(0x12)
	hdr << u8(0x02) // XFL
	hdr << u8(0x03) // OS
	hdr << u8(5) // FEXTRA XLEN
	hdr << u8(0)
	hdr << `A`
	hdr << `B`
	hdr << `C`
	hdr << `D`
	hdr << `E`
	for c in 'hi.txt'.bytes() {
		hdr << c
	}
	hdr << u8(0) // FNAME terminator
	for c in 'note'.bytes() {
		hdr << c
	}
	hdr << u8(0) // FCOMMENT terminator
	crc16 := u16(crc32.sum(hdr) & 0xffff)
	hdr << u8(crc16)
	hdr << u8(crc16 >> 8)
	mut full := hdr.clone()
	full << raw
	full << binary.little_endian_get_u32(crc32.sum(body))
	full << binary.little_endian_get_u32(u32(body.len))
	return GzipAllFields{
		bytes: full
	}
}

fn noise_bytes(len int) []u8 {
	mut data := []u8{len: len}
	mut state := u32(0x9e3779b9)
	for i in 0 .. data.len {
		state = state * 1664525 + 1013904223
		data[i] = u8(state >> 24)
	}
	return data
}

struct ChunkAcc {
mut:
	total  int
	chunks int
	limit  int
}

fn count_chunk(chunk []u8, mut acc ChunkAcc) int {
	acc.total += chunk.len
	acc.chunks++
	return chunk.len
}

fn count_first_chunk_only(chunk []u8, mut acc ChunkAcc) int {
	if acc.limit <= 0 {
		return 0
	}
	acc.total += chunk.len
	acc.chunks++
	acc.limit--
	return chunk.len
}

fn test_stored_block_roundtrip() {
	stored := stored_block_stream([[`a`, `b`, `c`]], true)
	assert stored == [u8(0x01), 0x03, 0x00, 0xfc, 0xff, `a`, `b`, `c`]
	assert decompress(stored)! == [u8(`a`), `b`, `c`]
}

fn test_multiple_stored_blocks_roundtrip() {
	stored := stored_block_stream([[`h`, `i`], [`!`]], true)
	res := decompress_raw_with_consumed(stored)!
	assert res.decoded == [u8(`h`), `i`, `!`]
	assert res.consumed == stored.len
}

fn test_decompress_raw_with_consumed_counts_input_bytes() {
	stored := stored_block_stream([[`h`, `i`], [`!`]], true)
	mut with_junk := stored.clone()
	with_junk << u8(0xde)
	with_junk << u8(0xad)
	res := decompress_raw_with_consumed(with_junk)!
	assert res.decoded == [u8(`h`), `i`, `!`]
	assert res.consumed == stored.len
	assert res.consumed < with_junk.len
}

fn test_manual_fixed_huffman_stream_roundtrip() {
	// 'A' followed by a length 3 / distance 1 match repeats it three more times.
	stream := fixed_huffman_stream([[0x41, -1], [257, 0]])
	assert stream == [u8(0x73), 0x04, 0x02, 0x00]
	res := decompress_raw_with_consumed(stream)!
	assert res.decoded == 'AAAA'.bytes()
	assert res.consumed == stream.len
}

fn test_inflate_rejects_bad_stored_block_length() {
	bad := [u8(0x01), 0x03, 0x00, 0x00, 0x00, `a`, `b`, `c`]
	decompress(bad) or {
		assert err.msg() == 'inflate: bad stored block length'
		return
	}
	assert false
}

fn test_inflate_rejects_reserved_block_type() {
	decompress([u8(0x07)]) or {
		assert err.msg() == 'inflate: reserved block type'
		return
	}
	assert false
}

fn test_inflate_rejects_truncated_stored_block() {
	truncated := [u8(0x01), 0x05, 0x00, 0xfa, 0xff, 0x61]
	decompress(truncated) or {
		assert err.msg() == 'inflate: unexpected end of stream'
		return
	}
	assert false
}

fn test_inflate_rejects_distance_past_output_start() {
	// A length/distance pair emitted before any literal exists.
	stream := fixed_huffman_stream([[257, 0]])
	assert stream == [u8(0x03), 0x02, 0x00]
	decompress(stream) or {
		assert err.msg() == 'inflate: distance past output start'
		return
	}
	assert false
}

fn test_inflate_rejects_invalid_length_symbol() {
	// Symbol 286 has no RFC 1951 meaning, although it is a valid code point in
	// the fixed lit/len tree.
	stream := fixed_huffman_stream([[286, -1]])
	assert stream == [u8(0x1b), 0x03, 0x00]
	decompress(stream) or {
		assert err.msg() == 'inflate: invalid length symbol 286'
		return
	}
	assert false
}

fn test_decompress_rejects_empty_input() {
	decompress([]u8{}) or {
		assert err.msg() == 'inflate: unexpected end of stream'
		return
	}
	assert false
}

fn test_validate_zlib_header_reports_payload_start() {
	header := validate_zlib_header([u8(0x78), 0x9c, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00])!
	assert header.payload_start == 2
}

fn test_validate_zlib_header_rejects_preset_dictionary() {
	// 0x78bb satisfies (CMF*256+FLG) % 31 == 0 while setting FDICT.
	preset := [u8(0x78), 0xbb, 0x03, 0x00, 0x00, 0x00, 0x01]
	validate_zlib_header(preset) or {
		assert err.msg() == 'invalid zlib stream: preset dictionary not supported'
		return
	}
	assert false
	decompress(preset) or {
		assert err.msg() == 'invalid zlib stream: preset dictionary not supported'
		return
	}
	assert false
}

fn test_validate_zlib_header_rejects_malformed_headers() {
	validate_zlib_header([u8(0x78), 0x9c]) or {
		assert err.msg() == 'invalid zlib stream: too short'
	}
	validate_zlib_header([u8(0x79), 0x9c, 0x00, 0x00, 0x00, 0x01]) or {
		assert err.msg() == 'invalid zlib stream: unsupported compression method'
	}
	validate_zlib_header([u8(0x78), 0x9d, 0x00, 0x00, 0x00, 0x01]) or {
		assert err.msg() == 'invalid zlib stream: bad header checksum'
	}
}

fn test_validate_gzip_header_parses_all_optional_fields() {
	header := validate_gzip_header(gzip_all_fields('header fields test'.bytes())!.bytes)!
	assert header.flags == u8(0x1e)
	assert header.payload_start == 31
	assert header.modification_time == 0x12345678
	assert header.operating_system == 0x03
	assert header.extra == [u8(`A`), `B`, `C`, `D`, `E`]
	assert header.filename == 'hi.txt'.bytes()
	assert header.comment == 'note'.bytes()
}

fn test_validate_gzip_header_rejects_truncated_extra() {
	mut data := []u8{len: 18}
	data[0] = 0x1f
	data[1] = 0x8b
	data[2] = 8
	data[3] = 0x04 // FEXTRA
	data[10] = 50 // XLEN overruns the stream
	validate_gzip_header(data) or {
		assert err.msg() == 'invalid gzip stream: truncated extra'
		return
	}
	assert false
}

fn test_validate_gzip_header_rejects_truncated_payload() {
	mut data := []u8{len: 25}
	data[0] = 0x1f
	data[1] = 0x8b
	data[2] = 8
	data[3] = 0x04 // FEXTRA
	data[10] = 8 // XLEN
	validate_gzip_header(data) or {
		assert err.msg() == 'invalid gzip stream: truncated payload'
		return
	}
	assert false
}

fn test_validate_gzip_header_rejects_truncated_fhcrc() {
	mut data := []u8{len: 21}
	data[0] = 0x1f
	data[1] = 0x8b
	data[2] = 8
	data[3] = u8(0x04 | 0x02) // FEXTRA | FHCRC
	data[10] = 8 // XLEN consumes every byte before FHCRC
	validate_gzip_header(data) or {
		assert err.msg() == 'invalid gzip stream: truncated fhcrc'
		return
	}
	assert false
}

fn test_validate_gzip_header_accepts_valid_fhcrc() {
	mut data := []u8{len: 26}
	data[0] = 0x1f
	data[1] = 0x8b
	data[2] = 8
	data[3] = u8(0x04 | 0x02) // FEXTRA | FHCRC
	data[10] = 4 // XLEN
	data[12] = 1
	data[13] = 2
	data[14] = 3
	data[15] = 4
	crc16 := u16(crc32.sum(data[..16]) & 0xffff)
	data[16] = u8(crc16)
	data[17] = u8(crc16 >> 8)
	header := validate_gzip_header(data)!
	assert header.payload_start == 18
	assert header.extra == [u8(1), 2, 3, 4]
	data[17] ^= 0xff
	validate_gzip_header(data) or {
		assert err.msg() == 'invalid gzip stream: header crc16 mismatch'
		return
	}
	assert false
}

fn test_roundtrip_varied_input_sizes() {
	inputs := [
		[]u8{},
		[u8(0x41)],
		[]u8{len: 256, init: u8(index)},
		'A'.repeat(100_000).bytes(),
	]
	for data in inputs {
		assert_zlib_roundtrip(data)!
		assert_gzip_roundtrip(data)!
		assert_raw_roundtrip(data)!
	}
}

fn assert_zlib_roundtrip(data []u8) ! {
	z := compress_zlib(data)!
	assert z[0] == 0x78 && z[1] == 0x9c
	assert binary.big_endian_u32_at(z, z.len - 4) == adler32.sum(data)
	assert decompress(z)! == data
	assert decompress_zlib(z)! == data
}

fn assert_gzip_roundtrip(data []u8) ! {
	g := compress_gzip(data)!
	assert g[..4] == [u8(0x1f), 0x8b, 0x08, 0x00]
	assert g[4..8] == [u8(0), 0, 0, 0] // MTIME
	assert g[8] == 0x00 // XFL
	assert g[9] == 0xff // OS
	assert binary.little_endian_u32_at(g, g.len - 8) == crc32.sum(data)
	assert binary.little_endian_u32_at(g, g.len - 4) == u32(data.len)
	assert decompress(g)! == data
	assert decompress_gzip(g)! == data
}

fn assert_raw_roundtrip(data []u8) ! {
	r := compress_raw(data)!
	res := decompress_raw_with_consumed(r)!
	assert res.decoded == data
	assert res.consumed == r.len
}

fn test_roundtrip_incompressible_data() {
	data := noise_bytes(64 * 1024)
	for fmt in [CompressFormat.zlib, .gzip, .raw_deflate] {
		c := compress(data, format: fmt)!
		assert decompress(c)! == data
	}
	assert compress_raw(data)!.len <= data.len + data.len / 4
}

fn test_roundtrip_highly_compressible_data() {
	data := 'abcabc'.repeat(20_000).bytes()
	for fmt in [CompressFormat.zlib, .gzip, .raw_deflate] {
		c := compress(data, format: fmt)!
		assert c.len < data.len
		assert decompress(c)! == data
	}
}

fn test_decompress_routes_by_magic_bytes() {
	z := compress('same payload'.repeat(4).bytes())!
	g := compress('same payload'.repeat(4).bytes(), format: .gzip)!
	decompress([z[0], z[1]]) or {
		assert err.msg() == 'invalid zlib stream: too short'
		return
	}
	assert false
}

fn test_decompress_rejects_two_byte_gzip_magic() {
	g := compress('same payload'.repeat(4).bytes(), format: .gzip)!
	decompress([g[0], g[1]]) or {
		assert err.msg() == 'invalid gzip stream: too short'
		return
	}
	assert false
}

fn test_decompress_with_callback_delivers_all_formats() {
	data := '321323'.repeat(10_000).bytes()
	streams := [
		compress_zlib(data)!,
		compress_gzip(data)!,
		compress_raw(data)!,
	]
	for stream in streams {
		mut acc := ChunkAcc{}
		n := decompress_with_callback(stream, count_chunk, &acc)!
		assert n == data.len
		assert acc.total == data.len
	}
}

fn test_decompress_with_callback_gzip_chunk_sizes() {
	data := '321323'.repeat(10_000).bytes()
	stream := compress_gzip(data)!
	mut acc := ChunkAcc{}
	n := decompress_with_callback(stream, count_chunk, &acc)!
	assert n == 60_000
	assert acc.total == 60_000
	assert acc.chunks == 2
}

fn test_decompress_with_callback_stops_when_callback_aborts() {
	data := '321323'.repeat(10_000).bytes()
	streams := [
		compress_zlib(data)!,
		compress_gzip(data)!,
		compress_raw(data)!,
	]
	for stream in streams {
		mut acc := ChunkAcc{
			limit: 1
		}
		n := decompress_with_callback(stream, count_first_chunk_only, &acc)!
		assert n == 32768 // one inflate_callback_chunk_size chunk
		assert acc.chunks == 1
		assert acc.total == 32768
	}
}
