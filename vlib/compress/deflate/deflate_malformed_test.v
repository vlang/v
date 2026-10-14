// Malformed DEFLATE streams that the decoder must reject, and the unusual but valid
// streams next to them that it must keep decoding. zlib gives the same verdict on
// every stream of this file.
import compress.deflate
import compress.gzip
import compress.zlib
import encoding.binary
import encoding.hex
import hash.adler32
import hash.crc32

// Every stream is decoded first through a callback that stops after max_output bytes,
// so that a decoder which produces output without end fails the test instead of
// hanging it. The entry points without such a limit only run after that.
const max_output = 1 << 20

struct Sink {
mut:
	out  []u8
	over bool // more than max_output bytes arrived
}

fn collect(chunk []u8, userdata voidptr) int {
	mut s := unsafe { &Sink(userdata) }
	if s.out.len + chunk.len > max_output {
		s.over = true
		return 0
	}
	s.out << chunk
	return chunk.len
}

// in_zlib puts a raw DEFLATE stream that decodes to `plain` in a zlib container.
fn in_zlib(raw []u8, plain []u8) []u8 {
	mut z := [u8(0x78), 0x9c]
	z << raw
	z << binary.big_endian_get_u32(adler32.sum(plain))
	return z
}

// in_gzip puts a raw DEFLATE stream that decodes to `plain` in a gzip container.
fn in_gzip(raw []u8, plain []u8) []u8 {
	mut g := [u8(0x1f), 0x8b, 8, 0, 0, 0, 0, 0, 0, 0xff]
	g << raw
	g << binary.little_endian_get_u32(crc32.sum(plain))
	g << binary.little_endian_get_u32(u32(plain.len))
	return g
}

// decode_error returns the error that every entry point reports for the raw DEFLATE
// stream `raw`, or a description of what happened instead.
fn decode_error(raw []u8) string {
	z := in_zlib(raw, [])
	g := in_gzip(raw, [])
	mut sink := Sink{}
	mut msg := ''
	if n := deflate.decompress_with_callback(raw, collect, &sink) {
		if sink.over {
			return 'more than ${max_output} bytes of output'
		}
		return 'no error, ${n} bytes of output'
	} else {
		msg = err.msg()
	}
	mut errors := []string{}
	zlib.decompress_with_callback(z, collect, &sink) or { errors << err.msg() }
	gzip.decompress_with_callback(g, collect, &sink) or { errors << err.msg() }
	zlib.decompress(z) or { errors << err.msg() }
	gzip.decompress(g) or { errors << err.msg() }
	deflate.decompress(z) or { errors << err.msg() }
	deflate.decompress(g) or { errors << err.msg() }
	deflate.decompress(raw) or { errors << err.msg() }
	deflate.decompress_raw_with_consumed(raw) or { errors << err.msg() }
	if errors.len != 8 {
		return 'only ${errors.len} of the 8 other entry points fail'
	}
	for e in errors {
		if e != msg {
			return 'different errors: `${msg}` and `${e}`'
		}
	}
	return msg
}

// decode_problem returns '' when every entry point decodes the raw DEFLATE stream
// `raw` to `plain`, and a description of the first one that does not otherwise.
fn decode_problem(raw []u8, plain []u8) string {
	z := in_zlib(raw, plain)
	g := in_gzip(raw, plain)
	mut sink := Sink{}
	n := deflate.decompress_with_callback(raw, collect, &sink) or { return 'error: ${err.msg()}' }
	if sink.over {
		return 'more than ${max_output} bytes of output'
	}
	if n != plain.len {
		return '${n} bytes delivered, expected ${plain.len}'
	}
	mut outputs := [sink.out]
	mut zlib_sink := Sink{}
	zlib.decompress_with_callback(z, collect, &zlib_sink) or { return 'error: ${err.msg()}' }
	outputs << zlib_sink.out
	mut gzip_sink := Sink{}
	gzip.decompress_with_callback(g, collect, &gzip_sink) or { return 'error: ${err.msg()}' }
	outputs << gzip_sink.out
	outputs << zlib.decompress(z) or { return 'error: ${err.msg()}' }
	outputs << gzip.decompress(g) or { return 'error: ${err.msg()}' }
	outputs << deflate.decompress(z) or { return 'error: ${err.msg()}' }
	outputs << deflate.decompress(g) or { return 'error: ${err.msg()}' }
	outputs << deflate.decompress(raw) or { return 'error: ${err.msg()}' }
	res := deflate.decompress_raw_with_consumed(raw) or { return 'error: ${err.msg()}' }
	outputs << res.decoded
	for i, out in outputs {
		if out != plain {
			return 'entry point ${i} decoded ${out.hex()}, expected ${plain.hex()}'
		}
	}
	if res.consumed != raw.len {
		return '${res.consumed} bytes consumed, expected ${raw.len}'
	}
	return ''
}

// Bits writes a DEFLATE bit stream: fields with their lowest bit first, Huffman
// codes with their highest bit first (RFC 1951 §3.1.1).
struct Bits {
mut:
	buf  []u8
	used int // bits taken in the last byte
}

fn (mut b Bits) put(value int, count int) {
	for i in 0 .. count {
		if b.used == 0 {
			b.buf << u8(0)
		}
		b.buf[b.buf.len - 1] |= u8(((value >> i) & 1) << b.used)
		b.used = (b.used + 1) & 7
	}
}

// pad appends zero bits, for a block that is rejected before its data is read.
fn (mut b Bits) pad(count int) {
	for _ in 0 .. count {
		b.put(0, 1)
	}
}

// code writes the code of symbol `sym` in the canonical Huffman code that the code
// lengths `lens` describe (RFC 1951 §3.2.2).
fn (mut b Bits) code(lens []int, sym int) {
	bits := lens[sym]
	if bits == 0 {
		panic('symbol ${sym} has no code')
	}
	// Codes are numbered by length, then by symbol.
	mut code := 0
	for s, l in lens {
		if l > 0 && l < bits {
			code += 1 << (bits - l)
		} else if l == bits && s < sym {
			code++
		}
	}
	for i := bits - 1; i >= 0; i-- {
		b.put(code >> i, 1)
	}
}

// code_lengths returns the code lengths of an alphabet of `symbols` symbols, where
// the symbols in `used` have the given lengths and the others have none.
fn code_lengths(symbols int, used map[int]int) []int {
	mut lens := []int{len: symbols}
	for sym, bits in used {
		lens[sym] = bits
	}
	return lens
}

// order in which a dynamic block header stores the code lengths of the code length code
const cl_order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

// plain_cl is a complete code length code without repeats: each length 0..15 has a
// code of 4 bits.
const plain_cl = []int{len: 19, init: if index < 16 { 4 } else { 0 }}

// header starts a final dynamic block (RFC 1951 §3.2.7) that announces `hlit`
// literal/length and `hdist` distance code lengths, written with the code length
// code `cl`. The code lengths, and then the data, are up to the caller.
fn header(hlit int, hdist int, cl []int) Bits {
	mut b := Bits{}
	b.put(1, 1) // BFINAL
	b.put(2, 2) // BTYPE: dynamic Huffman codes
	b.put(hlit - 257, 5)
	b.put(hdist - 1, 5)
	b.put(cl_order.len - 4, 4)
	for sym in cl_order {
		b.put(cl[sym], 3)
	}
	return b
}

// dynamic_block starts a final dynamic block with the literal/length code lengths
// `litlen` and the distance code lengths `dist`; the data is up to the caller.
fn dynamic_block(litlen []int, dist []int) Bits {
	mut b := header(litlen.len, dist.len, plain_cl)
	for l in litlen {
		b.code(plain_cl, l)
	}
	for l in dist {
		b.code(plain_cl, l)
	}
	return b
}

const no_distances = [0]

// literal/length symbols
const lit_a = 97
const lit_b = 98
const lit_c = 99
const end_of_block = 256

// A dynamic block whose literal/length code lengths are all zero. This stream made
// the decoder deliver zero bytes for as long as its caller accepted them.
fn test_no_literal_length_code_at_all() {
	stream := hex.decode('789c050080e47f1b00000001')!
	raw := stream[2..8].clone()
	assert in_zlib(raw, []) == stream
	assert decode_error(raw) == 'inflate: missing end-of-block code'
	mut b := dynamic_block(code_lengths(257, {}), no_distances)
	b.pad(16)
	assert decode_error(b.buf) == 'inflate: missing end-of-block code'
}

fn test_literal_length_code_without_end_of_block() {
	litlen := code_lengths(257, {
		lit_a: 1
		lit_b: 1
	})
	mut b := dynamic_block(litlen, no_distances)
	b.code(litlen, lit_a)
	b.code(litlen, lit_b)
	assert decode_error(b.buf) == 'inflate: missing end-of-block code'
}

fn test_empty_code_length_code() {
	mut b := header(257, 1, code_lengths(19, {}))
	b.pad(300)
	assert decode_error(b.buf) == 'inflate: incomplete code length code'
}

// block_with_repeats returns a block that decodes to `aa`. Its code lengths are
// written with the code length code `cl`, which needs the symbols 0, 1 and 18 (a
// run of zeros). After the length of the end-of-block symbol comes a run of
// `zeros_after_end_of_block` zeros, where 1, for the only distance code, is right.
fn block_with_repeats(cl []int, zeros_after_end_of_block int) Bits {
	mut b := header(257, 1, cl)
	b.code(cl, 18) // lengths 0..96
	b.put(97 - 11, 7)
	b.code(cl, 1) // `a`
	b.code(cl, 18) // lengths 98..235
	b.put(138 - 11, 7)
	b.code(cl, 18) // lengths 236..255
	b.put(20 - 11, 7)
	b.code(cl, 1) // end of block
	if zeros_after_end_of_block == 1 {
		b.code(cl, 0)
	} else {
		b.code(cl, 18)
		b.put(zeros_after_end_of_block - 11, 7)
	}
	litlen := code_lengths(257, {
		lit_a:        1
		end_of_block: 1
	})
	b.code(litlen, lit_a)
	b.code(litlen, lit_a)
	b.code(litlen, end_of_block)
	return b
}

fn test_incomplete_code_length_code() {
	complete := code_lengths(19, {
		0:  2
		1:  2
		18: 1
	})
	assert decode_problem(block_with_repeats(complete, 1).buf, 'aa'.bytes()) == ''
	incomplete := code_lengths(19, {
		0:  2
		1:  2
		18: 2
	})
	assert decode_error(block_with_repeats(incomplete, 1).buf) == 'inflate: incomplete code length code'
	single := code_lengths(19, {
		0: 1
	})
	mut b := header(257, 1, single)
	b.pad(300)
	assert decode_error(b.buf) == 'inflate: incomplete code length code'
}

fn test_code_length_repeat_past_the_last_length() {
	cl := code_lengths(19, {
		0:  2
		1:  2
		18: 1
	})
	// a run of 11 zeros where only the length of the one distance code is left
	assert decode_error(block_with_repeats(cl, 11).buf) == 'inflate: code length repeat past the last length'
}

fn test_code_length_repeat_without_a_previous_length() {
	cl := code_lengths(19, {
		0:  2
		1:  2
		16: 1
	})
	mut b := header(257, 1, cl)
	b.code(cl, 16)
	b.put(0, 2)
	b.pad(64)
	assert decode_error(b.buf) == 'inflate: repeat with empty history'
}

fn test_incomplete_literal_length_code() {
	three_of_four := code_lengths(257, {
		lit_a:        2
		lit_b:        2
		end_of_block: 2
	})
	mut b := dynamic_block(three_of_four, no_distances)
	b.code(three_of_four, lit_a)
	b.code(three_of_four, end_of_block)
	assert decode_error(b.buf) == 'inflate: incomplete literal/length code'
	// a single code is only valid with a length of one bit
	two_bits := code_lengths(257, {
		end_of_block: 2
	})
	b = dynamic_block(two_bits, no_distances)
	b.code(two_bits, end_of_block)
	assert decode_error(b.buf) == 'inflate: incomplete literal/length code'
}

fn test_incomplete_distance_code() {
	litlen := code_lengths(257, {
		lit_a:        1
		end_of_block: 1
	})
	three_of_four := code_lengths(3, {
		0: 2
		1: 2
		2: 2
	})
	mut b := dynamic_block(litlen, three_of_four)
	b.code(litlen, lit_a)
	b.code(litlen, end_of_block)
	assert decode_error(b.buf) == 'inflate: incomplete distance code'
	// a single code is only valid with a length of one bit
	b = dynamic_block(litlen, [2])
	b.code(litlen, lit_a)
	b.code(litlen, end_of_block)
	assert decode_error(b.buf) == 'inflate: incomplete distance code'
}

fn test_over_subscribed_codes() {
	litlen := code_lengths(257, {
		lit_a:        1
		lit_b:        1
		end_of_block: 1
	})
	mut b := dynamic_block(litlen, no_distances)
	b.pad(16)
	assert decode_error(b.buf).contains('over-subscribed')
	b = dynamic_block(code_lengths(257, {
		lit_a:        1
		end_of_block: 1
	}), [1, 1, 1])
	b.pad(16)
	assert decode_error(b.buf).contains('over-subscribed')
	b = header(257, 1, code_lengths(19, {
		0:  1
		1:  1
		18: 1
	}))
	b.pad(64)
	assert decode_error(b.buf).contains('over-subscribed')
}

// The alphabets have 286 literal/length and 30 distance symbols, but the counts in a
// block header can go up to 288 and 32.
fn test_too_many_literal_length_or_distance_codes() {
	used := {
		lit_a:        1
		end_of_block: 2
		285:          2
	}
	litlen := code_lengths(286, used)
	mut b := dynamic_block(litlen, code_lengths(30, {
		0:  1
		29: 1
	}))
	b.code(litlen, lit_a)
	b.code(litlen, 285) // a match of length 258
	b.put(0, 1) // at distance 1
	b.code(litlen, end_of_block)
	assert decode_problem(b.buf, 'a'.repeat(259).bytes()) == ''
	one_more := code_lengths(287, used)
	b = dynamic_block(one_more, no_distances)
	b.code(one_more, lit_a)
	b.code(one_more, end_of_block)
	assert decode_error(b.buf) == 'inflate: too many literal/length or distance codes'
	b = dynamic_block(litlen, code_lengths(31, {
		0:  1
		30: 1
	}))
	b.code(litlen, lit_a)
	b.code(litlen, end_of_block)
	assert decode_error(b.buf) == 'inflate: too many literal/length or distance codes'
}

// A block may consist of nothing but its end-of-block symbol, coded with one bit.
fn test_block_with_only_an_end_of_block_code() {
	litlen := code_lengths(257, {
		end_of_block: 1
	})
	mut b := dynamic_block(litlen, no_distances)
	b.code(litlen, end_of_block)
	assert decode_problem(b.buf, []) == ''
	// the other value of that bit is not a code
	b = dynamic_block(litlen, no_distances)
	b.put(1, 1)
	assert decode_error(b.buf) == 'inflate: invalid Huffman code'
}

// A block of literals only needs no distance code.
fn test_literals_only_block_without_distance_code() {
	litlen := code_lengths(257, {
		lit_a:        2
		lit_b:        2
		lit_c:        2
		end_of_block: 2
	})
	mut b := dynamic_block(litlen, no_distances)
	for c in 'abcabca' {
		b.code(litlen, c)
	}
	b.code(litlen, end_of_block)
	assert decode_problem(b.buf, 'abcabca'.bytes()) == ''
}

// Without a distance code, a length symbol has no distance to go with it. The decoder
// used to take a distance of 1 from no bits at all.
fn test_length_symbol_without_distance_code() {
	litlen := code_lengths(258, {
		lit_a:        1
		end_of_block: 2
		257:          2
	})
	mut b := dynamic_block(litlen, no_distances)
	b.code(litlen, lit_a)
	b.code(litlen, 257) // a match of length 3
	b.code(litlen, end_of_block)
	assert decode_error(b.buf) == 'inflate: invalid Huffman code'
}

// A block that uses a single distance codes it with one bit, not with none.
fn test_single_distance_code() {
	litlen := code_lengths(258, {
		lit_a:        1
		end_of_block: 2
		257:          2
	})
	mut b := dynamic_block(litlen, [1])
	b.code(litlen, lit_a)
	b.code(litlen, 257) // a match of length 3
	b.put(0, 1) // at distance 1
	b.code(litlen, end_of_block)
	assert decode_problem(b.buf, 'aaaa'.bytes()) == ''
	// the other value of that bit is not a code
	b = dynamic_block(litlen, [1])
	b.code(litlen, lit_a)
	b.code(litlen, 257)
	b.put(1, 1)
	b.code(litlen, end_of_block)
	assert decode_error(b.buf) == 'inflate: invalid Huffman code'
}

// Codes of every length up to the longest one, 15 bits, that use up the code space.
fn test_complete_code_with_the_longest_codes() {
	mut used := map[int]int{}
	for bits in 1 .. 16 {
		used[lit_a + bits - 1] = bits
	}
	used[end_of_block] = 15
	litlen := code_lengths(257, used)
	mut b := dynamic_block(litlen, no_distances)
	for c in 'aonb' {
		b.code(litlen, c)
	}
	b.code(litlen, end_of_block)
	assert decode_problem(b.buf, 'aonb'.bytes()) == ''
}
