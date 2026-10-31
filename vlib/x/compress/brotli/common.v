// Brotli compression and decompression implemented in V.
module brotli

import hash.huffman

const trivial_block_size = 65536

const brotli_dictionary_data = $embed_file('dictionary.bin').to_bytes()

const code_length_code_order = [1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15]

// CompressParams controls Brotli compression.
@[params]
pub struct CompressParams {
	// quality selects the compression effort from 0 to 11.
pub:
	quality int = 5
	// window_size selects the RFC 7932 window size from 16 to 24.
	window_size int = 16
	// large_window selects the incompatible large-window format from 10 to 30.
	large_window int
	// comment embeds a metadata comment of at most 80 bytes.
	comment []u8
	// dictionary supplies a custom LZ77 dictionary.
	dictionary []u8
}

// DecompressParams controls Brotli decompression.
@[params]
pub struct DecompressParams {
pub:
	// concatenated accepts multiple Brotli streams in one input buffer.
	concatenated bool
	// comment requires the stream metadata to match this comment.
	comment []u8
	// dictionary supplies a custom LZ77 dictionary for distance references.
	dictionary []u8
}

struct DecodedStream {
	data     []u8
	metadata []u8
	consumed int
}

struct BitReader {
	data []u8
mut:
	pos     int
	bit_pos int
}

fn (mut r BitReader) read_bit() !u8 {
	if r.pos >= r.data.len {
		return error('brotli: truncated stream')
	}
	b := (r.data[r.pos] >> r.bit_pos) & 1
	r.bit_pos++
	if r.bit_pos == 8 {
		r.bit_pos = 0
		r.pos++
	}
	return b
}

fn (mut r BitReader) read_bits(n int) !u32 {
	if n <= 0 {
		return 0
	}
	if r.bit_pos + n <= 8 {
		if r.pos >= r.data.len {
			return error('brotli: truncated stream')
		}
		mask := u8((1 << n) - 1)
		value := u32((r.data[r.pos] >> r.bit_pos) & mask)
		r.bit_pos += n
		if r.bit_pos == 8 {
			r.bit_pos = 0
			r.pos++
		}
		return value
	}
	mut value := u32(0)
	mut shift := 0
	mut remaining := n
	if r.bit_pos > 0 {
		if r.pos >= r.data.len {
			return error('brotli: truncated stream')
		}
		available := 8 - r.bit_pos
		take := if remaining < available { remaining } else { available }
		mask := u8((1 << take) - 1)
		value |= u32((r.data[r.pos] >> r.bit_pos) & mask) << shift
		r.bit_pos += take
		shift += take
		remaining -= take
		if r.bit_pos == 8 {
			r.bit_pos = 0
			r.pos++
		}
	}
	for remaining >= 8 {
		if r.pos >= r.data.len {
			return error('brotli: truncated stream')
		}
		value |= u32(r.data[r.pos]) << shift
		r.pos++
		shift += 8
		remaining -= 8
	}
	if remaining > 0 {
		if r.pos >= r.data.len {
			return error('brotli: truncated stream')
		}
		value |= u32(r.data[r.pos] & u8((1 << remaining) - 1)) << shift
		r.bit_pos = remaining
	}
	return value
}

fn (mut r BitReader) align_byte() {
	if r.bit_pos != 0 {
		r.bit_pos = 0
		r.pos++
	}
}

fn (r BitReader) padding_bits_are_zero() bool {
	if r.bit_pos == 0 || r.pos >= r.data.len {
		return r.bit_pos == 0
	}
	mask := u8((0xff << r.bit_pos) & 0xff)
	return r.data[r.pos] & mask == 0
}

fn (r BitReader) remaining_bytes() int {
	return r.data.len - r.pos - if r.bit_pos == 0 {
		0
	} else {
		1
	}
}

struct BitWriter {
mut:
	out     []u8
	cur     u8
	bit_pos int
}

struct PrefixDecoder {
	max_bits int
	entries  map[u64]int
	flat     []u32
	order    huffman.BitOrder
}

struct CmdLutElement {
	insert_len_extra_bits int
	copy_len_extra_bits   int
	distance_code         int
	context               int
	insert_len_offset     int
	copy_len_offset       int
}

struct DistanceCode {
	symbol      int
	extra       int
	extra_bits  int
	should_push bool
}

fn (mut w BitWriter) write_bit(v u8) {
	w.cur |= (v & 1) << w.bit_pos
	w.bit_pos++
	if w.bit_pos == 8 {
		w.out << w.cur
		w.cur = 0
		w.bit_pos = 0
	}
}

fn (mut w BitWriter) write_bits(v u32, n int) {
	if n <= 0 {
		return
	}
	if w.bit_pos + n <= 8 {
		mask := u32((1 << n) - 1)
		w.cur |= u8(v & mask) << w.bit_pos
		w.bit_pos += n
		if w.bit_pos == 8 {
			w.out << w.cur
			w.cur = 0
			w.bit_pos = 0
		}
		return
	}
	mut value := v
	mut remaining := n
	if w.bit_pos > 0 {
		space := 8 - w.bit_pos
		take := if remaining < space { remaining } else { space }
		mask := u32((1 << take) - 1)
		w.cur |= u8(value & mask) << w.bit_pos
		value >>= take
		remaining -= take
		if w.bit_pos + take == 8 {
			w.out << w.cur
			w.cur = 0
			w.bit_pos = 0
		} else {
			w.bit_pos += take
		}
	}
	for remaining >= 8 {
		w.out << u8(value & 0xff)
		value >>= 8
		remaining -= 8
	}
	if remaining > 0 {
		w.cur = u8(value & u32((1 << remaining) - 1))
		w.bit_pos = remaining
	}
}

fn (mut w BitWriter) align_byte() {
	if w.bit_pos != 0 {
		w.out << w.cur
		w.cur = 0
		w.bit_pos = 0
	}
}

fn (mut w BitWriter) write_bytes(data []u8) {
	w.align_byte()
	w.out << data
}

fn (mut w BitWriter) finish() []u8 {
	w.align_byte()
	return w.out
}

fn max_length(lengths []int) int {
	mut max := 0
	for l in lengths {
		if l > max {
			max = l
		}
	}
	return max
}

fn build_prefix_decoder_with_order(lengths []int, order huffman.BitOrder) !PrefixDecoder {
	max_bits := max_length(lengths)
	if max_bits == 0 {
		mut sym := -1
		for i, l in lengths {
			if l < 0 {
				sym = i
				break
			}
		}
		if sym < 0 {
			return error('brotli: empty prefix tree')
		}
		mut entries := map[u64]int{}
		entries[u64(0)] = sym
		return PrefixDecoder{
			max_bits: 0
			entries:  entries
			flat:     []u32{}
			order:    order
		}
	}
	t := huffman.build(lengths: lengths, max_bits: max_bits, bit_order: order)!
	mut entries := map[u64]int{}
	for sym, length in lengths {
		if length == 0 {
			continue
		}
		entries[(u64(length) << 32) | u64(t.codes[sym])] = sym
	}
	flat := huffman.flat_table(lengths: lengths, max_bits: max_bits, bit_order: order) or {
		[]u32{}
	}
	return PrefixDecoder{
		max_bits: max_bits
		entries:  entries
		flat:     flat
		order:    order
	}
}

fn build_prefix_decoder(lengths []int) !PrefixDecoder {
	return build_prefix_decoder_with_order(lengths, .lsb_first)
}

fn (mut br BitReader) read_prefix_symbol(dec PrefixDecoder) !int {
	if dec.max_bits == 0 {
		if sym := dec.entries[u64(0)] {
			return sym
		}
		return error('brotli: invalid prefix code')
	}
	available := (br.data.len - br.pos) * 8 - br.bit_pos
	if dec.order == .lsb_first && dec.flat.len > 0 && available >= dec.max_bits {
		mut probe := br
		bits := probe.read_bits(dec.max_bits)!
		entry := dec.flat[int(bits)]
		if entry == huffman.flat_invalid_entry {
			return error('brotli: invalid prefix code')
		}
		length := int(entry & ((u32(1) << huffman.flat_length_bits) - 1))
		br.read_bits(length)!
		return int(entry >> huffman.flat_length_bits)
	}
	mut code := u32(0)
	for len in 1 .. dec.max_bits + 1 {
		bit := br.read_bit()!
		if dec.order == .msb_first {
			code = (code << 1) | u32(bit)
		} else {
			code |= u32(bit) << (len - 1)
		}
		if sym := dec.entries[(u64(len) << 32) | u64(code)] {
			return sym
		}
	}
	return error('brotli: invalid prefix code')
}

fn (mut r BitReader) read_varlen_uint8() !int {
	if r.read_bit()! == 0 {
		return 0
	}
	bits := int(r.read_bits(3)!)
	if bits == 0 {
		return 1
	}
	return (1 << bits) | int(r.read_bits(bits)!)
}

fn write_varlen_uint8(mut w BitWriter, value int) {
	if value == 0 {
		w.write_bit(0)
		return
	}
	w.write_bit(1)
	bits := bit_width(value) - 1
	w.write_bits(u32(bits), 3)
	w.write_bits(u32(value - (1 << bits)), bits)
}

fn read_meta_block_length(mut r BitReader, mnibbles int) !int {
	mut mlen := 0
	for i in 0 .. mnibbles {
		nib := int(r.read_bits(4)!)
		if i + 1 == mnibbles && mnibbles > 4 && nib == 0 {
			return error('brotli: invalid meta-block length nibble')
		}
		mlen |= int(u32(nib) << u32(i * 4))
	}
	return mlen + 1
}

fn write_meta_block_length(mut w BitWriter, mlen int, mnibbles int) {
	chunk_minus_one := u32(mlen - 1)
	for nib in 0 .. mnibbles {
		w.write_bits((chunk_minus_one >> (nib * 4)) & 0xf, 4)
	}
}
