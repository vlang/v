// Snappy framing format — streaming wrapper around raw Snappy blocks.
//
// Spec: https://github.com/google/snappy/blob/main/framing_format.txt
//
// A framed stream is a sequence of typed chunks, each with a 4-byte header:
//
//   [type: 1 byte] [length: 3 bytes, little-endian]
//
// Chunk types:
//   0xff  Stream identifier  — must be the first chunk; payload = "sNaPpY"
//   0x00  Compressed data    — 4-byte masked CRC32C + Snappy-compressed block
//   0x01  Uncompressed data  — 4-byte masked CRC32C + raw block
//   0xfe  Padding            — payload is ignored
//   0x80..0xcd  Reserved skippable    — may be silently ignored
//   0x02..0x7f  Reserved unskippable  — must be treated as an error
//
// Public API:
//   encode_stream(input []u8) []u8
//   decode_stream(input []u8) ![]u8
//
// For incremental (push-based) I/O see StreamEncoder and StreamDecoder.

module snappy

import math
import io

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const stream_identifier_magic = 'sNaPpY'.bytes()
const chunk_type_stream_id = u8(0xff)
const chunk_type_compressed = u8(0x00)
const chunk_type_uncompressed = u8(0x01)
const chunk_type_padding = u8(0xfe)

// Pre encoded stream header
const stream_header = [
	chunk_type_stream_id,
	u8(stream_identifier_magic.len & 0xff),
	u8((stream_identifier_magic.len >> 8) & 0xff),
	u8((stream_identifier_magic.len >> 16) & 0xff),
	stream_identifier_magic[0],
	stream_identifier_magic[1],
	stream_identifier_magic[2],
	stream_identifier_magic[3],
	stream_identifier_magic[4],
	stream_identifier_magic[5],
]

// max_chunk_data_size is the largest block of uncompressed data per chunk.
const max_chunk_data_size = 65536

// ---------------------------------------------------------------------------
// One-shot API
// ---------------------------------------------------------------------------

// encode_stream wraps `input` in the Snappy framing format and returns the
// complete byte stream. Input is split into ≤64 KiB blocks automatically.
pub fn encode_stream(input []u8) []u8 {
	// Upper bound: identifier chunk + one compressed chunk per block.
	// Each block: 4-byte header + 4-byte CRC + max_compressed_length.
	blocks := (input.len + max_chunk_data_size - 1) / max_chunk_data_size
	mut out := []u8{cap: 10 + blocks * (8 + max_compressed_length(max_chunk_data_size))}

	out << stream_header

	mut pos := 0
	for pos < input.len {
		end := math.min(pos + max_chunk_data_size, input.len)
		write_data_chunk(mut out, input[pos..end])
		pos = end
	}

	// Always emit at least the identifier for empty input.
	return out
}

// decode_stream decodes a Snappy framing stream and returns the concatenated
// uncompressed data, or an error if the stream is malformed.
pub fn decode_stream(input []u8) ![]u8 {
	// The compressed stream is typically smaller than the uncompressed data,
	// so use a larger initial capacity estimate.
	mut out := []u8{cap: input.len * 2}
	mut pos := 0

	// The very first chunk must be the stream identifier.
	pos = read_stream_identifier(input, pos)!

	for pos < input.len {
		chunk_type, chunk_len, new_pos := read_chunk_header(input, pos)!
		pos = new_pos

		if pos + chunk_len > input.len {
			return error('snappy framing: chunk body truncated at pos ${pos}')
		}
		chunk_data := input[pos..pos + chunk_len]
		pos += chunk_len

		if chunk_type == chunk_type_compressed {
			uncompressed := decode_compressed_chunk(chunk_data)!
			out << uncompressed
		} else if chunk_type == chunk_type_uncompressed {
			raw := decode_uncompressed_chunk(chunk_data)!
			out << raw
		} else if chunk_type == chunk_type_padding || int(chunk_type) >= 0x80 {
			// 0xfe = padding, 0x80..0xfd = reserved skippable; ignore.
			continue
		} else {
			// 0x02..0x7f — reserved unskippable; must error.
			return error('snappy framing: unsupported unskippable chunk type 0x${chunk_type:02x}')
		}
	}

	return out
}

// ---------------------------------------------------------------------------
// Incremental encoder
// ---------------------------------------------------------------------------

// StreamEncoder holds state for incremental framing-format encoding.
// Call write() one or more times, then close() to flush the final chunk.
// Take the encoded bytes using read().
pub struct StreamEncoder {
mut:
	buf    []u8 // pending uncompressed bytes
	out    []u8 = stream_header // accumulated encoded output, initialized with stream identifier
	closed bool
}

// write appends `buf` to the encoder, flushing complete 64 KiB chunks.
pub fn (mut enc StreamEncoder) write(buf []u8) !int {
	if enc.closed {
		return error('snappy: write on a closed stream encoder')
	}

	enc.buf << buf
	for enc.buf.len >= max_chunk_data_size {
		block := enc.buf[..max_chunk_data_size]
		write_data_chunk(mut enc.out, block)
		enc.buf = enc.buf[max_chunk_data_size..].clone()
	}

	return buf.len
}

pub fn (mut enc StreamEncoder) read(mut buf []u8) !int {
	if enc.out.len == 0 {
		if enc.closed {
			return io.Eof{}
		}
		return 0 // not finished yet, just no data ready, the caller should feed more
	}

	n := copy(mut buf, enc.out)

	if n == enc.out.len {
		enc.out = []u8{}
	} else {
		enc.out = enc.out[n..].clone()
	}

	return n
}

// close flushes any remaining buffered bytes and returns the complete stream.
// The encoder must not be used after this call.
pub fn (mut enc StreamEncoder) close() {
	if enc.closed {
		return
	}

	if enc.buf.len > 0 {
		write_data_chunk(mut enc.out, enc.buf)
		enc.buf = []
	}
	enc.closed = true
}

// peek returns the encoded bytes produced so far without flushing.
pub fn (enc StreamEncoder) peek() []u8 {
	return enc.out
}

// ---------------------------------------------------------------------------
// Incremental decoder
// ---------------------------------------------------------------------------

// StreamDecoder holds state for incremental framing-format decoding.
// Call write() with successive chunks of compressed input, and call read() to read the decoded bytes.
// The writer must call close() when it finishes writing the bytes.
pub struct StreamDecoder {
mut:
	buf        []u8 // unparsed compressed bytes
	offset     int  // read position within buf (avoids repeated cloning)
	identified bool // have we seen the stream identifier chunk?
	err        ?IError
	output     []u8 // decoded bytes ready for the caller
	closed     bool
}

// write appends `buf` to the decoder's input buffer and processes as many
// complete chunks as possible. Returns an error if the stream is malformed.
pub fn (mut dec StreamDecoder) write(buf []u8) !int {
	if dec.closed {
		return error('snappy: write to closed stream decoder')
	}
	if err := dec.err {
		return err
	}
	dec.buf << buf

	for {
		remaining := dec.buf.len - dec.offset
		// Need at least 4 bytes for the chunk header.
		if remaining < 4 {
			break
		}
		chunk_type, chunk_len, hdr_end := read_chunk_header(dec.buf, dec.offset) or {
			dec_err := error('snappy: failed while reading chunk header: ${err}')
			dec.err = dec_err
			return dec_err
		}
		if hdr_end + chunk_len > dec.buf.len {
			break // wait for more data
		}
		chunk_data := dec.buf[hdr_end..hdr_end + chunk_len]

		if !dec.identified {
			if chunk_type != chunk_type_stream_id {
				dec_err := error('snappy framing: stream does not begin with identifier chunk')
				dec.err = dec_err
				return dec_err
			}
			if chunk_len != stream_identifier_magic.len {
				dec_err := error('snappy framing: bad identifier chunk length ${chunk_len}')
				dec.err = dec_err
				return dec_err
			}
			if chunk_data != stream_identifier_magic {
				dec_err := error('snappy framing: bad stream identifier payload')
				dec.err = dec_err
				return dec_err
			}
			dec.identified = true
		} else if chunk_type == chunk_type_compressed {
			uncompressed := decode_compressed_chunk(chunk_data) or {
				dec_err := error('snappy: failed while decoding compressed chunk: ${err}')
				dec.err = dec_err
				return dec_err
			}
			dec.output << uncompressed
		} else if chunk_type == chunk_type_uncompressed {
			raw := decode_uncompressed_chunk(chunk_data) or {
				dec_err := error('snappy: failed while decoding uncompressed chunk: ${err}')
				dec.err = dec_err
				return dec_err
			}
			dec.output << raw
		} else if chunk_type == chunk_type_padding || int(chunk_type) >= 0x80 {
			// skip
		} else {
			dec_err :=
				error('snappy framing: unsupported unskippable chunk type 0x${chunk_type:02x}')
			dec.err = dec_err
			return dec_err
		}

		dec.offset = hdr_end + chunk_len
	}

	// Compact the buffer: discard consumed bytes only when worthwhile
	// to avoid copying on every single chunk.
	if dec.offset > 0 {
		if dec.offset == dec.buf.len {
			dec.buf = []u8{}
		} else {
			dec.buf = dec.buf[dec.offset..].clone()
		}
		dec.offset = 0
	}

	return buf.len
}

// read removes and returns decoded bytes. When the output buffer is empty it
// surfaces any error recorded during write()/close(). Only then does it return
// io.Eof{} for a cleanly terminated stream.
pub fn (mut dec StreamDecoder) read(mut buf []u8) !int {
	// Always drain already-decoded output first so the caller never loses data.
	if dec.output.len > 0 {
		n := copy(mut buf, dec.output)
		dec.output = dec.output[n..]
		return n
	}

	if dec.closed {
		// Surface truncation / malformed-stream errors before EOF.
		if err := dec.err {
			return err
		}
		return io.Eof{}
	}

	return 0
}

// close marks the stream finished and validates its terminal state.
// Returns an error if the stream was truncated or the identifier was never received.
// The error is also stored internally so read() will surface it after draining output.
pub fn (mut dec StreamDecoder) close() ! {
	if dec.closed {
		return
	}
	dec.closed = true

	remaining := dec.buf.len - dec.offset

	if remaining > 0 {
		err :=
			error('snappy framing: stream closed with ${remaining} unprocessed bytes (truncated chunk)')
		dec.err = err
		return err
	}

	if !dec.identified && dec.buf.len > 0 {
		err := error('snappy framing: stream closed before identifier chunk was received')
		dec.err = err
		return err
	}
}

// ---------------------------------------------------------------------------
// Chunk writers
// ---------------------------------------------------------------------------

// write_data_chunk compresses `block`, chooses the compact representation,
// and appends the chunk (type + header + masked CRC + payload) to `out`.
fn write_data_chunk(mut out []u8, block []u8) {
	crc := mask_crc(crc32c(block))
	compressed := compress(block)

	// Use the compressed form only when it is strictly smaller.
	if compressed.len < block.len {
		// Compressed chunk: type=0x00, length=4+compressed.len
		out << chunk_type_compressed
		write_u24le(mut out, 4 + compressed.len)
		write_u32le(mut out, crc)
		out << compressed
	} else {
		// Uncompressed chunk: type=0x01, length=4+block.len
		out << chunk_type_uncompressed
		write_u24le(mut out, 4 + block.len)
		write_u32le(mut out, crc)
		out << block
	}
}

// ---------------------------------------------------------------------------
// Chunk readers
// ---------------------------------------------------------------------------

// read_stream_identifier reads and validates the identifier chunk starting at
// `pos` and returns the position immediately after it.
fn read_stream_identifier(data []u8, pos int) !int {
	chunk_type, chunk_len, new_pos := read_chunk_header(data, pos)!
	if chunk_type != chunk_type_stream_id {
		return error('snappy framing: stream must begin with identifier chunk, got 0x${chunk_type:02x}')
	}
	if chunk_len != stream_identifier_magic.len {
		return error('snappy framing: bad identifier chunk length ${chunk_len}')
	}
	if new_pos + chunk_len > data.len {
		return error('snappy framing: identifier chunk truncated')
	}
	magic := data[new_pos..new_pos + chunk_len]
	if magic != stream_identifier_magic {
		return error('snappy framing: bad stream identifier magic')
	}
	return new_pos + chunk_len
}

// read_chunk_header reads the 4-byte chunk header at `pos` and returns
// (chunk_type, chunk_body_length, pos_after_header).
@[inline]
fn read_chunk_header(data []u8, pos int) !(u8, int, int) {
	if pos + 4 > data.len {
		return error('snappy framing: truncated chunk header at pos ${pos}')
	}
	chunk_type := data[pos]
	chunk_len := int(u32(data[pos + 1]) | u32(data[pos + 2]) << 8 | u32(data[pos + 3]) << 16)
	return chunk_type, chunk_len, pos + 4
}

// decode_compressed_chunk decompresses a compressed data chunk payload
// (CRC32C[4] + snappy_data[…]) and verifies the checksum.
fn decode_compressed_chunk(payload []u8) ![]u8 {
	if payload.len < 4 {
		return error('snappy framing: compressed chunk too short')
	}
	stored_masked_crc := u32(payload[0]) | u32(payload[1]) << 8 | u32(payload[2]) << 16 | u32(payload[3]) << 24
	uncompressed := decompress(payload[4..])!
	actual_crc := mask_crc(crc32c(uncompressed))
	if actual_crc != stored_masked_crc {
		return error('snappy framing: CRC32C mismatch in compressed chunk')
	}
	return uncompressed
}

// decode_uncompressed_chunk validates and returns the payload of an
// uncompressed data chunk (CRC32C[4] + raw_data[…]).
fn decode_uncompressed_chunk(payload []u8) ![]u8 {
	if payload.len < 4 {
		return error('snappy framing: uncompressed chunk too short')
	}
	stored_masked_crc := u32(payload[0]) | u32(payload[1]) << 8 | u32(payload[2]) << 16 | u32(payload[3]) << 24
	raw := payload[4..].clone()
	actual_crc := mask_crc(crc32c(raw))
	if actual_crc != stored_masked_crc {
		return error('snappy framing: CRC32C mismatch in uncompressed chunk')
	}
	return raw
}

// ---------------------------------------------------------------------------
// Little-endian integer writers
// ---------------------------------------------------------------------------

@[inline]
fn write_u24le(mut out []u8, v int) {
	out << u8(v & 0xff)
	out << u8((v >> 8) & 0xff)
	out << u8((v >> 16) & 0xff)
}

@[inline]
fn write_u32le(mut out []u8, v u32) {
	out << u8(v & 0xff)
	out << u8((v >> 8) & 0xff)
	out << u8((v >> 16) & 0xff)
	out << u8((v >> 24) & 0xff)
}
