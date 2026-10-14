/**********************************************************************
*
* Shared decoder types: decoded image, animation frame, bit reader
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module common

// Image is a decoded, top-down RGBA image (straight, non-premultiplied alpha).
pub struct Image {
pub:
	width  int
	height int
	rgba   []u8 // len == width * height * 4
}

// Frame is one coalesced animation frame.
pub struct Frame {
pub:
	image    Image
	delay_ms int
	disposal int
}

// Animation is a sequence of frames with a loop count.
pub struct Animation {
pub:
	frames     []Frame
	loop_count int // 0 = forever, >= 1 finite
}

// BitReader reads bits from a byte buffer in either LSB-first (GIF/WebP)
// or MSB-first (JPEG) order. Format-specific byte stuffing (e.g. JPEG
// FF00) is handled by the caller.
pub struct BitReader {
mut:
	data     []u8
	byte_pos int
	bit_pos  int // bits already consumed in the current byte (0..8)
	msb      bool
}

// new_bitreader_lsb creates an LSB-first reader.
pub fn new_bitreader_lsb(data []u8) BitReader {
	return BitReader{
		data: data
		msb:  false
	}
}

// new_bitreader_msb creates an MSB-first reader.
pub fn new_bitreader_msb(data []u8) BitReader {
	return BitReader{
		data: data
		msb:  true
	}
}

// read_bits reads up to 32 bits and returns them as a u32.
// Bits are assembled in the order they are read.
pub fn (mut r BitReader) read_bits(n int) !u32 {
	mut val := u32(0)
	if r.msb {
		for _ in 0 .. n {
			if r.bit_pos == 8 {
				r.bit_pos = 0
				r.byte_pos++
			}
			if r.byte_pos >= r.data.len {
				return error('BitReader: unexpected EOF (msb)')
			}
			bit := (r.data[r.byte_pos] >> (7 - r.bit_pos)) & 1
			val = (val << 1) | u32(bit)
			r.bit_pos++
		}
	} else {
		for i in 0 .. n {
			if r.bit_pos == 8 {
				r.bit_pos = 0
				r.byte_pos++
			}
			if r.byte_pos >= r.data.len {
				return error('BitReader: unexpected EOF (lsb)')
			}
			bit := (r.data[r.byte_pos] >> r.bit_pos) & 1
			val |= u32(bit) << i
			r.bit_pos++
		}
	}
	return val
}

// align_bytes skips to the next byte boundary (no-op if already aligned).
pub fn (mut r BitReader) align_bytes() {
	if r.bit_pos != 0 {
		r.bit_pos = 0
		r.byte_pos++
	}
}

// remaining returns the number of whole bytes left (approximate).
pub fn (r BitReader) remaining() int {
	return r.data.len - r.byte_pos
}
