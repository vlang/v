module base64

// encode_in_buffer base64 encodes the `[]u8` passed in `data` into `buffer`.
// encode_in_buffer returns the size of the encoded data in the buffer.
// Please note: The buffer should be large enough (i.e. 4/3 of the data.len, or larger) to hold the encoded data.
// Please note: The function does NOT allocate new memory, and is suitable for handling very large strings.
pub fn encode_in_buffer(data []u8, buffer &u8) int {
	return encode_from_buffer(buffer, data.data, data.len)
}

// encode_from_buffer will perform encoding from any type of src buffer
// and write the bytes into `dest`.
// Please note: The `dest` buffer should be large enough (i.e. 4/3 of the src_len, or larger) to hold the encoded data.
// Please note: This function is for internal base64 encoding
fn encode_from_buffer(dest &u8, src &u8, src_len int) int {
	if src_len == 0 {
		return 0
	}
	output_length := 4 * ((src_len + 2) / 3)

	mut d := unsafe { src }
	mut b := unsafe { dest }
	etable := enc_table.str

	mut di := 0
	mut si := 0
	n := (src_len / 3) * 3
	for si < n {
		// Convert 3x 8bit source bytes into 4 bytes
		unsafe {
			val := u32(d[si + 0]) << 16 | u32(d[si + 1]) << 8 | u32(d[si + 2])

			b[di + 0] = etable[val >> 18 & 0x3F]
			b[di + 1] = etable[val >> 12 & 0x3F]
			b[di + 2] = etable[val >> 6 & 0x3F]
			b[di + 3] = etable[val & 0x3F]
		}
		si += 3
		di += 4
	}

	remain := src_len - si
	if remain == 0 {
		return output_length
	}

	// Add the remaining small block and padding
	unsafe {
		mut val := u32(d[si + 0]) << 16
		if remain == 2 {
			val |= u32(d[si + 1]) << 8
		}

		b[di + 0] = etable[val >> 18 & 0x3F]
		b[di + 1] = etable[val >> 12 & 0x3F]

		match remain {
			2 {
				b[di + 2] = etable[val >> 6 & 0x3F]
				b[di + 3] = u8(`=`)
			}
			1 {
				b[di + 2] = u8(`=`)
				b[di + 3] = u8(`=`)
			}
			else {
				panic('base64: This case should never occur.')
			}
		}
	}
	return output_length
}

// decode_in_buffer decodes the base64 encoded `string` reference passed in `data` into `buffer`.
// decode_in_buffer returns the size of the decoded data in the buffer.
// Please note: The `buffer` should be large enough (i.e. 3/4 of the data.len, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
pub fn decode_in_buffer(data &string, buffer &u8) int {
	return decode_from_buffer(buffer, data.str, data.len)
}

// decode_from_buffer decodes the base64 encoded ASCII bytes from `data` into `buffer`.
// decode_from_buffer returns the size of the decoded data in the buffer.
// Please note: The `buffer` should be large enough (i.e. 3/4 of the data.len, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
pub fn decode_in_buffer_bytes(data []u8, buffer &u8) int {
	return decode_from_buffer(buffer, data.data, data.len)
}

// decode_from_buffer decodes the base64 encoded ASCII bytes from `src` into `dest`.
// decode_from_buffer returns the size of the decoded data in the buffer.
// Please note: The `dest` buffer should be large enough (i.e. 3/4 of the `src_len`, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
// Please note: This function is for internal base64 decoding
fn decode_from_buffer(dest &u8, src &u8, src_len int) int {
	if src_len < 4 {
		return 0
	}

	mut padding := 0
	if unsafe { src[src_len - 1] == `=` } {
		if unsafe { src[src_len - 2] == `=` } {
			padding = 2
		} else {
			padding = 1
		}
	}

	mut d := unsafe { src }
	mut b := unsafe { dest }

	unsafe {
		mut n_decoded_bytes := 0 // padding bytes are also counted towards this.
		mut si := 0

		mut datablock_64 := B64_64_datablock{
			data: 0
		}
		mut datablock_32 := B64_32_datablock{
			data: 0
		}

		for src_len - si >= 8 {
			// Converting 8 bytes of input into 6 bytes of output. Storing these in the upper bytes of an u64.
			datablock_64.data = assemble64(u8(index[d[si + 0]]), u8(index[d[si + 1]]),
				u8(index[d[si + 2]]), u8(index[d[si + 3]]), u8(index[d[si + 4]]), u8(index[d[si + 5]]),
				u8(index[d[si + 6]]), u8(index[d[si + 7]]))

			// Reading out the individual bytes from the u64. Watch out with endianess.
			$if little_endian {
				b[n_decoded_bytes + 0] = datablock_64.data_byte[7]
				b[n_decoded_bytes + 1] = datablock_64.data_byte[6]
				b[n_decoded_bytes + 2] = datablock_64.data_byte[5]
				b[n_decoded_bytes + 3] = datablock_64.data_byte[4]
				b[n_decoded_bytes + 4] = datablock_64.data_byte[3]
				b[n_decoded_bytes + 5] = datablock_64.data_byte[2]
			} $else {
				b[n_decoded_bytes + 0] = datablock_64.data_byte[0]
				b[n_decoded_bytes + 1] = datablock_64.data_byte[1]
				b[n_decoded_bytes + 2] = datablock_64.data_byte[2]
				b[n_decoded_bytes + 3] = datablock_64.data_byte[3]
				b[n_decoded_bytes + 4] = datablock_64.data_byte[4]
				b[n_decoded_bytes + 5] = datablock_64.data_byte[5]
			}

			n_decoded_bytes += 6
			si += 8
		}

		for src_len - si >= 4 {
			datablock_32.data = assemble32(u8(index[d[si + 0]]), u8(index[d[si + 1]]),
				u8(index[d[si + 2]]), u8(index[d[si + 3]]))
			$if little_endian {
				b[n_decoded_bytes + 0] = datablock_32.data_byte[3]
				b[n_decoded_bytes + 1] = datablock_32.data_byte[2]
				b[n_decoded_bytes + 2] = datablock_32.data_byte[1]
				b[n_decoded_bytes + 3] = datablock_32.data_byte[0]
			} $else {
				b[n_decoded_bytes + 0] = datablock_32.data_byte[0]
				b[n_decoded_bytes + 1] = datablock_32.data_byte[1]
				b[n_decoded_bytes + 2] = datablock_32.data_byte[2]
				b[n_decoded_bytes + 3] = datablock_32.data_byte[3]
			}

			n_decoded_bytes += 3
			si += 4
		}

		return n_decoded_bytes - padding
	}
}

union B64_64_datablock {
mut:
	data      u64
	data_byte [8]u8
}

union B64_32_datablock {
mut:
	data      u32
	data_byte [4]u8
}

// decode decodes the base64 encoded `string` value passed in `data`.
// Please note: If you need to decode many strings repeatedly, take a look at `decode_in_buffer`.
// Example: assert base64.decode('ViBpbiBiYXNlIDY0') == 'V in base 64'
pub fn decode(data string) []u8 {
	mut size := i64(data.len) * 3 / 4
	if size <= 0 || data.len % 4 != 0 {
		return []
	}
	size = (size + 3) & ~0x03 // round to the next multiple of 4 (the decoding loop writes multiples of 4 bytes)
	unsafe {
		buffer := malloc(int(size))
		n := decode_in_buffer(data, buffer)
		return buffer.vbytes(n)
	}
}

// decode_str is the string variant of decode
pub fn decode_str(data string) string {
	size := data.len * 3 / 4
	if size <= 0 || data.len % 4 != 0 {
		return ''
	}
	unsafe {
		buffer := malloc_noscan(size + 1)
		buffer[size] = 0
		blen := decode_in_buffer(data, buffer)
		return tos(buffer, blen)
	}
}

// encode encodes the `[]u8` value passed in `data` to base64.
// Please note: base64 encoding returns a `string` that is ~ 4/3 larger than the input.
// Please note: If you need to encode many strings repeatedly, take a look at `encode_in_buffer`.
// Example: assert base64.encode('V in base 64') == 'ViBpbiBiYXNlIDY0'
pub fn encode(data []u8) string {
	return alloc_and_encode(data.data, data.len)
}

// encode_str is the string variant of encode
pub fn encode_str(data string) string {
	return alloc_and_encode(data.str, data.len)
}

// alloc_and_encode is a private function that allocates and encodes data into a string
// Used by encode and encode_str
fn alloc_and_encode(src &u8, len int) string {
	if len == 0 {
		return ''
	}
	size := 4 * ((len + 2) / 3)
	if size <= 0 {
		return ''
	}
	unsafe {
		buffer := malloc_noscan(size + 1)
		buffer[size] = 0
		blen := encode_from_buffer(buffer, src, len)
		return tos(buffer, blen)
	}
}
