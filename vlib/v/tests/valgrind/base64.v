import encoding.base64

fn main() {
	repeats := 1000
	input_size := 3000

	s_original := []u8{len: input_size, init: `a`}
	s_encoded := base64.encode(s_original)
	s_encoded_bytes := s_encoded.bytes()
	s_decoded := base64.decode(s_encoded)

	assert s_encoded.len > s_original.len
	assert s_original == s_decoded

	ebuffer := unsafe { malloc(s_encoded.len) }
	dbuffer := unsafe { malloc(s_decoded.len) }
	defer {
		unsafe { free(ebuffer) }
		unsafe { free(dbuffer) }
	}
	//
	encoded_size := base64.encode_in_buffer(s_original, ebuffer)
	mut encoded_in_buf := []u8{len: encoded_size}
	unsafe { C.memcpy(encoded_in_buf.data, ebuffer, encoded_size) }
	assert input_size * 4 / 3 == encoded_size
	assert encoded_in_buf[0] == `Y`
	assert encoded_in_buf[1] == `W`
	assert encoded_in_buf[2] == `F`
	assert encoded_in_buf[3] == `h`

	assert encoded_in_buf[encoded_size - 4] == `Y`
	assert encoded_in_buf[encoded_size - 3] == `W`
	assert encoded_in_buf[encoded_size - 2] == `F`
	assert encoded_in_buf[encoded_size - 1] == `h`

	assert encoded_in_buf == s_encoded_bytes

	decoded_size := base64.decode_in_buffer(s_encoded, dbuffer)
	assert decoded_size == input_size
	mut decoded_in_buf := []u8{len: decoded_size}
	unsafe { C.memcpy(decoded_in_buf.data, dbuffer, decoded_size) }
	assert decoded_in_buf == s_original

	mut s := 0
	for _ in 0 .. repeats {
		resultsize := base64.encode_in_buffer(s_original, ebuffer)
		s += resultsize
		assert resultsize == s_encoded.len
	}

	for _ in 0 .. repeats {
		resultsize := base64.decode_in_buffer(s_encoded, dbuffer)
		s += resultsize
		assert resultsize == s_decoded.len
	}

	println('Final s: $s')
	//	assert s == 39147008
}
