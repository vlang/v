module bcrypt

const alphabet = './ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'

fn char64(c u8) u8 {
	for i, ch in alphabet {
		if ch == c {
			return u8(i)
		}
	}
	return 255
}

fn base64_decode(data string) []u8 {
	mut dest_index := 0
	mut result := []u8{}
	for src_index := 0; src_index < data.len - 1; src_index += 4 {
		c1 := char64(data[src_index])

		if src_index + 1 >= data.len {
			break
		}

		c2 := char64(data[src_index + 1])

		// Invalid data */
		if c1 == 255 || c2 == 255 {
			break
		}

		result << ((c1 << 2) | ((c2 & 0x30) >> 4))
		dest_index += 1

		if src_index + 2 >= data.len || dest_index == 16 {
			break
		}

		c3 := char64(data[src_index + 2])
		if c3 == 255 {
			break
		}

		result << (((c2 & 0x0f) << 4) | ((c3 & 0x3c) >> 2))
		dest_index += 1

		if src_index + 3 >= data.len || dest_index == 16 {
			break
		}

		c4 := char64(data[src_index + 3])
		if c4 == 255 {
			break
		}

		result << (((c3 & 0x03) << 6) | c4)
		dest_index += 1

		if dest_index == 16 {
			break
		}
	}

	return result
}

fn base64_encode(data []u8) string {
	mut src_index := 0
	mut result := []u8{}
	for src_index < data.len {
		mut c1 := data[src_index]
		src_index += 1
		result << alphabet[c1 >> 2]
		c1 = (c1 & 0x03) << 4
		if src_index >= data.len {
			result << alphabet[c1]
			break
		}

		mut c2 := data[src_index]
		src_index += 1
		c1 |= (c2 >> 4) & 0x0f
		result << alphabet[c1]
		c1 = (c2 & 0x0f) << 2
		if src_index >= data.len {
			result << alphabet[c1]
			break
		}

		c2 = data[src_index]
		src_index += 1
		c1 |= (c2 >> 6) & 0x03
		result << alphabet[c1]
		result << alphabet[c2 & 0x3f]
	}

	return result.bytestr()
}
