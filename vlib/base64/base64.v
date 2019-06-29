// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module base64

const (
	Index = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	62, 63, 62, 62, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0,
	0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
	17, 18, 19, 20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 63, 0, 26, 27, 28, 29,
	30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
	47, 48, 49, 50, 51]
)

pub fn decode(data string) string {
	mut padding := 0
	if data.ends_with('=') {
		if data.ends_with('==') {
			padding = 2
		} else {
			padding = 1
		}
	}
	//input_length is the length of meaningful data
	input_length := data.len - padding
	output_length := input_length * 3 / 4

	mut i := 0
	mut j := 0
	mut str := malloc(output_length)

	for i < input_length {
		mut char_a := 0
		mut char_b := 0
		mut char_c := 0
		mut char_d := 0

		if i < input_length {
			char_a = Index[int(data[i])]
			i++
		}
		if i < input_length {
			char_b = Index[int(data[i])]
			i++
		}
		if i < input_length {
			char_c = Index[int(data[i])]
			i++
		}
		if i < input_length {
			char_d = Index[int(data[i])]
			i++
		}

		decoded_bytes := (char_a << 18) | (char_b << 12) | (char_c << 6) | (char_d << 0)
		str[j] = decoded_bytes >> 16
		str[j+1] = (decoded_bytes >> 8) & 0xff
		str[j+2] = (decoded_bytes >> 0) & 0xff

		j += 3
	}
	return tos(str, output_length)
}

const (
	EncodingTable = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
)

pub fn encode(data string) string {
	input_length := data.len
	output_length := 4 * ((input_length + 2) / 3)

	mut i := 0
	mut j := 0
	mut str := malloc(output_length)

	for i < input_length {
		mut octet_a := 0
		mut octet_b := 0
		mut octet_c := 0

		if i < input_length {
			octet_a = int(data[i])
			i++
		}
		if i < input_length {
			octet_b = int(data[i])
			i++
		}
		if i < input_length {
			octet_c = int(data[i])
			i++
		}

		triple := ((octet_a << 0x10) + (octet_b << 0x08) + octet_c)

		str[j+0] = EncodingTable[(triple >> 3 * 6) & 63] // 63 is 0x3F
		str[j+1] = EncodingTable[(triple >> 2 * 6) & 63]
		str[j+2] = EncodingTable[(triple >> 1 * 6) & 63]
		str[j+3] = EncodingTable[(triple >> 0 * 6) & 63]
		j += 4
	}

	mod_table := [0, 2, 1]
	for i = 0; i < mod_table[input_length % 3]; i++ {
		str[output_length - 1 - i] = `=`
	}

	return tos(str, output_length)
}
