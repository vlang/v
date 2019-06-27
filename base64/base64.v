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
	p := data.cstr()
	len := data.len
	mut pad := 0
	if len > 0 && (len % 4 != 0 || p[len - 1] == `=`) {
		pad = 1
	}
	L := ((len + 3) / 4 - pad) * 4
	str_len := L / 4 * 3 + pad
	mut str := malloc(str_len + 2)
	mut j := 0
	for i := 0; i < L; i = i+4 {
		n := (Index[p[i]] << 18) | (Index[p[i + 1]] << 12) |
			(Index[p[i + 2]] << 6) | (Index[p[i + 3]])
		str[j] = n >> 16
		j++
		str[j] = n >> 8 & 0xff
		j++
		str[j] = n & 0xff
		j++
	}
	if pad > 0 {
		mut nn := (Index[p[L]] << 18) | (Index[p[L + 1]] << 12)
		str[str_len - 1] = nn >> 16
		if len > L + 2 && p[L + 2] != `=` {
			nn = nn | (Index[p[L + 2]] << 6)
			str[str_len] = nn >> 8 & 0xff
		}
	}
	str[str_len + 1] = `\0`
	return tos(str, str_len+1)
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
