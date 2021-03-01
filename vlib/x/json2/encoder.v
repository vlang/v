// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strings

fn write_value(v Any, i int, len int, mut wr strings.Builder) {
	str := v.json_str()
	if v is string {
		wr.write_string('"$str"')
	} else {
		wr.write_string(str)
	}
	if i >= len - 1 {
		return
	}
	wr.write_b(`,`)
}

// str returns the string representation of the `map[string]Any`.
pub fn (flds map[string]Any) str() string {
	mut wr := strings.new_builder(200)
	wr.write_b(`{`)
	mut i := 0
	for k, v in flds {
		wr.write_string('"$k":')
		write_value(v, i, flds.len, mut wr)
		i++
	}
	wr.write_b(`}`)
	defer {
		unsafe { wr.free() }
	}
	res := wr.str()
	return res
}

// str returns the string representation of the `[]Any`.
pub fn (flds []Any) str() string {
	mut wr := strings.new_builder(200)
	wr.write_b(`[`)
	for i, v in flds {
		write_value(v, i, flds.len, mut wr)
	}
	wr.write_b(`]`)
	defer {
		unsafe { wr.free() }
	}
	res := wr.str()
	return res
}

// str returns the string representation of the `Any` type. Use the `json_str` method
// if you want to use the escaped str() version of the `Any` type.
pub fn (f Any) str() string {
	if f is string {
		return f
	} else {
		return f.json_str()
	}
}

// json_str returns the JSON string representation of the `Any` type.
pub fn (f Any) json_str() string {
	match f {
		string {
			return json_string(f)
		}
		int {
			return f.str()
		}
		i64 {
			return f.str()
		}
		f32 {
			str_f32 := f.str()
			return if str_f32.ends_with('.') { '${str_f32}0' } else { str_f32 }
		}
		f64 {
			str_f64 := f.str()
			return if str_f64.ends_with('.') { '${str_f64}0' } else { str_f64 }
		}
		bool {
			return f.str()
		}
		map[string]Any {
			return f.str()
		}
		[]Any {
			return f.str()
		}
		Null {
			return 'null'
		}
	}
}

// char_len_list is a modified version of builtin.utf8_str_len
// that returns an array of character lengths. (e.g "t✔" => [1,2])
fn char_len_list(s string) []int {
	mut l := 1
	mut ls := []int{}
	for i := 0; i < s.len; i++ {
		c := s[i]
		if (c & (1 << 7)) != 0 {
			for t := byte(1 << 6); (c & t) != 0; t >>= 1 {
				l++
				i++
			}
		}
		ls << l
		l = 1
	}
	return ls
}

const escaped_chars = [r'\b', r'\f', r'\n', r'\r', r'\t']

// json_string returns the JSON spec-compliant version of the string.
[manualfree]
fn json_string(s string) string {
	// not the best implementation but will revisit it soon
	char_lens := char_len_list(s)
	mut sb := strings.new_builder(s.len)
	mut i := 0
	defer {
		unsafe {
			char_lens.free()
			// freeing string builder on defer after
			// returning .str() still isn't working :(
			// sb.free()
		}
	}
	for char_len in char_lens {
		if char_len == 1 {
			chr := s[i]
			if chr in json2.important_escapable_chars {
				for j := 0 ; j < json2.important_escapable_chars.len; j++ {
					if chr == json2.important_escapable_chars[j] {
						sb.write_string(escaped_chars[j])
						break
					}
				}
			} else if chr == `"` || chr == `/` || chr == `\\` {
				sb.write_string('\\' + chr.ascii_str())
			} else {
				sb.write_b(chr)
			}
		} else {
			slice := s[i .. i + char_len]
			hex_code := slice.utf32_code().hex()
			if hex_code.len == 4 {
				sb.write_string('\\u$hex_code')
			} else {
				// TODO: still figuring out what
				// to do with more than 4 chars
				sb.write_b(` `)
			}
			unsafe {
				slice.free()
				hex_code.free()
			}
		}
		i += char_len
	}
	str := sb.str()
	unsafe { sb.free() }
	return str
}
