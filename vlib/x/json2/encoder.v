// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strings

fn write_value(v Any, i int, len int, mut wr strings.Builder) {
	str := v.str()
	if v is string {
		wr.write('"$str"')
	} else {
		wr.write(str)
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
		wr.write('"$k":')
		write_value(v, i, flds.len, mut wr)
		i++
	}
	wr.write_b(`}`)
	defer {
		wr.free()
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
		wr.free()
	}
	res := wr.str()
	return res
}

// str returns the string representation of the `Any` type.
pub fn (f Any) str() string {
	match f {
		string {
			return f
		}
		int {
			return f.str()
		}
		i64 {
			return f.str()
		}
		f32 {
			str_f32 := f.str()
			return if str_f32.ends_with('.') {
				str_f32 + '0'
			} else {
				str_f32
			}
		}
		f64 {
			str_f64 := f.str()
			return if str_f64.ends_with('.') {
				str_f64 + '0'
			} else {
				str_f64
			}
		}
		bool {
			return f.str()
		}
		map[string]Any {
			return f.str()
		}
		Null {
			return 'null'
		}
		else {
			if f is []Any {
				return f.str()
			}
			return ''
		}
	}
}
