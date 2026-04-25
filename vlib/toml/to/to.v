// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module to

import toml
import x.json2

type DocOrAny = toml.Any | toml.Doc

// json returns `doa` as a JSON encoded string.
pub fn json(doa DocOrAny) string {
	match doa {
		toml.Doc {
			return doa.ast.table.str()
		}
		toml.Any {
			return any_to_json(doa)
		}
	}
}

// json returns `a` as a JSON encoded string.
fn any_to_json(a toml.Any) string {
	match a {
		toml.Null {
			return 'null'
		}
		toml.DateTime {
			return json2.Any(toml.Any(a).string()).json_str()
		}
		toml.Date {
			return json2.Any(toml.Any(a).string()).json_str()
		}
		toml.Time {
			return json2.Any(toml.Any(a).string()).json_str()
		}
		string {
			return json2.Any(toml.Any(a).string()).json_str()
		}
		bool {
			return json2.Any(toml.Any(a).bool()).json_str()
		}
		f32 {
			return json2.Any(toml.Any(a).f32()).json_str()
		}
		f64 {
			return json2.Any(toml.Any(a).f64()).json_str()
		}
		i64 {
			return json2.Any(toml.Any(a).i64()).json_str()
		}
		int {
			return json2.Any(toml.Any(a).int()).json_str()
		}
		u64 {
			return json2.Any(toml.Any(a).u64()).json_str()
		}
		map[string]toml.Any {
			mut str := '{'
			for key, val in a {
				json_key := json2.Any(key)
				str += ' ${json_key.json_str()}: ${any_to_json(val)},'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]toml.Any {
			mut str := '['
			for val in a {
				str += ' ${any_to_json(val)},'
			}
			str = str.trim_right(',')
			str += ' ]'
			return str
		}
	}
}

// json_any returns `Any` as a `x.json2.Any` type.
pub fn json_any(a toml.Any) json2.Any {
	match a {
		toml.Null {
			return json2.Null{}
		}
		toml.DateTime {
			return json2.Any(a.str())
		}
		toml.Date {
			return json2.Any(a.str())
		}
		toml.Time {
			return json2.Any(a.str())
		}
		string {
			return json2.Any(a.str())
		}
		bool {
			return json2.Any(bool(a))
		}
		int {
			return json2.Any(int(a))
		}
		f32 {
			return json2.Any(f32(a))
		}
		f64 {
			return json2.Any(f64(a))
		}
		i64 {
			return json2.Any(i64(a))
		}
		u64 {
			return json2.Any(u64(a))
		}
		map[string]toml.Any {
			mut jmap := map[string]json2.Any{}
			for key, val in a {
				jmap[key] = json_any(val)
			}
			return jmap
		}
		[]toml.Any {
			mut jarr := []json2.Any{}

			for val in a {
				jarr << json_any(val)
			}

			return jarr
		}
	}
}
