module yaml

import strings

// write_spaces appends `n` spaces to `sb`.
fn write_spaces(mut sb strings.Builder, n int) {
	for _ in 0 .. n {
		sb.write_u8(` `)
	}
}

// emit_yaml_any streams `value` into `sb` as block-style YAML.
fn emit_yaml_any(mut sb strings.Builder, value Any, indent int) {
	match value {
		map[string]Any { emit_yaml_map(mut sb, value, indent) }
		[]Any { emit_yaml_array(mut sb, value, indent) }
		else { emit_yaml_scalar(mut sb, value) }
	}
}

// emit_yaml_map writes `value` as a block-style YAML mapping. An empty map
// is emitted as the inline `{}` form; otherwise each key is JSON-quoted (see
// `write_json_escaped_string`) and nested containers indent one level deeper.
fn emit_yaml_map(mut sb strings.Builder, value map[string]Any, indent int) {
	if value.len == 0 {
		sb.write_string('{}')
		return
	}
	mut first := true
	for key, item in value {
		if !first {
			sb.write_u8(`\n`)
		}
		first = false
		write_spaces(mut sb, indent)
		write_json_escaped_string(mut sb, key)
		match item {
			map[string]Any, []Any {
				sb.write_u8(`:`)
				sb.write_u8(`\n`)
				emit_yaml_any(mut sb, item, indent + 2)
			}
			else {
				sb.write_string(': ')
				emit_yaml_scalar(mut sb, item)
			}
		}
	}
}

// emit_yaml_array writes `value` as a block-style YAML sequence. An empty
// array is emitted as the inline `[]` form; otherwise each item is prefixed
// with `- ` and nested containers indent one level deeper.
fn emit_yaml_array(mut sb strings.Builder, value []Any, indent int) {
	if value.len == 0 {
		sb.write_string('[]')
		return
	}
	mut first := true
	for item in value {
		if !first {
			sb.write_u8(`\n`)
		}
		first = false
		write_spaces(mut sb, indent)
		match item {
			map[string]Any, []Any {
				sb.write_u8(`-`)
				sb.write_u8(`\n`)
				emit_yaml_any(mut sb, item, indent + 2)
			}
			else {
				sb.write_string('- ')
				emit_yaml_scalar(mut sb, item)
			}
		}
	}
}

// emit_yaml_scalar writes `value` as a single YAML scalar token: strings go
// through `write_json_escaped_string`, booleans / numbers / null print their
// literal form. The container branch is type-required by V's exhaustive
// `match` over `Any` but is unreachable: `emit_yaml_any` routes maps and
// arrays to their dedicated emitters before falling back here.
fn emit_yaml_scalar(mut sb strings.Builder, value Any) {
	match value {
		string { write_json_escaped_string(mut sb, value) }
		bool { sb.write_string(if value { 'true' } else { 'false' }) }
		f64 { sb.write_string(value.str()) }
		i64 { sb.write_string(value.str()) }
		int { sb.write_string(value.str()) }
		u64 { sb.write_string(value.str()) }
		Null { sb.write_string('null') }
		[]Any, map[string]Any { emit_yaml_any(mut sb, value, 0) }
	}
}

// write_json_escaped_string writes `value` as a JSON string literal directly
// into `sb`. Matches `json2.encode`'s rules: standard short escapes for control
// chars, `\u00XX` for the rest below 0x20, and UTF-8 bytes passed through
// verbatim (no per-byte `\uXXXX` re-escape). Passes safe runs through in bulk
// via `write_string` on the original slice — the overwhelmingly common case for
// human YAML — and only switches to per-byte handling at escape boundaries.
fn write_json_escaped_string(mut sb strings.Builder, value string) {
	sb.write_u8(`"`)
	mut start := 0
	for i := 0; i < value.len; i++ {
		c := value[i]
		if c >= 0x20 && c != `"` && c != `\\` {
			continue
		}
		if start < i {
			sb.write_string(value[start..i])
		}
		match c {
			`"` {
				sb.write_string('\\"')
			}
			`\\` {
				sb.write_string('\\\\')
			}
			`\n` {
				sb.write_string('\\n')
			}
			`\r` {
				sb.write_string('\\r')
			}
			`\t` {
				sb.write_string('\\t')
			}
			`\b` {
				sb.write_string('\\b')
			}
			`\f` {
				sb.write_string('\\f')
			}
			else {
				sb.write_string('\\u00')
				hex := '0123456789abcdef'
				sb.write_u8(hex[(c >> 4) & 0xf])
				sb.write_u8(hex[c & 0xf])
			}
		}

		start = i + 1
	}
	if start < value.len {
		sb.write_string(value[start..])
	}
	sb.write_u8(`"`)
}

// emit_any_as_json writes `a` as a compact JSON document.
fn emit_any_as_json(mut sb strings.Builder, a Any) {
	match a {
		map[string]Any {
			sb.write_u8(`{`)
			mut first := true
			for key, value in a {
				if !first {
					sb.write_u8(`,`)
				}
				first = false
				write_json_escaped_string(mut sb, key)
				sb.write_u8(`:`)
				emit_any_as_json(mut sb, value)
			}
			sb.write_u8(`}`)
		}
		[]Any {
			sb.write_u8(`[`)
			mut first := true
			for value in a {
				if !first {
					sb.write_u8(`,`)
				}
				first = false
				emit_any_as_json(mut sb, value)
			}
			sb.write_u8(`]`)
		}
		string {
			write_json_escaped_string(mut sb, a)
		}
		bool {
			sb.write_string(if a { 'true' } else { 'false' })
		}
		f64 {
			sb.write_string(a.str())
		}
		i64 {
			sb.write_string(a.str())
		}
		int {
			sb.write_string(a.str())
		}
		u64 {
			sb.write_string(a.str())
		}
		Null {
			sb.write_string('null')
		}
	}
}
