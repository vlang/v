module json2

// EncoderOptions provides a list of options for encoding
@[params]
pub struct EncoderOptions {
pub:
	prettify       bool
	indent_string  string = '    '
	newline_string string = '\n'

	enum_as_int bool

	escape_unicode bool
}

struct Encoder {
	EncoderOptions
mut:
	level  int
	prefix string

	output []u8 = []u8{cap: 2048}
}

@[inline]
fn workaround_cast[T](val voidptr) T {
	return *(&T(val))
}

// encode is a generic function that encodes a type into a JSON string.
pub fn encode[T](val T, config EncoderOptions) string {
	mut encoder := Encoder{
		EncoderOptions: config
	}

	encoder.encode_value(val)

	return encoder.output.bytestr()
}

fn (mut encoder Encoder) encode_value[T](val T) {
	$if T.unaliased_typ is string {
		encoder.encode_string(workaround_cast[string](&val))
	} $else $if T.unaliased_typ is bool {
		encoder.encode_boolean(workaround_cast[bool](&val))
	} $else $if T.unaliased_typ is u8 {
		encoder.encode_number(workaround_cast[u8](&val))
	} $else $if T.unaliased_typ is u16 {
		encoder.encode_number(workaround_cast[u16](&val))
	} $else $if T.unaliased_typ is u32 {
		encoder.encode_number(workaround_cast[u32](&val))
	} $else $if T.unaliased_typ is u64 {
		encoder.encode_number(workaround_cast[u64](&val))
	} $else $if T.unaliased_typ is i8 {
		encoder.encode_number(workaround_cast[i8](&val))
	} $else $if T.unaliased_typ is i16 {
		encoder.encode_number(workaround_cast[i16](&val))
	} $else $if T.unaliased_typ is int || T.unaliased_typ is i32 {
		encoder.encode_number(workaround_cast[int](&val))
	} $else $if T.unaliased_typ is i64 {
		encoder.encode_number(workaround_cast[i64](&val))
	} $else $if T.unaliased_typ is usize {
		encoder.encode_number(workaround_cast[usize](&val))
	} $else $if T.unaliased_typ is isize {
		encoder.encode_number(workaround_cast[isize](&val))
	} $else $if T.unaliased_typ is f32 {
		encoder.encode_number(workaround_cast[f32](&val))
	} $else $if T.unaliased_typ is f64 {
		encoder.encode_number(workaround_cast[f64](&val))
	} $else $if T.unaliased_typ is $array {
		encoder.encode_array(val)
	} $else $if T.unaliased_typ is $map {
		encoder.encode_map(val)
	} $else $if T.unaliased_typ is $enum {
		encoder.encode_enum(val)
	} $else $if T.unaliased_typ is $sumtype {
		encoder.encode_sumtype(val)
	} $else $if T is JsonEncoder { // uses T, because alias could be implementing JsonEncoder, while the base type does not
		encoder.encode_custom(val)
	} $else $if T is Encodable { // uses T, because alias could be implementing JsonEncoder, while the base type does not
		encoder.encode_custom2(val)
	} $else $if T.unaliased_typ is $struct {
		unsafe { encoder.encode_struct(val) }
	}
}

fn (mut encoder Encoder) encode_string(val string) {
	encoder.output << `"`
	mut buffer_start := 0
	mut buffer_end := 0
	for buffer_end < val.len {
		character := val[buffer_end]
		match character {
			`"`, `\\` {
				unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
				buffer_end++
				buffer_start = buffer_end

				encoder.output << `\\`
				encoder.output << character
			}
			`\b` {
				unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
				buffer_end++
				buffer_start = buffer_end

				encoder.output << `\\`
				encoder.output << `b`
			}
			`\n` {
				unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
				buffer_end++
				buffer_start = buffer_end

				encoder.output << `\\`
				encoder.output << `n`
			}
			`\f` {
				unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
				buffer_end++
				buffer_start = buffer_end

				encoder.output << `\\`
				encoder.output << `f`
			}
			`\t` {
				unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
				buffer_end++
				buffer_start = buffer_end

				encoder.output << `\\`
				encoder.output << `t`
			}
			`\r` {
				unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
				buffer_end++
				buffer_start = buffer_end

				encoder.output << `\\`
				encoder.output << `r`
			}
			else {
				if character < 0x20 { // control characters
					unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
					buffer_end++
					buffer_start = buffer_end

					encoder.output << `\\`
					encoder.output << `u`

					hex_string := '${character:04x}'

					unsafe { encoder.output.push_many(hex_string.str, 4) }

					continue
				}
				if encoder.escape_unicode {
					if character >= 0b1111_0000 { // four bytes
						unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
						unicode_point_low := val[buffer_end..buffer_end + 4].bytes().byterune() or {
							0
						} - 0x10000

						hex_string := '\\u${0xD800 + ((unicode_point_low >> 10) & 0x3FF):04X}\\u${
							0xDC00 + (unicode_point_low & 0x3FF):04x}'

						buffer_end += 4
						buffer_start = buffer_end

						unsafe { encoder.output.push_many(hex_string.str, 12) }

						continue
					} else if character >= 0b1110_0000 { // three bytes
						unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
						hex_string := '\\u${val[buffer_end..buffer_end + 3].bytes().byterune() or {
							0
						}:04x}'

						buffer_end += 3
						buffer_start = buffer_end

						unsafe { encoder.output.push_many(hex_string.str, 6) }

						continue
					} else if character >= 0b1100_0000 { // two bytes
						unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }
						hex_string := '\\u${val[buffer_end..buffer_end + 2].bytes().byterune() or {
							0
						}:04x}'

						buffer_end += 2
						buffer_start = buffer_end

						unsafe { encoder.output.push_many(hex_string.str, 6) }

						continue
					}
				}

				buffer_end++
			}
		}
	}
	unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }

	encoder.output << `"`
}

fn (mut encoder Encoder) encode_boolean(val bool) {
	if val {
		unsafe { encoder.output.push_many(true_string.str, true_string.len) }
	} else {
		unsafe { encoder.output.push_many(false_string.str, false_string.len) }
	}
}

fn (mut encoder Encoder) encode_number[T](val T) {
	integer_val := val.str()
	$if T is $float {
		if integer_val[integer_val.len - 1] == `0` { // ends in .0
			unsafe {
				integer_val.len -= 2
			}
		}
	}
	unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
}

fn (mut encoder Encoder) encode_null() {
	unsafe { encoder.output.push_many(null_string.str, null_string.len) }
}

fn (mut encoder Encoder) encode_array[T](val []T) {
	encoder.output << `[`
	if encoder.prettify {
		encoder.increment_level()
		encoder.add_indent()
	}

	for i, item in val {
		encoder.encode_value(item)
		if i < val.len - 1 {
			encoder.output << `,`
			if encoder.prettify {
				encoder.add_indent()
			}
		} else {
			if encoder.prettify {
				encoder.decrement_level()
				encoder.add_indent()
			}
		}
	}

	encoder.output << `]`
}

fn (mut encoder Encoder) encode_map[T](val map[string]T) {
	encoder.output << `{`
	if encoder.prettify {
		encoder.increment_level()
		encoder.add_indent()
	}

	mut i := 0
	for key, value in val {
		encoder.encode_string(key)
		encoder.output << `:`
		if encoder.prettify {
			encoder.output << ` `
		}
		encoder.encode_value(value)
		if i < val.len - 1 {
			encoder.output << `,`
			if encoder.prettify {
				encoder.add_indent()
			}
		} else {
			if encoder.prettify {
				encoder.decrement_level()
				encoder.add_indent()
			}
		}

		i++
	}

	encoder.output << `}`
}

fn (mut encoder Encoder) encode_enum[T](val T) {
	if encoder.enum_as_int {
		encoder.encode_number(workaround_cast[int](&val))
	} else {
		mut enum_val := val.str()
		$if val is $alias {
			mut i := enum_val.len - 3
			for enum_val[i] != `(` {
				i--
			}

			enum_val = enum_val[i + 1..enum_val.len - 1]
		}
		encoder.output << `"`
		unsafe { encoder.output.push_many(enum_val.str, enum_val.len) }
		encoder.output << `"`
	}
}

fn (mut encoder Encoder) encode_sumtype[T](val T) {
	$for variant in T.variants {
		if val is variant {
			encoder.encode_value(val)
		}
	}
}

struct EncoderFieldInfo {
	key_name string

	is_skip      bool
	is_omitempty bool
	is_required  bool
}

fn check_not_empty[T](val T) ?bool {
	$if val is string {
		if val == '' {
			return false
		}
	} $else $if val is $int || val is $float {
		if val == 0 {
			return false
		}
	} $else $if val is ?string {
		return val ? != ''
	} $else $if val is ?int {
		return val ? != 0
	} $else $if val is ?f64 || val is ?f32 {
		return val ? != 0.0
	}
	return true
}

// TODO: fix compilation with -autofree, and remove the tag @[manualfree] here:
@[manualfree; unsafe]
fn (mut encoder Encoder) cached_field_infos[T]() []EncoderFieldInfo {
	static field_infos := &[]EncoderFieldInfo(nil)
	if field_infos == nil {
		field_infos = &[]EncoderFieldInfo{}
		$for field in T.fields {
			mut is_skip := false
			mut key_name := ''
			mut is_omitempty := false
			mut is_required := false
			for attr in field.attrs {
				match attr {
					'skip' {
						is_skip = true
						break
					}
					'omitempty' {
						is_omitempty = true
					}
					'required' {
						is_required = true
					}
					else {}
				}
				if attr.starts_with('json:') {
					if attr == 'json: -' {
						is_skip = true
						break
					}
					key_name = attr[6..]
				}
			}
			field_infos << EncoderFieldInfo{
				key_name:     if key_name == '' { field.name } else { key_name }
				is_skip:      is_skip
				is_omitempty: is_omitempty
				is_required:  is_required
			}
		}
	}
	return *field_infos
}

@[unsafe]
fn (mut encoder Encoder) encode_struct[T](val T) {
	encoder.output << `{`
	field_infos := encoder.cached_field_infos[T]()
	mut i := 0
	mut is_first := true
	$for field in T.fields {
		field_info := field_infos[i]
		i++

		mut write_field := true

		if field_info.is_skip {
			write_field = false
		} else {
			value := val.$(field.name)

			if field_info.is_omitempty {
				$if value is $option {
					write_field = check_not_empty(value) or { false }
				} $else {
					write_field = check_not_empty(value) or { false }
				}
			}

			if !field_info.is_required {
				$if value is $option {
					if value == none {
						write_field = false
					}
				}
			}
		}

		$if field.indirections != 0 {
			if val.$(field.name) == unsafe { nil } {
				write_field = false
			}
		}

		if write_field {
			if is_first {
				if encoder.prettify {
					encoder.increment_level()
				}
				is_first = false
			} else {
				encoder.output << `,`
			}
			if encoder.prettify {
				encoder.add_indent()
			}

			encoder.encode_string(field_info.key_name)

			encoder.output << `:`
			if encoder.prettify {
				encoder.output << ` `
			}

			$if field is $option {
				if val.$(field.name) == none {
					unsafe { encoder.output.push_many(null_string.str, null_string.len) }
				} else {
					encoder.encode_value(val.$(field.name))
				}
			} $else $if field.indirections == 1 {
				encoder.encode_value(*val.$(field.name))
			} $else $if field.indirections == 2 {
				encoder.encode_value(**val.$(field.name))
			} $else $if field.indirections == 3 {
				encoder.encode_value(***val.$(field.name))
			} $else {
				encoder.encode_value(val.$(field.name))
			}
		}
	}
	if encoder.prettify && !is_first {
		encoder.decrement_level()
		encoder.add_indent()
	}

	encoder.output << `}`
}

fn (mut encoder Encoder) encode_custom[T](val T) {
	integer_val := val.to_json()
	unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
}

fn (mut encoder Encoder) encode_custom2[T](val T) {
	integer_val := val.json_str()
	unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
}

fn (mut encoder Encoder) increment_level() {
	encoder.level++
	encoder.prefix = encoder.newline_string + encoder.indent_string.repeat(encoder.level)
}

fn (mut encoder Encoder) decrement_level() {
	encoder.level--
	encoder.prefix = encoder.newline_string + encoder.indent_string.repeat(encoder.level)
}

fn (mut encoder Encoder) add_indent() {
	unsafe { encoder.output.push_many(encoder.prefix.str, encoder.prefix.len) }
}
