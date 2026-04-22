module json2

import time

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

// encode is a generic function that encodes a type into a JSON string.
pub fn encode[T](val T, config EncoderOptions) string {
	mut encoder := Encoder{
		EncoderOptions: config
	}

	encoder.encode_value[T](val)

	return encoder.output.bytestr()
}

fn (mut encoder Encoder) encode_value[T](val T) {
	$if T.unaliased_typ is string {
		encoder.encode_string(string(val))
	} $else $if T.unaliased_typ is bool {
		encoder.encode_boolean(bool(val))
	} $else $if T.unaliased_typ is u8 {
		encoder.encode_number(u8(val))
	} $else $if T.unaliased_typ is u16 {
		encoder.encode_number(u16(val))
	} $else $if T.unaliased_typ is u32 {
		encoder.encode_number(u32(val))
	} $else $if T.unaliased_typ is u64 {
		encoder.encode_number(u64(val))
	} $else $if T.unaliased_typ is i8 {
		encoder.encode_number(i8(val))
	} $else $if T.unaliased_typ is i16 {
		encoder.encode_number(i16(val))
	} $else $if T.unaliased_typ is int || T.unaliased_typ is i32 {
		encoder.encode_number(i32(val))
	} $else $if T.unaliased_typ is i64 {
		encoder.encode_number(i64(val))
	} $else $if T.unaliased_typ is usize {
		encoder.encode_number(usize(val))
	} $else $if T.unaliased_typ is isize {
		encoder.encode_number(isize(val))
	} $else $if T.unaliased_typ is f32 {
		encoder.encode_number(f32(val))
	} $else $if T.unaliased_typ is f64 {
		encoder.encode_number(f64(val))
	} $else $if T.unaliased_typ is voidptr {
		encoder.encode_number(0)
	} $else $if T is $pointer {
		if voidptr(val) == unsafe { nil } {
			encoder.encode_null()
		} else {
			encoder.encode_value(*val)
		}
	} $else $if T.unaliased_typ is $array_fixed {
		encoder.output << `[`
		for i in 0 .. val.len {
			encoder.encode_value(val[i])
			if i < val.len - 1 {
				encoder.output << `,`
			}
		}
		encoder.output << `]`
	} $else $if T.unaliased_typ is $array {
		encoder.encode_array(val)
	} $else $if T.unaliased_typ is $map {
		encoder.output << `{`
		if encoder.prettify {
			encoder.increment_level()
			encoder.add_indent()
		}
		mut mi := 0
		for key, value in val {
			encoder.encode_string('${key}')
			encoder.output << `:`
			if encoder.prettify {
				encoder.output << ` `
			}
			encoder.encode_value(value)
			if mi < val.len - 1 {
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
			mi++
		}
		encoder.output << `}`
	} $else $if T.unaliased_typ is $enum {
		if encoder.enum_as_int || enum_uses_json_as_number[T]() {
			encoder.encode_number(int(val))
		} else {
			mut enum_val := 'unknown enum value'
			$for member in T.values {
				if member.value == val {
					enum_val = member.name
					for attr in member.attrs {
						if json_attr := json_attr_value(attr) {
							enum_val = json_attr
						}
					}
				}
			}
			encoder.output << `"`
			unsafe { encoder.output.push_many(enum_val.str, enum_val.len) }
			encoder.output << `"`
		}
	} $else $if T.unaliased_typ is $sumtype {
		encoder.encode_sumtype[T](val)
	} $else $if T is JsonEncoder { // uses T, because alias could be implementing JsonEncoder, while the base type does not
		integer_val := val.to_json()
		unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
	} $else $if T is Encodable { // uses T, because alias could be implementing JsonEncoder, while the base type does not
		integer_val := val.json_str()
		unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
	} $else $if T.unaliased_typ is $struct {
		unsafe {
			encoder.output << `{`
			is_first := encoder.encode_struct_fields[T](val, true, [], '')
			if encoder.prettify && !is_first {
				encoder.decrement_level()
				encoder.add_indent()
			}
			encoder.output << `}`
		}
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
					unsafe {
						encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start)
					}
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
						unsafe {
							encoder.output.push_many(val.str + buffer_start,
								buffer_end - buffer_start)
						}
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
						unsafe {
							encoder.output.push_many(val.str + buffer_start,
								buffer_end - buffer_start)
						}
						hex_string := '\\u${val[buffer_end..buffer_end + 3].bytes().byterune() or {
							0
						}:04x}'

						buffer_end += 3
						buffer_start = buffer_end

						unsafe { encoder.output.push_many(hex_string.str, 6) }

						continue
					} else if character >= 0b1100_0000 { // two bytes
						unsafe {
							encoder.output.push_many(val.str + buffer_start,
								buffer_end - buffer_start)
						}
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
	mut integer_val := ''
	$if T is u8 {
		integer_val = u8(val).str()
	} $else $if T is u16 {
		integer_val = u16(val).str()
	} $else $if T is u32 {
		integer_val = u32(val).str()
	} $else $if T is u64 {
		integer_val = u64(val).str()
	} $else $if T is i8 {
		integer_val = i8(val).str()
	} $else $if T is i16 {
		integer_val = i16(val).str()
	} $else $if T is int || T is i32 {
		integer_val = i32(val).str()
	} $else $if T is i64 {
		integer_val = i64(val).str()
	} $else $if T is usize {
		integer_val = usize(val).str()
	} $else $if T is isize {
		integer_val = isize(val).str()
	} $else $if T is f32 {
		integer_val = f32(val).str()
	} $else $if T is f64 {
		integer_val = f64(val).str()
	}
	$if T is $float {
		if integer_val.len > 2 && integer_val[integer_val.len - 2] == `.`
			&& integer_val[integer_val.len - 1] == `0` { // ends in .0
			// `2.0` = > `2`
			// but skip float in scientific notation, `1e+10`
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

fn (mut encoder Encoder) encode_array[T](val T) {
	encoder.output << `[`
	if encoder.prettify {
		encoder.increment_level()
		encoder.add_indent()
	}

	for i, item in val {
		$if T is $pointer {
			if voidptr(item) == unsafe { nil } {
				encoder.encode_null()
			} else {
				unsafe { encoder.encode_pointer_array_item(item) }
			}
		} $else {
			encoder.encode_value(item)
		}
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

@[unsafe]
fn (mut encoder Encoder) encode_pointer_array_item[T](item T) {
	encoder.encode_struct(item)
}

fn (mut encoder Encoder) encode_map[K, T](val map[K]T) {
	encoder.output << `{`
	if encoder.prettify {
		encoder.increment_level()
		encoder.add_indent()
	}

	mut i := 0
	for key, value in val {
		encoder.encode_string('${key}')
		encoder.output << `:`
		if encoder.prettify {
			encoder.output << ` `
		}
		encoder.encode_value[T](value)
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
	if encoder.enum_as_int || enum_uses_json_as_number[T]() {
		encoder.encode_number(int(val))
	} else {
		mut enum_val := 'unknown enum value'
		$for member in T.values {
			if member.value == val {
				enum_val = member.name
				for attr in member.attrs {
					if json_attr := json_attr_value(attr) {
						enum_val = json_attr
					}
				}
			}
		}
		encoder.output << `"`
		unsafe { encoder.output.push_many(enum_val.str, enum_val.len) }
		encoder.output << `"`
	}
}

fn (mut encoder Encoder) encode_sumtype[T](val T) {
	$if T is $pointer {
		// Pointer types are handled by encode_value's $pointer branch;
		// this instantiation is generated but never called.
	} $else {
		$for variant in T.variants {
			if val is variant {
				variant_name := sumtype_variant_name(typeof(variant.typ).name)
				$if variant.typ is time.Time {
					encoder.encode_sumtype_time_variant(val, variant_name)
				} $else $if variant.typ is $struct {
					encoder.encode_sumtype_struct_variant(val, variant_name)
				} $else {
					encoder.encode_value(val)
				}
			}
		}
	}
}

fn (mut encoder Encoder) encode_object_key(is_first bool, key string) bool {
	if is_first {
		if encoder.prettify {
			encoder.increment_level()
		}
	} else {
		encoder.output << `,`
	}
	if encoder.prettify {
		encoder.add_indent()
	}
	encoder.encode_string(key)
	encoder.output << `:`
	if encoder.prettify {
		encoder.output << ` `
	}
	return false
}

fn (mut encoder Encoder) encode_sumtype_struct_variant[T](val T, variant_name string) {
	encoder.output << `{`
	mut is_first := unsafe { encoder.encode_struct_fields[T](val, true, [], '') }
	is_first = encoder.encode_object_key(is_first, '_type')
	encoder.encode_string(variant_name)
	if encoder.prettify && !is_first {
		encoder.decrement_level()
		encoder.add_indent()
	}
	encoder.output << `}`
}

@[markused]
fn (mut encoder Encoder) encode_sumtype_time_variant(val time.Time, variant_name string) {
	encoder.output << `{`
	mut is_first := true
	is_first = encoder.encode_object_key(is_first, '_type')
	encoder.encode_string(variant_name)
	is_first = encoder.encode_object_key(is_first, 'value')
	encoder.encode_number(val.unix())
	if encoder.prettify && !is_first {
		encoder.decrement_level()
		encoder.add_indent()
	}
	encoder.output << `}`
}

struct EncoderFieldInfo {
	key_name string

	is_skip      bool
	is_omitempty bool
	is_required  bool
	is_json_null bool
}

fn get_value_from_optional[T](val ?T) T {
	return val or { T{} }
}

fn check_not_empty[T](val T) ?bool {
	$if T.indirections != 0 {
		return val != unsafe { nil }
	} $else $if T.unaliased_typ is string {
		if val == '' {
			return false
		}
	} $else $if T.unaliased_typ is $int || T.unaliased_typ is $float {
		if val == 0 {
			return false
		}
	} $else $if T.unaliased_typ is $array || T.unaliased_typ is $map {
		return val.len != 0
	} $else $if val is ?string {
		opt := ?string(val)
		if sval := opt {
			return sval != ''
		}
		return false
	} $else $if val is ?int {
		opt := ?int(val)
		if ival := opt {
			return ival != 0
		}
		return false
	} $else $if val is ?f64 {
		opt := ?f64(val)
		if fval := opt {
			return fval != 0.0
		}
		return false
	} $else $if val is ?f32 {
		opt := ?f32(val)
		if fval := opt {
			return fval != 0.0
		}
		return false
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
			mut is_json_null := false
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
					'json_null' {
						is_json_null = true
					}
					else {}
				}

				if attr.starts_with('json:') {
					json_attr := json_attr_value(attr) or { continue }
					if json_attr == '-' {
						is_skip = true
						break
					}
					key_name = json_attr
				}
			}
			field_infos << EncoderFieldInfo{
				key_name:     if key_name == '' { field.name } else { key_name }
				is_skip:      is_skip
				is_omitempty: is_omitempty
				is_required:  is_required
				is_json_null: is_json_null
			}
		}
	}
	return *field_infos
}

fn (mut encoder Encoder) encode_struct_field_value[T](val T) {
	$if T is $option {
		if val == none {
			unsafe { encoder.output.push_many(null_string.str, null_string.len) }
		} else {
			encoder.encode_value(get_value_from_optional(val))
		}
	} $else $if T.indirections == 1 {
		encoder.encode_value(*val)
	} $else $if T.indirections == 2 {
		encoder.encode_value(**val)
	} $else $if T.indirections == 3 {
		encoder.encode_value(***val)
	} $else {
		encoder.encode_value(val)
	}
}

fn struct_field_is_none[T](val T) bool {
	$if T is $option {
		return val == none
	}
	return false
}

fn struct_field_is_nil[T](val T) bool {
	$if T.indirections != 0 {
		return val == unsafe { nil }
	}
	return false
}

fn struct_field_should_encode[T](field_info EncoderFieldInfo, val T) bool {
	if field_info.is_skip {
		return false
	}
	if field_info.is_omitempty {
		if !(check_not_empty(val) or { false }) {
			return false
		}
	}
	if !field_info.is_required && !field_info.is_json_null && struct_field_is_none(val) {
		return false
	}
	if struct_field_is_nil(val) {
		return false
	}
	return true
}

@[unsafe]
fn (mut encoder Encoder) encode_struct[T](val T) {
	encoder.output << `{`

	is_first := encoder.encode_struct_fields[T](val, true, [], '')

	if encoder.prettify && !is_first {
		encoder.decrement_level()
		encoder.add_indent()
	}

	encoder.output << `}`
}

@[unsafe]
fn (mut encoder Encoder) encode_struct_fields[T](val T, was_first bool, old_used_keys []string, prefix string) bool {
	field_infos := encoder.cached_field_infos[T]()
	mut i := 0
	mut is_first := was_first
	mut used_keys := old_used_keys

	$for field in T.fields {
		$if !field.is_embed {
			field_info := field_infos[i]

			mut write_field := true

			$if field.typ is $shared {
				shared field_value := unsafe { val.$(field.name) }
				rlock field_value {
					write_field = struct_field_should_encode(field_info, field_value)

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

						if field_info.key_name in old_used_keys {
							encoder.encode_string(prefix + field_info.key_name)
						} else {
							encoder.encode_string(field_info.key_name)
							used_keys << field_info.key_name
						}

						encoder.output << `:`
						if encoder.prettify {
							encoder.output << ` `
						}

						encoder.encode_struct_field_value(field_value)
					}
				}
			} $else {
				write_field = struct_field_should_encode(field_info, val.$(field.name))

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

					if field_info.key_name in old_used_keys {
						encoder.encode_string(prefix + field_info.key_name)
					} else {
						encoder.encode_string(field_info.key_name)
						used_keys << field_info.key_name
					}

					encoder.output << `:`
					if encoder.prettify {
						encoder.output << ` `
					}

					encoder.encode_struct_field_value(val.$(field.name))
				}
			}
		}
		i++
	}
	$for field in T.fields {
		$if field.is_embed {
			new_prefix := prefix + field.name + '.'
			$if field.typ is $shared {
				shared field_value := unsafe { val.$(field.name) }
				rlock field_value {
					is_first = encoder.encode_struct_fields(field_value, is_first, used_keys,
						new_prefix)
				}
			} $else {
				is_first = encoder.encode_struct_fields(val.$(field.name), is_first, used_keys,
					new_prefix)
			}
		}
	}
	return is_first
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
