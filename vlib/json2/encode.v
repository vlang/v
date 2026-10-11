module json2

import time
import sync.stdatomic

// EncoderOptions provides a list of options for encoding
@[params]
pub struct EncoderOptions {
pub:
	prettify       bool
	indent_string  string = '    '
	newline_string string = '\n'

	enum_as_int bool

	escape_unicode bool
	// time_as_unix encodes a `time.Time` as its Unix timestamp in seconds, like the
	// removed `json` module did, instead of an RFC 3339 string.
	time_as_unix bool
	// legacy_layout, with `prettify: true`, lays the output out like `encode_pretty`
	// of the removed `json` module: members indented with tabs, a tab after each
	// key, and array elements on one line, separated by `, `.
	legacy_layout bool
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

// encode_append appends the JSON representation of val to destination, preserving
// its existing bytes and reusing its capacity. Clear destination before calling
// to replace its contents. The caller owns the buffer and must not mutate it from
// another thread during encoding or pass a value that aliases its storage.
@[manualfree]
pub fn encode_append[T](val T, mut destination []u8, config EncoderOptions) {
	mut encoder := Encoder{
		EncoderOptions: config
		// Only this encoder accesses the caller's buffer until it is returned below.
		output:         unsafe { destination }
	}
	encoder.encode_value[T](val)
	destination = unsafe { encoder.output }
}

fn (mut encoder Encoder) encode_value[T](val T) {
	$if T is $interface {
		encoder.encode_null()
	} $else $if T is $option {
		// Options outside of struct fields (sum type variants, array elements, map
		// values, the top-level value) must still produce a JSON value.
		if val == none {
			encoder.encode_null()
		} else {
			encoder.encode_value(get_value_from_optional(val))
		}
	} $else $if T.unaliased_typ is voidptr {
		encoder.encode_null()
	} $else $if T.unaliased_typ is string {
		encoder.encode_string(string(val))
	} $else $if T.unaliased_typ is bool {
		encoder.encode_boolean(bool(val))
	} $else $if T.unaliased_typ is rune {
		// Like the removed `json` module, a rune is a JSON string of its character.
		encoder.encode_string(rune(val).str())
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
		// Only the legacy layout spreads a fixed size array like a dynamic one.
		spread := encoder.prettify && encoder.legacy_layout
		if spread {
			encoder.open_items(val.len, true)
		}
		for i in 0 .. val.len {
			if i > 0 {
				if spread {
					encoder.separate_items(true)
				} else {
					encoder.output << `,`
				}
			}
			encoder.encode_value(val[i])
		}
		if spread {
			encoder.close_items(val.len, true)
		}
		encoder.output << `]`
	} $else $if T.unaliased_typ is $array {
		encoder.encode_array(val)
	} $else $if T.unaliased_typ is $map {
		encoder.encode_map(val)
	} $else $if T.unaliased_typ is $enum {
		if encoder.enum_as_int || enum_uses_json_as_number[T]() {
			encoder.encode_enum_number(val)
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
			// A `@[json: '...']` name is arbitrary text; escape it like any string.
			encoder.encode_string(enum_val)
		}
	} $else $if T.unaliased_typ is $sumtype {
		encoder.encode_sumtype[T](val)
	} $else $if T.unaliased_typ is time.Time {
		// `time_as_unix` covers aliases of `time.Time` too, like the removed module;
		// otherwise the value's own (or inherited) `to_json` applies.
		if encoder.time_as_unix {
			encoder.encode_number(time.Time(val).unix())
		} else {
			time_val := val.to_json()
			unsafe { encoder.output.push_many(time_val.str, time_val.len) }
		}
	} $else $if T is JsonEncoder { // uses T, because alias could be implementing JsonEncoder, while the base type does not
		integer_val := val.to_json()
		unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
	} $else $if T is Encodable { // uses T, because alias could be implementing JsonEncoder, while the base type does not
		integer_val := val.json_str()
		unsafe { encoder.output.push_many(integer_val.str, integer_val.len) }
	} $else $if T.unaliased_typ is $struct {
		unsafe {
			$for field in T.fields {
				$if field.is_embed {
					encoder.encode_struct_with_embeds(val)
					return
				}
			}
			encoder.output << `{`
			is_first := encoder.encode_struct_fields[T](val, true, [], '')
			encoder.close_object(!is_first)
		}
	}
}

// next_string_escape returns the next byte needing JSON escaping, or val.len.
@[direct_array_access; inline]
fn next_string_escape(val string, start int, escape_unicode bool) int {
	mut i := start
	for i <= val.len - 8 {
		mut word := u64(0)
		// The length check bounds every load; memcpy also permits unaligned strings.
		unsafe { vmemcpy(&word, val.str + i, 8) }
		// A byte below 0x20 leaves a high bit after subtraction and masking.
		control := (word - u64(0x2020202020202020)) & ~word & u64(0x8080808080808080)
		if control != 0 || word_has_byte(word, `"`) || word_has_byte(word, `\\`)
			|| (escape_unicode && word & u64(0x8080808080808080) != 0) {
			break
		}
		i += 8
	}
	// Locate an escape within a flagged word, or handle the final short tail.
	for i < val.len {
		b := val[i]
		if b < 0x20 || b == `"` || b == `\\` || (escape_unicode && b >= 0x80) {
			break
		}
		i++
	}
	return i
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
				if encoder.escape_unicode && character >= 0x80 {
					unsafe {
						encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start)
					}
					code_point, width := utf8_rune_at(val, buffer_end)
					hex_string := if code_point > 0xffff {
						unicode_point_low := u32(code_point) - 0x10000
						'\\u${0xD800 + ((unicode_point_low >> 10) & 0x3FF):04X}\\u${0xDC00 + (unicode_point_low & 0x3FF):04x}'
					} else {
						'\\u${u32(code_point):04x}'
					}
					buffer_end += width
					buffer_start = buffer_end

					unsafe { encoder.output.push_many(hex_string.str, hex_string.len) }

					continue
				}

				buffer_end = next_string_escape(val, buffer_end + 1, encoder.escape_unicode)
			}
		}
	}
	unsafe { encoder.output.push_many(val.str + buffer_start, buffer_end - buffer_start) }

	encoder.output << `"`
}

// utf8_rune_at decodes the UTF-8 sequence at `val[i]` like `string.runes()` does: an
// invalid or truncated sequence (arbitrary bytes such as `0xff`) is U+FFFD with a
// width of 1, like in the removed `json` module, so every byte is consumed once.
@[direct_array_access]
fn utf8_rune_at(val string, i int) (rune, int) {
	b0 := val[i]
	if b0 < 0x80 {
		return rune(b0), 1
	}
	if b0 < 0xc2 || b0 >= 0xf5 {
		return 0xfffd, 1
	}
	width := if b0 < 0xe0 {
		2
	} else if b0 < 0xf0 {
		3
	} else {
		4
	}
	if i + width > val.len {
		return 0xfffd, 1
	}
	for j in 1 .. width {
		if val[i + j] & 0xc0 != 0x80 {
			return 0xfffd, 1
		}
	}
	b1 := val[i + 1]
	if (b0 == 0xe0 && b1 < 0xa0) || (b0 == 0xed && b1 >= 0xa0)
		|| (b0 == 0xf0 && b1 < 0x90) || (b0 == 0xf4 && b1 > 0x8f) {
		return 0xfffd, 1
	}
	code_point := match width {
		2 { ((rune(b0) & 0x1f) << 6) | (rune(b1) & 0x3f) }
		3 { ((rune(b0) & 0x0f) << 12) | ((rune(b1) & 0x3f) << 6) | (rune(val[i + 2]) & 0x3f) }
		else {
			((rune(b0) & 0x07) << 18) | ((rune(b1) & 0x3f) << 12) | ((rune(val[i + 2]) & 0x3f) << 6) | (rune(val[i + 3]) & 0x3f)
		}
	}
	return code_point, width
}

// encode_enum_number writes the backing value of an enum, like the removed `json`
// module: `int(val)` would truncate a 64 bit backing value, and misread a large
// unsigned one as negative.
fn (mut encoder Encoder) encode_enum_number[T](val T) {
	// The comparison happens in the enum's C type, which is unsigned for an unsigned
	// backing type.
	if unsafe { T(-1) } < unsafe { T(0) } {
		encoder.encode_number(i64(val))
	} else {
		mut bits := u64(val)
		if sizeof(T) < 8 {
			// A narrow enum is passed as an `int`, which sign-extends large values.
			bits &= (u64(1) << (sizeof(T) * 8)) - 1
		}
		encoder.encode_number(bits)
	}
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
		// JSON has no NaN or infinity, which V formats as `nan`, `+inf` and `-inf`.
		if integer_val == 'nan' || integer_val.ends_with('inf') {
			encoder.encode_null()
			return
		}
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

@[markused]
fn (mut encoder Encoder) encode_null() {
	unsafe { encoder.output.push_many(null_string.str, null_string.len) }
}

fn (mut encoder Encoder) encode_array[T](val T) {
	encoder.output << `[`
	encoder.open_items(val.len, true)
	for i, item in val {
		if i > 0 {
			encoder.separate_items(true)
		}
		$if T is $pointer {
			if voidptr(item) == unsafe { nil } {
				encoder.encode_null()
			} else {
				unsafe { encoder.encode_pointer_array_item(item) }
			}
		} $else {
			encoder.encode_value(item)
		}
	}
	encoder.close_items(val.len, true)
	encoder.output << `]`
}

@[unsafe]
fn (mut encoder Encoder) encode_pointer_array_item[T](item T) {
	encoder.encode_value(*item)
}

struct EncoderMapKey[K] {
	name string
	key  K
}

fn (mut encoder Encoder) encode_map[K, T](val map[K]T) {
	mut keys := []EncoderMapKey[K]{cap: val.len}
	for key, _ in val {
		keys << EncoderMapKey[K]{
			name: '${key}'
			key:  key
		}
	}
	// Compare the converted object names: integer keys also sort lexically.
	keys.sort(a.name < b.name)
	encoder.output << `{`
	encoder.open_items(val.len, false)
	for i, entry in keys {
		if i > 0 {
			encoder.separate_items(false)
		}
		encoder.encode_string(entry.name)
		encoder.write_key_separator()
		encoder.encode_value[T](val[entry.key])
	}
	encoder.close_items(val.len, false)
	encoder.output << `}`
}

fn (mut encoder Encoder) encode_enum[T](val T) {
	if encoder.enum_as_int || enum_uses_json_as_number[T]() {
		encoder.encode_enum_number(val)
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
		encoder.encode_string(enum_val)
	}
}

fn (mut encoder Encoder) encode_sumtype[T](val T) {
	$if T is $pointer {
		// Pointer types are handled by encode_value's $pointer branch;
		// this instantiation is generated but never called.
	} $else {
		$for variant in T.variants {
			if val is variant {
				// An option variant comes first: the time and struct checks below also
				// hold for an option of a time or a struct.
				$if variant.typ is $option {
					// Like the removed `json` module, a `none` option variant is `{}`.
					variant_value := val
					if variant_value == none {
						encoder.output << `{`
						encoder.output << `}`
					} else {
						encoder.encode_sumtype_option_payload(get_value_from_optional(variant_value))
					}
				} $else $if variant.typ.unaliased_typ is time.Time {
					// A `time.Time` alias variant (`type Timestamp = time.Time`) is a time too.
					if T.name in ['x.json2.Any', 'json2.Any', 'Any'] {
						variant_value := val
						encoder.encode_value(variant_value)
					} else {
						// Like the removed `json` module, every time variant is `Time`, also
						// an alias such as `type Timestamp = time.Time`.
						encoder.encode_sumtype_time_variant(time.Time(val), 'Time')
					}
				} $else $if variant.typ is $struct {
					if T.name in ['x.json2.Any', 'json2.Any', 'Any'] {
						variant_value := val
						encoder.encode_value(variant_value)
					} else {
						// A struct alias variant is tagged with the struct's name, like in the
						// removed module (`Foo` for `type Alias = Foo`).
						encoder.encode_sumtype_struct_variant(val,
							sumtype_variant_name(typeof(variant.typ.unaliased_typ).name))
					}
				} $else $if variant.typ is $map {
					encoder.encode_value(val)
				} $else $if variant.typ is $array_dynamic {
					if T.name in ['x.json2.Any', 'json2.Any', 'Any'] {
						variant_value := val
						encoder.encode_value(variant_value)
					} else {
						variant_value := val
						encoder.encode_array_of_sumtype_variants(variant_value)
					}
				} $else $if variant.typ is $array_fixed {
					variant_value := val
					encoder.encode_fixed_array_of_sumtype_variants(variant_value)
				} $else {
					variant_value := val
					encoder.encode_value(variant_value)
				}
				// An alias variant and its base type both match `is`
				// (`type MyString = string` in `MyString | string`); encode the
				// value once.
				return
			}
		}
	}
}

// encode_sumtype_option_payload writes the value of a set option variant of a sum
// type like the variant it holds, as the removed `json` module did: a time (also an
// alias of one) as `{"_type":"Time","value":...}`, and a struct with its `_type`, also
// as an element of a (nested or fixed size) array.
fn (mut encoder Encoder) encode_sumtype_option_payload[P](payload P) {
	$if P.unaliased_typ is time.Time {
		encoder.encode_sumtype_time_variant(time.Time(payload), 'Time')
	} $else $if P.unaliased_typ is $struct {
		encoder.encode_sumtype_struct_variant(payload, struct_variant_tag[P]())
	} $else $if P is $array_dynamic {
		encoder.encode_array_of_sumtype_variants(payload)
	} $else $if P is $array_fixed {
		encoder.encode_fixed_array_of_sumtype_variants(payload)
	} $else {
		encoder.encode_value(payload)
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
	encoder.write_key_separator()
	return false
}

fn (mut encoder Encoder) encode_sumtype_struct_variant[T](val T, variant_name string) {
	$for field in T.fields {
		$if field.is_embed {
			unsafe { encoder.encode_sumtype_struct_variant_with_embeds(val, variant_name) }
			return
		}
	}
	encoder.output << `{`
	mut is_first := unsafe { encoder.encode_struct_fields[T](val, true, [], '') }
	is_first = encoder.encode_object_key(is_first, '_type')
	encoder.encode_string(variant_name)
	encoder.close_object(!is_first)
}

// encode_array_of_sumtype_variants writes an array held by a sum type variant. Like
// the removed `json` module, its struct elements get their `_type`, also in nested
// and fixed size arrays.
fn (mut encoder Encoder) encode_array_of_sumtype_variants[T](val []T) {
	encoder.output << `[`
	encoder.open_items(val.len, true)
	for i, item in val {
		if i > 0 {
			encoder.separate_items(true)
		}
		encoder.encode_sumtype_array_item(item)
	}
	encoder.close_items(val.len, true)
	encoder.output << `]`
}

// encode_fixed_array_of_sumtype_variants writes a fixed size array held by a sum type
// variant, laid out like `encode_value` lays out other fixed size arrays.
fn (mut encoder Encoder) encode_fixed_array_of_sumtype_variants[A](val A) {
	encoder.output << `[`
	// Only the legacy layout spreads a fixed size array like a dynamic one.
	spread := encoder.prettify && encoder.legacy_layout
	if spread {
		encoder.open_items(val.len, true)
	}
	for i in 0 .. val.len {
		if i > 0 {
			if spread {
				encoder.separate_items(true)
			} else {
				encoder.output << `,`
			}
		}
		encoder.encode_sumtype_array_item(val[i])
	}
	if spread {
		encoder.close_items(val.len, true)
	}
	encoder.output << `]`
}

fn (mut encoder Encoder) encode_sumtype_array_item[T](item T) {
	// An option element comes first, like an option variant: `none` is `{}`, and a set
	// value is written like the variant it holds, as in the removed module.
	$if T is $option {
		if item == none {
			encoder.output << `{`
			encoder.output << `}`
		} else {
			encoder.encode_sumtype_option_payload(get_value_from_optional(item))
		}
	} $else $if T.unaliased_typ is time.Time {
		// Like a time variant, `{"_type":"Time","value":...}` as in the removed module.
		encoder.encode_sumtype_time_variant(time.Time(item), 'Time')
	} $else $if T is JsonEncoder {
		encoder.encode_value(item)
	} $else $if T is Encodable {
		encoder.encode_value(item)
	} $else $if T is $struct {
		encoder.encode_sumtype_struct_variant(item, struct_variant_tag[T]())
	} $else $if T is $array_dynamic {
		encoder.encode_array_of_sumtype_variants(item)
	} $else $if T is $array_fixed {
		encoder.encode_fixed_array_of_sumtype_variants(item)
	} $else {
		encoder.encode_value(item)
	}
}

@[markused]
fn (mut encoder Encoder) encode_sumtype_time_variant(val time.Time, variant_name string) {
	encoder.output << `{`
	mut is_first := true
	is_first = encoder.encode_object_key(is_first, '_type')
	encoder.encode_string(variant_name)
	is_first = encoder.encode_object_key(is_first, 'value')
	encoder.encode_number(val.unix())
	encoder.close_object(!is_first)
}

struct EncoderFieldInfo {
	key_name string
	// Compact ASCII keys include a leading comma, skipped for the first member.
	compact_key string

	is_skip      bool
	is_omitempty bool
	is_required  bool
	is_json_null bool
}

struct EncoderFieldInfoCache {
mut:
	field_infos []EncoderFieldInfo
}

// Keep runtime attribute parsing outside the compile-time field loop. This is called only while
// a struct type's field metadata cache is initialized.
@[manualfree; noinline]
fn encoder_field_info(field_name string, attrs []string) EncoderFieldInfo {
	mut is_skip := false
	mut key_name := ''
	mut is_omitempty := false
	mut is_required := false
	mut is_json_null := false
	for attr in attrs {
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
	resolved_key := if key_name == '' { field_name } else { key_name.clone() }
	compact_key := if !is_skip && next_string_escape(resolved_key, 0, true) == resolved_key.len {
		',"' + resolved_key + '":'
	} else {
		''
	}
	return EncoderFieldInfo{
		key_name:     resolved_key
		compact_key:  compact_key
		is_skip:      is_skip
		is_omitempty: is_omitempty
		is_required:  is_required
		is_json_null: is_json_null
	}
}

fn get_value_from_optional[T](val ?T) T {
	return val or { T{} }
}

fn check_not_empty[T](val T) ?bool {
	$if val is ?string {
		opt := ?string(val)
		if sval := opt {
			return sval != ''
		}
		return false
	} $else $if val is ?bool {
		opt := ?bool(val)
		if bval := opt {
			return bval
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
	} $else $if T is $option {
		if struct_field_is_none(val) {
			return false
		}
		return check_not_empty(get_value_from_optional(val)) or { true }
	} $else $if T.indirections != 0 {
		return val != unsafe { nil }
	} $else $if T.unaliased_typ is bool {
		return bool(val)
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
	} $else $if T.unaliased_typ is $enum {
		return val != unsafe { T(0) }
	} $else $if T.unaliased_typ is $struct || T.unaliased_typ is $sumtype {
		// Like the removed `json` module, a struct or sum type value is empty when it
		// is its type's default value (a struct with its field defaults).
		return val != T{}
	}
	return true
}

// TODO: fix compilation with -autofree, and remove the tag @[manualfree] here:
@[manualfree; unsafe]
fn (mut encoder Encoder) cached_field_infos[T]() &EncoderFieldInfoCache {
	static cache := &EncoderFieldInfoCache(nil)
	static initializing := u64(0)
	static initialized := u64(0)
	// Elect one initializer, then publish the completed immutable cache. This makes
	// every field visible before another thread can read the immutable cache.
	if stdatomic.load_u64(&initialized) == 0 {
		if stdatomic.fetch_add_u64(&initializing, 1) == 0 {
			cache = &EncoderFieldInfoCache{}
			$for field in T.fields {
				cache.field_infos << encoder_field_info(field.name, field.attrs)
			}
			stdatomic.store_u64(&initialized, 1)
		} else {
			// Only concurrent first use waits; warm encodes need one atomic load.
			for stdatomic.load_u64(&initialized) == 0 {
				time.sleep(time.microsecond)
			}
		}
	}
	return cache
}

fn (mut encoder Encoder) encode_struct_field_value[T](val T) {
	$if T is $interface {
		encoder.encode_null()
	} $else $if T.unaliased_typ is voidptr {
		encoder.encode_null()
	} $else $if T.pointee_type is $interface {
		encoder.encode_null()
	} $else $if T is $option {
		if val == none {
			unsafe { encoder.output.push_many(null_string.str, null_string.len) }
		} else {
			encoder.encode_value(get_value_from_optional(val))
		}
	} $else $if T is $pointer {
		// encode_value follows the pointer one level at a time, so a nil pointer at
		// any level (`&&int` pointing to a nil `&int`) is written as `null`.
		encoder.encode_value(val)
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

// encode_cached_struct_key copies ordinary compact keys from immutable metadata.
// Pretty layouts and keys requiring escaping use the existing encoder.
fn (mut encoder Encoder) encode_cached_struct_key(is_first bool, field_info EncoderFieldInfo) bool {
	if !encoder.prettify && field_info.compact_key.len > 0 {
		start := if is_first { 1 } else { 0 }
		// The cached string always contains a comma, quotes, the key, and a colon.
		unsafe {
			encoder.output.push_many(field_info.compact_key.str + start,
				field_info.compact_key.len - start)
		}
		return false
	}
	return encoder.encode_object_key(is_first, field_info.key_name)
}

// encode_struct_field_key keeps the non-type-specific part of struct field
// encoding out of the comptime field loop. Otherwise every field gets its own
// copy of the key-collision scan and key selection code.
@[noinline]
fn (mut encoder Encoder) encode_struct_field_key(mut used_keys []string, old_used_keys []string, prefix string, field_info EncoderFieldInfo, is_first bool, track_keys bool) bool {
	if field_info.key_name in old_used_keys {
		return encoder.encode_object_key(is_first, prefix + field_info.key_name)
	}
	if track_keys {
		used_keys << field_info.key_name
	}
	return encoder.encode_cached_struct_key(is_first, field_info)
}

@[noinline]
fn (mut encoder Encoder) encode_embedded_struct_field_key(mut used_keys []string, reserved_keys []string, prefix string, field_info EncoderFieldInfo, is_first bool) bool {
	should_prefix := field_info.key_name in used_keys || field_info.key_name in reserved_keys
	if !should_prefix {
		used_keys << field_info.key_name
		return encoder.encode_cached_struct_key(is_first, field_info)
	}
	return encoder.encode_object_key(is_first, prefix + field_info.key_name)
}

// encode_struct_field writes a struct field with its key, unless the field is skipped or
// left out as empty, and returns the new `is_first`. It is specialized per field type, so
// all structs share it and each struct only pays for one call per field. `other_keys` are
// the keys of the outer struct for an embedded struct field, else the keys used before.
fn (mut encoder Encoder) encode_struct_field[F](val F, field_info EncoderFieldInfo, is_first bool, mut used_keys []string, other_keys []string, prefix string, embedded bool, track_keys bool) bool {
	if !struct_field_should_encode(field_info, val) {
		return is_first
	}
	new_is_first := if embedded {
		encoder.encode_embedded_struct_field_key(mut used_keys, other_keys, prefix, field_info,
			is_first)
	} else {
		encoder.encode_struct_field_key(mut used_keys, other_keys, prefix, field_info, is_first,
			track_keys)
	}
	encoder.encode_struct_field_value(val)
	return new_is_first
}

@[unsafe]
fn (mut encoder Encoder) encode_struct_with_embeds[T](val T) {
	encoder.output << `{`
	is_first := encoder.encode_embedded_struct_fields[T](val, true, [], [], '')
	encoder.close_object(!is_first)
}

@[unsafe]
fn (mut encoder Encoder) encode_sumtype_struct_variant_with_embeds[T](val T, variant_name string) {
	encoder.output << `{`
	mut is_first := encoder.encode_embedded_struct_fields[T](val, true, [], [], '')
	is_first = encoder.encode_object_key(is_first, '_type')
	encoder.encode_string(variant_name)
	encoder.close_object(!is_first)
}

@[unsafe]
fn (mut encoder Encoder) encode_struct_fields[T](val T, was_first bool, old_used_keys []string, prefix string) bool {
	field_info_cache := encoder.cached_field_infos[T]()
	mut is_first := was_first
	mut used_keys := old_used_keys
	mut i := 0
	// Only embedded children consume the keys collected by this struct.
	mut track_keys := false
	$for field in T.fields {
		$if field.is_embed {
			track_keys = true
		}
	}

	$for field in T.fields {
		$if !field.is_embed {
			if !field.attrs.contains('skip') {
				$if field.typ is $shared {
					shared field_value := unsafe { val.$(field.name) }
					rlock field_value {
						is_first = encoder.encode_struct_field(field_value, field_info_cache.field_infos[i],
							is_first, mut used_keys, old_used_keys, prefix, false, track_keys)
					}
				} $else {
					is_first = encoder.encode_struct_field(val.$(field.name), field_info_cache.field_infos[i],
						is_first, mut used_keys, old_used_keys, prefix, false, track_keys)
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

@[unsafe]
fn (mut encoder Encoder) encode_embedded_struct_fields[T](val T, was_first bool, old_used_keys []string, reserved_keys []string, prefix string) bool {
	field_info_cache := encoder.cached_field_infos[T]()
	mut is_first := was_first
	mut used_keys := old_used_keys.clone()
	mut i := 0

	$for field in T.fields {
		$if field.is_embed {
			mut child_reserved_keys := reserved_keys.clone()
			mut reserved_i := 0
			$for reserved_field in T.fields {
				reserved_field_info := field_info_cache.field_infos[reserved_i]
				$if !reserved_field.is_embed {
					if !reserved_field_info.is_skip {
						child_reserved_keys << reserved_field_info.key_name
					}
				}
				reserved_i++
			}
			new_prefix := prefix + field.name + '.'
			$if field.typ is $shared {
				shared field_value := unsafe { val.$(field.name) }
				rlock field_value {
					is_first = encoder.encode_embedded_struct_fields(field_value, is_first,
						used_keys, child_reserved_keys, new_prefix)
				}
			} $else {
				is_first = encoder.encode_embedded_struct_fields(val.$(field.name), is_first,
					used_keys, child_reserved_keys, new_prefix)
			}
		} $else {
			if !field.attrs.contains('skip') {
				$if field.typ is $shared {
					shared field_value := unsafe { val.$(field.name) }
					rlock field_value {
						is_first = encoder.encode_struct_field(field_value, field_info_cache.field_infos[i],
							is_first, mut used_keys, reserved_keys, prefix, true, true)
					}
				} $else {
					is_first = encoder.encode_struct_field(val.$(field.name), field_info_cache.field_infos[i],
						is_first, mut used_keys, reserved_keys, prefix, true, true)
				}
			}
		}
		i++
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
	encoder.prefix = encoder.line_prefix()
}

fn (mut encoder Encoder) decrement_level() {
	encoder.level--
	encoder.prefix = encoder.line_prefix()
}

// line_prefix is the line break and indentation before a member or element at the
// current level.
fn (encoder &Encoder) line_prefix() string {
	if encoder.legacy_layout {
		return '\n' + '\t'.repeat(encoder.level)
	}
	return encoder.newline_string + encoder.indent_string.repeat(encoder.level)
}

// open_items starts the members of an object or the elements of an array, after its
// `{` or `[`. Without members or elements no level is raised: its lowering happens
// in close_items only for a container that has some.
fn (mut encoder Encoder) open_items(count int, is_array bool) {
	if encoder.prettify && count > 0 {
		encoder.increment_level()
		if !(is_array && encoder.legacy_layout) {
			encoder.add_indent()
		}
	}
}

// separate_items writes the separator between two members or elements.
fn (mut encoder Encoder) separate_items(is_array bool) {
	encoder.output << `,`
	if encoder.prettify {
		if is_array && encoder.legacy_layout {
			encoder.output << ` `
		} else {
			encoder.add_indent()
		}
	}
}

// close_items ends the members or elements started by open_items, before the `}`
// or `]`.
fn (mut encoder Encoder) close_items(count int, is_array bool) {
	if !encoder.prettify {
		return
	}
	if count > 0 {
		encoder.decrement_level()
		if !(is_array && encoder.legacy_layout) {
			encoder.add_indent()
		}
	} else if !is_array && encoder.legacy_layout {
		// The removed module wrote an empty object as `{`, a line break and `}`.
		prefix := encoder.line_prefix()
		unsafe { encoder.output.push_many(prefix.str, prefix.len) }
	}
}

// close_object ends an object whose members were started with encode_object_key.
fn (mut encoder Encoder) close_object(has_members bool) {
	encoder.close_items(if has_members { 1 } else { 0 }, false)
	encoder.output << `}`
}

fn (mut encoder Encoder) write_key_separator() {
	encoder.output << `:`
	if encoder.prettify {
		encoder.output << if encoder.legacy_layout { `\t` } else { ` ` }
	}
}

fn (mut encoder Encoder) add_indent() {
	unsafe { encoder.output.push_many(encoder.prefix.str, encoder.prefix.len) }
}
