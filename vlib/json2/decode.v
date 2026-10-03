module json2

import strconv
import time

const null_in_string = 'null'

const true_in_string = 'true'

const false_in_string = 'false'

const whitespace_chars = [` `, `\t`, `\n`, `\r`]!

// ValueInfo represents the position and length of a value, such as string, number, array, object key, and object value in a JSON string.
struct ValueInfo {
mut:
	position   int       // The position of the value in the JSON string.
	value_kind ValueKind // The kind of the value.
	length     int       // The length of the value in the JSON string.
}

struct DecoderFieldInfo {
	key_name string

	is_skip     bool
	is_required bool
	is_raw      bool
}

@[markused]
struct StructFieldInfo {
	name          string // name is the V name of the field, for error messages.
	json_name_ptr voidptr
	json_name_len int
	is_skip       bool
	is_required   bool
	is_raw        bool
}

// Keep runtime attribute parsing outside the compile-time field loop. This is called only while
// a struct type's field metadata cache is initialized.
@[noinline]
fn struct_field_info(field_name string, attrs []string) StructFieldInfo {
	mut json_name_str := field_name.str
	mut json_name_len := field_name.len
	mut is_json_skip := false
	for attr in attrs {
		if start, end := json_attr_value_range(attr) {
			if end <= start {
				continue
			}
			if end == start + 1 && attr[start] == `-` {
				is_json_skip = true
				break
			}
			json_name_str = unsafe { attr.str + start }
			json_name_len = end - start
			break
		}
	}
	return StructFieldInfo{
		name:          field_name
		json_name_ptr: voidptr(json_name_str)
		json_name_len: json_name_len
		is_skip:       attrs.contains('skip') || is_json_skip
		is_required:   attrs.contains('required')
		is_raw:        attrs.contains('raw')
	}
}

struct StructKeyDecodeResult[T] {
	matched bool
	value   T
}

// DecoderOptions provides options for JSON decoding.
// By default, decoding is lenient. Use `strict: true` for strict JSON spec compliance.
@[markused; params]
pub struct DecoderOptions {
pub:
	// In strict mode, quoted strings are not accepted as numbers.
	// For example, '"123"' will fail to decode as int in strict mode,
	// but will succeed in default mode.
	// Strict mode also requires a fixed size array to get a JSON array with exactly
	// as many elements. By default, `null` or missing trailing elements keep their
	// default values, and extra elements are ignored.
	// Strict mode also rejects `null` for a value that is not an option. By default,
	// like in the removed `json` module, `null` is the zero value of a number, bool,
	// string, enum or time, an empty array or map, and a nested struct with its
	// default field values.
	strict bool
}

// Decoder is the internal decoding state.
@[markused]
struct Decoder {
	json   string // json is the JSON data to be decoded.
	strict bool   // strict mode rejects quoted strings as numbers, fixed arrays of another length, and null values
mut:
	// values_info describes every value of the JSON string, in the order in which they
	// start: an array or an object is followed by its elements, or by its keys and their
	// values. It is one flat array, so nothing is allocated per value, and moving to
	// the next value is an index increment.
	values_info []ValueInfo
	values_len  int // values_len is the number of values that the checker has put in values_info.
	checker_idx int // checker_idx is the current index of the decoder.
	current_idx int // current_idx is the index in values_info of the value that is decoded next.
}

// no_value_idx is the index of a value that is not there, such as the `_type` of an
// object without that key.
const no_value_idx = -1

// has_value reports whether `value_idx` is the index of a value in values_info. The
// index after the last value, and no_value_idx, are not.
@[inline; markused]
fn (decoder &Decoder) has_value(value_idx int) bool {
	return value_idx >= 0 && value_idx < decoder.values_info.len
}

// current_value returns the value that is decoded next.
@[inline; markused]
fn (decoder &Decoder) current_value() ValueInfo {
	return decoder.values_info[decoder.current_idx]
}

// skip_strings_min_json_len is the length of a JSON string from which
// value_count_bound() skips the strings in it. That takes longer, which a shorter
// JSON string is not worth: the room that it saves there is small.
const skip_strings_min_json_len = 16384

// value_count_bound returns an upper bound of the number of values in the `json`
// string, to allocate values_info once, with (nearly) no room to spare. Outside of
// strings, every value but the root follows a `,`, a `:`, a `[` or a `{` of its own,
// so the bound is only higher than the count for empty arrays and objects, and in a
// short JSON string, for those characters in strings.
@[direct_array_access; markused]
fn value_count_bound(json string) int {
	mut count := 1
	if json.len < skip_strings_min_json_len {
		for c in json {
			// `[` and `{` only differ in the bit that is set here.
			count += int(c == `,`) + int(c == `:`) + int((c | 0x20) == `{`)
		}
		return count
	}
	mut i := 0
	for i < json.len {
		c := json[i]
		i++
		if c == `"` {
			i = json_string_end(json, i) + 1
			continue
		}
		count += int(c == `,`) + int(c == `:`) + int((c | 0x20) == `{`)
	}
	return count
}

// json_string_end returns the index of the `"` that ends the JSON string whose content
// starts at `start` in `json`, or `json.len` when the string is not closed.
@[direct_array_access]
fn json_string_end(json string, start int) int {
	mut i := start
	for {
		// A long string is skipped 8 bytes at a time, as long as none is a `"` or a `\`.
		for i + 8 <= json.len {
			mut word := u64(0)
			unsafe { vmemcpy(&word, json.str + i, 8) }
			if word_has_byte(word, `"`) || word_has_byte(word, `\\`) {
				break
			}
			i += 8
		}
		for i < json.len && json[i] != `"` && json[i] != `\\` {
			i++
		}
		if i >= json.len {
			return json.len
		}
		if json[i] == `"` {
			return i
		}
		// The character after a `\` is escaped: a `"` there does not end the string.
		i += 2
	}
	return json.len
}

// word_has_byte reports whether one of the 8 bytes of `word` is `b`.
@[inline]
fn word_has_byte(word u64, b u8) bool {
	// A byte of `diff` is zero where `word` has `b`. The subtraction sets the high bit
	// of a zero byte, that `~diff` keeps, and without a zero byte no high bit is left.
	diff := word ^ (u64(b) * 0x0101010101010101)
	return ((diff - 0x0101010101010101) & ~diff & 0x8080808080808080) != 0
}

// add_value puts a value of `value_kind`, that starts at the checker's position, in
// values_info. Its length is set once its end is known.
@[direct_array_access; inline; markused]
fn (mut checker Decoder) add_value(value_kind ValueKind) {
	if checker.values_len >= checker.values_info.len {
		// value_count_bound() leaves room for every value of a valid JSON string. In
		// an invalid one the checker can see more values, before it reports the error.
		checker.values_info << ValueInfo{}
	}
	checker.values_info[checker.values_len].position = checker.checker_idx
	checker.values_info[checker.values_len].value_kind = value_kind
	checker.values_len++
}

const max_context_length = 50
const max_extra_characters = 5
const tab_width = 8

pub struct JsonDecodeError {
	Error
	context string
pub:
	message string

	line      int
	character int
}

fn (e JsonDecodeError) msg() string {
	return '\n${e.line}:${e.character}: Invalid json: ${e.message}\n${e.context}'
}

// checker_error generates a checker error message showing the position in the json string
@[markused]
fn (mut checker Decoder) checker_error(message string) ! {
	position := checker.checker_idx

	mut line_number := 0
	mut character_number := 0
	mut last_newline := 0

	for i := position - 1; i >= 0; i-- {
		if last_newline == 0 {
			if checker.json[i] == `\n` {
				last_newline = i + 1
			} else if checker.json[i] == `\t` {
				character_number += tab_width
			} else {
				character_number++
			}
		}
		if checker.json[i] == `\n` {
			line_number++
		}
	}

	cutoff := character_number > max_context_length

	// either start of string, last newline or a limited amount of characters
	context_start := if cutoff { position - max_context_length } else { last_newline }

	// print some extra characters
	mut context_end := int_min(checker.json.len, position + max_extra_characters)
	context_end_newline := checker.json[position..context_end].index_u8(`\n`)

	if context_end_newline > 0 {
		context_end = position + context_end_newline
	}

	mut context := ''

	if cutoff {
		context += '...'
	}
	context += checker.json[context_start..position]
	context += '\\e[31m${checker.json[position].ascii_str()}\\e[0m'
	context += checker.json[position + 1..context_end]
	context += '\n'

	if cutoff {
		context += ' '.repeat(max_context_length + 3)
	} else {
		context += ' '.repeat(character_number)
	}
	context += '\e[31m^\e[0m'

	return JsonDecodeError{
		context:   context
		message:   'Syntax: ${message}'
		line:      line_number + 1
		character: character_number + 1
	}
}

// decode_error generates a decoding error from the decoding stage
@[markused]
fn (mut decoder Decoder) decode_error(message string) ! {
	mut error_info := ValueInfo{}
	if decoder.current_idx < decoder.values_info.len {
		error_info = decoder.current_value()
	} else if decoder.values_info.len > 0 {
		error_info = decoder.values_info.last()
	}

	start := error_info.position
	end := start + int_min(error_info.length, max_context_length)

	mut line_number := 0
	mut character_number := 0
	mut last_newline := 0

	for i := start - 1; i >= 0; i-- {
		if last_newline == 0 {
			if decoder.json[i] == `\n` {
				last_newline = i + 1
			} else if decoder.json[i] == `\t` {
				character_number += tab_width
			} else {
				character_number++
			}
		}
		if decoder.json[i] == `\n` {
			line_number++
		}
	}

	cutoff := character_number > max_context_length

	// either start of string, last newline or a limited amount of characters
	context_start := if cutoff { start - max_context_length } else { last_newline }

	// print some extra characters
	mut context_end := int_min(decoder.json.len, end + max_extra_characters)
	context_end_newline := decoder.json[end..context_end].index_u8(`\n`)

	if context_end_newline != -1 {
		context_end = end + context_end_newline
	}

	mut context := ''

	if cutoff {
		context += '...'
	}
	context += decoder.json[context_start..start]
	context += '\\e[31m${decoder.json[start..end]}\\e[0m'
	context += decoder.json[end..context_end]
	context += '\n'

	if cutoff {
		context += ' '.repeat(max_context_length + 3)
	} else {
		context += ' '.repeat(character_number)
	}
	context += '\\e[31m${'~'.repeat(error_info.length)}\\e[0m'

	return JsonDecodeError{
		context:   context
		message:   'Data: ${message}'
		line:      line_number + 1
		character: character_number + 1
	}
}

// decode decodes a JSON string into a specified type.
// By default, decoding is lenient. Use `strict: true` for strict JSON spec compliance.
@[manualfree]
pub fn decode[T](val string, params DecoderOptions) !T {
	if val == '' {
		return JsonDecodeError{
			message:   'empty string'
			line:      1
			character: 1
		}
	}
	mut decoder := Decoder{
		json:        val
		strict:      params.strict
		values_info: []ValueInfo{len: value_count_bound(val)}
	}
	// Nothing that is decoded refers to values_info, so it is released right away.
	defer {
		unsafe { decoder.values_info.free() }
	}

	decoder.check_json_format()!
	decoder.values_info.trim(decoder.values_len)

	mut result := T{}
	$if T.unaliased_typ is $array_dynamic {
		result.clear()
		decoder.decode_array(mut result)!
	} $else $if T.unaliased_typ is $map {
		decoder.decode_map(mut result)!
	} $else $if T is $pointer {
		// `&T`, `&&T` and `&&&T` point to a newly decoded value; `null` keeps `result`.
		result = decoder.decode_array_element(result)!
	} $else {
		decoder.decode_value(mut result)!
	}
	return result
}

// decode_new_pointer decodes the current value into newly allocated memory, behind
// as many pointers as `P` has (`&T`, `&&T`, `&&&&T`, ...).
fn (mut decoder Decoder) decode_new_pointer[P](_ P) !P {
	$if P.indirections == 1 {
		mut decoded_ptr := $new(P.pointee_type)
		decoder.decode_value(mut decoded_ptr)!
		return decoded_ptr
	} $else {
		return new_pointer_to(decoder.decode_new_pointer($zero(P.pointee_type))!)
	}
}

// new_pointer_to returns a new heap pointer to `value`: one more level of
// indirection, for decoding `&&T`, `&&&T`, ... values.
fn new_pointer_to[T](value T) &T {
	mut ptr := unsafe { &T(vcalloc(sizeof(T))) }
	unsafe {
		*ptr = value
	}
	return ptr
}

// option_element_is_none reports whether the current array element is `none` for an
// option element type: `null`, or `{}` (how the removed module wrote a `none` element
// of a sum type's array) when the payload does not take an object, as for `?int`.
fn (mut decoder Decoder) option_element_is_none[P](element ?P) bool {
	return decoder.current_value().value_kind == .null
		|| (decoder.is_empty_object(decoder.current_idx)
			&& option_payload_fit(element, .object) == 0)
}

// decode_option_payload decodes the current value as the payload of an option.
fn (mut decoder Decoder) decode_option_payload[P](_ ?P) !P {
	$if P is $pointer {
		return decoder.decode_array_element(P{})!
	} $else {
		mut payload := P{}
		decoder.decode_value(mut payload)!
		return payload
	}
}

fn create_decoded_ptr[T](_ &T) &T {
	$if T is $interface {
		return unsafe { nil }
	} $else $if T.unaliased_typ is voidptr {
		return unsafe { nil }
	} $else $if T is $sumtype {
		// `&T(ptr)` would wrap the pointer in the sum type instead of casting it.
		return $new(T)
	} $else {
		return unsafe { &T(vcalloc(sizeof(T))) }
	}
}

fn decoder_field_infos[T]() []DecoderFieldInfo {
	mut field_infos := []DecoderFieldInfo{}
	$for field in T.fields {
		mut key_name := field.name
		mut is_json_skip := false
		for attr in field.attrs {
			if start, end := json_attr_value_range(attr) {
				if end <= start {
					continue
				}
				if end == start + 1 && attr[start] == `-` {
					is_json_skip = true
					break
				}
				key_name = attr[start..end]
				break
			}
		}
		field_infos << DecoderFieldInfo{
			key_name:    key_name
			is_skip:     field.attrs.contains('skip') || is_json_skip
			is_required: field.attrs.contains('required')
			is_raw:      field.attrs.contains('raw')
		}
	}
	return field_infos
}

struct StructFieldInfoCache {
mut:
	field_infos []StructFieldInfo
}

@[manualfree; unsafe]
fn (mut decoder Decoder) cached_struct_field_infos[T]() &StructFieldInfoCache {
	static cache := &StructFieldInfoCache(nil)
	if cache == nil {
		cache = &StructFieldInfoCache{}
		$for field in T.fields {
			cache.field_infos << struct_field_info(field.name, field.attrs)
		}
	}
	return cache
}

@[inline; markused]
fn struct_field_is_decoded(decoded_mask u64, decoded_fields []bool, field_idx int) bool {
	if field_idx < 64 {
		return (decoded_mask & (u64(1) << u64(field_idx))) != 0
	}
	return decoded_fields[field_idx]
}

@[inline; markused]
fn mark_struct_field_decoded(decoded_mask u64, mut decoded_fields []bool, field_idx int) u64 {
	if field_idx < 64 {
		return decoded_mask | (u64(1) << u64(field_idx))
	}
	decoded_fields[field_idx] = true
	return decoded_mask
}

// find_struct_field centralizes the runtime part of struct key matching. Keeping
// this loop outside the comptime field loop avoids emitting the same skip,
// length, and memory-comparison checks once for every struct field.
@[noinline]
fn (decoder &Decoder) find_struct_field(field_infos []StructFieldInfo, key_ptr voidptr, key_len int) int {
	for field_idx, field_info in field_infos {
		// `@[omitempty]` only affects encoding: an explicit empty value (`0`, `""`) is
		// decoded like any other, as in the removed `json` module.
		field_can_match := !field_info.is_skip || field_info.is_required
		field_name_matches := key_len == field_info.json_name_len && unsafe {
			vmemcmp(key_ptr, field_info.json_name_ptr, field_info.json_name_len) == 0
		}
		if field_can_match && field_name_matches {
			return field_idx
		}
	}
	return -1
}

// key_has_escape reports whether the JSON string key described by `key_info`
// contains a `\` escape sequence.
@[direct_array_access; inline]
fn (decoder &Decoder) key_has_escape(key_info ValueInfo) bool {
	for i in key_info.position + 1 .. key_info.position + key_info.length - 1 {
		if decoder.json[i] == `\\` {
			return true
		}
	}
	return false
}

@[inline]
fn (mut decoder Decoder) json_key_matches(key_info ValueInfo, key_name string) !bool {
	if decoder.key_has_escape(key_info) {
		return decoder.decode_string_value(key_info)! == key_name
	}
	if key_info.length - 2 != key_name.len {
		return false
	}
	return unsafe {
		vmemcmp(decoder.json.str + key_info.position + 1, key_name.str, key_name.len) == 0
	}
}

// skip_current_value moves past the current value, and past the values nested in it:
// those follow it in values_info, and start before its end.
@[direct_array_access; markused]
fn (mut decoder Decoder) skip_current_value() {
	if decoder.current_idx >= decoder.values_info.len {
		return
	}
	value_info := decoder.values_info[decoder.current_idx]
	value_end := value_info.position + value_info.length
	mut next_idx := decoder.current_idx + 1
	for next_idx < decoder.values_info.len && decoder.values_info[next_idx].position < value_end {
		next_idx++
	}
	decoder.current_idx = next_idx
}

@[manualfree]
fn decode_struct_key[T](mut decoder Decoder, val T, key_info ValueInfo, prefix string, mut seen_required []string) !StructKeyDecodeResult[T] {
	field_infos := decoder_field_infos[T]()
	mut new_val := val
	mut i := 0
	$for field in T.fields {
		field_info := field_infos[i]
		$if !field.is_embed {
			if decoder.json_key_matches(key_info, field_info.key_name)!
				|| (prefix != ''
					&& decoder.json_key_matches(key_info, prefix + field_info.key_name)!) {
				decoder.current_idx++

				if field_info.is_skip {
					if field_info.is_required {
						seen_required << prefix + field.name
					}
					decoder.skip_current_value()
					return StructKeyDecodeResult[T]{
						matched: true
						value:   new_val
					}
				}

				if !field.attrs.contains('skip') {
					if field_info.is_required {
						seen_required << prefix + field.name
					}

					$if field.is_shared {
						decoder.reject_shared_struct_field(field_info.is_raw, field_info.is_required,
							field.name)!
					} $else $if field.typ is ?rune {
						decoder.decode_option_rune_struct_field(&new_val.$(field.name),
							field_info.is_raw)!
					} $else {
						decoder.decode_struct_field(&new_val.$(field.name), field_info.is_raw,
							field_info.is_required, field.name)!
					}
					return StructKeyDecodeResult[T]{
						matched: true
						value:   new_val
					}
				}
			}
		}
		i++
	}
	i = 0
	$for field in T.fields {
		field_info := field_infos[i]
		$if field.is_embed {
			if decoder.json_key_matches(key_info, field_info.key_name)!
				&& decoder.has_value(decoder.current_idx + 1)
				&& decoder.values_info[decoder.current_idx + 1].value_kind == .object {
				if field_info.is_required {
					seen_required << prefix + field.name
				}
				decoder.current_idx++
				decoder.decode_value(mut new_val.$(field.name))!
				return StructKeyDecodeResult[T]{
					matched: true
					value:   new_val
				}
			}
			{
				embed_result := decode_struct_key(mut decoder, new_val.$(field.name), key_info,

					prefix + field.name + '.', mut seen_required)!
				if embed_result.matched {
					new_val.$(field.name) = embed_result.value
					return StructKeyDecodeResult[T]{
						matched: true
						value:   new_val
					}
				}
			}
		}
		i++
	}
	return StructKeyDecodeResult[T]{
		matched: false
		value:   val
	}
}

fn check_required_struct_fields[T](mut decoder Decoder, val T, seen_required []string, prefix string) ! {
	field_infos := decoder_field_infos[T]()
	mut i := 0
	$for field in T.fields {
		field_info := field_infos[i]
		if field_info.is_required && prefix + field.name !in seen_required {
			decoder.decode_error('missing required field `${field.name}`')!
		}
		$if field.is_embed {
			check_required_struct_fields(mut decoder, val.$(field.name), seen_required, prefix +
				field.name + '.')!
		}
		i++
	}
}

// decode_struct_with_embeds decodes a JSON object into a struct with embedded structs,
// whose fields can also be matched through those embeds.
@[manualfree]
fn (mut decoder Decoder) decode_struct_with_embeds[T](mut val T, struct_info ValueInfo) ! {
	struct_end := struct_info.position + struct_info.length
	decoder.current_idx++
	mut seen_required := []string{}

	// json object loop
	for {
		if decoder.current_idx >= decoder.values_info.len {
			break
		}

		key_info := decoder.current_value()

		if key_info.position >= struct_end {
			break
		}

		decode_result := decode_struct_key(mut decoder, val, key_info, '', mut seen_required)!
		if decode_result.matched {
			val = decode_result.value
		} else {
			// The key doesn't match any field in the struct, skip the entire value
			// including all nested objects/arrays.
			decoder.current_idx++
			decoder.skip_current_value()
		}
	}

	check_required_struct_fields(mut decoder, val, seen_required, '')!
}

// decode_struct_fields decodes a JSON object into a struct without embedded structs.
// Everything that does not depend on the field types is done by non-generic helpers, and
// the field values by decode_struct_field, which is specialized per field type and shared
// by all structs. That keeps the code generated for each struct small.
@[manualfree]
fn (mut decoder Decoder) decode_struct_fields[T](mut val T, struct_info ValueInfo) ! {
	struct_end := struct_info.position + struct_info.length
	field_info_cache := unsafe { decoder.cached_struct_field_infos[T]() }
	mut decoded_mask := u64(0)
	mut decoded_fields := []bool{}
	if field_info_cache.field_infos.len > 64 {
		decoded_fields = []bool{len: field_info_cache.field_infos.len}
	}
	decoder.current_idx++

	// json object loop
	for {
		field_idx := decoder.next_struct_field_idx(field_info_cache.field_infos, struct_end)!
		if field_idx == struct_key_object_end {
			break
		}
		if field_idx == struct_key_no_field {
			continue
		}
		decoded_mask = mark_struct_field_decoded(decoded_mask, mut decoded_fields, field_idx)
		field_info := field_info_cache.field_infos[field_idx]
		if field_info.is_skip {
			// Preserve the existing decode behavior for `skip`+`required`.
			decoder.current_idx++
			continue
		}
		mut i := 0
		$for field in T.fields {
			if field.attrs.contains('skip') {
				// Handled above, without decoding (or even compiling a decoder for) its type.
			} else if i == field_idx {
				$if field.is_shared {
					decoder.reject_shared_struct_field(field_info.is_raw, field_info.is_required,
						field.name)!
				} $else $if field.typ is ?rune {
					decoder.decode_option_rune_struct_field(&val.$(field.name), field_info.is_raw)!
				} $else {
					decoder.decode_struct_field(&val.$(field.name), field_info.is_raw,
						field_info.is_required, field.name)!
				}
			}
			i++
		}
	}

	decoder.check_required_struct_fields_decoded(field_info_cache.field_infos, decoded_mask,
		decoded_fields)!
}

const struct_key_no_field = -1
const struct_key_object_end = -2

// next_struct_field_idx reads the next key of the JSON object that ends at `struct_end`,
// and returns the index of the struct field that it names, with the decoder at the field
// value. A key that names no field is skipped with its value, and gives
// `struct_key_no_field`. The end of the object gives `struct_key_object_end`.
@[manualfree]
fn (mut decoder Decoder) next_struct_field_idx(field_infos []StructFieldInfo, struct_end int) !int {
	if decoder.current_idx >= decoder.values_info.len {
		return struct_key_object_end
	}

	key_info := decoder.current_value()

	if key_info.position >= struct_end {
		return struct_key_object_end
	}

	mut key_ptr := unsafe { voidptr(decoder.json.str + key_info.position + 1) }
	mut key_len := key_info.length - 2
	mut unescaped_key := ''
	if decoder.key_has_escape(key_info) {
		unescaped_key = decoder.decode_string_value(key_info)!
		key_ptr = voidptr(unescaped_key.str)
		key_len = unescaped_key.len
	}

	field_idx := decoder.find_struct_field(field_infos, key_ptr, key_len)
	// value node
	decoder.current_idx++
	if field_idx < 0 {
		// The key doesn't match any field in the struct, skip the entire value
		// including all nested objects/arrays.
		decoder.skip_current_value()
		return struct_key_no_field
	}
	return field_idx
}

// check_required_struct_fields_decoded reports a required field that got no value.
fn (mut decoder Decoder) check_required_struct_fields_decoded(field_infos []StructFieldInfo, decoded_mask u64, decoded_fields []bool) ! {
	for field_idx, field_info in field_infos {
		if field_info.is_required
			&& !struct_field_is_decoded(decoded_mask, decoded_fields, field_idx) {
			decoder.decode_error('missing required field `${field_info.name}`')!
		}
	}
}

// decode_struct_field decodes the current JSON value into the struct field at `field`.
// `field_name` is the V name of the field, for error messages.
@[manualfree]
fn (mut decoder Decoder) decode_struct_field[F](field &F, is_raw bool, is_required bool, field_name string) ! {
	if is_raw {
		$if F.unaliased_typ is $enum {
			decoder.decode_error('`raw` attribute cannot be used with enum fields')!
		} $else $if F is ?string || F is string {
			unsafe {
				*field = decoder.decode_raw_value()
			}
		} $else {
			decoder.decode_error('`raw` attribute can only be used with string fields')!
		}
		return
	}
	value_kind := decoder.current_value().value_kind
	$if F is $option {
		if value_kind == .null {
			unsafe {
				*field = none
			}
			decoder.current_idx++
		} else {
			unsafe {
				*field = decoder.decode_option_payload(F(none))!
			}
		}
	} $else {
		if value_kind == .null {
			// A `@[required]` field needs a value: `null` is rejected like in the removed
			// `json` module, before the value decoders treat it leniently.
			if is_required {
				decoder.decode_error('required field `${field_name}` cannot be null')!
			}
			$if F.unaliased_typ is $array_dynamic || F.unaliased_typ is $map {
				mut target := unsafe { field }
				target.clear()
				decoder.skip_current_value()
				return
			} $else $if F.unaliased_typ is string {
				unsafe {
					*field = F('')
				}
				decoder.skip_current_value()
				return
			} $else $if F.indirections != 0 {
				unsafe {
					*field = nil
				}
				decoder.current_idx++
				return
			}
		}
		$if F.indirections == 1 {
			mut decoded_ptr := create_decoded_ptr(*field)
			decoder.decode_value(mut decoded_ptr)!
			unsafe {
				*field = decoded_ptr
			}
		} $else $if F.indirections > 1 {
			unsafe {
				*field = decoder.decode_new_pointer(*field)!
			}
		} $else {
			mut target := unsafe { field }
			decoder.decode_value(mut target)!
		}
	}
}

// decode_option_rune_struct_field decodes a `?rune` struct field. The payload of a generic
// `?rune` does not keep its `rune` type, so decode_struct_field would decode a number.
fn (mut decoder Decoder) decode_option_rune_struct_field(field &?rune, is_raw bool) ! {
	if is_raw {
		decoder.decode_error('`raw` attribute can only be used with string fields')!
	}
	if decoder.current_value().value_kind == .null {
		unsafe {
			*field = none
		}
		decoder.current_idx++
		return
	}
	mut unwrapped_rune := rune(0)
	decoder.decode_value(mut unwrapped_rune)!
	unsafe {
		*field = unwrapped_rune
	}
}

// reject_shared_struct_field reports the error for a value of a `shared` struct field,
// which cannot be decoded.
fn (mut decoder Decoder) reject_shared_struct_field(is_raw bool, is_required bool, field_name string) ! {
	if is_raw {
		decoder.decode_error('`raw` attribute can only be used with string fields')!
	}
	if is_required && decoder.current_value().value_kind == .null {
		decoder.decode_error('required field `${field_name}` cannot be null')!
	}
	decoder.decode_error('shared fields cannot be decoded')!
}

// decode_raw_value returns the JSON text of the current value, for a `@[raw]` field, and
// moves past it.
fn (mut decoder Decoder) decode_raw_value() string {
	value_info := decoder.current_value()
	raw := decoder.json[value_info.position..value_info.position + value_info.length]
	decoder.skip_current_value()
	return raw
}

// decode_value decodes a value from the JSON nodes.
@[manualfree]
fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is voidptr {
		// skip voidptr fields - they cannot be decoded from JSON
		decoder.current_idx++
		return
	} $else $if T is $interface {
		// skip interface fields - they cannot be decoded from JSON
		decoder.current_idx++
		return
	} $else $if T is $option {
		// An option element of an array or map (`[]?int`): `null` is `none`, anything
		// else is the payload, like in the removed `json` module.
		if decoder.current_value().value_kind == .null {
			val = none
			decoder.current_idx++
			return
		}
		val = decoder.decode_option_payload(val)!
		return
	} $else {
		// Custom Decoders
		$if val is StringDecoder {
			struct_info := decoder.current_value()

			if struct_info.value_kind == .string {
				val.from_json_string(decoder.json[struct_info.position + 1..struct_info.position +
					struct_info.length - 1]) or {
					decoder.decode_error('${typeof(val).name}: ${err.msg()}')!
				}
				decoder.current_idx++

				return
			}
		}
		$if val is NumberDecoder {
			struct_info := decoder.current_value()

			if struct_info.value_kind == .number {
				val.from_json_number(decoder.json[struct_info.position..struct_info.position +
					struct_info.length]) or {
					decoder.decode_error('${typeof(val).name}: ${err.msg()}')!
				}
				decoder.current_idx++

				return
			}
		}
		$if val is BooleanDecoder {
			struct_info := decoder.current_value()

			if struct_info.value_kind == .boolean {
				val.from_json_boolean(decoder.json[struct_info.position] == `t`)
				decoder.current_idx++

				return
			}
		}
		$if val is NullDecoder {
			struct_info := decoder.current_value()

			if struct_info.value_kind == .null {
				val.from_json_null()
				decoder.current_idx++

				return
			}
		}
		$if T.unaliased_typ is $int || T.unaliased_typ is $float || T.unaliased_typ is bool || T.unaliased_typ is string || T.unaliased_typ is $enum {
			if decoder.current_value().value_kind == .null && !decoder.strict {
				// Outside of strict mode, `null` is the zero value, like in the removed
				// `json` module: `[1, null]` decodes into `[]int` as `[1, 0]`.
				val = $zero(T)
				decoder.current_idx++
				return
			}
		}
		$if T.unaliased_typ is voidptr {
			// skip voidptr fields - they cannot be decoded from JSON
			decoder.current_idx++
			return
		} $else $if T.unaliased_typ is string {
			value_info := decoder.current_value()
			if (value_info.value_kind == .object || value_info.value_kind == .array)
				&& decoder.current_idx != 0 {
				// Like a string field (and the removed `json` module), an object or array
				// decodes into a string as its JSON text, also as an element or map value.
				// The removed module had no string root, which stays an error.
				val = T(decoder.json[value_info.position..value_info.position + value_info.length])
				decoder.skip_current_value()
				return
			}
			decoder.decode_string(mut val)!
		} $else $if T.unaliased_typ is time.Time {
			value_info := decoder.current_value()
			mut decoded_time := time.Time{}
			if value_info.value_kind == .string {
				decoded_time.from_json_string(decoder.json[value_info.position + 1..value_info.position + value_info.length - 1]) or {
					decoder.decode_error('${typeof(val).name}: ${err.msg()}')!
				}
			} else if value_info.value_kind == .number {
				decoded_time.from_json_number(decoder.json[value_info.position..value_info.position + value_info.length]) or {
					decoder.decode_error('${typeof(val).name}: ${err.msg()}')!
				}
			} else if value_info.value_kind == .null && !decoder.strict {
				// Outside of strict mode `null` is the zero time, like in the removed module.
			} else if value_info.value_kind == .object {
				// The removed module wrote a time in a sum type, also as an element of an
				// array variant or an option payload, as `{"_type":"Time","value":...}`.
				// Only that wrapper is a time: other objects are rejected, like before.
				type_field_idx := decoder.get_sumtype_type_field_idx(decoder.current_idx)
				if !decoder.sumtype_type_field_matches(type_field_idx, 'Time')
					&& !decoder.sumtype_type_field_matches(type_field_idx, sumtype_variant_name(T.name)) {
					decoder.decode_error('Expected string, number or `Time` object, but got another object')!
				}
				decoder.decode_sumtype_time(mut decoded_time)!
				val = T(decoded_time)
				return
			} else {
				decoder.decode_error('Expected string or number, but got ${value_info.value_kind}')!
			}
			val = T(decoded_time)
		} $else $if T.unaliased_typ is $sumtype {
			decoder.decode_sumtype(mut val)!
			return
		} $else $if T.unaliased_typ is $map {
			decoder.decode_map(mut val)!
			return
		} $else $if T.unaliased_typ is $array_dynamic {
			val.clear()
			decoder.decode_array(mut val)!
			// return to avoid the next increment of the current node
			// this is because the current node is already incremented in the decode_array function
			// remove this line will cause the current node to be incremented twice
			// and bug recursive array decoding like `[][]int{}`
			return
		} $else $if T.unaliased_typ is $array_fixed {
			decoder.decode_fixed_array(mut val)!
			return
		} $else $if T.unaliased_typ is $struct {
			struct_info := decoder.current_value()

			if struct_info.value_kind == .object {
				// Only a struct with an embedded struct needs its keys matched through its
				// embeds too. Deciding that at compile time keeps the larger embed path out
				// of the code generated for every other struct.
				$for field in T.fields {
					$if field.is_embed {
						decoder.decode_struct_with_embeds(mut val, struct_info)!
						return
					}
				}
				decoder.decode_struct_fields(mut val, struct_info)!
			} else if struct_info.value_kind == .null && !decoder.strict
				&& decoder.current_idx != 0 {
				// Outside of strict mode a nested `null` is the struct with its default
				// field values, like in the removed `json` module, which rejected a
				// `null` root.
				val = T{}
				decoder.current_idx++
			} else {
				decoder.decode_error('Expected object, but got ${struct_info.value_kind}')!
			}
			return
		} $else $if T.unaliased_typ is bool {
			value_info := decoder.current_value()

			if value_info.value_kind != .boolean {
				decoder.decode_error('Expected boolean, but got ${value_info.value_kind}')!
			}

			unsafe {
				val = vmemcmp(decoder.json.str + value_info.position, c'true', 'true'.len) == 0
			}
		} $else $if T.unaliased_typ is rune {
			// Like the removed `json` module, a rune is decoded from a JSON string (its
			// first character); a number is taken as the code point.
			value_info := decoder.current_value()
			if value_info.value_kind == .string {
				decoded := decoder.decode_string_value(value_info)!
				val = if decoded.len > 0 { T(decoded.runes()[0]) } else { T(0) }
			} else if value_info.value_kind == .number {
				mut code_point := u32(0)
				unsafe { decoder.decode_number(&code_point)! }
				val = T(code_point)
			} else {
				decoder.decode_error('Expected string, but got ${value_info.value_kind}')!
			}
		} $else $if T.unaliased_typ is $float || T.unaliased_typ is $int {
			value_info := decoder.current_value()

			if value_info.value_kind == .number {
				unsafe { decoder.decode_number(&val)! }
			} else if value_info.value_kind == .string && !decoder.strict {
				// In default mode, try to parse quoted strings as numbers
				val = decoder.decode_number_from_string[T]()!
			} else {
				decoder.decode_error('Expected number, but got ${value_info.value_kind}')!
			}
		} $else $if T.unaliased_typ is $enum {
			decoder.decode_enum(mut val)!
		} $else {
			decoder.decode_error('cannot decode value with ${typeof(val).name} type')!
		}

		decoder.current_idx++
	} // $else (not voidptr / interface)
}

fn (mut decoder Decoder) decode_string[T](mut val T) ! {
	_ = val
	string_info := decoder.current_value()

	if string_info.value_kind == .string {
		val = decoder.decode_string_value(string_info)!
	} else {
		decoder.decode_error('Expected string, but got ${string_info.value_kind}')!
	}
}

// decode_string_value returns the unescaped content of the JSON string described by `string_info`.
fn (mut decoder Decoder) decode_string_value(string_info ValueInfo) !string {
	string_start := string_info.position + 1
	string_end := string_info.position + string_info.length - 1
	string_body := decoder.json[string_start..string_end]
	if string_body.index_u8(`\\`) == -1 {
		return string_body
	}

	mut string_buffer := []u8{cap: string_info.length} // might be too long but most json strings don't contain many escape characters anyways

	mut buffer_index := 1
	mut string_index := 1

	for string_index < string_info.length - 1 {
		current_byte := decoder.json[string_info.position + string_index]

		if current_byte == `\\` {
			// push all characters up to this point
			unsafe {
				string_buffer.push_many(decoder.json.str + string_info.position + buffer_index,
					string_index - buffer_index)
			}

			string_index++

			escaped_char := decoder.json[string_info.position + string_index]

			string_index++

			match escaped_char {
				`/`, `"`, `\\` {
					string_buffer << escaped_char
				}
				`b` {
					string_buffer << `\b`
				}
				`f` {
					string_buffer << `\f`
				}
				`n` {
					string_buffer << `\n`
				}
				`r` {
					string_buffer << `\r`
				}
				`t` {
					string_buffer << `\t`
				}
				`u` {
					unicode_point := rune(strconv.parse_uint(decoder.json[string_info.position +
						string_index..string_info.position + string_index + 4], 16, 32)!)

					string_index += 4

					if unicode_point < 0xD800 || unicode_point > 0xDFFF { // normal utf-8
						string_buffer << unicode_point.bytes()
					} else if unicode_point >= 0xDC00 { // trail surrogate -> invalid
						decoder.decode_error('Got trail surrogate: ${u32(unicode_point):04X} before head surrogate.')!
					} else { // head surrogate -> treat as utf-16
						if string_index > string_info.length - 6 {
							decoder.decode_error('Expected a trail surrogate after a head surrogate, but got no valid escape sequence.')!
						}
						if decoder.json[string_info.position + string_index..string_info.position +
							string_index + 2] != '\\u' {
							decoder.decode_error('Expected a trail surrogate after a head surrogate, but got no valid escape sequence.')!
						}

						string_index += 2

						unicode_point2 := rune(strconv.parse_uint(decoder.json[string_info.position + string_index..string_info.position +
							string_index + 4], 16, 32)!)

						string_index += 4

						if unicode_point2 < 0xDC00 {
							decoder.decode_error('Expected a trail surrogate after a head surrogate, but got ${u32(unicode_point):04X}.')!
						}

						final_unicode_point := (unicode_point2 & 0x3FF) +
							((unicode_point & 0x3FF) << 10) + 0x10000
						string_buffer << final_unicode_point.bytes()
					}
				}
				else {}
			}

			// has already been checked

			buffer_index = string_index
		} else {
			string_index++
		}
	}

	// push the rest
	unsafe {
		string_buffer.push_many(decoder.json.str + string_info.position + buffer_index,
			string_index - buffer_index)
	}

	return string_buffer.bytestr()
}

fn (mut decoder Decoder) decode_array[T](mut val []T) ! {
	$if T is $interface {
		decoder.skip_current_value()
		return
	} $else $if T.unaliased_typ is voidptr {
		decoder.skip_current_value()
		return
	} $else {
		array_info := decoder.current_value()

		if array_info.value_kind == .array {
			decoder.current_idx++

			array_position := array_info.position
			array_end := array_position + array_info.length

			for {
				if decoder.current_idx >= decoder.values_info.len
					|| decoder.current_value().position >= array_end {
					break
				}

				$if T is $option {
					// An option element (`[]?int`): `null` is `none`. Decoded directly,
					// since v3 cannot return `!?T` from the element helper.
					if decoder.option_element_is_none(T(none)) {
						val << T(none)
						decoder.skip_current_value()
					} else {
						val << decoder.decode_option_payload(T(none))!
					}
				} $else {
					val << decoder.decode_array_element(T{})!
				}
			}
		} else if array_info.value_kind == .null && !decoder.strict {
			// Outside of strict mode `null` is an empty array, like in the removed module.
			decoder.current_idx++
		} else {
			decoder.decode_error('Expected array, but got ${array_info.value_kind}')!
		}
	}
}

// decode_fixed_array decodes a JSON array into the elements of a fixed size array.
// Outside of strict mode it behaves like the removed `json` module: `null` keeps the
// default elements, a shorter array only replaces the leading elements, and extra
// elements are skipped.
fn (mut decoder Decoder) decode_fixed_array[T](mut val T) ! {
	array_info := decoder.current_value()
	if array_info.value_kind == .null && !decoder.strict {
		decoder.current_idx++
		return
	}
	if array_info.value_kind != .array {
		decoder.decode_error('Expected array, but got ${array_info.value_kind}')!
	}
	decoder.current_idx++
	array_end := array_info.position + array_info.length
	mut idx := 0
	for decoder.current_idx < decoder.values_info.len
		&& decoder.current_value().position < array_end {
		if idx < val.len {
			decoder.decode_fixed_array_element(&val[idx])!
		} else {
			decoder.skip_current_value()
		}
		idx++
	}
	if decoder.strict && idx != val.len {
		decoder.decode_error('Fixed size array expected ${val.len} elements but got ${idx} elements')!
	}
}

// decode_fixed_array_element decodes the current JSON value into a fixed size array
// element. Elements are decoded in place, since v3 cannot yet assign a fixed array
// element that is itself a fixed array through a `mut` parameter; only a pointer
// element is replaced.
fn (mut decoder Decoder) decode_fixed_array_element[E](element &E) ! {
	$if E is $option {
		if decoder.option_element_is_none(E(none)) {
			unsafe {
				*element = E(none)
			}
			decoder.skip_current_value()
		} else {
			payload := decoder.decode_option_payload(E(none))!
			unsafe {
				*element = payload
			}
		}
	} $else $if E is $pointer {
		unsafe {
			*element = decoder.decode_array_element(*element)!
		}
	} $else {
		mut target := unsafe { element }
		decoder.decode_value(mut target)!
	}
}

// decode_array_element decodes the current JSON value as an array element that
// starts out as `initial`. It takes and returns the element by value: for a `mut`
// parameter, a pointer element type would be inferred without its `&`.
fn (mut decoder Decoder) decode_array_element[E](initial E) !E {
	mut element := initial
	$if E is $interface {
		decoder.skip_current_value()
	} $else $if E.unaliased_typ is voidptr {
		decoder.skip_current_value()
	} $else $if E.indirections == 1 {
		if decoder.current_value().value_kind == .null {
			decoder.current_idx++
		} else {
			mut decoded_ptr := create_decoded_ptr(element)
			decoder.decode_value(mut decoded_ptr)!
			element = decoded_ptr
		}
	} $else $if E.indirections > 1 {
		// `&&T`, `&&&T`, ... elements point to a newly decoded value, like fields.
		if decoder.current_value().value_kind == .null {
			decoder.current_idx++
		} else {
			element = decoder.decode_new_pointer(element)!
		}
	} $else {
		decoder.decode_value(mut element)!
	}
	return element
}

// decode_enum_map_key accepts member names and the map encoder's flag-enum syntax.
// JSON attributes only rename scalar enum values, not map keys.
fn (mut decoder Decoder) decode_enum_map_key[K](key_str string) !K {
	$for member in K.values {
		if key_str == member.name {
			return K(member.value)
		}
	}
	zero := unsafe { K(0) }
	zero_str := '${zero}'
	// Flag enums, including aliases, stringify zero as `Enum{}`. Use the same
	// type prefix as the map encoder instead of assuming the alias's name.
	if zero_str.ends_with('{}') {
		prefix := zero_str[..zero_str.len - 1]
		if key_str.starts_with(prefix) && key_str.ends_with('}') {
			flags := key_str[prefix.len..key_str.len - 1].trim_space()
			if flags == '' {
				return zero
			}
			mut bits := u64(0)
			mut valid := true
			for flag in flags.split('|') {
				name := flag.trim_space()
				mut matched := false
				$for member in K.values {
					if name == '.${member.name}' {
						bits |= u64(member.value)
						matched = true
					}
				}
				if !matched {
					valid = false
					break
				}
			}
			if valid {
				return unsafe { K(bits) }
			}
		}
	}
	decoder.decode_error('String map key: `${key_str}` does not match any field in enum: ${K.name}')!
	return zero
}

fn (mut decoder Decoder) decode_map[K, V](mut val map[K]V) ! {
	$if V is $interface {
		decoder.skip_current_value()
		return
	} $else $if V.unaliased_typ is voidptr {
		decoder.skip_current_value()
		return
	} $else {
		map_info := decoder.current_value()

		if map_info.value_kind == .object {
			map_position := map_info.position
			map_end := map_position + map_info.length

			decoder.current_idx++
			for {
				if decoder.current_idx >= decoder.values_info.len
					|| decoder.current_value().position >= map_end {
					break
				}

				key_info := decoder.current_value()

				if key_info.position >= map_end {
					break
				}

				key_str := if decoder.key_has_escape(key_info) {
					decoder.decode_string_value(key_info)!
				} else {
					decoder.json[key_info.position + 1..key_info.position + key_info.length - 1]
				}

				mut key := K{}
				$if K is string {
					key = K(key_str)
				} $else $if K.unaliased_typ is $enum {
					key = decoder.decode_enum_map_key[K](key_str)!
				} $else $if K is rune {
					key = K(key_str.int())
				} $else $if K is u8 || K is u16 || K is u32 || K is u64 || K is usize {
					key = K(key_str.u64())
				} $else $if K is i64 || K is isize {
					key = K(key_str.i64())
				} $else $if K is $int {
					key = K(key_str.int())
				} $else {
					key = K(key_str)
				}

				decoder.current_idx++

				value_info := decoder.current_value()

				if value_info.position + value_info.length > map_end {
					break
				}

				$if V is $option {
					// An option map value: `null` is `none`.
					if decoder.current_value().value_kind == .null {
						val[key] = V(none)
						decoder.current_idx++
					} else {
						val[key] = decoder.decode_option_payload(V(none))!
					}
					continue
				}
				mut map_value := V{}

				$if V is $pointer {
					map_value = decoder.decode_array_element(map_value)!
				} $else {
					decoder.decode_value(mut map_value)!
				}

				// Map alias values (`type Props = map[string]int`) also need to move.
				$if V is $map || (V is $alias && V.unaliased_typ is $map) {
					val[key] = map_value.move()
				} $else {
					val[key] = map_value
				}
			}
		} else if map_info.value_kind == .null && !decoder.strict {
			// Outside of strict mode `null` is an empty map, like in the removed module.
			val.clear()
			decoder.current_idx++
		} else {
			decoder.decode_error('Expected object, but got ${map_info.value_kind}')!
		}
	}
}

fn (mut decoder Decoder) decode_enum[T](mut val T) ! {
	enum_info := decoder.current_value()

	if enum_info.value_kind == .number {
		$if T.unaliased_typ is $enum {
			if enum_uses_json_as_number[T]() {
				// Like the removed `json` module, a `@[json_as_number]` enum takes the
				// number as its backing value, declared or not (`Status(99)`). A
				// non-negative number is read as `u64`, so a `u64` backing value above
				// `max_i64` fits; a negative one as `i64`.
				if decoder.json[enum_info.position] == `-` {
					mut backing := i64(0)
					unsafe { decoder.decode_number(&backing)! }
					val = unsafe { T(backing) }
				} else {
					mut backing := u64(0)
					unsafe { decoder.decode_number(&backing)! }
					val = unsafe { T(backing) }
				}
				return
			}
		}
		mut result := 0
		unsafe { decoder.decode_number(&result)! }

		$for value in T.values {
			if int(value.value) == result {
				val = value.value
				return
			}
		}
		decoder.decode_error('Number value: `${result}` does not match any field in enum: ${typeof(val).name}')!
	} else if enum_info.value_kind == .string {
		mut result := ''
		decoder.decode_string(mut result)!

		$for value in T.values {
			for attr in value.attrs {
				if json_attr := json_attr_value(attr) {
					if json_attr == result {
						val = value.value
						return
					}
				}
			}
			if value.name == result {
				val = value.value
				return
			}
		}
		decoder.decode_error('String value: `${result}` does not match any field in enum: ${typeof(val).name}')!
	}

	decoder.decode_error('Expected number or string value for enum, got: ${enum_info.value_kind}')!
}

const max_integer_number_digits = 20

@[markused]
fn has_exponent_number_syntax(str string) bool {
	for c in str {
		if c == `e` || c == `E` {
			return true
		}
	}
	return false
}

@[markused]
fn scientific_number_to_integer_string(str string) !string {
	if !has_exponent_number_syntax(str) {
		// Handle plain decimal numbers with zero fractional part (e.g., "-123.0")
		dot_pos := str.index_u8(`.`)
		if dot_pos >= 0 {
			frac := str[dot_pos + 1..]
			mut all_zeros := frac.len > 0
			for c in frac {
				if c != `0` {
					all_zeros = false
					break
				}
			}
			if all_zeros {
				result := str[..dot_pos]
				if result.len == 0 || result == '-' || result == '+' {
					return '0'
				}
				return result
			}
		}
		return str
	}
	if str.len == 0 {
		return error('invalid scientific notation number')
	}
	mut i := 0
	mut is_negative := false
	if str[i] == `+` || str[i] == `-` {
		is_negative = str[i] == `-`
		i++
	}
	if i >= str.len {
		return error('invalid scientific notation number')
	}
	mut digits := []u8{cap: str.len}
	mut fractional_digits := 0
	mut seen_digit := false
	mut seen_dot := false
	for i < str.len {
		c := str[i]
		if c >= `0` && c <= `9` {
			digits << c
			seen_digit = true
			if seen_dot {
				fractional_digits++
			}
			i++
			continue
		}
		if c == `.` && !seen_dot {
			seen_dot = true
			i++
			continue
		}
		break
	}
	if !seen_digit || i >= str.len || (str[i] != `e` && str[i] != `E`) {
		return error('invalid scientific notation number')
	}
	i++
	mut exponent_sign := 1
	if i < str.len && (str[i] == `+` || str[i] == `-`) {
		if str[i] == `-` {
			exponent_sign = -1
		}
		i++
	}
	if i >= str.len || str[i] < `0` || str[i] > `9` {
		return error('invalid scientific notation number')
	}
	mut exponent := 0
	exponent_cap := max_integer_number_digits + fractional_digits + 1
	for i < str.len && str[i] >= `0` && str[i] <= `9` {
		if exponent < exponent_cap {
			exponent = (exponent * 10) + int(str[i] - `0`)
		}
		i++
	}
	if i != str.len {
		return error('invalid scientific notation number')
	}
	if exponent_sign == -1 {
		exponent = -exponent
	}
	mut first_non_zero := 0
	for first_non_zero < digits.len && digits[first_non_zero] == `0` {
		first_non_zero++
	}
	if first_non_zero == digits.len {
		return '0'
	}
	digits = digits[first_non_zero..].clone()
	scale := exponent - fractional_digits
	if scale < 0 {
		truncated_digits := -scale
		if truncated_digits >= digits.len {
			return '0'
		}
		digits = digits[..digits.len - truncated_digits].clone()
	} else if scale > 0 {
		final_len := digits.len + scale
		if final_len > max_integer_number_digits {
			return error('number `${str}` exceeds 64-bit integer range')
		}
		mut out := []u8{cap: final_len + if is_negative { 1 } else { 0 }}
		if is_negative {
			out << `-`
		}
		out << digits
		for _ in 0 .. scale {
			out << `0`
		}
		return out.bytestr()
	}
	if digits.len == 0 {
		return '0'
	}
	mut out := []u8{cap: digits.len + if is_negative { 1 } else { 0 }}
	if is_negative {
		out << `-`
	}
	out << digits
	return out.bytestr()
}

fn parse_integer_number[T](str string) !T {
	int_str := scientific_number_to_integer_string(str)!
	$if T.unaliased_typ is i8 {
		return T(strconv.atoi8(int_str)!)
	} $else $if T.unaliased_typ is i16 {
		return T(strconv.atoi16(int_str)!)
	} $else $if T.unaliased_typ is i32 {
		return T(strconv.atoi32(int_str)!)
	} $else $if T.unaliased_typ is i64 {
		return T(strconv.atoi64(int_str)!)
	} $else $if T.unaliased_typ is u8 {
		return T(strconv.atou8(int_str)!)
	} $else $if T.unaliased_typ is u16 {
		return T(strconv.atou16(int_str)!)
	} $else $if T.unaliased_typ is u32 {
		return T(strconv.atou32(int_str)!)
	} $else $if T.unaliased_typ is u64 {
		return T(strconv.atou64(int_str)!)
	} $else $if T is int {
		return int(strconv.atoi64(int_str)!)
	} $else $if T.unaliased_typ is int {
		return T(int(strconv.atoi64(int_str)!))
	} $else $if T.unaliased_typ is isize {
		return T(isize(strconv.atoi64(int_str)!))
	} $else $if T.unaliased_typ is usize {
		return T(usize(strconv.atou64(int_str)!))
	} $else {
		return error('`parse_integer_number` cannot decode ${T.name} type')
	}
}

@[markused]
fn parse_int_number(str string) !int {
	int_str := scientific_number_to_integer_string(str)!
	return int(strconv.atoi64(int_str)!)
}

fn parse_float_number[T](str string) !T {
	$if js {
		$if T.unaliased_typ is f32 {
			return T(f32(strconv.atof64(str)!))
		} $else $if T.unaliased_typ is f64 {
			return T(strconv.atof64(str)!)
		} $else {
			return error('`parse_float_number` cannot decode ${T.name} type')
		}
	} $else {
		$if T.unaliased_typ is f32 {
			return T(f32(strconv.atof64(str, allow_extra_chars: false)!))
		} $else $if T.unaliased_typ is f64 {
			return T(strconv.atof64(str, allow_extra_chars: false)!)
		} $else {
			return error('`parse_float_number` cannot decode ${T.name} type')
		}
	}
}

// use pointer instead of mut so enum cast works
@[unsafe]
fn (mut decoder Decoder) decode_number[T](val &T) ! {
	_ = val
	number_info := decoder.current_value()
	str := decoder.json[number_info.position..number_info.position + number_info.length]
	$match T.unaliased_typ {
		i8 { *val = parse_integer_number[T](str)! }
		i16 { *val = parse_integer_number[T](str)! }
		i32 { *val = parse_integer_number[T](str)! }
		i64 { *val = parse_integer_number[T](str)! }
		u8 { *val = parse_integer_number[T](str)! }
		u16 { *val = parse_integer_number[T](str)! }
		u32 { *val = parse_integer_number[T](str)! }
		u64 { *val = parse_integer_number[T](str)! }
		int { *val = parse_int_number(str)! }
		isize { *val = parse_integer_number[T](str)! }
		usize { *val = parse_integer_number[T](str)! }
		f32 { *val = parse_float_number[T](str)! }
		f64 { *val = parse_float_number[T](str)! }
		$else { return error('`decode_number` can not decode ${T.name} type') }
	}
}

// decode_number_from_string parses a number from a JSON string value (default mode).
// This extracts the content between quotes and parses it as a number.
fn (mut decoder Decoder) decode_number_from_string[T]() !T {
	string_info := decoder.current_value()
	// Extract string content without quotes (position+1 to skip opening quote, length-2 to exclude both quotes)
	if string_info.length < 2 {
		return error('invalid string for number conversion')
	}
	str := decoder.json[string_info.position + 1..string_info.position + string_info.length - 1]
	$if T.unaliased_typ is i8 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is i16 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is i32 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is i64 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is u8 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is u16 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is u32 {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is u64 {
		return parse_integer_number[T](str)!
	} $else $if T is int {
		return parse_int_number(str)!
	} $else $if T.unaliased_typ is int {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is isize {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is usize {
		return parse_integer_number[T](str)!
	} $else $if T.unaliased_typ is f32 {
		return parse_float_number[T](str)!
	} $else $if T.unaliased_typ is f64 {
		return parse_float_number[T](str)!
	} $else {
		return error('`decode_number_from_string` cannot decode ${T.name} type')
	}
}
