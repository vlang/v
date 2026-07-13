module decoder2

import strconv
import time

// Node represents a node in a linked list to store ValueInfo.
struct Node {
	value ValueInfo
mut:
	next &Node = unsafe { nil } // next is the next node in the linked list.
}

// ValueInfo represents the position and length of a value, such as string, number, array, object key, and object value in a JSON string.
struct ValueInfo {
	position   int       // The position of the value in the JSON string.
	value_kind ValueKind // The kind of the value.
mut:
	length int // The length of the value in the JSON string.
}

// Decoder represents a JSON decoder.
struct Decoder {
	json string // json is the JSON data to be decoded.
mut:
	values_info  LinkedList // A linked list to store ValueInfo.
	checker_idx  int        // checker_idx is the current index of the decoder.
	current_node &Node = unsafe { nil } // The current node in the linked list.
}

interface NullDecoder {
mut:
	from_json_null()
}

// LinkedList represents a linked list to store ValueInfo.
struct LinkedList {
mut:
	head &Node = unsafe { nil } // head is the first node in the linked list.
	tail &Node = unsafe { nil } // tail is the last node in the linked list.
	len  int // len is the length of the linked list.
}

// push adds a new element to the linked list.
fn (mut list LinkedList) push(value ValueInfo) {
	new_node := &Node{
		value: value
	}
	if list.head == unsafe { nil } {
		list.head = new_node
		list.tail = new_node
	} else {
		list.tail.next = new_node
		list.tail = new_node
	}
	list.len++
}

// last returns the last element added to the linked list.
fn (list LinkedList) last() &ValueInfo {
	return &list.tail.value
}

// str returns a string representation of the linked list.
fn (list LinkedList) str() string {
	mut result_buffer := []u8{}
	mut current := list.head
	for current != unsafe { nil } {
		value_kind_as_string := current.value.value_kind.str()
		unsafe { result_buffer.push_many(value_kind_as_string.str, value_kind_as_string.len) }
		result_buffer << u8(` `)

		current = current.next
	}
	return result_buffer.bytestr()
}

@[unsafe]
fn (list &LinkedList) free() {
	mut current := list.head
	for current != unsafe { nil } {
		mut next := current.next
		current.next = unsafe { nil }
		unsafe { free(current) }
		current = next
	}
	list.head = unsafe { nil }
	list.tail = unsafe { nil }
	list.len = 0
}

// ValueKind represents the kind of a JSON value.
pub enum ValueKind {
	unknown
	array
	object
	string_
	number
	boolean
	null
}

// check_value_kind_match checks if a JSON value kind matches the expected type T.
fn check_value_kind_match[T](value_kind ValueKind) ! {
	$if T.unaliased_typ is $option {
		// TODO
	} $else $if T.unaliased_typ is $sumtype {
		// TODO
	} $else $if T.unaliased_typ is $string {
		if value_kind != .string_ {
			return error('Expected string, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ is time.Time {
		if value_kind != .string_ {
			return error('Expected string, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ is $map {
		if value_kind != .object {
			return error('Expected object, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ is $array {
		if value_kind != .array {
			return error('Expected array, but got ${value_kind}')
		}
	} $else $if T is NullDecoder {
		if value_kind != .null {
			return error('Expected null, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ is $struct {
		if value_kind != .object {
			return error('Expected object, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ is $enum {
		if value_kind !in [.number, .string_] {
			return error('Expected number or string, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ in [$int, $float] {
		if value_kind != .number {
			return error('Expected number, but got ${value_kind}')
		}
	} $else $if T.unaliased_typ is bool {
		if value_kind != .boolean {
			return error('Expected boolean, but got ${value_kind}')
		}
	} $else {
		return error('cannot encode value with ${value_kind} type')
	}
}

// check_if_json_match checks if the JSON string matches the expected type T.
fn check_if_json_match[T](val string) ! {
	if val == '' {
		return error('empty string')
	}

	mut value_idx := 0
	for value_idx < val.len && val[value_idx] in [` `, `\t`, `\n`, `\r`] {
		value_idx++
	}
	if value_idx >= val.len {
		return error('empty string')
	}

	check_value_kind_match[T](get_value_kind(val[value_idx]))!
}

// error generates an error message with context from the JSON string.
fn (mut checker Decoder) error(message string) ! {
	json := if checker.json.len < checker.checker_idx + 5 {
		checker.json
	} else {
		checker.json[0..checker.checker_idx + 5]
	}

	mut error_message := '\n'
	last_new_line := json.last_index_u8(`\n`)
	if last_new_line != -1 {
		error_message += json[last_new_line..checker.checker_idx]
	} else {
		error_message += json[0..checker.checker_idx]
	}
	error_message += [json[checker.checker_idx]].bytestr()

	error_message += '\n'

	if last_new_line != -1 {
		error_message += ' '.repeat(checker.checker_idx - last_new_line)
	} else {
		error_message += ' '.repeat(checker.checker_idx)
	}

	error_message += '^ ${message}'

	return error(error_message)
}

// check_json_format checks if the JSON string is valid and updates the decoder state.
fn (mut checker Decoder) check_json_format(val string) ! {
	checker_end := checker.json.len
	is_root_value := checker.values_info.len == 0
	// check if the JSON string is empty
	if val == '' {
		return error('empty string')
	}
	if is_root_value {
		for checker.checker_idx < checker_end && val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
			checker.checker_idx++
		}
		if checker.checker_idx >= checker_end {
			checker.checker_idx = checker_end - 1
			return checker.error('empty string')
		}
	}

	// check if generic type matches the JSON type
	value_kind := get_value_kind(val[checker.checker_idx])
	start_idx_position := checker.checker_idx
	checker.values_info.push(ValueInfo{
		position:   start_idx_position
		value_kind: value_kind
	})

	mut actual_value_info_pointer := checker.values_info.last()
	match value_kind {
		.unknown {
			return checker.error('unknown value kind')
		}
		.null {
			// check if the JSON string is a null value
			if checker_end - checker.checker_idx <= 3 {
				return checker.error('EOF error: expecting `null`')
			}

			is_not_ok := unsafe {
				vmemcmp(checker.json.str + checker.checker_idx, c'null', 4)
			}

			if is_not_ok != 0 {
				return checker.error('invalid null value. Got `${checker.json[checker.checker_idx..
					checker.checker_idx + 4]}` instead of `null`')
			}
			checker.checker_idx += 3
		}
		.object {
			checker.checker_idx++
			for {
				// skip whitespace
				for checker.checker_idx < checker_end
					&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
					checker.checker_idx++
				}
				if checker.checker_idx >= checker_end {
					checker.checker_idx = checker_end - 1
					return checker.error('EOF error: object not closed')
				}
				if val[checker.checker_idx] == `}` {
					break
				}

				match val[checker.checker_idx] {
					`"` {
						// Object key
						checker.check_json_format(val)!

						for checker.checker_idx < checker_end && val[checker.checker_idx] != `:` {
							if val[checker.checker_idx] !in [` `, `\t`, `\n`, `\r`] {
								return checker.error('invalid value after object key')
							}
							checker.checker_idx++
						}
						if checker.checker_idx >= checker_end {
							checker.checker_idx = checker_end - 1
							return checker.error('EOF error: key colon not found')
						}
					}
					`[`, `{`, `0`...`9`, `-`, `n`, `t`, `f` {
						// skip
					}
					`}` {
						return
					}
					`]` {
						return checker.error('Expecting key. Found closing bracket')
					}
					`,` {
						return checker.error('invalid object key')
					}
					`:` {
						return checker.error('empty object key')
					}
					else {
						return checker.error('`${[val[checker.checker_idx]].bytestr()}` is an invalid object key')
					}
				}

				if val[checker.checker_idx] != `:` {
					return checker.error('Expecting `:` after object key')
				}
				// skip `:`
				checker.checker_idx++

				// skip whitespace
				for checker.checker_idx < checker_end
					&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
					checker.checker_idx++
				}
				if checker.checker_idx >= checker_end {
					checker.checker_idx = checker_end - 1
					return checker.error('EOF error: object value not found')
				}

				match val[checker.checker_idx] {
					`"`, `[`, `{`, `0`...`9`, `-`, `n`, `t`, `f` {
						for val[checker.checker_idx] != `}` {
							if checker.checker_idx >= checker_end - 1 {
								return checker.error('EOF error: object value not closed')
							}
							checker.check_json_format(val)!
							// whitespace
							for checker.checker_idx < checker_end
								&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
								checker.checker_idx++
							}
							if checker.checker_idx >= checker_end {
								checker.checker_idx = checker_end - 1
								return checker.error('EOF error: braces are not closed')
							}
							if val[checker.checker_idx] == `}` {
								break
							}
							if checker.checker_idx >= checker_end - 1 {
								return checker.error('EOF error: braces are not closed')
							}

							if val[checker.checker_idx] == `,` {
								checker.checker_idx++
								for checker.checker_idx < checker_end
									&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
									checker.checker_idx++
								}
								if checker.checker_idx >= checker_end {
									checker.checker_idx = checker_end - 1
									return checker.error('EOF error: object key not found')
								}
								if val[checker.checker_idx] != `"` {
									return checker.error('Expecting object key')
								} else {
									break
								}
							} else {
								if val[checker.checker_idx] == `}` {
									break
								} else {
									return checker.error('invalid delimiter after object value')
								}
							}
						}
					}
					else {
						return checker.error('invalid object value')
					}
				}
			}
		}
		.array {
			// check if the JSON string is an empty array
			if checker_end >= checker.checker_idx + 2 {
				checker.checker_idx++
			} else {
				return checker.error('EOF error: There are not enough length for an array')
			}

			for {
				// skip whitespace
				for checker.checker_idx < checker_end
					&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
					checker.checker_idx++
				}
				if checker.checker_idx >= checker_end {
					checker.checker_idx = checker_end - 1
					return checker.error('EOF error: array not closed')
				}

				if val[checker.checker_idx] == `]` {
					break
				}

				if checker.checker_idx >= checker_end - 1 {
					return checker.error('EOF error: array not closed')
				}

				checker.check_json_format(val)!

				// whitespace
				for checker.checker_idx < checker_end
					&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
					checker.checker_idx++
				}
				if checker.checker_idx >= checker_end {
					checker.checker_idx = checker_end - 1
					return checker.error('EOF error: array not closed')
				}
				if val[checker.checker_idx] == `]` {
					break
				}
				if checker.checker_idx >= checker_end - 1 {
					return checker.error('EOF error: braces are not closed')
				}

				if val[checker.checker_idx] == `,` {
					checker.checker_idx++
					for checker.checker_idx < checker_end
						&& val[checker.checker_idx] in [` `, `\t`, `\n`, `\r`] {
						checker.checker_idx++
					}
					if checker.checker_idx >= checker_end {
						checker.checker_idx = checker_end - 1
						return checker.error('EOF error: array value not found')
					}
					if val[checker.checker_idx] == `]` {
						return checker.error('Cannot use `,`, before `]`')
					}
					continue
				} else {
					if val[checker.checker_idx] == `]` {
						break
					} else {
						return checker.error('`]` after value')
					}
				}
			}
		}
		.string_ {
			// check if the JSON string is a valid string

			if checker.checker_idx >= checker_end - 1 {
				return checker.error('EOF error: string not closed')
			}

			checker.checker_idx++

			// check if the JSON string is a valid escape sequence
			for {
				if checker.checker_idx >= checker_end {
					return checker.error('EOF error: string not closed')
				}
				if val[checker.checker_idx] == `"` {
					break
				}
				if val[checker.checker_idx] < 0x20 {
					return checker.error('unescaped control character in string')
				}
				if val[checker.checker_idx] == `\\` {
					if checker.checker_idx + 1 >= checker_end {
						return checker.error('invalid escape sequence')
					}
					escaped_char := val[checker.checker_idx + 1]
					match escaped_char {
						`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {
							checker.checker_idx += 2
							continue
						}
						`u` {
							// check if the JSON string is a valid unicode escape sequence
							escape_end := checker.checker_idx + 6
							if escape_end > checker_end {
								return checker.error('short unicode escape sequence')
							}
							for escape_idx in checker.checker_idx + 2 .. escape_end {
								match val[escape_idx] {
									`0`...`9`, `a`...`f`, `A`...`F` {}
									else {
										return checker.error('invalid unicode escape sequence')
									}
								}
							}
							checker.checker_idx = escape_end
							continue
						}
						else {
							return checker.error('unknown escape sequence')
						}
					}
				}
				checker.checker_idx++
			}
		}
		.number {
			// Validate the JSON number grammar:
			// -?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?
			mut idx := checker.checker_idx
			mut digits_count := 0
			if val[idx] == `-` {
				idx++
				if idx >= checker_end {
					return checker.error('invalid number')
				}
			}
			if val[idx] == `0` {
				idx++
				digits_count++
			} else if val[idx] >= `1` && val[idx] <= `9` {
				for idx < checker_end && val[idx] >= `0` && val[idx] <= `9` {
					idx++
					digits_count++
					if digits_count > 64 {
						return checker.error('number exceeds 64 digits')
					}
				}
			} else {
				return checker.error('invalid number')
			}
			if idx < checker_end && val[idx] == `.` {
				idx++
				if idx >= checker_end || val[idx] < `0` || val[idx] > `9` {
					return checker.error('invalid number')
				}
				for idx < checker_end && val[idx] >= `0` && val[idx] <= `9` {
					idx++
					digits_count++
					if digits_count > 64 {
						return checker.error('number exceeds 64 digits')
					}
				}
			}
			if idx < checker_end && val[idx] in [`e`, `E`] {
				idx++
				if idx < checker_end && val[idx] in [`+`, `-`] {
					idx++
				}
				if idx >= checker_end || val[idx] < `0` || val[idx] > `9` {
					return checker.error('invalid number')
				}
				for idx < checker_end && val[idx] >= `0` && val[idx] <= `9` {
					idx++
					digits_count++
					if digits_count > 64 {
						return checker.error('number exceeds 64 digits')
					}
				}
			}
			if idx < checker_end && val[idx] !in [`,`, `}`, `]`, ` `, `\t`, `\n`, `\r`] {
				checker.checker_idx = idx
				return checker.error('invalid number')
			}
			checker.checker_idx = idx - 1
		}
		.boolean {
			// check if the JSON string is a valid boolean
			match val[checker.checker_idx] {
				`t` {
					if checker_end - checker.checker_idx <= 3 {
						return checker.error('EOF error: expecting `true`')
					}

					is_not_ok := unsafe {
						vmemcmp(checker.json.str + checker.checker_idx, c'true', 4)
					}

					if is_not_ok != 0 {
						return checker.error('invalid boolean value. Got `${checker.json[checker.checker_idx..
							checker.checker_idx + 4]}` instead of `true`')
					}
					checker.checker_idx += 3
				}
				`f` {
					if checker_end - checker.checker_idx <= 4 {
						return checker.error('EOF error: expecting `false`')
					}

					is_not_ok := unsafe {
						vmemcmp(checker.json.str + checker.checker_idx, c'false', 5)
					}

					if is_not_ok != 0 {
						return checker.error('invalid boolean value. Got `${checker.json[checker.checker_idx..
							checker.checker_idx + 5]}` instead of `false`')
					}

					checker.checker_idx += 4
				}
				else {
					return checker.error('invalid boolean')
				}
			}
		}
	}

	actual_value_info_pointer.length = checker.checker_idx + 1 - start_idx_position

	if checker.checker_idx >= checker_end - 1 {
		return
	}
	checker.checker_idx++

	for checker.checker_idx < checker_end
		&& (is_root_value || val[checker.checker_idx] !in [`,`, `:`, `}`, `]`]) {
		// get trash characters after the value
		if val[checker.checker_idx] !in [` `, `\t`, `\n`, `\r`] {
			checker.error('invalid value. Unexpected character after ${value_kind} end')!
		}
		checker.checker_idx++
	}
	if checker.checker_idx >= checker_end {
		checker.checker_idx = checker_end - 1
	}
}

// decode decodes a JSON string into a specified type.
pub fn decode[T](val string) !T {
	mut decoder := Decoder{
		json: val
	}

	decoder.check_json_format(val)!
	check_if_json_match[T](val)!

	mut result := T{}
	decoder.current_node = decoder.values_info.head
	decoder.decode_value(mut result)!
	return result
}

// decode_value decodes a value from the JSON nodes.
fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	if decoder.current_node == unsafe { nil } {
		return error('unexpected end of JSON values')
	}
	check_value_kind_match[T](decoder.current_node.value.value_kind)!

	$if T.unaliased_typ is string {
		string_info := decoder.current_node.value

		if string_info.value_kind == .string_ {
			val = decoder.decode_string(string_info)!
		}
	} $else $if T.unaliased_typ is $sumtype {
		decoder.decode_sumtype(mut val)!
	} $else $if T.unaliased_typ is time.Time {
		time_info := decoder.current_node.value

		if time_info.value_kind == .string_ {
			string_time := decoder.decode_string(time_info)!
			val = time.parse_rfc3339(string_time)!
		}
	} $else $if T.unaliased_typ is $map {
		decoder.decode_map(mut val)!
		return
	} $else $if T.unaliased_typ is $array {
		decoder.decode_array(mut val)!
		// return to avoid the next increment of the current node
		// this is because the current node is already incremented in the decode_array function
		// remove this line will cause the current node to be incremented twice
		// and bug recursive array decoding like `[][]int{}`
		return
	} $else $if T is NullDecoder {
		val.from_json_null()
	} $else $if T.unaliased_typ is $struct {
		struct_info := decoder.current_node.value

		if struct_info.value_kind == .object {
			struct_position := struct_info.position
			struct_end := struct_position + struct_info.length
			mut seen_required := []string{}

			decoder.current_node = decoder.current_node.next
			for {
				if decoder.current_node == unsafe { nil } {
					break
				}

				key_info := decoder.current_node.value

				if key_info.position >= struct_end {
					break
				}
				key := decoder.decode_string(key_info)!

				decoder.current_node = decoder.current_node.next
				mut field_matched := false

				$for field in T.fields {
					mut json_field_name := field.name
					mut is_json_skip := field.attrs.contains('skip')
					is_required := field.attrs.contains('required')
					is_raw := field.attrs.contains('raw')
					for attr in field.attrs {
						if json_name := json_attr_value(attr) {
							if json_name == '-' {
								is_json_skip = true
							} else if json_name != '' {
								json_field_name = json_name
							}
							break
						}
					}
					if (!is_json_skip || is_required) && key == json_field_name {
						field_matched = true
						if is_required {
							seen_required << field.name
						}
						if is_json_skip {
							decoder.skip_value()
						} else if is_raw {
							value_info := decoder.current_node.value
							raw_value := decoder.json[value_info.position..value_info.position +
								value_info.length]
							$if field.typ is ?string {
								val.$(field.name) = raw_value
							} $else $if field.typ is string {
								val.$(field.name) = raw_value
							} $else {
								return error('`raw` attribute can only be used with string fields')
							}
							decoder.skip_value()
						} else {
							$if field.typ is $option {
								if decoder.current_node.value.value_kind == .null {
									val.$(field.name) = none
									decoder.skip_value()
								} else {
									mut unwrapped_val :=
										create_value_from_optional(val.$(field.name))
									decoder.decode_value(mut unwrapped_val)!
									val.$(field.name) = unwrapped_val
								}
							} $else {
								decoder.decode_value(mut val.$(field.name))!
							}
						}
					}
				}
				if !field_matched {
					decoder.skip_value()
				}
			}

			$for field in T.fields {
				if field.attrs.contains('required') && field.name !in seen_required {
					return error('missing required field `${field.name}`')
				}
			}
		}
		return
	} $else $if T.unaliased_typ is bool {
		value_info := decoder.current_node.value

		unsafe {
			val = vmemcmp(decoder.json.str + value_info.position, c'true', 4) == 0
		}
	} $else $if T.unaliased_typ is $enum {
		enum_info := decoder.current_node.value
		if enum_info.value_kind == .string_ {
			decoded_value := decoder.decode_string(enum_info)!
			mut matched := false
			$for value in T.values {
				if !matched {
					for attr in value.attrs {
						if json_name := json_attr_value(attr) {
							if decoded_value == json_name {
								val = value.value
								matched = true
								break
							}
						}
					}
					if !matched && decoded_value == value.name {
						val = value.value
						matched = true
					}
				}
			}
			if !matched {
				return error('string value `${decoded_value}` does not match any field in enum ${typeof(val).name}')
			}
		} else {
			bytes := unsafe { (decoder.json.str + enum_info.position).vbytes(enum_info.length) }
			integer_string := integer_number_string(bytes)!
			decoded_value := parse_integer_value[int](integer_string)!
			mut matched := false
			$for value in T.values {
				if !matched && decoded_value == int(value.value) {
					val = value.value
					matched = true
				}
			}
			if !matched {
				return error('number value `${decoded_value}` does not match any field in enum ${typeof(val).name}')
			}
		}
	} $else $if T.unaliased_typ in [$float, $int] {
		value_info := decoder.current_node.value

		if value_info.value_kind == .number {
			bytes := unsafe { (decoder.json.str + value_info.position).vbytes(value_info.length) }
			$if T.unaliased_typ is $float {
				val = T(strconv.atof64(bytes.bytestr())!)
			} $else $if T.unaliased_typ is $int {
				integer_string := integer_number_string(bytes)!
				val = parse_integer_value[T](integer_string)!
			} $else {
				unsafe { string_buffer_to_generic_number(val, bytes) }
			}
		}
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}

	if decoder.current_node != unsafe { nil } {
		decoder.current_node = decoder.current_node.next
	}
}

fn (decoder &Decoder) decode_string(value_info ValueInfo) !string {
	mut result := []u8{cap: value_info.length - 2}
	mut idx := value_info.position + 1
	end := value_info.position + value_info.length - 1
	for idx < end {
		current_byte := decoder.json[idx]
		if current_byte != `\\` {
			result << current_byte
			idx++
			continue
		}
		if idx + 1 >= end {
			return error('Invalid escape sequence at the end of string')
		}
		escaped_char := decoder.json[idx + 1]
		match escaped_char {
			`"`, `\\`, `/` {
				result << escaped_char
			}
			`b` {
				result << u8(8)
			}
			`f` {
				result << u8(12)
			}
			`n` {
				result << `\n`
			}
			`r` {
				result << `\r`
			}
			`t` {
				result << `\t`
			}
			`u` {
				if idx + 5 >= end {
					return error('Invalid unicode escape sequence')
				}
				unicode_value :=
					parse_unicode_escape_sequence(decoder.json[idx + 2..idx + 6].bytes())!
				if unicode_value >= 0xD800 && unicode_value <= 0xDBFF {
					if idx + 11 >= end || decoder.json[idx + 6] != `\\`
						|| decoder.json[idx + 7] != `u` {
						return error('High unicode surrogate must be followed by a low surrogate')
					}
					low_surrogate :=
						parse_unicode_escape_sequence(decoder.json[idx + 8..idx + 12].bytes())!
					if low_surrogate < 0xDC00 || low_surrogate > 0xDFFF {
						return error('Invalid low unicode surrogate')
					}
					codepoint := u32(0x10000) + ((unicode_value - 0xD800) << 10) +
						(low_surrogate - 0xDC00)
					result << encode_unicode_codepoint(codepoint)!
					idx += 12
					continue
				}
				if unicode_value >= 0xDC00 && unicode_value <= 0xDFFF {
					return error('Low unicode surrogate must follow a high surrogate')
				}
				result << encode_unicode_codepoint(unicode_value)!
				idx += 6
				continue
			}
			else {
				return error('Unknown escape sequence')
			}
		}

		idx += 2
	}
	return result.bytestr()
}

fn (mut decoder Decoder) skip_value() {
	if decoder.current_node == unsafe { nil } {
		return
	}
	value_end := decoder.current_node.value.position + decoder.current_node.value.length
	decoder.current_node = decoder.current_node.next
	for decoder.current_node != unsafe { nil } && decoder.current_node.value.position < value_end {
		decoder.current_node = decoder.current_node.next
	}
}

fn (mut decoder Decoder) decode_array[T](mut val []T) ! {
	array_info := decoder.current_node.value

	if array_info.value_kind == .array {
		val.clear()
		decoder.current_node = decoder.current_node.next

		array_position := array_info.position
		array_end := array_position + array_info.length

		for {
			if decoder.current_node == unsafe { nil }
				|| decoder.current_node.value.position >= array_end {
				break
			}

			mut array_element := T{}

			decoder.decode_value(mut array_element)!

			val << array_element
		}
	}
}

fn (mut decoder Decoder) decode_map[V](mut val map[string]V) ! {
	map_info := decoder.current_node.value

	if map_info.value_kind == .object {
		val.clear()
		map_position := map_info.position
		map_end := map_position + map_info.length

		decoder.current_node = decoder.current_node.next
		for {
			if decoder.current_node == unsafe { nil }
				|| decoder.current_node.value.position >= map_end {
				break
			}

			key_info := decoder.current_node.value

			if key_info.position >= map_end {
				break
			}

			key := decoder.decode_string(key_info)!

			decoder.current_node = decoder.current_node.next

			value_info := decoder.current_node.value

			if value_info.position + value_info.length >= map_end {
				break
			}

			mut map_value := V{}

			decoder.decode_value(mut map_value)!

			val[key] = map_value
		}
	}
}

// get_value_kind returns the kind of a JSON value.
fn get_value_kind(value u8) ValueKind {
	if value == u8(`"`) {
		return .string_
	} else if value == u8(`t`) || value == u8(`f`) {
		return .boolean
	} else if value == u8(`{`) {
		return .object
	} else if value == u8(`[`) {
		return .array
	} else if (value >= u8(48) && value <= u8(57)) || value == u8(`-`) {
		return .number
	} else if value == u8(`n`) {
		return .null
	}
	return .unknown
}

fn create_value_from_optional[T](val ?T) T {
	return T{}
}

fn json_attr_value(attr string) ?string {
	if !attr.starts_with('json:') {
		return none
	}
	mut value := attr[5..].trim_space()
	if value.len > 1 && ((value[0] == `'` && value[value.len - 1] == `'`)
		|| (value[0] == `"` && value[value.len - 1] == `"`)) {
		value = value[1..value.len - 1]
	}
	return value
}

const max_integer_number_digits = 20

fn parse_integer_value[T](value string) !T {
	$if T.unaliased_typ is i8 {
		return T(strconv.atoi8(value)!)
	} $else $if T.unaliased_typ is i16 {
		return T(strconv.atoi16(value)!)
	} $else $if T.unaliased_typ is i32 {
		return T(strconv.atoi32(value)!)
	} $else $if T.unaliased_typ is i64 {
		return T(strconv.atoi64(value)!)
	} $else $if T.unaliased_typ is u8 {
		return T(strconv.atou8(value)!)
	} $else $if T.unaliased_typ is u16 {
		return T(strconv.atou16(value)!)
	} $else $if T.unaliased_typ is u32 {
		return T(strconv.atou32(value)!)
	} $else $if T.unaliased_typ is u64 {
		return T(strconv.atou64(value)!)
	} $else $if T.unaliased_typ is int {
		if sizeof(int) == 4 {
			return T(strconv.atoi(value)!)
		}
		return T(strconv.atoi64(value)!)
	} $else $if T.unaliased_typ is isize {
		if sizeof(isize) == 4 {
			return T(strconv.atoi32(value)!)
		}
		return T(strconv.atoi64(value)!)
	} $else $if T.unaliased_typ is usize {
		if sizeof(usize) == 4 {
			return T(strconv.atou32(value)!)
		}
		return T(strconv.atou64(value)!)
	} $else {
		return error('cannot decode integer type ${typeof(T{}).name}')
	}
}

fn integer_number_string(data []u8) !string {
	mut idx := 0
	mut is_negative := false
	if data[idx] == `-` {
		is_negative = true
		idx++
	}

	mut digits := []u8{cap: data.len}
	mut fractional_digits := 0
	mut seen_decimal := false
	for idx < data.len && data[idx] !in [`e`, `E`] {
		if data[idx] == `.` {
			seen_decimal = true
		} else {
			digits << data[idx]
			if seen_decimal {
				fractional_digits++
			}
		}
		idx++
	}

	mut exponent := 0
	if idx < data.len {
		idx++
		mut exponent_is_negative := false
		if data[idx] == `+` || data[idx] == `-` {
			exponent_is_negative = data[idx] == `-`
			idx++
		}
		exponent_cap := max_integer_number_digits + fractional_digits + 1
		for idx < data.len {
			if exponent < exponent_cap {
				exponent = exponent * 10 + int(data[idx] - `0`)
			}
			idx++
		}
		if exponent_is_negative {
			exponent = -exponent
		}
	}

	mut first_nonzero := 0
	for first_nonzero < digits.len && digits[first_nonzero] == `0` {
		first_nonzero++
	}
	if first_nonzero == digits.len {
		return '0'
	}
	digits = digits[first_nonzero..].clone()

	scale := exponent - fractional_digits
	if scale < 0 {
		removed_digits := -scale
		if removed_digits >= digits.len {
			return error('cannot decode fractional number `${data.bytestr()}` into an integer')
		}
		for digit in digits[digits.len - removed_digits..] {
			if digit != `0` {
				return error('cannot decode fractional number `${data.bytestr()}` into an integer')
			}
		}
		digits = digits[..digits.len - removed_digits].clone()
	}

	appended_zeros := if scale > 0 { scale } else { 0 }
	if digits.len + appended_zeros > max_integer_number_digits {
		return error('number `${data.bytestr()}` exceeds 64-bit integer range')
	}
	mut result := []u8{cap: digits.len + appended_zeros + if is_negative { 1 } else { 0 }}
	if is_negative {
		result << `-`
	}
	result << digits
	for _ in 0 .. appended_zeros {
		result << `0`
	}
	return result.bytestr()
}

fn utf8_byte_length(unicode_value u32) int {
	if unicode_value <= 0x7F {
		return 1
	} else if unicode_value <= 0x7FF {
		return 2
	} else if unicode_value <= 0xFFFF {
		return 3
	} else {
		return 4
	}
}

fn (mut decoder Decoder) calculate_string_space_and_escapes() !(int, []int) {
	value_info := decoder.current_node.value
	len := value_info.length

	if len < 2 || decoder.json[value_info.position] != `"`
		|| decoder.json[value_info.position + len - 1] != `"` {
		return error('Invalid JSON string format')
	}

	mut space_required := 0
	mut escape_positions := []int{}
	mut idx := 1 // Start after the opening quote

	for idx < len - 1 {
		current_byte := decoder.json[value_info.position + idx]

		if current_byte == `\\` {
			// Escape sequence, handle accordingly
			idx++
			if idx >= len - 1 {
				return error('Invalid escape sequence at the end of string')
			}
			escaped_char := decoder.json[value_info.position + idx]
			match escaped_char {
				// All simple escapes take 1 byte of space
				`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {
					space_required++
				}
				`u` {
					// Unicode escape sequence \uXXXX
					if idx + 4 >= len - 1 {
						return error('Invalid unicode escape sequence')
					}
					// Extract the hex value from the \uXXXX sequence
					hex_str := decoder.json[value_info.position + idx + 1..value_info.position +
						idx + 5]
					unicode_value := u32(strconv.parse_int(hex_str, 16, 32)!)
					// Determine the number of bytes needed for this Unicode character in UTF-8
					space_required += utf8_byte_length(unicode_value)
					idx += 4 // Skip the next 4 hex digits

					// REVIEW: If the Unicode character is a surrogate pair, we need to skip the next \uXXXX sequence?

					// \\uXXXX is 6 bytes, so we need to skip 5 more bytes
					escape_positions << value_info.position + idx - 5
				}
				else {
					return error('Unknown escape sequence')
				}
			}
		} else {
			// Regular character, just increment space required by 1 byte
			space_required++
		}
		idx++
	}

	return space_required, escape_positions
}

// parse_unicode_escape_sequence parses the four hexadecimal digits in a JSON Unicode escape.
fn parse_unicode_escape_sequence(escape_sequence_byte []u8) !u32 {
	if escape_sequence_byte.len != 4 {
		return error('Invalid unicode escape sequence')
	}
	return u32(strconv.parse_int(escape_sequence_byte.bytestr(), 16, 32)!)
}

// encode_unicode_codepoint encodes a Unicode code point as UTF-8.
fn encode_unicode_codepoint(unicode_value u32) ![]u8 {
	if unicode_value > 0x10FFFF || (unicode_value >= 0xD800 && unicode_value <= 0xDFFF) {
		return error('Invalid unicode code point')
	}
	mut utf8_bytes := []u8{cap: utf8_byte_length(unicode_value)}

	if unicode_value <= 0x7F {
		utf8_bytes << u8(unicode_value)
	} else if unicode_value <= 0x7FF {
		utf8_bytes << u8(0xC0 | (unicode_value >> 6))
		utf8_bytes << u8(0x80 | (unicode_value & 0x3F))
	} else if unicode_value <= 0xFFFF {
		utf8_bytes << u8(0xE0 | (unicode_value >> 12))
		utf8_bytes << u8(0x80 | ((unicode_value >> 6) & 0x3F))
		utf8_bytes << u8(0x80 | (unicode_value & 0x3F))
	} else {
		utf8_bytes << u8(0xF0 | (unicode_value >> 18))
		utf8_bytes << u8(0x80 | ((unicode_value >> 12) & 0x3F))
		utf8_bytes << u8(0x80 | ((unicode_value >> 6) & 0x3F))
		utf8_bytes << u8(0x80 | (unicode_value & 0x3F))
	}

	return utf8_bytes
}

// string_buffer_to_generic_number converts a buffer of bytes (data) into a generic type T and
// stores the result in the provided result pointer.
// The function supports conversion to the following types:
// - Signed integers: i8, i16, i32, i64
// - Unsigned integers: u8, u16, u32, u64
// - Floating-point numbers: f32, f64
//
// For signed integers, the function handles negative numbers by checking for a '-' character.
// For floating-point numbers, the function handles decimal points and adjusts the result
// accordingly.
//
// If the type T is not supported, the function will panic with an appropriate error message.
//
// Parameters:
// - data []u8: The buffer of bytes to be converted.
// - result &T: A pointer to the variable where the converted result will be stored.
//
// NOTE: This aims works with not new memory allocated data, to more efficient use `vbytes` before
@[direct_array_access; unsafe]
pub fn string_buffer_to_generic_number[T](result &T, data []u8) {
	$if T.unaliased_typ is $int {
		mut is_negative := false
		for ch in data {
			if ch == `-` {
				is_negative = true
				continue
			}
			digit := T(ch - `0`)
			*result = T(*result * 10 + digit)
		}
		if is_negative {
			*result *= -1
		}
	} $else $if T.unaliased_typ is $float {
		mut is_negative := false
		mut decimal_seen := false
		mut decimal_divider := T(1)

		for ch in data {
			if ch == `-` {
				is_negative = true
				continue
			}
			if ch == `.` {
				decimal_seen = true
				continue
			}

			digit := T(ch - u8(`0`))

			if decimal_seen {
				decimal_divider *= 10
				*result += T(digit / decimal_divider)
			} else {
				*result = T(*result * 10 + digit)
			}
		}
		if is_negative {
			*result *= -1
		}
	} $else $if T.unaliased_typ is $enum {
		// Convert the string to an integer
		mut enumeration := int(0)
		for ch in data {
			digit := int(ch - `0`)
			enumeration = enumeration * 10 + digit
		}
		*result = unsafe { T(enumeration) }
	} $else {
		panic('unsupported type ${typeof[T]().name}')
	}
}
