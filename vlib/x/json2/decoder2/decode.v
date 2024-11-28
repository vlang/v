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

// check_if_json_match checks if the JSON string matches the expected type T.
fn check_if_json_match[T](val string) ! {
	// check if the JSON string is empty
	if val == '' {
		return error('empty string')
	}

	// check if generic type matches the JSON type
	value_kind := get_value_kind(val[0])

	$if T is $option {
		// TODO
	} $else $if T is $sumtype {
		// TODO
	} $else $if T is $alias {
		// TODO
	} $else $if T is $string {
		if value_kind != .string_ {
			return error('Expected string, but got ${value_kind}')
		}
	} $else $if T is time.Time {
		if value_kind != .string_ {
			return error('Expected string, but got ${value_kind}')
		}
	} $else $if T is $map {
		if value_kind != .object {
			return error('Expected object, but got ${value_kind}')
		}
	} $else $if T is $array {
		if value_kind != .array {
			return error('Expected array, but got ${value_kind}')
		}
	} $else $if T is $struct {
		if value_kind != .object {
			return error('Expected object, but got ${value_kind}')
		}
	} $else $if T in [$enum, $int, $float] {
		if value_kind != .number {
			return error('Expected number, but got ${value_kind}')
		}
	} $else $if T is bool {
		if value_kind != .boolean {
			return error('Expected boolean, but got ${value_kind}')
		}
	} $else {
		return error('cannot encode value with ${value_kind} type')
	}
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
	// check if the JSON string is empty
	if val == '' {
		return checker.error('empty string')
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
				vmemcmp(checker.json.str + checker.checker_idx, 'null'.str, 4)
			}

			if is_not_ok != 0 {
				return checker.error('invalid null value. Got `${checker.json[checker.checker_idx..
					checker.checker_idx + 4]}` instead of `null`')
			}
			checker.checker_idx += 3
		}
		.object {
			checker.checker_idx++
			for val[checker.checker_idx] != `}` {
				// check if the JSON string is an empty object
				if checker_end - checker.checker_idx <= 2 {
					continue
				}

				if val[checker.checker_idx] != `"` {
					checker.checker_idx++
				}

				// skip whitespace
				for val[checker.checker_idx] in [` `, `\t`, `\n`] {
					if checker.checker_idx >= checker_end - 1 {
						break
					}
					checker.checker_idx++
				}

				if val[checker.checker_idx] == `}` {
					continue
				}

				match val[checker.checker_idx] {
					`"` {
						// Object key
						checker.check_json_format(val)!

						for val[checker.checker_idx] != `:` {
							if checker.checker_idx >= checker_end - 1 {
								return checker.error('EOF error: key colon not found')
							}
							if val[checker.checker_idx] !in [` `, `\t`, `\n`] {
								return checker.error('invalid value after object key')
							}
							checker.checker_idx++
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
				for val[checker.checker_idx] in [` `, `\t`, `\n`] {
					checker.checker_idx++
				}

				match val[checker.checker_idx] {
					`"`, `[`, `{`, `0`...`9`, `-`, `n`, `t`, `f` {
						for val[checker.checker_idx] != `}` {
							if checker.checker_idx >= checker_end - 1 {
								return checker.error('EOF error: object value not closed')
							}
							checker.check_json_format(val)!
							// whitespace
							for val[checker.checker_idx] in [` `, `\t`, `\n`] {
								checker.checker_idx++
							}
							if val[checker.checker_idx] == `}` {
								break
							}
							if checker.checker_idx >= checker_end - 1 {
								return checker.error('EOF error: braces are not closed')
							}

							if val[checker.checker_idx] == `,` {
								checker.checker_idx++
								for val[checker.checker_idx] in [` `, `\t`, `\n`] {
									checker.checker_idx++
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
									return
								}
							}
						}
					}
					else {
						return checker.error('invalid object value')
					}
				}
			}
			if checker.checker_idx < checker_end - 1 {
				checker.checker_idx++
			}
		}
		.array {
			// check if the JSON string is an empty array
			if checker_end >= checker.checker_idx + 2 {
				checker.checker_idx++
				if val[checker.checker_idx] == `]` {
					return
				}
			} else {
				return checker.error('EOF error: There are not enough length for an array')
			}

			for val[checker.checker_idx] != `]` {
				// skip whitespace
				for val[checker.checker_idx] in [` `, `\t`, `\n`] {
					if checker.checker_idx >= checker_end - 1 {
						break
					}
					checker.checker_idx++
				}

				if val[checker.checker_idx] == `]` {
					return
				}

				if checker.checker_idx >= checker_end - 1 {
					return checker.error('EOF error: array not closed')
				}

				checker.check_json_format(val)!

				// whitespace
				for val[checker.checker_idx] in [` `, `\t`, `\n`] {
					checker.checker_idx++
				}
				if val[checker.checker_idx] == `]` {
					break
				}
				if checker.checker_idx >= checker_end - 1 {
					return checker.error('EOF error: braces are not closed')
				}

				if val[checker.checker_idx] == `,` {
					checker.checker_idx++
					for val[checker.checker_idx] in [` `, `\t`, `\n`] {
						checker.checker_idx++
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
			for val[checker.checker_idx] != `"` && val[checker.checker_idx - 1] != `\\` {
				if val[checker.checker_idx] == `\\` {
					if checker.checker_idx + 1 >= checker_end - 1 {
						return checker.error('invalid escape sequence')
					}
					escaped_char := val[checker.checker_idx + 1]
					match escaped_char {
						`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {}
						`u` {
							// check if the JSON string is a valid unicode escape sequence
							escaped_char_last_index := checker.checker_idx + 5

							if escaped_char_last_index < checker_end - 1 {
								// 2 bytes for the unicode escape sequence `\u`
								checker.checker_idx += 2

								for checker.checker_idx < escaped_char_last_index {
									match val[checker.checker_idx] {
										`0`...`9`, `a`...`f`, `A`...`F` {
											checker.checker_idx++
										}
										else {
											return checker.error('invalid unicode escape sequence')
										}
									}
								}
								// REVIEW: Should we increment the index here?
								continue
							} else {
								return checker.error('short unicode escape sequence ${checker.json[checker.checker_idx..
									escaped_char_last_index + 1]}')
							}
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
			// check if the JSON string is a valid float or integer
			mut is_negative := val[0] == `-`
			mut has_dot := false

			mut digits_count := 1

			if is_negative {
				checker.checker_idx++
			}

			for checker.checker_idx < checker_end - 1
				&& val[checker.checker_idx + 1] !in [`,`, `}`, `]`, ` `, `\t`, `\n`]
				&& checker.checker_idx < checker_end - 1 {
				if val[checker.checker_idx] == `.` {
					if has_dot {
						return checker.error('invalid float. Multiple dots')
					}
					has_dot = true
					checker.checker_idx++
					continue
				} else if val[checker.checker_idx] == `-` {
					if is_negative {
						return checker.error('invalid float. Multiple negative signs')
					}
					checker.checker_idx++
					continue
				} else {
					if val[checker.checker_idx] < `0` || val[checker.checker_idx] > `9` {
						return checker.error('invalid number')
					}
				}

				if digits_count >= 64 {
					return checker.error('number exceeds 64 digits')
				}
				digits_count++
				checker.checker_idx++
			}
		}
		.boolean {
			// check if the JSON string is a valid boolean
			match val[checker.checker_idx] {
				`t` {
					if checker_end - checker.checker_idx <= 3 {
						return checker.error('EOF error: expecting `true`')
					}

					is_not_ok := unsafe {
						vmemcmp(checker.json.str + checker.checker_idx, 'true'.str, 4)
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
						vmemcmp(checker.json.str + checker.checker_idx, 'false'.str, 5)
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

	if checker.checker_idx < checker_end - 1 {
		checker.checker_idx++
	}

	for checker.checker_idx < checker_end - 1 && val[checker.checker_idx] !in [`,`, `:`, `}`, `]`] {
		// get trash characters after the value
		if val[checker.checker_idx] !in [` `, `\t`, `\n`] {
			checker.error('invalid value. Unexpected character after ${value_kind} end')!
		} else {
			// whitespace
		}
		checker.checker_idx++
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
	decoder.decode_value(mut &result)!
	return result
}

// decode_value decodes a value from the JSON nodes.
fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T is $option {
		mut unwrapped_val := create_value_from_optional(val.$(field.name))
		decoder.decode_value(mut unwrapped_val)!
		val.$(field.name) = unwrapped_val
	} $else $if T.unaliased_typ is string {
		string_info := decoder.current_node.value

		if string_info.value_kind == .string_ {
			buffer_lenght, escape_positions := decoder.calculate_string_space_and_escapes()!

			string_buffer := []u8{cap: buffer_lenght}

			if escape_positions.len == 0 {
				if string_info.length != 0 {
					unsafe {
						string_buffer.push_many(decoder.json.str + string_info.position + 1,
							buffer_lenght)
					}
				}
			} else {
				for i := 0; i < escape_positions.len; i++ {
					escape_position := escape_positions[i]
					if i == 0 {
						// Pushes a substring from the JSON string into the string buffer.
						// The substring starts at the position of the value in the JSON string plus one,
						// and ends at the escape position minus one.
						// This is used to handle escaped characters within the JSON string.
						unsafe {
							string_buffer.push_many(decoder.json.str + string_info.position + 1,
								escape_position - string_info.position - 1)
						}
					} else {
						// Pushes a substring from the JSON string into the string buffer, starting after the previous escape position
						// and ending just before the current escape position. This handles the characters between escape sequences.
						unsafe {
							string_buffer.push_many(decoder.json.str + escape_positions[i - 1] + 6,
								escape_position - escape_positions[i - 1] - 6)
						}
					}

					unescaped_buffer := generate_unicode_escape_sequence(unsafe {
						(decoder.json.str + escape_positions[i] + 2).vbytes(4)
					})!

					unsafe { string_buffer.push_many(&unescaped_buffer[0], unescaped_buffer.len) }
				}
				end_of_last_escape_position := escape_positions[escape_positions.len - 1] + 6
				unsafe {
					string_buffer.push_many(decoder.json.str + end_of_last_escape_position,
						string_info.length - end_of_last_escape_position - 1)
				}
			}

			val = string_buffer.bytestr()
		}
	} $else $if T.unaliased_typ is $sumtype {
		decoder.decode_sumtype(mut val)!
	} $else $if T.unaliased_typ is time.Time {
		time_info := decoder.current_node.value

		if time_info.value_kind == .string_ {
			string_time := decoder.json.substr_unsafe(time_info.position + 1, time_info.position +
				time_info.length - 1)

			val = time.parse_rfc3339(string_time) or { time.Time{} }
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
	} $else $if T.unaliased_typ is $struct {
		struct_info := decoder.current_node.value

		if struct_info.value_kind == .object {
			struct_position := struct_info.position
			struct_end := struct_position + struct_info.length

			decoder.current_node = decoder.current_node.next
			for {
				if decoder.current_node == unsafe { nil } {
					break
				}

				key_info := decoder.current_node.value

				if key_info.position >= struct_end {
					break
				}

				decoder.current_node = decoder.current_node.next

				$for field in T.fields {
					if key_info.length - 2 == field.name.len {
						// This `vmemcmp` compares the name of a key in a JSON with a given struct field.
						if unsafe {
							vmemcmp(decoder.json.str + key_info.position + 1, field.name.str,
								field.name.len) == 0
						} {
							$if field.typ is $option {
								mut unwrapped_val := create_value_from_optional(val.$(field.name))
								decoder.decode_value(mut unwrapped_val)!
								val.$(field.name) = unwrapped_val
							} $else {
								decoder.decode_value(mut val.$(field.name))!
							}
						}
					}
				}
			}
		}
	} $else $if T.unaliased_typ is bool {
		value_info := decoder.current_node.value

		unsafe {
			val = vmemcmp(decoder.json.str + value_info.position, 'true'.str, 4) == 0
		}
	} $else $if T.unaliased_typ in [$float, $int, $enum] {
		value_info := decoder.current_node.value

		if value_info.value_kind == .number {
			bytes := unsafe { (decoder.json.str + value_info.position).vbytes(value_info.length) }

			unsafe {
				string_buffer_to_generic_number(val, bytes)
			}
		}
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}

	if decoder.current_node != unsafe { nil } {
		decoder.current_node = decoder.current_node.next
	}
}

fn (mut decoder Decoder) decode_array[T](mut val []T) ! {
	array_info := decoder.current_node.value

	if array_info.value_kind == .array {
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

fn (mut decoder Decoder) decode_map[K, V](mut val map[K]V) ! {
	map_info := decoder.current_node.value

	if map_info.value_kind == .object {
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

			key := decoder.json[key_info.position + 1..key_info.position + key_info.length - 1]

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

// \uXXXX to unicode with 4 hex digits
fn generate_unicode_escape_sequence(escape_sequence_byte []u8) ![]u8 {
	if escape_sequence_byte.len != 4 {
		return error('Invalid unicode escape sequence')
	}

	unicode_value := u32(strconv.parse_int(escape_sequence_byte.bytestr(), 16, 32)!)
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
		enumeration := 0
		for ch in data {
			digit := int(ch - `0`)
			enumeration = enumeration * 10 + digit
		}
		*result = T(enumeration)
	} $else {
		panic('unsupported type ${typeof[T]().name}')
	}
}
