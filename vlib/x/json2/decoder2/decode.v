module decoder2

import strconv
import time
import strings

const null_in_string = 'null'

const true_in_string = 'true'

const false_in_string = 'false'

const float_zero_in_string = '0.0'

const whitespace_chars = [` `, `\t`, `\n`, `\r`]!

// Node represents a node in a linked list to store ValueInfo.
struct Node[T] {
mut:
	value T
	next  &Node[T] = unsafe { nil } // next is the next node in the linked list.
}

// ValueInfo represents the position and length of a value, such as string, number, array, object key, and object value in a JSON string.
struct ValueInfo {
	position int // The position of the value in the JSON string.
pub:
	value_kind ValueKind // The kind of the value.
mut:
	length int // The length of the value in the JSON string.
}

struct StructFieldInfo {
	field_name_str voidptr
	field_name_len int
	json_name_ptr  voidptr
	json_name_len  int
	is_omitempty   bool
	is_skip        bool
	is_required    bool
	is_raw         bool
mut:
	decoded_with_value_info_node &Node[ValueInfo] = unsafe { nil }
}

// Decoder represents a JSON decoder.
struct Decoder {
	json string // json is the JSON data to be decoded.
mut:
	values_info  LinkedList[ValueInfo] // A linked list to store ValueInfo.
	checker_idx  int                   // checker_idx is the current index of the decoder.
	current_node &Node[ValueInfo] = unsafe { nil } // The current node in the linked list.
}

// new_decoder creates a new JSON decoder.
pub fn new_decoder[T](json string) !Decoder {
	mut decoder := Decoder{
		json: json
	}

	decoder.check_json_format(json)!
	check_if_json_match[T](json)!

	decoder.current_node = decoder.values_info.head

	return decoder
}

// LinkedList represents a linked list to store ValueInfo.
struct LinkedList[T] {
mut:
	head &Node[T] = unsafe { nil } // head is the first node in the linked list.
	tail &Node[T] = unsafe { nil } // tail is the last node in the linked list.
	len  int // len is the length of the linked list.
}

// push adds a new element to the linked list.
fn (mut list LinkedList[T]) push(value T) {
	new_node := &Node[T]{
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
fn (list &LinkedList[T]) last() &T {
	return &list.tail.value
}

// str returns a string representation of the linked list.
fn (list &LinkedList[ValueInfo]) str() string {
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

@[manualfree]
fn (list &LinkedList[T]) str() string {
	mut sb := strings.new_builder(128)
	defer {
		unsafe { sb.free() }
	}
	mut current := list.head
	for current != unsafe { nil } {
		value_as_string := current.value.str()
		sb.write_string(value_as_string)
		sb.write_u8(u8(` `))
		current = current.next
	}
	return sb.str()
}

@[unsafe]
fn (list &LinkedList[T]) free() {
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

const max_context_lenght = 50
const max_extra_charaters = 5
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

	cutoff := character_number > max_context_lenght

	// either start of string, last newline or a limited amount of characters
	context_start := if cutoff { position - max_context_lenght } else { last_newline }

	// print some extra characters
	mut context_end := int_min(checker.json.len, position + max_extra_charaters)
	context_end_newline := checker.json[position..context_end].index_u8(`\n`)

	if context_end_newline != -1 {
		context_end = position + context_end_newline
	}

	mut context := ''

	if cutoff {
		context += '...'
	}
	context += checker.json[context_start..position]
	context += '\e[31m${checker.json[position].ascii_str()}\e[0m'
	context += checker.json[position + 1..context_end]
	context += '\n'

	if cutoff {
		context += ' '.repeat(max_context_lenght + 3)
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
fn (mut decoder Decoder) decode_error(message string) ! {
	mut error_info := ValueInfo{}
	if decoder.current_node != unsafe { nil } {
		error_info = decoder.current_node.value
	} else {
		error_info = decoder.values_info.tail.value
	}

	start := error_info.position
	end := start + int_min(error_info.length, max_context_lenght)

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

	cutoff := character_number > max_context_lenght

	// either start of string, last newline or a limited amount of characters
	context_start := if cutoff { start - max_context_lenght } else { last_newline }

	// print some extra characters
	mut context_end := int_min(decoder.json.len, end + max_extra_charaters)
	context_end_newline := decoder.json[end..context_end].index_u8(`\n`)

	if context_end_newline != -1 {
		context_end = end + context_end_newline
	}

	mut context := ''

	if cutoff {
		context += '...'
	}
	context += decoder.json[context_start..start]
	context += '\e[31m${decoder.json[start..end]}\e[0m'
	context += decoder.json[end..context_end]
	context += '\n'

	if cutoff {
		context += ' '.repeat(max_context_lenght + 3)
	} else {
		context += ' '.repeat(character_number)
	}
	context += '\e[31m${'~'.repeat(error_info.length)}\e[0m'

	return JsonDecodeError{
		context:   context
		message:   'Data: ${message}'
		line:      line_number + 1
		character: character_number + 1
	}
}

// check_json_format checks if the JSON string is valid and updates the decoder state.
fn (mut checker Decoder) check_json_format(val string) ! {
	checker_end := checker.json.len
	// check if the JSON string is empty
	if val == '' {
		return checker.checker_error('empty string')
	}

	// skip whitespace
	for val[checker.checker_idx] in whitespace_chars {
		if checker.checker_idx >= checker_end - 1 {
			break
		}
		checker.checker_idx++
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
			return checker.checker_error('unknown value kind')
		}
		.null {
			// check if the JSON string is a null value
			if checker_end - checker.checker_idx <= 3 {
				return checker.checker_error('EOF error: expecting `null`')
			}

			is_not_ok := unsafe {
				vmemcmp(checker.json.str + checker.checker_idx, null_in_string.str, null_in_string.len)
			}

			if is_not_ok != 0 {
				return checker.checker_error('invalid null value. Got `${checker.json[checker.checker_idx..
					checker.checker_idx + 4]}` instead of `null`')
			}
			checker.checker_idx += 3
		}
		.object {
			if checker_end - checker.checker_idx < 2 {
				return checker.checker_error('EOF error: expecting a complete object after `{`')
			}
			checker.checker_idx++
			for val[checker.checker_idx] != `}` {
				// skip whitespace
				for val[checker.checker_idx] in whitespace_chars {
					if checker.checker_idx >= checker_end - 1 {
						break
					}
					checker.checker_idx++
				}

				if val[checker.checker_idx] == `}` {
					continue
				}

				if val[checker.checker_idx] != `"` {
					return checker.checker_error('Expecting object key')
				}

				// Object key
				checker.check_json_format(val)!

				for val[checker.checker_idx] != `:` {
					if checker.checker_idx >= checker_end - 1 {
						return checker.checker_error('EOF error: key colon not found')
					}
					if val[checker.checker_idx] !in whitespace_chars {
						return checker.checker_error('invalid value after object key')
					}
					checker.checker_idx++
				}

				if val[checker.checker_idx] != `:` {
					return checker.checker_error('Expecting `:` after object key')
				}
				// skip `:`
				checker.checker_idx++

				// skip whitespace
				for val[checker.checker_idx] in whitespace_chars {
					checker.checker_idx++
				}

				match val[checker.checker_idx] {
					`"`, `[`, `{`, `0`...`9`, `-`, `n`, `t`, `f` {
						checker.check_json_format(val)!
						// whitespace
						for val[checker.checker_idx] in whitespace_chars {
							checker.checker_idx++
						}
						if val[checker.checker_idx] == `}` {
							break
						}
						if checker.checker_idx >= checker_end - 1 {
							return checker.checker_error('EOF error: braces are not closed')
						}

						if val[checker.checker_idx] == `,` {
							checker.checker_idx++
							for val[checker.checker_idx] in whitespace_chars {
								checker.checker_idx++
							}
							if val[checker.checker_idx] != `"` {
								return checker.checker_error('Expecting object key after `,`')
							}
						} else {
							if val[checker.checker_idx] == `}` {
								break
							} else {
								return checker.checker_error('invalid object value')
							}
						}
					}
					else {
						return checker.checker_error('invalid object value')
					}
				}
			}
		}
		.array {
			// check if the JSON string is an empty array
			if checker_end >= checker.checker_idx + 2 {
				checker.checker_idx++
			} else {
				return checker.checker_error('EOF error: There are not enough length for an array')
			}

			for val[checker.checker_idx] != `]` {
				// skip whitespace
				for val[checker.checker_idx] in whitespace_chars {
					if checker.checker_idx >= checker_end - 1 {
						break
					}
					checker.checker_idx++
				}

				if val[checker.checker_idx] == `]` {
					break
				}

				if checker.checker_idx >= checker_end - 1 {
					return checker.checker_error('EOF error: array not closed')
				}

				checker.check_json_format(val)!

				// whitespace
				for val[checker.checker_idx] in whitespace_chars {
					checker.checker_idx++
				}
				if val[checker.checker_idx] == `]` {
					break
				}
				if checker.checker_idx >= checker_end - 1 {
					return checker.checker_error('EOF error: braces are not closed')
				}

				if val[checker.checker_idx] == `,` {
					checker.checker_idx++
					for val[checker.checker_idx] in whitespace_chars {
						checker.checker_idx++
					}
					if val[checker.checker_idx] == `]` {
						return checker.checker_error('Cannot use `,`, before `]`')
					}
					continue
				} else {
					if val[checker.checker_idx] == `]` {
						break
					} else {
						return checker.checker_error('`]` after value')
					}
				}
			}
		}
		.string_ {
			// check if the JSON string is a valid string

			if checker.checker_idx >= checker_end - 1 {
				return checker.checker_error('EOF error: string not closed')
			}

			checker.checker_idx++

			// check if the JSON string is a valid escape sequence
			for val[checker.checker_idx] != `"` {
				if val[checker.checker_idx] == `\\` {
					if checker.checker_idx + 1 >= checker_end - 1 {
						return checker.checker_error('invalid escape sequence')
					}
					escaped_char := val[checker.checker_idx + 1]
					match escaped_char {
						`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {
							checker.checker_idx++ // make sure escaped quotation marks are skipped
						}
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
											return checker.checker_error('invalid unicode escape sequence')
										}
									}
								}
								continue
							} else {
								return checker.checker_error('short unicode escape sequence ${checker.json[checker.checker_idx..escaped_char_last_index]}')
							}
						}
						else {
							return checker.checker_error('unknown escape sequence')
						}
					}
				}
				checker.checker_idx++
			}
		}
		.number {
			// check if the JSON string is a valid float or integer
			if val[checker.checker_idx] == `-` {
				checker.checker_idx++
			}

			if checker.checker_idx == checker_end {
				checker.checker_idx--
				return checker.checker_error('expected digit got EOF')
			}

			// integer part
			if val[checker.checker_idx] == `0` {
				checker.checker_idx++
			} else if val[checker.checker_idx] >= `1` && val[checker.checker_idx] <= `9` {
				checker.checker_idx++

				for checker.checker_idx < checker_end && val[checker.checker_idx] >= `0`
					&& val[checker.checker_idx] <= `9` {
					checker.checker_idx++
				}
			} else {
				return checker.checker_error('expected digit got ${val[checker.checker_idx].ascii_str()}')
			}

			// fraction part
			if checker.checker_idx != checker_end && val[checker.checker_idx] == `.` {
				checker.checker_idx++

				if checker.checker_idx == checker_end {
					checker.checker_idx--
					return checker.checker_error('expected digit got EOF')
				}

				if val[checker.checker_idx] >= `0` && val[checker.checker_idx] <= `9` {
					for checker.checker_idx < checker_end && val[checker.checker_idx] >= `0`
						&& val[checker.checker_idx] <= `9` {
						checker.checker_idx++
					}
				} else {
					return checker.checker_error('expected digit got ${val[checker.checker_idx].ascii_str()}')
				}
			}

			// exponent part
			if checker.checker_idx != checker_end
				&& (val[checker.checker_idx] == `e` || val[checker.checker_idx] == `E`) {
				checker.checker_idx++

				if checker.checker_idx == checker_end {
					checker.checker_idx--
					return checker.checker_error('expected digit got EOF')
				}

				if val[checker.checker_idx] == `-` || val[checker.checker_idx] == `+` {
					checker.checker_idx++

					if checker.checker_idx == checker_end {
						checker.checker_idx--
						return checker.checker_error('expected digit got EOF')
					}
				}

				if val[checker.checker_idx] >= `0` && val[checker.checker_idx] <= `9` {
					for checker.checker_idx < checker_end && val[checker.checker_idx] >= `0`
						&& val[checker.checker_idx] <= `9` {
						checker.checker_idx++
					}
				} else {
					return checker.checker_error('expected digit got ${val[checker.checker_idx].ascii_str()}')
				}
			}

			checker.checker_idx--
		}
		.boolean {
			// check if the JSON string is a valid boolean
			match val[checker.checker_idx] {
				`t` {
					if checker_end - checker.checker_idx <= 3 {
						return checker.checker_error('EOF error: expecting `true`')
					}

					is_not_ok := unsafe {
						vmemcmp(checker.json.str + checker.checker_idx, true_in_string.str,
							true_in_string.len)
					}

					if is_not_ok != 0 {
						return checker.checker_error('invalid boolean value. Got `${checker.json[checker.checker_idx..
							checker.checker_idx + 4]}` instead of `true`')
					}
					checker.checker_idx += 3
				}
				`f` {
					if checker_end - checker.checker_idx <= 4 {
						return checker.checker_error('EOF error: expecting `false`')
					}

					is_not_ok := unsafe {
						vmemcmp(checker.json.str + checker.checker_idx, false_in_string.str,
							false_in_string.len)
					}

					if is_not_ok != 0 {
						return checker.checker_error('invalid boolean value. Got `${checker.json[checker.checker_idx..
							checker.checker_idx + 5]}` instead of `false`')
					}

					checker.checker_idx += 4
				}
				else {
					return checker.checker_error('invalid boolean')
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
		if val[checker.checker_idx] !in whitespace_chars {
			checker.checker_error('invalid value. Unexpected character after ${value_kind} end')!
		} else {
			// whitespace
		}
		checker.checker_idx++
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

// decode decodes a JSON string into a specified type.
@[manualfree]
pub fn decode[T](val string) !T {
	if val == '' {
		return JsonDecodeError{
			message:   'empty string'
			line:      1
			character: 1
		}
	}
	mut decoder := Decoder{
		json: val
	}

	decoder.check_json_format(val)!

	mut result := T{}
	decoder.current_node = decoder.values_info.head
	decoder.decode_value(mut result)!
	unsafe {
		decoder.values_info.free()
	}
	return result
}

// decode_value decodes a value from the JSON nodes.
@[manualfree]
fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is string {
		string_info := decoder.current_node.value

		if string_info.value_kind == .string_ {
			mut string_buffer := []u8{cap: string_info.length} // might be too long but most json strings don't contain many escape characters anyways

			mut buffer_index := 1
			mut string_index := 1

			for string_index < string_info.length - 1 {
				current_byte := decoder.json[string_info.position + string_index]

				if current_byte == `\\` {
					// push all characters up to this point
					unsafe {
						string_buffer.push_many(decoder.json.str + string_info.position +
							buffer_index, string_index - buffer_index)
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
							string_buffer << rune(strconv.parse_uint(decoder.json[
								string_info.position + string_index..string_info.position +
								string_index + 4], 16, 32)!).bytes()

							string_index += 4
						}
						else {} // has already been checked
					}

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

			val = string_buffer.bytestr()
		} else {
			return decoder.decode_error('Expected string, but got ${string_info.value_kind}')
		}
	} $else $if T.unaliased_typ is $sumtype {
		decoder.decode_sumtype(mut val)!
		return
	} $else $if T.unaliased_typ is $map {
		decoder.decode_map(mut val)!
		return
	} $else $if T.unaliased_typ is $array {
		unsafe {
			val.len = 0
		}
		decoder.decode_array(mut val)!
		// return to avoid the next increment of the current node
		// this is because the current node is already incremented in the decode_array function
		// remove this line will cause the current node to be incremented twice
		// and bug recursive array decoding like `[][]int{}`
		return
	} $else $if T.unaliased_typ is $struct {
		struct_info := decoder.current_node.value

		// Custom Decoders
		$if val is StringDecoder {
			if struct_info.value_kind == .string_ {
				val.from_json_string(decoder.json[struct_info.position + 1..struct_info.position +
					struct_info.length - 1])!
				if decoder.current_node != unsafe { nil } {
					decoder.current_node = decoder.current_node.next
				}

				return
			}
		}
		$if val is NumberDecoder {
			if struct_info.value_kind == .number {
				val.from_json_number(decoder.json[struct_info.position..struct_info.position +
					struct_info.length])!
				if decoder.current_node != unsafe { nil } {
					decoder.current_node = decoder.current_node.next
				}

				return
			}
		}
		$if val is BooleanDecoder {
			if struct_info.value_kind == .boolean {
				val.from_json_boolean(decoder.json[struct_info.position] == `t`)
				if decoder.current_node != unsafe { nil } {
					decoder.current_node = decoder.current_node.next
				}

				return
			}
		}
		$if val is NullDecoder {
			if struct_info.value_kind == .null {
				val.from_json_null()
				if decoder.current_node != unsafe { nil } {
					decoder.current_node = decoder.current_node.next
				}

				return
			}
		}

		// struct field info linked list
		mut struct_fields_info := LinkedList[StructFieldInfo]{}

		$for field in T.fields {
			mut json_name_str := field.name.str
			mut json_name_len := field.name.len

			for attr in field.attrs {
				if attr.starts_with('json:') {
					if attr.len <= 6 {
						return decoder.decode_error('`json` attribute must have an argument')
					}
					json_name_str = unsafe { attr.str + 6 }
					json_name_len = attr.len - 6
					break
				}
				continue
			}

			struct_fields_info.push(StructFieldInfo{
				field_name_str: voidptr(field.name.str)
				field_name_len: field.name.len
				json_name_ptr:  voidptr(json_name_str)
				json_name_len:  json_name_len
				is_omitempty:   field.attrs.contains('omitempty')
				is_skip:        field.attrs.contains('skip') || field.attrs.contains('json: -')
				is_required:    field.attrs.contains('required')
				is_raw:         field.attrs.contains('raw')
			})
		}
		if struct_info.value_kind == .object {
			struct_position := struct_info.position
			struct_end := struct_position + struct_info.length

			decoder.current_node = decoder.current_node.next

			mut current_field_info := struct_fields_info.head

			// json object loop
			for {
				if decoder.current_node == unsafe { nil } {
					break
				}

				key_info := decoder.current_node.value

				if key_info.position >= struct_end {
					break
				}

				current_field_info = struct_fields_info.head

				// field loop
				for {
					if current_field_info == unsafe { nil } {
						decoder.current_node = decoder.current_node.next
						break
					}

					if current_field_info.value.is_skip {
						if current_field_info.value.is_required == false {
							current_field_info = current_field_info.next
							continue
						}
					}

					if current_field_info.value.is_omitempty {
						match decoder.current_node.next.value.value_kind {
							.null {
								current_field_info = current_field_info.next
								continue
							}
							.string_ {
								if decoder.current_node.next.value.length == 2 {
									current_field_info = current_field_info.next
									continue
								}
							}
							.number {
								if decoder.json[decoder.current_node.next.value.position] == `0` {
									if decoder.current_node.next.value.length == 1 {
										current_field_info = current_field_info.next
										continue
									} else if decoder.current_node.next.value.length == 3 {
										if unsafe {
											vmemcmp(decoder.json.str +
												decoder.current_node.next.value.position,
												float_zero_in_string.str, float_zero_in_string.len) == 0
										} {
											current_field_info = current_field_info.next
											continue
										}
									}
								}
							}
							else {}
						}
					}

					// check if the key matches the field name
					if key_info.length - 2 == current_field_info.value.json_name_len {
						if unsafe {
							vmemcmp(decoder.json.str + key_info.position + 1, current_field_info.value.json_name_ptr,
								current_field_info.value.json_name_len) == 0
						} {
							$for field in T.fields {
								if field.name.len == current_field_info.value.field_name_len {
									if unsafe {
										(&u8(current_field_info.value.field_name_str)).vstring_with_len(field.name.len) == field.name
									} {
										// value node
										decoder.current_node = decoder.current_node.next

										if current_field_info.value.is_skip {
											if current_field_info.value.is_required == false {
												return decoder.decode_error('This should not happen. Please, file a bug. `skip` field should not be processed here without a `required` attribute')
											}
											current_field_info.value.decoded_with_value_info_node = decoder.current_node
											break
										}

										if current_field_info.value.is_raw {
											$if field.unaliased_typ is $enum {
												// workaround to avoid the error: enums can only be assigned `int` values
												return decoder.decode_error('`raw` attribute cannot be used with enum fields')
											} $else $if field.typ is ?string {
												position := decoder.current_node.value.position
												end := position + decoder.current_node.value.length

												val.$(field.name) = decoder.json[position..end]
												decoder.current_node = decoder.current_node.next

												for {
													if decoder.current_node == unsafe { nil }
														|| decoder.current_node.value.position + decoder.current_node.value.length >= end {
														break
													}

													decoder.current_node = decoder.current_node.next
												}
											} $else $if field.typ is string {
												position := decoder.current_node.value.position
												end := position + decoder.current_node.value.length

												val.$(field.name) = decoder.json[position..end]
												decoder.current_node = decoder.current_node.next

												for {
													if decoder.current_node == unsafe { nil }
														|| decoder.current_node.value.position + decoder.current_node.value.length >= end {
														break
													}

													decoder.current_node = decoder.current_node.next
												}
											} $else {
												return decoder.decode_error('`raw` attribute can only be used with string fields')
											}
										} else {
											$if field.typ is $option {
												// it would be nicer to do this at the start of the function
												// but options cant be passed to generic functions
												if decoder.current_node.value.value_kind == .null {
													val.$(field.name) = none
												} else {
													mut unwrapped_val := create_value_from_optional(val.$(field.name)) or {
														return
													}
													decoder.decode_value(mut unwrapped_val)!
													val.$(field.name) = unwrapped_val
												}
											} $else {
												decoder.decode_value(mut val.$(field.name))!
											}
										}
										current_field_info.value.decoded_with_value_info_node = decoder.current_node
										break
									}
								}
							}
						}
					}
					current_field_info = current_field_info.next
				}
			}

			// check if all required fields are present
			current_field_info = struct_fields_info.head

			for {
				if current_field_info == unsafe { nil } {
					break
				}

				if current_field_info.value.is_required == false {
					current_field_info = current_field_info.next
					continue
				}
				if current_field_info.value.decoded_with_value_info_node == unsafe { nil } {
					return decoder.decode_error('missing required field `${unsafe {
						tos(current_field_info.value.field_name_str, current_field_info.value.field_name_len)
					}}`')
				}
				current_field_info = current_field_info.next
			}
		} else {
			return decoder.decode_error('Expected object, but got ${struct_info.value_kind}')
		}
		unsafe {
			struct_fields_info.free()
		}
		return
	} $else $if T.unaliased_typ is bool {
		value_info := decoder.current_node.value

		if value_info.value_kind != .boolean {
			return decoder.decode_error('Expected boolean, but got ${value_info.value_kind}')
		}

		unsafe {
			val = vmemcmp(decoder.json.str + value_info.position, true_in_string.str,
				true_in_string.len) == 0
		}
	} $else $if T.unaliased_typ is $float || T.unaliased_typ is $int || T.unaliased_typ is $enum {
		value_info := decoder.current_node.value

		if value_info.value_kind == .number {
			unsafe { decoder.decode_number(&val)! }
		} else {
			return decoder.decode_error('Expected number, but got ${value_info.value_kind}')
		}
	} $else {
		return decoder.decode_error('cannot decode value with ${typeof(val).name} type')
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
	} else {
		return decoder.decode_error('Expected array, but got ${array_info.value_kind}')
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

			$if V is $map {
				val[key] = map_value.move()
			} $else {
				val[key] = map_value
			}
			decoder.decode_value(mut val[key])!
		}
	} else {
		return decoder.decode_error('Expected object, but got ${map_info.value_kind}')
	}
}

fn create_value_from_optional[T](val ?T) ?T {
	return T{}
}

fn get_number_max[T](num T) T {
	$if num is i8 {
		return max_i8
	} $else $if num is i16 {
		return max_i16
	} $else $if num is i32 {
		return max_i32
	} $else $if num is i64 {
		return max_i64
	} $else $if num is u8 {
		return max_u8
	} $else $if num is u16 {
		return max_u16
	} $else $if num is u32 {
		return max_u32
	} $else $if num is u64 {
		return max_u64
	} $else $if num is int {
		return max_int
	}
	return 0
}

fn get_number_min[T](num T) T {
	$if num is i8 {
		return min_i8
	} $else $if num is i16 {
		return min_i16
	} $else $if num is i32 {
		return min_i32
	} $else $if num is i64 {
		return min_i64
	} $else $if num is u8 {
		return min_u8
	} $else $if num is u16 {
		return min_u16
	} $else $if num is u32 {
		return min_u32
	} $else $if num is u64 {
		return min_u64
	} $else $if num is int {
		return min_int
	}
	return 0
}

fn get_number_digits[T](num T) int {
	return $if T.unaliased_typ is i8 || T.unaliased_typ is u8 {
		3
	} $else $if T.unaliased_typ is i16 || T.unaliased_typ is u16 {
		5
	} $else $if T.unaliased_typ is i32 || T.unaliased_typ is u32 || T.unaliased_typ is int {
		10
	} $else $if T.unaliased_typ is i64 {
		19
	} $else $if T.unaliased_typ is u64 {
		20
	} $else {
		0
	}
}

// use pointer instead of mut so enum cast works
@[unsafe]
fn (mut decoder Decoder) decode_number[T](val &T) ! {
	number_info := decoder.current_node.value

	$if T.unaliased_typ is $float {
		*val = T(strconv.atof_quick(decoder.json[number_info.position..number_info.position +
			number_info.length]))
	} $else $if T.unaliased_typ is $enum {
		mut result := 0
		decoder.decode_number(&result)!
		*val = T(result)
	} $else { // this part is a minefield
		mut is_negative := false
		mut index := 0

		if decoder.json[number_info.position] == `-` {
			$if T.unaliased_typ is u8 || T.unaliased_typ is u16 || T.unaliased_typ is u32
				|| T.unaliased_typ is u64 || T.unaliased_typ is $enum {
				decoder.decode_error('expected positive integer for ${typeof(val).name} but got ${decoder.json[number_info.position..
					number_info.position + number_info.length]}')!
			}

			is_negative = true
			index++
		}

		// doing it like this means the minimum of signed numbers does not overflow before being inverted
		if !is_negative {
			digit_amount := get_number_digits(*val)

			if number_info.length > digit_amount {
				decoder.decode_error('overflows ${typeof(val).name}')!
			}

			for index < int_min(number_info.length, digit_amount - 1) {
				digit := T(decoder.json[number_info.position + index] - `0`)

				if digit > 9 { // comma, e and E are all smaller 0 in ASCII so they underflow
					decoder.decode_error('expected integer but got real number')!
				}

				*val = *val * 10 + digit

				index++
			}

			if index == digit_amount - 1 {
				digit := T(decoder.json[number_info.position + index] - `0`)

				if digit > 9 { // comma, e and E are all smaller 0 in ASCII so they underflow
					decoder.decode_error('expected integer but got real number')!
				}

				type_max := get_number_max(*val)
				max_digits := type_max / 10
				last_digit := type_max % 10

				if *val > max_digits || (*val == max_digits && digit > last_digit) {
					decoder.decode_error('overflows ${typeof(val).name}s')!
				}

				*val = *val * 10 + digit
			}
		} else {
			digit_amount := get_number_digits(*val) + 1

			if number_info.length > digit_amount {
				decoder.decode_error('underflows ${typeof(val).name}')!
			}

			for index < int_min(number_info.length, digit_amount - 1) {
				digit := T(decoder.json[number_info.position + index] - `0`)

				if digit > 9 { // comma, e and E are all smaller 0 in ASCII so they underflow
					decoder.decode_error('expected integer but got real number')!
				}

				*val = *val * 10 - digit

				index++
			}

			if index == digit_amount - 1 {
				digit := T(decoder.json[number_info.position + index] - `0`)

				if digit > 9 { // comma, e and E are all smaller 0 in ASCII so they underflow
					decoder.decode_error('expected integer but got real number')!
				}

				type_min := get_number_min(*val)
				min_digits := type_min / 10
				last_digit := type_min % 10

				if *val < min_digits || (*val == min_digits && -digit < last_digit) {
					decoder.decode_error('underflows ${typeof(val).name}')!
				}

				*val = *val * 10 - digit
			}
		}
	}
}
