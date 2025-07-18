module decoder2

import strconv
import time
import strings

const null_in_string = 'null'

const true_in_string = 'true'

const false_in_string = 'false'

const float_zero_in_string = '0.0'

const whitespace_chars = [` `, `\t`, `\n`]!

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
			return checker.error('unknown value kind')
		}
		.null {
			// check if the JSON string is a null value
			if checker_end - checker.checker_idx <= 3 {
				return checker.error('EOF error: expecting `null`')
			}

			is_not_ok := unsafe {
				vmemcmp(checker.json.str + checker.checker_idx, null_in_string.str, null_in_string.len)
			}

			if is_not_ok != 0 {
				return checker.error('invalid null value. Got `${checker.json[checker.checker_idx..
					checker.checker_idx + 4]}` instead of `null`')
			}
			checker.checker_idx += 3
		}
		.object {
			if checker_end - checker.checker_idx < 2 {
				return checker.error('EOF error: expecting a complete object after `{`')
			}
			checker.checker_idx++
			for val[checker.checker_idx] != `}` {
				// check if the JSON string is an empty object
				if checker_end - checker.checker_idx <= 2 {
					continue
				}

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
					return checker.error('Expecting object key')
				}

				// Object key
				checker.check_json_format(val)!

				for val[checker.checker_idx] != `:` {
					if checker.checker_idx >= checker_end - 1 {
						return checker.error('EOF error: key colon not found')
					}
					if val[checker.checker_idx] !in whitespace_chars {
						return checker.error('invalid value after object key')
					}
					checker.checker_idx++
				}

				if val[checker.checker_idx] != `:` {
					return checker.error('Expecting `:` after object key')
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
							return checker.error('EOF error: braces are not closed')
						}

						if val[checker.checker_idx] == `,` {
							checker.checker_idx++
							for val[checker.checker_idx] in whitespace_chars {
								checker.checker_idx++
							}
							if val[checker.checker_idx] != `"` {
								return checker.error('Expecting object key after `,`')
							}
						} else {
							if val[checker.checker_idx] == `}` {
								break
							} else {
								return checker.error('invalid object value')
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
				if val[checker.checker_idx] == `]` {
					return
				}
			} else {
				return checker.error('EOF error: There are not enough length for an array')
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
					return
				}

				if checker.checker_idx >= checker_end - 1 {
					return checker.error('EOF error: array not closed')
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
					return checker.error('EOF error: braces are not closed')
				}

				if val[checker.checker_idx] == `,` {
					checker.checker_idx++
					for val[checker.checker_idx] in whitespace_chars {
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
			for val[checker.checker_idx] != `"` {
				if val[checker.checker_idx] == `\\` {
					if checker.checker_idx + 1 >= checker_end - 1 {
						return checker.error('invalid escape sequence')
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
											return checker.error('invalid unicode escape sequence')
										}
									}
								}
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
						vmemcmp(checker.json.str + checker.checker_idx, true_in_string.str,
							true_in_string.len)
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
						vmemcmp(checker.json.str + checker.checker_idx, false_in_string.str,
							false_in_string.len)
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
		if val[checker.checker_idx] !in whitespace_chars {
			checker.error('invalid value. Unexpected character after ${value_kind} end')!
		} else {
			// whitespace
		}
		checker.checker_idx++
	}
}

// decode decodes a JSON string into a specified type.
@[manualfree]
pub fn decode[T](val string) !T {
	if val == '' {
		return error('empty string')
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
			return error('Expected string, but got ${string_info.value_kind}')
		}
	} $else $if T.unaliased_typ is $sumtype {
		decoder.decode_sumtype(mut val)!
		return
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

		// struct field info linked list
		mut struct_fields_info := LinkedList[StructFieldInfo]{}

		$for field in T.fields {
			mut json_name_str := field.name.str
			mut json_name_len := field.name.len

			for attr in field.attrs {
				if attr.starts_with('json:') {
					if attr.len <= 6 {
						return error('`json` attribute must have an argument')
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
												return error('This should not happen. Please, file a bug. `skip` field should not be processed here without a `required` attribute')
											}
											current_field_info.value.decoded_with_value_info_node = decoder.current_node
											break
										}

										if current_field_info.value.is_raw {
											$if field.unaliased_typ is $enum {
												// workaround to avoid the error: enums can only be assigned `int` values
												return error('`raw` attribute cannot be used with enum fields')
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
												return error('`raw` attribute can only be used with string fields')
											}
										} else {
											$if field.typ is $option {
												// it would be nicer to do this at the start of the function
												// but options cant be passed to generic functions
												if decoder.current_node.value.length == 4
													&& decoder.json[decoder.current_node.value.position..decoder.current_node.value.position + 4] == 'null' {
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
					return error('missing required field `${unsafe {
						tos(current_field_info.value.field_name_str, current_field_info.value.field_name_len)
					}}`')
				}
				current_field_info = current_field_info.next
			}
		} else {
			return error('Expected object, but got ${struct_info.value_kind}')
		}
		unsafe {
			struct_fields_info.free()
		}
		return
	} $else $if T.unaliased_typ is bool {
		value_info := decoder.current_node.value

		if value_info.value_kind != .boolean {
			return error('Expected boolean, but got ${value_info.value_kind}')
		}

		unsafe {
			val = vmemcmp(decoder.json.str + value_info.position, true_in_string.str,
				true_in_string.len) == 0
		}
	} $else $if T.unaliased_typ in [$float, $int, $enum] {
		value_info := decoder.current_node.value

		if value_info.value_kind == .number {
			bytes := unsafe { (decoder.json.str + value_info.position).vbytes(value_info.length) }

			unsafe {
				string_buffer_to_generic_number(val, bytes)
			}
		} else {
			return error('Expected number, but got ${value_info.value_kind}')
		}
	} $else {
		return error('cannot decode value with ${typeof(val).name} type')
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
		return error('Expected array, but got ${array_info.value_kind}')
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
		return error('Expected object, but got ${map_info.value_kind}')
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

fn create_value_from_optional[T](val ?T) ?T {
	return T{}
}

fn utf8_byte_len(unicode_value u32) int {
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
		panic('unsupported type ' + typeof[T]().name)
	}
}
