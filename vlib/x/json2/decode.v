module json2

import strconv
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
	position   int       // The position of the value in the JSON string.
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
	is_decoded bool
}

// Decoder represents a JSON decoder.
struct Decoder {
	json string // json is the JSON data to be decoded.
mut:
	values_info  LinkedList[ValueInfo] // A linked list to store ValueInfo.
	checker_idx  int                   // checker_idx is the current index of the decoder.
	current_node &Node[ValueInfo] = unsafe { nil } // The current node in the linked list.
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
fn (mut decoder Decoder) decode_error(message string) ! {
	mut error_info := ValueInfo{}
	if decoder.current_node != unsafe { nil } {
		error_info = decoder.current_node.value
	} else {
		error_info = decoder.values_info.tail.value
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
	context += '\e[31m${decoder.json[start..end]}\e[0m'
	context += decoder.json[end..context_end]
	context += '\n'

	if cutoff {
		context += ' '.repeat(max_context_length + 3)
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

	decoder.check_json_format()!

	mut result := T{}
	decoder.current_node = decoder.values_info.head
	decoder.decode_value(mut result)!
	unsafe {
		decoder.values_info.free()
	}
	return result
}

fn get_dynamic_from_element[T](t T) []T {
	return []T{}
}

// decode_value decodes a value from the JSON nodes.
@[manualfree]
fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	// Custom Decoders
	$if val is StringDecoder {
		struct_info := decoder.current_node.value

		if struct_info.value_kind == .string {
			val.from_json_string(decoder.json[struct_info.position + 1..struct_info.position +
				struct_info.length - 1]) or {
				decoder.decode_error('${typeof(*val).name}: ${err.msg()}')!
			}
			if decoder.current_node != unsafe { nil } {
				decoder.current_node = decoder.current_node.next
			}

			return
		}
	}
	$if val is NumberDecoder {
		struct_info := decoder.current_node.value

		if struct_info.value_kind == .number {
			val.from_json_number(decoder.json[struct_info.position..struct_info.position +
				struct_info.length]) or {
				decoder.decode_error('${typeof(*val).name}: ${err.msg()}')!
			}
			if decoder.current_node != unsafe { nil } {
				decoder.current_node = decoder.current_node.next
			}

			return
		}
	}
	$if val is BooleanDecoder {
		struct_info := decoder.current_node.value

		if struct_info.value_kind == .boolean {
			val.from_json_boolean(decoder.json[struct_info.position] == `t`)
			if decoder.current_node != unsafe { nil } {
				decoder.current_node = decoder.current_node.next
			}

			return
		}
	}
	$if val is NullDecoder {
		struct_info := decoder.current_node.value

		if struct_info.value_kind == .null {
			val.from_json_null()
			if decoder.current_node != unsafe { nil } {
				decoder.current_node = decoder.current_node.next
			}

			return
		}
	}
	$if T.unaliased_typ is string {
		decoder.decode_string(mut val)!
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
		mut dynamic_val := get_dynamic_from_element(val[0])

		// avoid copying by pointing dynamic_val to val
		unsafe {
			dynamic_val.len = 0
			dynamic_val.cap = val.len // ensures data wont reallocate
			dynamic_val.data = &val
		}
		decoder.decode_array(mut dynamic_val)!

		if dynamic_val.len != val.len {
			decoder.decode_error('Fixed size array expected ${val.len} elements but got ${dynamic_val.len} elements')!
		}
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
						decoder.decode_error('`json` attribute must have an argument')!
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
						decoder.current_node = decoder.current_node.next // skip value

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
							.string {
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
												decoder.decode_error('This should not happen. Please, file a bug. `skip` field should not be processed here without a `required` attribute')!
											}
											current_field_info.value.is_decoded = true
											if decoder.current_node != unsafe { nil } {
												decoder.current_node = decoder.current_node.next
											}
											break
										}

										if current_field_info.value.is_raw {
											$if field.unaliased_typ is $enum {
												// workaround to avoid the error: enums can only be assigned `int` values
												decoder.decode_error('`raw` attribute cannot be used with enum fields')!
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
												decoder.decode_error('`raw` attribute can only be used with string fields')!
											}
										} else {
											$if field.typ is $option {
												// it would be nicer to do this at the start of the function
												// but options cant be passed to generic functions
												if decoder.current_node.value.value_kind == .null {
													val.$(field.name) = none

													if decoder.current_node != unsafe { nil } {
														decoder.current_node = decoder.current_node.next
													}
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
										current_field_info.value.is_decoded = true
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
				if !current_field_info.value.is_decoded {
					decoder.decode_error('missing required field `${unsafe {
						tos(current_field_info.value.field_name_str, current_field_info.value.field_name_len)
					}}`')!
				}
				current_field_info = current_field_info.next
			}
		} else {
			decoder.decode_error('Expected object, but got ${struct_info.value_kind}')!
		}
		unsafe {
			struct_fields_info.free()
		}
		return
	} $else $if T.unaliased_typ is bool {
		value_info := decoder.current_node.value

		if value_info.value_kind != .boolean {
			decoder.decode_error('Expected boolean, but got ${value_info.value_kind}')!
		}

		unsafe {
			val = vmemcmp(decoder.json.str + value_info.position, true_in_string.str,
				true_in_string.len) == 0
		}
	} $else $if T.unaliased_typ is $float || T.unaliased_typ is $int {
		value_info := decoder.current_node.value

		if value_info.value_kind == .number {
			unsafe { decoder.decode_number(&val)! }
		} else if value_info.value_kind == .string {
			// recheck if string contains number
			decoder.checker_idx = value_info.position + 1
			decoder.check_number()!

			unsafe { decoder.decode_number(&val)! }
		} else {
			decoder.decode_error('Expected number, but got ${value_info.value_kind}')!
		}
	} $else $if T.unaliased_typ is $enum {
		decoder.decode_enum(mut val)!
	} $else {
		decoder.decode_error('cannot decode value with ${typeof(val).name} type')!
	}

	if decoder.current_node != unsafe { nil } {
		decoder.current_node = decoder.current_node.next
	}
}

fn (mut decoder Decoder) decode_string[T](mut val T) ! {
	string_info := decoder.current_node.value

	if string_info.value_kind == .string {
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
						unicode_point := rune(strconv.parse_uint(decoder.json[
							string_info.position + string_index..string_info.position +
							string_index + 4], 16, 32)!)

						string_index += 4

						if unicode_point < 0xD800 || unicode_point > 0xDFFF { // normal utf-8
							string_buffer << unicode_point.bytes()
						} else if unicode_point >= 0xDC00 { // trail surrogate -> invalid
							decoder.decode_error('Got trail surrogate: ${u32(unicode_point):04X} before head surrogate.')!
						} else { // head surrogate -> treat as utf-16
							if string_index > string_info.length - 6 {
								decoder.decode_error('Expected a trail surrogate after a head surrogate, but got no valid escape sequence.')!
							}
							if decoder.json[string_info.position + string_index..
								string_info.position + string_index + 2] != '\\u' {
								decoder.decode_error('Expected a trail surrogate after a head surrogate, but got no valid escape sequence.')!
							}

							string_index += 2

							unicode_point2 := rune(strconv.parse_uint(decoder.json[
								string_info.position + string_index..string_info.position +
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
		decoder.decode_error('Expected string, but got ${string_info.value_kind}')!
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
		decoder.decode_error('Expected array, but got ${array_info.value_kind}')!
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
		decoder.decode_error('Expected object, but got ${map_info.value_kind}')!
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

fn (mut decoder Decoder) decode_enum[T](mut val T) ! {
	enum_info := decoder.current_node.value

	if enum_info.value_kind == .number {
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
		unsafe { decoder.decode_value(mut result)! }

		$for value in T.values {
			if value.name == result {
				val = value.value
				return
			}
		}
		decoder.decode_error('String value: `${result}` does not match any field in enum: ${typeof(val).name}')!
	}

	decoder.decode_error('Expected number or string value for enum, got: ${enum_info.value_kind}')!
}

// use pointer instead of mut so enum cast works
@[unsafe]
fn (mut decoder Decoder) decode_number[T](val &T) ! {
	mut number_info := decoder.current_node.value

	if decoder.json[number_info.position] == `"` { // fake number
		number_info = ValueInfo{
			position: number_info.position + 1
			length:   number_info.length - 2
		}
	}

	$if T.unaliased_typ is $float {
		*val = T(strconv.atof_quick(decoder.json[number_info.position..number_info.position +
			number_info.length]))
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
