module decoder2

import time

// Node represents a node in a JSON decoder tree. Used to decode object in JSON.
struct Node {
	key_pos  int     // The position of the key in the JSON string.
	key_len  int     // The length of the key in the JSON string.
	children ?[]Node // The children nodes of the current node.
}

// ValueInfo represents the position and length of a value, like string, number, array, object key and object value in a JSON string.
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
	values_info    []ValueInfo
	idx            int // idx is byte offset from the start in json
	checker_idx    int // checker_idx is the current index of the decoder.
	value_info_idx int // value_info_idx is the current index of the values_info.
}

pub enum ValueKind {
	unknown
	array
	object
	string_
	number
	boolean
	null
}

// check_json checks if the JSON string is valid.
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

// check_json checks if the JSON string is valid.
fn (mut checker Decoder) check_json_format(val string) ! {
	checker_end := checker.json.len
	// check if the JSON string is empty
	if val == '' {
		return checker.error('empty string')
	}

	// check if generic type matches the JSON type
	value_kind := get_value_kind(val[checker.checker_idx])
	start_idx_position := checker.checker_idx
	checker.values_info << ValueInfo{
		position:   start_idx_position
		length:     0
		value_kind: value_kind
	}

	value_info_index := checker.values_info.len - 1
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

	checker.values_info[value_info_index].length = checker.checker_idx + 1 - start_idx_position

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
		json:        val
		values_info: []ValueInfo{}
	}

	decoder.check_json_format(val)!
	check_if_json_match[T](val)!

	mut result := T{}
	decoder.decode_value(mut &result)!
	return result
}

// decode_value decodes a value from the JSON nodes.
fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T is $option {
	} $else $if T is string {
	} $else $if T is $sumtype {
		$for v in val.variants {
			if val is v {
				decoder.decode_value(val)
			}
		}
	} $else $if T is $alias {
	} $else $if T is time.Time {
	} $else $if T is $map {
	} $else $if T is $array {
	} $else $if T is $struct {
		mut nodes := []Node{}
		// TODO: needs performance improvements
		decoder.fulfill_nodes(mut nodes)

		decoder.decode_struct(nodes, val)
	} $else $if T is bool {
		value_info := decoder.values_info[decoder.value_info_idx]

		unsafe {
			val = vmemcmp(decoder.json.str + value_info.position, 'true'.str, 4) == 0
		}
	} $else $if T in [$int, $float, $enum] {
		value_info := decoder.values_info[decoder.value_info_idx]

		if value_info.value_kind == .number {
			bytes := unsafe { (decoder.json.str + value_info.position).vbytes(value_info.length) }

			unsafe {
				string_buffer_to_generic_number(val, bytes)
			}
		}
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
	decoder.value_info_idx++
}

// get_value_kind returns the kind of a JSON value.
fn get_value_kind(value rune) ValueKind {
	return match value {
		`"` { .string_ }
		`t`, `f` { .boolean }
		`{` { .object }
		`[` { .array }
		`0`...`9`, `-` { .number }
		`n` { .null }
		else { .unknown }
	}
}

// decode_optional_value_in_actual_node decodes an optional value in a node.
fn (mut decoder Decoder) decode_optional_value_in_actual_node[T](node Node, val ?T) T {
	start := (node.key_pos + node.key_len) + 3
	mut end := start
	for decoder.json[end] != `,` && decoder.json[end] != `}` {
		end++
	}
	mut value_kind := get_value_kind(decoder.json[start])

	$if T is string {
		if value_kind == .string_ {
			return decoder.json[start + 1..end - 1]
		} else if value_kind == .object {
		} else if value_kind == .array {
		} else {
			return decoder.json[start..end]
		}
		return ''
	} $else $if T is $int {
		if value_kind == .string_ {
			return decoder.json[start + 1..end - 1].int()
		} else if value_kind == .object {
		} else if value_kind == .array {
		} else {
			return decoder.json[start..end].int()
		}
	}
	return T{}
}

// decode_struct decodes a struct from the JSON nodes.
fn (mut decoder Decoder) decode_struct[T](nodes []Node, value &T) {
	$for field in T.fields {
		for i := 0; i < nodes.len; i++ {
			mut node := nodes[i]

			if node.key_len == field.name.len {
				// This `vmemcmp` compares the name of a key in a JSON with a given struct field.
				if unsafe {
					vmemcmp(decoder.json.str + node.key_pos, field.name.str, field.name.len) == 0
				} {
					start := (node.key_pos + node.key_len) + 3
					mut end := start
					for decoder.json[end] != `,` && decoder.json[end] != `}` {
						end++
					}
					value_kind := get_value_kind(decoder.json[start])
					$if field.indirections != 0 {
						// REVIEW Needs clone?
						$if field.indirections == 1 {
							// TODO
							// unsafe {
							// 	value.$(field.name) = &(decoder.json[start + 1..end - 1])
							// }
						} $else $if field.indirections == 2 {
							// TODO
							// unsafe {
							// 	value.$(field.name) = &&(decoder.json[start + 1..end - 1])
							// }
						} $else $if field.indirections == 3 {
							// TODO
							// unsafe {
							// 	value.$(field.name) = &&&(decoder.json[start + 1..end - 1])
							// }
						}
					} $else $if field.typ is $option {
						value.$(field.name) = decoder.decode_optional_value_in_actual_node(node,
							value.$(field.name))
					} $else $if field.typ is $sumtype {
						// dump(value.$(field.name))

						workaround := value.$(field.name)
						// z := value.$(field.name)

						$for v in workaround.variants {
							$if v.typ is string {
								if value_kind == .string_ {
									// value.$(field.name) = decoder.json[start + 1..end - 1]
								} else {
									// value.$(field.name) = decoder.json[start..end]
								}
							} $else $if v.typ in [$int, $float] {
								$if v.typ is u32 {
									value.$(field.name) = decoder.json[start..end].u32()
								} $else $if v.typ is u32 {
								}

								$if v.typ is i8 {
									value.$(field.name) = decoder.json[start..end].i8()
								} $else $if v.typ is i16 {
									value.$(field.name) = decoder.json[start..end].i16()
								} $else $if v.typ is i32 {
									value.$(field.name) = decoder.json[start..end].i32()
								} $else $if v.typ is int {
									value.$(field.name) = decoder.json[start..end].int()
								} $else $if v.typ is i64 {
									value.$(field.name) = decoder.json[start..end].i64()
								} $else $if v.typ is u8 {
									value.$(field.name) = decoder.json[start..end].u8()
								} $else $if v.typ is u16 {
									value.$(field.name) = decoder.json[start..end].u16()
								} $else $if v.typ is u32 {
									value.$(field.name) = decoder.json[start..end].u32()
								} $else $if v.typ is u64 {
									value.$(field.name) = decoder.json[start..end].u64()
								} $else $if v.typ is f32 {
									value.$(field.name) = decoder.json[start..end].f32()
								} $else $if v.typ is f64 {
									value.$(field.name) = decoder.json[start..end].f64()
								}
							} $else $if v.typ is bool {
								if decoder.json[start] == `t` {
									value.$(field.name) = true
								} else if decoder.json[start] == `f` {
									value.$(field.name) = false
								}
							} $else $if v.typ is time.Time {
								if value_kind == .string_ {
									value.$(field.name) = time.parse(decoder.json[start + 1..end - 1]) or {
										time.Time{}
									}
								}
							} $else $if v.typ is $struct {
								if node.children != none {
									// FIXME
									// decoder.decode_value(node.children or {
									// 	panic('It will never happens')
									// }, value.$(field.name))
								}
							} $else $if v.typ is $array {
								if value_kind == .array {
									// TODO
								}
							} $else $if v.typ is $map {
								if value_kind == .object {
									// TODO
								}
							} $else $if T is $enum {
							} $else {
								eprintln('not supported')
							}
						}
						if value_kind == .string_ {
							// value.$(field.name) = decoder.json[start + 1..end - 1]
						} else if decoder.json[start] == `t` {
							value.$(field.name) = true
						} else if decoder.json[start] == `f` {
							value.$(field.name) = false
						} else if value_kind == .object {
						} else if value_kind == .array {
						} else if value_kind == .number {
							// value.$(field.name) = decoder.json[start..end].int()
						} else {
						}
					} $else $if field.typ is string {
						value.$(field.name) = if value_kind == .string_ {
							decoder.json[start + 1..end - 1]
						} else {
							decoder.json[start..end]
						}
					} $else $if field.typ in [$int, $float] {
						$if field.typ is i8 {
							value.$(field.name) = decoder.json[start..end].i8()
						} $else $if field.typ is i16 {
							value.$(field.name) = decoder.json[start..end].i16()
						} $else $if field.typ is i32 {
							value.$(field.name) = decoder.json[start..end].i32()
						} $else $if field.typ is int {
							value.$(field.name) = decoder.json[start..end].int()
						} $else $if field.typ is i64 {
							value.$(field.name) = decoder.json[start..end].i64()
						} $else $if field.typ is u8 {
							value.$(field.name) = decoder.json[start..end].u8()
						} $else $if field.typ is u16 {
							value.$(field.name) = decoder.json[start..end].u16()
						} $else $if field.typ is u32 {
							value.$(field.name) = decoder.json[start..end].u32()
						} $else $if field.typ is u64 {
							value.$(field.name) = decoder.json[start..end].u64()
						} $else $if field.typ is f32 {
							value.$(field.name) = decoder.json[start..end].f32()
						} $else $if field.typ is f64 {
							value.$(field.name) = decoder.json[start..end].f64()
						}
					} $else $if field.typ is bool {
						value.$(field.name) = decoder.json[start] == `t`
					} $else $if field.typ is time.Time {
						if value_kind == .string_ {
							value.$(field.name) = time.parse_rfc3339(decoder.json[start + 1..end - 1]) or {
								time.Time{}
							}
						}
					} $else $if field.typ is $struct {
						if node.children != none {
							decoder.decode_value(node.children or { panic('It will never happen') },
								value.$(field.name))
						}
					} $else $if field.typ is $array {
						if value_kind == .array {
							// TODO
						}
					} $else $if field.typ is $map {
						if value_kind == .object && node.children != none {
							decoder.decode_map(node.children or { panic('It will never happen') }, mut
								value.$(field.name))
						}
					} $else $if field.typ is $enum {
						value.$(field.name) = decoder.json[start..end].int()
					} $else $if field.typ is $alias {
						$if field.unaliased_typ is string {
							if value_kind == .string_ {
								value.$(field.name) = decoder.json[start + 1..end - 1]
							}
						} $else $if field.unaliased_typ is time.Time {
						} $else $if field.unaliased_typ is bool {
						} $else $if field.unaliased_typ in [$float, $int] {
							$if field.unaliased_typ is i8 {
								value.$(field.name) = decoder.json[start..end].i8()
							} $else $if field.unaliased_typ is i16 {
								value.$(field.name) = decoder.json[start..end].i16()
							} $else $if field.unaliased_typ is i32 {
								value.$(field.name) = decoder.json[start..end].i32()
							} $else $if field.unaliased_typ is int {
								value.$(field.name) = decoder.json[start..end].int()
							} $else $if field.unaliased_typ is i64 {
								value.$(field.name) = decoder.json[start..end].i64()
							} $else $if field.unaliased_typ is u8 {
								value.$(field.name) = decoder.json[start..end].u8()
							} $else $if field.unaliased_typ is u16 {
								value.$(field.name) = decoder.json[start..end].u16()
							} $else $if field.unaliased_typ is u32 {
								value.$(field.name) = decoder.json[start..end].u32()
							} $else $if field.unaliased_typ is u64 {
								value.$(field.name) = decoder.json[start..end].u64()
							} $else $if field.unaliased_typ is f32 {
								value.$(field.name) = decoder.json[start..end].f32()
							} $else $if field.unaliased_typ is f64 {
								value.$(field.name) = decoder.json[start..end].f64()
							}
						} $else $if field.unaliased_typ is $array {
							// TODO
						} $else $if field.unaliased_typ is $struct {
						} $else $if field.unaliased_typ is $enum {
							// TODO
						} $else $if field.unaliased_typ is $sumtype {
							// TODO
						} $else {
							eprintln('the alias ${field.unaliased_typ} cannot be encoded')
						}
					} $else {
						eprintln('not supported')
					}
					break
				}
			}
		}
	}
}

// decode_map decodes a map from the JSON nodes.
fn (mut decoder Decoder) decode_map[T](nodes []Node, mut val T) {
	for i := 0; i < nodes.len; i++ {
		mut node := nodes[i]

		start := (node.key_pos + node.key_len) + 3
		mut end := start
		for decoder.json[end] != `,` && decoder.json[end] != `}` {
			end++
		}
		value_kind := get_value_kind(decoder.json[start])
		val[decoder.json[node.key_pos..node.key_pos + node.key_len]] = if value_kind == .string_ {
			decoder.json[start + 1..end - 1]
		} else {
			decoder.json[start..end]
		}
	}
}

// fulfill_nodes fills the nodes from the JSON string.
fn (mut decoder Decoder) fulfill_nodes(mut nodes []Node) {
	mut inside_string := false
	mut inside_key := false
	mut actual_key_len := 0

	for decoder.idx < decoder.json.len {
		letter := decoder.json[decoder.idx]
		match letter {
			` ` {
				if !inside_string {
				}
			}
			`\"` {
				if decoder.json[decoder.idx - 1] == `{` || decoder.json[decoder.idx - 2] == `,` {
					inside_key = true
				} else if decoder.json[decoder.idx + 1] == `:` {
					if decoder.json[decoder.idx + 3] == `{` {
						mut children := []Node{}
						key_pos := decoder.idx - actual_key_len
						key_len := actual_key_len

						decoder.idx += 3
						decoder.fulfill_nodes(mut children)

						nodes << Node{
							key_pos:  key_pos
							key_len:  key_len
							children: children
						}
					} else {
						nodes << Node{
							key_pos: decoder.idx - actual_key_len
							key_len: actual_key_len
						}
					}
					inside_key = false
				}
				inside_string = !inside_string
				decoder.idx++
				continue
			}
			`:` {
				actual_key_len = 0
			}
			`,`, `{`, `}`, `[`, `]` {}
			else {}
		}
		if inside_key {
			actual_key_len++
		}
		decoder.idx++
	}
}

// string_buffer_to_generic_number converts a buffer of bytes (data) into a generic type T and
// stores the result in the provided result pointer.
// The function supports conversion to the following types:
// - Signed integers: i8, i16, int, i64
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
	mut is_negative := false

	$if T is $int {
		for ch in data {
			if ch == `-` {
				is_negative = true
				continue
			}
			digit := T(ch - `0`)
			*result = T(*result * 10 + digit)
		}
	} $else $if T is $float {
		mut decimal_seen := false
		mut decimal_divider := int(1)

		for ch in data {
			if ch == `-` {
				is_negative = true
				continue
			}
			if ch == `.` {
				decimal_seen = true
				continue
			}

			digit := T(ch - `0`)

			if decimal_seen {
				decimal_divider *= 10
				*result += T(digit / decimal_divider)
			} else {
				*result = *result * 10 + digit
			}
		}
	} $else $if T is $enum {
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

	if is_negative {
		*result = -*result
	}
}
