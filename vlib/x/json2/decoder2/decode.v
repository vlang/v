module decoder2

import time
import strconv

// Node represents a node in a JSON decoder tree.
struct Node {
	key_pos  int     // The position of the key in the JSON string.
	key_len  int     // The length of the key in the JSON string.
	children ?[]Node // The children nodes of the current node.
}

// Decoder represents a JSON decoder.
struct Decoder {
	json string // json is the JSON data to be decoded.
mut:
	idx int // idx is the current index of the decoder.
}

struct Checker {
	end  int
	json string // json is the JSON data to be decoded.
mut:
	idx int // idx is the current index of the decoder.
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

fn (mut checker Checker) error(message string) ! {
	json := if checker.end < checker.idx + 5 {
		checker.json
	} else {
		checker.json[0..checker.idx + 5]
	}

	mut error_message := '\n'
	last_new_line := json.last_index_u8(`\n`)
	if last_new_line != -1 {
		error_message += json[last_new_line..checker.idx]
	} else {
		error_message += json[0..checker.idx]
	}
	error_message += [json[checker.idx]].bytestr()

	error_message += '\n'

	if last_new_line != -1 {
		error_message += ' '.repeat(checker.idx - last_new_line)
	} else {
		error_message += ' '.repeat(checker.idx)
	}

	error_message += '^ ${message}'

	return error(error_message)
}

// check_json checks if the JSON string is valid.
fn (mut checker Checker) check_json_format(val string) ! {
	// check if the JSON string is empty
	if val == '' {
		return checker.error('empty string')
	}

	// check if generic type matches the JSON type
	value_kind := get_value_kind(val[checker.idx])
	match value_kind {
		.unknown {
			return checker.error('unknown value kind')
		}
		.null {
			// assert false
			// check if the JSON string is a null value
			for letter in 'null' {
				if val[checker.idx] != letter {
					return checker.error('invalid null value')
				}
				checker.idx++
			}
		}
		.object {
			checker.idx++
			for val[checker.idx] != `}` {
				// check if the JSON string is an empty object
				if checker.end - checker.idx <= 2 {
					return
				}

				if val[checker.idx] != `"` {
					checker.idx++
				}

				// skip whitespace
				for val[checker.idx] in [` `, `\t`, `\n`] {
					if checker.idx >= checker.end - 1 {
						break
					}
					checker.idx++
				}

				if val[checker.idx] == `}` {
					return
				}

				match val[checker.idx] {
					`"` {
						// Object key
						checker.check_json_format(val)!

						for val[checker.idx] != `:` {
							if checker.idx >= checker.end - 1 {
								return checker.error('EOF error')
							}
							if val[checker.idx] !in [` `, `\t`, `\n`] {
								return checker.error('invalid value after object key')
							}
							checker.idx++
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
						return checker.error('`${[val[checker.idx]].bytestr()}` is an invalid object key')
					}
				}

				if val[checker.idx] != `:` {
					return checker.error('Expecting `:` after object key')
				}
				// skip `:`
				checker.idx++

				// skip whitespace
				for val[checker.idx] in [` `, `\t`, `\n`] {
					checker.idx++
				}

				match val[checker.idx] {
					`"`, `[`, `{`, `0`...`9`, `-`, `n`, `t`, `f` {
						for val[checker.idx] != `}` {
							if checker.idx >= checker.end - 1 {
								return checker.error('EOF error')
							}
							checker.check_json_format(val)!
							// whitespace
							for val[checker.idx] in [` `, `\t`, `\n`] {
								checker.idx++
							}
							if val[checker.idx] == `}` {
								break
							}
							if checker.idx >= checker.end - 1 {
								return checker.error('EOF error: braces are not closed')
							}

							if val[checker.idx] == `,` {
								checker.idx++
								for val[checker.idx] in [` `, `\t`, `\n`] {
									checker.idx++
								}
								if val[checker.idx] != `"` {
									return checker.error('Expecting object key')
								} else {
									break
								}
							} else {
								if val[checker.idx] == `}` {
									break
								} else {
									// return checker.error('`}` after value')
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
			if checker.idx < checker.end - 1 {
				checker.idx++
			}
		}
		.array {
			// check if the JSON string is an empty array
			if checker.end >= checker.idx + 2 {
				checker.idx++
				if val[checker.idx] == `]` {
					return
				}
			} else {
				return checker.error('EOF error: There are not enough leght for an array')
			}

			for val[checker.idx] != `]` {
				// skip whitespace
				for val[checker.idx] in [` `, `\t`, `\n`] {
					if checker.idx >= checker.end - 1 {
						break
					}
					checker.idx++
				}

				if val[checker.idx] == `]` {
					return
				}

				if checker.idx >= checker.end - 1 {
					return checker.error('EOF error')
				}

				checker.check_json_format(val)!

				// whitespace
				for val[checker.idx] in [` `, `\t`, `\n`] {
					checker.idx++
				}
				if val[checker.idx] == `]` {
					break
				}
				if checker.idx >= checker.end - 1 {
					return checker.error('EOF error: braces are not closed')
				}

				if val[checker.idx] == `,` {
					checker.idx++
					for val[checker.idx] in [` `, `\t`, `\n`] {
						checker.idx++
					}
					if val[checker.idx] == `]` {
						return checker.error('Cannot use `,`, before `]`')
					}
					continue
				} else {
					if val[checker.idx] == `]` {
						break
					} else {
						return checker.error('`]` after value')
					}
				}
			}
			// skip `]`
			if checker.idx < checker.end - 1 {
				checker.idx++
			}
		}
		.string_ {
			// check if the JSON string is a valid string

			if checker.idx >= checker.end - 1 {
				return checker.error('EOF error: string not closed')
			}

			checker.idx++

			if val[checker.idx] == `"` {
				return
			}

			// check if the JSON string is a valid escape sequence
			for val[checker.idx] != `"` && val[checker.idx - 1] != `\\` {
				if val[checker.idx] == `\\` {
					if checker.idx + 1 >= checker.end - 1 {
						return checker.error('invalid escape sequence')
					}
					escaped_char := val[checker.idx + 1]
					match escaped_char {
						`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {}
						`u` {
							// check if the JSON string is a valid unicode escape sequence
							escaped_char_last_index := checker.idx + 5

							if escaped_char_last_index >= checker.end - 1 {
								for checker.idx < escaped_char_last_index {
									match val[checker.idx] {
										`0`...`9`, `a`...`f`, `A`...`F` {
											checker.idx++
										}
										else {
											return checker.error('invalid unicode escape sequence')
										}
									}
								}
								// REVIEW: Should we increment the index here?
								continue
							} else {
								return checker.error('short unicode escape sequence')
							}
						}
						else {
							return checker.error('unknown escape sequence')
						}
					}
				}
				checker.idx++
			}
			// `"` is the last character
			if checker.idx < checker.end - 1 {
				checker.idx++
			}
		}
		.number {
			// check if the JSON string is a valid float or integer
			mut is_negative := val[0] == `-`
			mut has_dot := false

			mut digits_count := 0

			for val[checker.idx] !in [`,`, `}`, `]`] && checker.idx < checker.end - 1 {
				digits_count++

				// skip whitespace
				for val[checker.idx] in [` `, `\t`, `\n`] {
					checker.idx++
				}
				if val[checker.idx] == `.` {
					if has_dot {
						return checker.error('invalid float. Multiple dots')
					}
					has_dot = true
					checker.idx++
					continue
				} else if val[checker.idx] == `-` {
					if is_negative {
						return checker.error('invalid float. Multiple negative signs')
					}
					checker.idx++
					continue
				} else {
					if val[checker.idx] < `0` || val[checker.idx] > `9` {
						return checker.error('invalid number')
					}
				}

				if digits_count >= 64 {
					return checker.error('number exeeds 64 digits')
				}

				checker.idx++
			}
		}
		.boolean {
			// check if the JSON string is a valid boolean
			match val[checker.idx] {
				`t` {
					for letter in 'rue' {
						checker.idx++
						if val[checker.idx] != letter {
							return checker.error('invalid boolean')
						}
					}
				}
				`f` {
					for letter in 'alse' {
						checker.idx++
						if val[checker.idx] != letter {
							return checker.error('invalid boolean')
						}
					}
				}
				else {
					return checker.error('invalid boolean')
				}
			}
			if checker.idx < checker.end - 1 {
				checker.idx++
			}
		}
	}

	for checker.idx < checker.end - 1 && val[checker.idx] !in [`,`, `:`, `}`, `]`] {
		// get trash characters after the value
		if val[checker.idx] !in [` `, `\t`, `\n`] {
			last_new_line := val.last_index_u8(`\n`)
			mut error_message := '\n'
			if last_new_line != -1 {
				error_message += val[last_new_line + 1..checker.idx]
			} else {
				error_message += val[0..checker.idx]
			}
			error_message += [val[checker.idx]].bytestr()
			error_message += '\n'

			if last_new_line != -1 {
				error_message += ' '.repeat(checker.idx - last_new_line)
			} else {
				error_message += ' '.repeat(checker.idx)
			}
			error_message += '^ invalid value. Unexpected character after ${value_kind} end'
			return error(error_message)
		} else {
			// whitespace
			checker.idx++
		}
	}
}

// decode decodes a JSON string into a specified type.
pub fn decode[T](val string) !T {
	mut checker := Checker{
		idx:  0
		end:  val.len
		json: val
	}
	checker.check_json_format(val)!
	check_if_json_match[T](val)!

	mut decoder := Decoder{
		json: val
	}

	mut result := T{}
	decoder.decode_value(&result)
	return result
}

// decode_value decodes a value from the JSON nodes.
fn (mut decoder Decoder) decode_value[T](val &T) {
	$if val is $option {
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
	} $else $if T is $enum {
	} $else $if T is $int {
	} $else $if T is $float {
	} $else $if T is bool {
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
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

fn precalculate_string_decoded_space(encoded string) !int {
	if encoded.len < 2 || encoded[0] != `"` || encoded[encoded.len - 1] != `"` {
		return error('Invalid JSON string format')
	}

	mut space_required := 0
	mut idx := 1 // Start after the opening quote

	for idx < encoded.len - 1 {
		current_byte := encoded[idx]

		if current_byte == `\\` {
			// Escape sequence, handle accordingly
			idx++
			if idx >= encoded.len - 1 {
				return error('Invalid escape sequence at the end of string')
			}
			escaped_char := encoded[idx]
			match escaped_char {
				// All simple escapes take 1 byte of space
				`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {
					space_required++
				}
				`u` {
					// Unicode escape sequence \uXXXX
					if idx + 4 >= encoded.len - 1 {
						return error('Invalid unicode escape sequence')
					}
					// Extract the hex value from the \uXXXX sequence
					hex_str := encoded.substr(idx + 1, idx + 5)
					unicode_value := u32(strconv.parse_int(hex_str, 16, 32)!)
					// Determine the number of bytes needed for this Unicode character in UTF-8
					space_required += utf8_byte_length(unicode_value)
					idx += 4 // Skip the next 4 hex digits
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

	return space_required
}
