module decoder2

// check_json_format checks if the JSON string is valid and updates the decoder state.
fn (mut checker Decoder) check_json_format() ! {
	// skip whitespace
	for checker.json[checker.checker_idx] in whitespace_chars {
		if checker.checker_idx == checker.json.len {
			break
		}
		checker.checker_idx++
	}

	start_idx_position := checker.checker_idx

	mut actual_value_info_pointer := unsafe { &ValueInfo(nil) }

	match checker.json[checker.checker_idx] {
		`"` {
			checker.values_info.push(ValueInfo{
				position:   checker.checker_idx
				value_kind: .string
			})

			actual_value_info_pointer = checker.values_info.last()

			checker.check_string()!
		}
		`-`, `0`...`9` {
			checker.values_info.push(ValueInfo{
				position:   checker.checker_idx
				value_kind: .number
			})

			actual_value_info_pointer = checker.values_info.last()

			checker.check_number()!
		}
		`t`, `f` {
			checker.values_info.push(ValueInfo{
				position:   checker.checker_idx
				value_kind: .boolean
			})

			actual_value_info_pointer = checker.values_info.last()

			checker.check_boolean()!
		}
		`n` {
			checker.values_info.push(ValueInfo{
				position:   checker.checker_idx
				value_kind: .null
			})

			actual_value_info_pointer = checker.values_info.last()

			checker.check_null()!
		}
		`[` {
			checker.values_info.push(ValueInfo{
				position:   checker.checker_idx
				value_kind: .array
			})

			actual_value_info_pointer = checker.values_info.last()

			checker.check_array()!
		}
		`{` {
			checker.values_info.push(ValueInfo{
				position:   checker.checker_idx
				value_kind: .object
			})

			actual_value_info_pointer = checker.values_info.last()

			checker.check_object()!
		}
		else {
			checker.checker_error('unknown value kind')!
		}
	}

	actual_value_info_pointer.length = checker.checker_idx + 1 - start_idx_position

	if checker.checker_idx < checker.json.len {
		checker.checker_idx++
	}

	for checker.checker_idx < checker.json.len
		&& checker.json[checker.checker_idx] !in [`,`, `:`, `}`, `]`] {
		// get trash characters after the value
		if checker.json[checker.checker_idx] !in whitespace_chars {
			checker.checker_error('invalid value. Unexpected character after ${actual_value_info_pointer.value_kind} end')!
		} else {
			// whitespace
		}
		checker.checker_idx++
	}
}

fn (mut checker Decoder) check_string() ! {
	// check if the JSON string is a valid string
	if checker.checker_idx == checker.json.len {
		checker.checker_idx--
		return checker.checker_error('EOF error: string not closed')
	}

	checker.checker_idx++

	// check if the JSON string is a valid escape sequence
	for checker.json[checker.checker_idx] != `"` {
		if checker.json[checker.checker_idx] == `\\` {
			if checker.checker_idx + 1 >= checker.json.len - 1 {
				return checker.checker_error('invalid escape sequence')
			}
			escaped_char := checker.json[checker.checker_idx + 1]
			match escaped_char {
				`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {
					checker.checker_idx++ // make sure escaped quotation marks are skipped
				}
				`u` {
					// check if the JSON string is a valid unicode escape sequence
					escaped_char_last_index := checker.checker_idx + 5

					if escaped_char_last_index < checker.json.len {
						// 2 bytes for the unicode escape sequence `\u`
						checker.checker_idx += 2

						for checker.checker_idx < escaped_char_last_index {
							match checker.json[checker.checker_idx] {
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

fn (mut checker Decoder) check_number() ! {
	// check if the JSON string is a valid float or integer
	if checker.json[checker.checker_idx] == `-` {
		checker.checker_idx++
	}

	if checker.checker_idx == checker.json.len {
		checker.checker_idx--
		return checker.checker_error('expected digit got EOF')
	}

	// integer part
	if checker.json[checker.checker_idx] == `0` {
		checker.checker_idx++
	} else if checker.json[checker.checker_idx] >= `1` && checker.json[checker.checker_idx] <= `9` {
		checker.checker_idx++

		for checker.checker_idx < checker.json.len && checker.json[checker.checker_idx] >= `0`
			&& checker.json[checker.checker_idx] <= `9` {
			checker.checker_idx++
		}
	} else {
		return checker.checker_error('expected digit got ${checker.json[checker.checker_idx].ascii_str()}')
	}

	// fraction part
	if checker.checker_idx != checker.json.len && checker.json[checker.checker_idx] == `.` {
		checker.checker_idx++

		if checker.checker_idx == checker.json.len {
			checker.checker_idx--
			return checker.checker_error('expected digit got EOF')
		}

		if checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9` {
			for checker.checker_idx < checker.json.len && checker.json[checker.checker_idx] >= `0`
				&& checker.json[checker.checker_idx] <= `9` {
				checker.checker_idx++
			}
		} else {
			return checker.checker_error('expected digit got ${checker.json[checker.checker_idx].ascii_str()}')
		}
	}

	// exponent part
	if checker.checker_idx != checker.json.len
		&& (checker.json[checker.checker_idx] == `e` || checker.json[checker.checker_idx] == `E`) {
		checker.checker_idx++

		if checker.checker_idx == checker.json.len {
			checker.checker_idx--
			return checker.checker_error('expected digit got EOF')
		}

		if checker.json[checker.checker_idx] == `-` || checker.json[checker.checker_idx] == `+` {
			checker.checker_idx++

			if checker.checker_idx == checker.json.len {
				checker.checker_idx--
				return checker.checker_error('expected digit got EOF')
			}
		}

		if checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9` {
			for checker.checker_idx < checker.json.len && checker.json[checker.checker_idx] >= `0`
				&& checker.json[checker.checker_idx] <= `9` {
				checker.checker_idx++
			}
		} else {
			return checker.checker_error('expected digit got ${checker.json[checker.checker_idx].ascii_str()}')
		}
	}

	checker.checker_idx--
}

fn (mut checker Decoder) check_boolean() ! {
	// check if the JSON string is a valid boolean
	match checker.json[checker.checker_idx] {
		`t` {
			if checker.json.len - checker.checker_idx <= 3 {
				return checker.checker_error('EOF error: expecting `true`')
			}

			is_not_ok := unsafe {
				vmemcmp(checker.json.str + checker.checker_idx, true_in_string.str, true_in_string.len)
			}

			if is_not_ok != 0 {
				return checker.checker_error('invalid boolean value. Got `${checker.json[checker.checker_idx..
					checker.checker_idx + 4]}` instead of `true`')
			}
			checker.checker_idx += 3
		}
		`f` {
			if checker.json.len - checker.checker_idx <= 4 {
				return checker.checker_error('EOF error: expecting `false`')
			}

			is_not_ok := unsafe {
				vmemcmp(checker.json.str + checker.checker_idx, false_in_string.str, false_in_string.len)
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

fn (mut checker Decoder) check_null() ! {
	// check if the JSON string is a null value
	if checker.json.len - checker.checker_idx <= 3 {
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

fn (mut checker Decoder) check_array() ! {
	// check if the JSON string is an empty array
	if checker.json.len >= checker.checker_idx + 2 {
		checker.checker_idx++
	} else {
		return checker.checker_error('EOF error: There are not enough length for an array')
	}

	for checker.json[checker.checker_idx] != `]` {
		// skip whitespace
		for checker.json[checker.checker_idx] in whitespace_chars {
			if checker.checker_idx == checker.json.len {
				checker.checker_idx--
				break
			}
			checker.checker_idx++
		}

		if checker.json[checker.checker_idx] == `]` {
			break
		}

		if checker.checker_idx == checker.json.len {
			checker.checker_idx--
			return checker.checker_error('EOF error: array not closed')
		}

		checker.check_json_format()!

		// whitespace
		for checker.json[checker.checker_idx] in whitespace_chars {
			checker.checker_idx++
		}
		if checker.json[checker.checker_idx] == `]` {
			break
		}
		if checker.checker_idx == checker.json.len {
			checker.checker_idx--
			return checker.checker_error('EOF error: braces are not closed')
		}

		if checker.json[checker.checker_idx] == `,` {
			checker.checker_idx++
			for checker.json[checker.checker_idx] in whitespace_chars {
				checker.checker_idx++
			}
			if checker.json[checker.checker_idx] == `]` {
				return checker.checker_error('Cannot use `,`, before `]`')
			}
			continue
		} else {
			if checker.json[checker.checker_idx] == `]` {
				break
			} else {
				return checker.checker_error('`]` after value')
			}
		}
	}
}

fn (mut checker Decoder) check_object() ! {
	if checker.json.len - checker.checker_idx < 2 {
		return checker.checker_error('EOF error: expecting a complete object after `{`')
	}
	checker.checker_idx++
	for checker.json[checker.checker_idx] != `}` {
		// skip whitespace
		for checker.json[checker.checker_idx] in whitespace_chars {
			if checker.checker_idx == checker.json.len {
				checker.checker_idx--
				break
			}
			checker.checker_idx++
		}

		if checker.json[checker.checker_idx] == `}` {
			continue
		}

		if checker.json[checker.checker_idx] != `"` {
			return checker.checker_error('Expecting object key')
		}

		// Object key
		checker.check_json_format()!

		for checker.json[checker.checker_idx] != `:` {
			if checker.checker_idx == checker.json.len {
				checker.checker_idx--
				return checker.checker_error('EOF error: key colon not found')
			}
			if checker.json[checker.checker_idx] !in whitespace_chars {
				return checker.checker_error('invalid value after object key')
			}
			checker.checker_idx++
		}

		if checker.json[checker.checker_idx] != `:` {
			return checker.checker_error('Expecting `:` after object key')
		}

		// skip `:`
		checker.checker_idx++

		// skip whitespace
		for checker.json[checker.checker_idx] in whitespace_chars {
			checker.checker_idx++
		}

		match checker.json[checker.checker_idx] {
			`"`, `[`, `{`, `0`...`9`, `-`, `n`, `t`, `f` {
				checker.check_json_format()!

				if checker.checker_idx == checker.json.len {
					checker.checker_idx--
					return checker.checker_error('EOF error: braces are not closed')
				}

				// whitespace
				for checker.json[checker.checker_idx] in whitespace_chars {
					checker.checker_idx++
				}
				if checker.json[checker.checker_idx] == `}` {
					break
				}

				if checker.checker_idx == checker.json.len {
					checker.checker_idx--
					return checker.checker_error('EOF error: braces are not closed')
				}

				if checker.json[checker.checker_idx] == `,` {
					checker.checker_idx++

					if checker.checker_idx == checker.json.len {
						checker.checker_idx--
						return checker.checker_error('EOF error: Expecting object key after `,`')
					}

					for checker.json[checker.checker_idx] in whitespace_chars {
						checker.checker_idx++
					}
					if checker.json[checker.checker_idx] != `"` {
						return checker.checker_error('Expecting object key after `,`')
					}
				} else {
					if checker.json[checker.checker_idx] == `}` {
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
