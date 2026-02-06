module json2

// increment checks eof and increments checker by one
@[inline]
fn (mut checker Decoder) increment(message string) ! {
	if checker.checker_idx + 1 == checker.json.len {
		if message == '' {
			return Error{}
		}
		checker.checker_error('EOF: ' + message)!
	}
	checker.checker_idx++
}

// skip_whitespace checks eof and increments checker until next non whitespace character
@[inline]
fn (mut checker Decoder) skip_whitespace(message string) ! {
	for checker.json[checker.checker_idx] in whitespace_chars {
		checker.increment(message)!
	}
}

// check_json_format checks if the JSON string is valid and updates the decoder state.
fn (mut checker Decoder) check_json_format() ! {
	checker.skip_whitespace('empty json')!

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

	checker.increment('') or { return }
	checker.skip_whitespace('') or { return }

	if checker.json[checker.checker_idx] !in [`,`, `:`, `}`, `]`] {
		checker.checker_error('invalid value. Unexpected character after ${actual_value_info_pointer.value_kind} end')!
	}
}

fn (mut checker Decoder) check_string() ! {
	checker.increment('string not closed')!

	// check if the JSON string is a valid escape sequence
	for checker.json[checker.checker_idx] != `"` {
		if checker.json[checker.checker_idx] == `\\` {
			checker.increment('invalid escape sequence')!
			escaped_char := checker.json[checker.checker_idx]
			match escaped_char {
				`/`, `b`, `f`, `n`, `r`, `t`, `"`, `\\` {}
				`u` {
					// check if the JSON string is a valid unicode escape sequence
					escaped_char_last_index := checker.checker_idx + 4

					if escaped_char_last_index < checker.json.len {
						// 2 bytes for the unicode escape sequence `\u`
						checker.increment('invalid escape sequence')!

						for checker.checker_idx < escaped_char_last_index {
							match checker.json[checker.checker_idx] {
								`0`...`9`, `a`...`f`, `A`...`F` {
									checker.increment('invalid unicode escape sequence')!
								}
								else {
									return checker.checker_error('invalid unicode escape sequence')
								}
							}
						}
						continue
					} else {
						return checker.checker_error('short unicode escape sequence ${checker.json[checker.checker_idx - 1..checker.json.len - 1]}')
					}
				}
				else {
					return checker.checker_error('unknown escape sequence')
				}
			}
		}
		checker.increment('string not closed')!
	}
}

fn (mut checker Decoder) check_number() ! {
	// check if the JSON string is a valid float or integer
	if checker.json[checker.checker_idx] == `-` {
		checker.increment('expected digit')!
	}

	// integer part
	if checker.json[checker.checker_idx] == `0` {
		checker.increment('') or { return }
	} else if checker.json[checker.checker_idx] >= `1` && checker.json[checker.checker_idx] <= `9` {
		checker.increment('') or { return }

		for checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9` {
			checker.increment('') or { return }
		}
	} else {
		return checker.checker_error('expected digit got ${checker.json[checker.checker_idx].ascii_str()}')
	}

	// fraction part
	if checker.json[checker.checker_idx] == `.` {
		checker.increment('expected digit')!

		if !(checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9`) {
			return checker.checker_error('expected digit got ${checker.json[checker.checker_idx].ascii_str()}')
		}

		for checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9` {
			checker.increment('') or { return }
		}
	}

	// exponent part
	if checker.json[checker.checker_idx] == `e` || checker.json[checker.checker_idx] == `E` {
		checker.increment('expected digit')!

		if checker.json[checker.checker_idx] == `-` || checker.json[checker.checker_idx] == `+` {
			checker.increment('expected digit')!
		}

		if !(checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9`) {
			return checker.checker_error('expected digit got ${checker.json[checker.checker_idx].ascii_str()}')
		}

		for checker.json[checker.checker_idx] >= `0` && checker.json[checker.checker_idx] <= `9` {
			checker.increment('') or { return }
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
	checker.increment('expected array end')!

	checker.skip_whitespace('expected array end')!

	for checker.json[checker.checker_idx] != `]` {
		checker.check_json_format()!

		checker.skip_whitespace('expected array end')!

		if checker.json[checker.checker_idx] == `,` {
			checker.increment('expected array value')!
			checker.skip_whitespace('') or {}

			if checker.json[checker.checker_idx] == `]` {
				return checker.checker_error('Cannot use `,`, before `]`')
			}
		}
	}
}

fn (mut checker Decoder) check_object() ! {
	checker.increment('expected object end')!

	checker.skip_whitespace('expected object end')!

	for checker.json[checker.checker_idx] != `}` {
		if checker.json[checker.checker_idx] != `"` {
			checker.checker_error('Expecting object key')!
		}

		checker.check_json_format()!

		checker.skip_whitespace('expected `:`')!

		if checker.json[checker.checker_idx] != `:` {
			checker.checker_error('expected `:`, got `${checker.json[checker.checker_idx].ascii_str()}`')!
		}

		checker.increment('expected object value')!

		checker.skip_whitespace('expected object value')!

		checker.check_json_format()!

		checker.skip_whitespace('expected object end')!

		if checker.json[checker.checker_idx] == `,` {
			checker.increment('expected object key')!
			checker.skip_whitespace('') or {}

			if checker.json[checker.checker_idx] == `}` {
				return checker.checker_error('Cannot use `,`, before `}`')
			}
		}
	}
}
