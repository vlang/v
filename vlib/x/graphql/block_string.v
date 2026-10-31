module graphql

// @internal
// Produces the value of a block string from its parsed raw value.
// This implements the GraphQL spec's BlockStringValue() static algorithm.
fn dedent_block_string_lines(lines []string) []string {
	mut common_indent := max_i64
	mut first_non_empty_line := 0
	mut last_non_empty_line := -1

	for i := 0; i < lines.len; i++ {
		line := lines[i]
		indent := leading_whitespace(line)

		if indent == line.len {
			continue // skip empty lines
		}

		if first_non_empty_line == 0 {
			first_non_empty_line = i
		}

		last_non_empty_line = i

		if i != 0 && indent < common_indent {
			common_indent = indent
		}
	}

	new_lines := lines.map(fn [lines, common_indent] (line string) string {
		if lines.index(line) == 0 {
			return line
		} else {
			// EDGE case: sometimes commonIndent can be greater than the bounds here. IN JS if you
			// call string.slice(number_greater_than_list_len) it will return an empty string but
			// not in Vlang (index out of bounds)
			if common_indent >= line.len {
				return ''
			}
			return line[common_indent..]
		}
	})

	// println(new_lines)

	return new_lines[first_non_empty_line..last_non_empty_line + 1]
}

// Helper function to find leading whitespace in a string.
fn leading_whitespace(str string) int {
	mut counter := 0

	for i := 0; counter < str.len && is_white_space(str[counter]); i++ {
		counter += i
	}
	return counter
}

// Check if a string is printable as a block string.
pub fn is_printable_as_block_string(value string) bool {
	if value == '' {
		return true // empty string is printable
	}

	mut is_empty_line := true
	mut has_indent := false
	mut has_common_indent := true
	mut seen_non_empty_line := false

	for c in value.runes() {
		match c {
			0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x000b, 0x000c,
			0x000e, 0x000f {
				return false // Has non-printable characters
			}
			0x000d {
				return false // Has \r or \r\n which will be replaced as \n
			}
			10 {
				if is_empty_line && !seen_non_empty_line {
					return false // Has leading new line
				}
				seen_non_empty_line = true
				is_empty_line = true
				has_indent = false
			}
			9, 32 {
				has_indent = is_empty_line
			}
			else {
				has_common_indent = has_indent
				is_empty_line = false
			}
		}
	}

	if is_empty_line {
		return false // Has trailing empty lines
	}

	if has_common_indent && seen_non_empty_line {
		return false // Has internal indent
	}

	return true
}

pub struct BlockStringOptions {
pub:
	minimize bool
}

// Print a block string in the indented block form by adding a leading and trailing blank line.
// However, if a block string starts with whitespace and is a single-line, adding a leading blank line would strip that whitespace.
pub fn print_block_string(value string, options ?BlockStringOptions) ?string {
	escaped_value := value.replace('"""', '\\"""')
	lines := escaped_value.split('\r\n|\n|\r')

	// Expand a block string's raw value into independent lines.
	is_single_line := lines.len == 1

	// If common indentation is found we can fix some of those cases by adding leading new line
	force_leading_new_line := lines.len > 1 && lines[0..].all(it.len == 0 || is_white_space(it[0]))
	// lines[1..] |line| {
	//     len(line) == 0 or is_white_space(line[0] as int)
	// }

	//     // Trailing triple quotes just look confusing but doesn't force trailing new line
	has_trailing_triple_quotes := escaped_value.ends_with('\\"""')

	//     // Trailing quote (single or double) or slash forces trailing new line
	has_trailing_quote := value.ends_with('"') && !has_trailing_triple_quotes
	has_trailing_slash := value.ends_with('\\')
	force_trailing_newline := has_trailing_quote || has_trailing_slash

	// add leading and trailing new lines only if it improves readability
	print_as_multiple_lines := !options?.minimize && (!is_single_line
		|| value.len > 70 || force_trailing_newline || force_leading_new_line
		|| has_trailing_triple_quotes)

	mut result := ''

	// Format a multi-line block quote to account for leading space.
	skip_leading_new_line := is_single_line && is_white_space(value[0])
	if (print_as_multiple_lines && !skip_leading_new_line) || force_leading_new_line {
		result += '\n'
	}

	result += escaped_value
	if print_as_multiple_lines || force_trailing_newline {
		result += '\n'
	}

	return '"""' + result + '"""'
}
