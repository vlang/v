module graphql

import regex

// LineRegExp regex pattern to match line breaks
const line_reg_exp = r'/\r\n|[\n\r]/g'

struct SourceLocation {
pub:
	line   i64 = 1
	column i64 = 1
}

fn compile_and_get_rex(source string) ![]string {
	mut re := regex.regex_opt(graphql.line_reg_exp)!

	return re.find_all_str(source)
}

// getLocation takes a Source and a UTF-8 character offset,
// and returns the corresponding line and column as a SourceLocation.
pub fn get_location(source Source, position int) !SourceLocation {
	mut last_line_start := 0
	mut line := 1

	for i, matched in compile_and_get_rex(source.body)! {
		//     // invariant @match.index is int
		// println('>>>>>>> MATCHED LENGTH ${matched.len} (${matched})')

		if i >= position {
			break
		}

		last_line_start = i + matched.len
		line += 1
	}

	return SourceLocation{
		line: line
		column: position + 1 - last_line_start
	}
}
