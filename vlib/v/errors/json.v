@[has_globals]
module errors

import strings
import v.flat
import v.token

__global json_output_enabled = false

// set_json_output selects the machine-readable form of the compiler diagnostics (`-json-errors`).
pub fn set_json_output(enabled bool) {
	json_output_enabled = enabled
}

// json_output reports whether the compiler diagnostics are printed as JSON.
pub fn json_output() bool {
	return json_output_enabled
}

// json_error renders a compiler diagnostic as one line of JSON. It locates the diagnostic
// like `formatted_error` does, so both forms report the same file, line and column.
pub fn json_error(kind string, message string, details []string, a &flat.FlatAst, node flat.NodeId, pos token.Pos) string {
	mut source_pos := pos
	if !pos.is_valid() {
		if int(node) < 0 || int(node) >= a.nodes.len {
			return json_message(kind, message, details)
		}
		source_pos = a.nodes[int(node)].pos
	}
	file := a.source_files[source_pos.id] or { return json_message(kind, message, details) }
	action_message := if action := a.template_actions[source_pos.id] {
		'${message} (veb action: ${action})'
	} else {
		message
	}
	return json_source_error(kind, action_message, details, file, source_pos, template_call_positions(a,
		source_pos), a)
}

// json_parser_diagnostic renders a parser diagnostic as one line of JSON.
pub fn json_parser_diagnostic(kind string, message string, details []string, a &flat.FlatAst, pos token.Pos) string {
	file := a.source_files[pos.id] or { return json_message(kind, message, details) }
	return json_source_error(kind, message, details, file, pos, template_call_positions(a, pos), a)
}

// json_located_message renders a diagnostic of a file that the compiler holds no source of.
// Its span is the single column that was reported.
pub fn json_located_message(kind string, message string, details []string, path string, line int, column int) string {
	mut out := strings.new_builder(message.len + path.len + 96)
	out.write_string('{')
	write_json_location(mut out, path, line, column, line, column + 1)
	out.write_string(',')
	write_json_message(mut out, kind, message, details)
	out.write_string('}')
	return out.str()
}

// json_message renders a diagnostic that has no source position as one line of JSON.
pub fn json_message(kind string, message string, details []string) string {
	mut out := strings.new_builder(message.len + 64)
	out.write_string('{')
	write_json_message(mut out, kind, message, details)
	out.write_string('}')
	return out.str()
}

fn json_source_error(kind string, message string, details []string, file &token.File, pos token.Pos, call_positions []token.Pos, a &flat.FlatAst) string {
	// Like the text form, the location follows the `#line` directives of the file.
	start := file.logical_position(pos)
	column := if pos.reported_column() > 0 { pos.reported_column() } else { start.column }
	// A span is half-open; an empty one covers the byte it points at, as the caret of the
	// text form does.
	end := file.logical_position_at(int_max(pos.offset + 1, pos.end))
	// A span that a `#line` directive cuts off in another logical file, or before its
	// start, is just the column of its start.
	mut end_line := end.line
	mut end_column := if end.line == start.line {
		column + end.column - start.column
	} else {
		end.column
	}
	if end.filename != start.filename || end.line < start.line {
		end_line = start.line
		end_column = column + 1
	}
	mut out := strings.new_builder(message.len + start.filename.len + 128)
	out.write_string('{')
	write_json_location(mut out, relative_error_path(start.filename), start.line, column, end_line,
		end_column)
	out.write_string(',')
	write_json_message(mut out, kind, message, details)
	mut has_call_sites := false
	for call_pos in call_positions {
		call_file := a.source_files[call_pos.id] or { continue }
		call := call_file.logical_position(call_pos)
		out.write_string(if has_call_sites { ',' } else { ',"called_from":[' })
		has_call_sites = true
		out.write_string('{"file":"${json_escape(relative_error_path(call.filename))}","line":${call.line},"col":${call.column}}')
	}
	if has_call_sites {
		out.write_string(']')
	}
	out.write_string('}')
	return out.str()
}

fn write_json_location(mut out strings.Builder, path string, line int, column int, end_line int, end_column int) {
	out.write_string('"file":"${json_escape(path)}","line":${line},"col":${column},"end_line":${end_line},"end_col":${end_column}')
}

// write_json_message writes the members that every diagnostic has. A kind is the label of
// the text form, like `error:`, `warning:`, `notice:` or `builder error:`. `severity` is
// always `error`, `warning` or `notice`; a label that says more than that is kept as `label`.
fn write_json_message(mut out strings.Builder, kind string, message string, details []string) {
	label := kind.trim_right(':')
	severity := match label {
		'warning' { 'warning' }
		'notice' { 'notice' }
		else { 'error' }
	}
	out.write_string('"severity":"${severity}"')
	if label != severity {
		out.write_string(',"label":"${json_escape(label)}"')
	}
	out.write_string(',"message":"${json_escape(message)}"')
	if details.len > 0 {
		out.write_string(',"details":"${json_escape(details.join('\n'))}"')
	}
}

// json_escape escapes text for a JSON string. Valid UTF-8 passes through, so source text
// stays readable. A byte that is not part of a valid sequence, as in a message quoting a
// malformed source file, becomes U+FFFD: a strict reader rejects the whole line otherwise.
fn json_escape(text string) string {
	mut out := strings.new_builder(text.len + 8)
	mut i := 0
	for i < text.len {
		c := text[i]
		if c >= 0x80 {
			sequence_len := valid_utf8_sequence_len(text, i)
			if sequence_len == 0 {
				out.write_string('\\ufffd')
				i++
				continue
			}
			out.write_string(text[i..i + sequence_len])
			i += sequence_len
			continue
		}
		match c {
			`"` {
				out.write_string('\\"')
			}
			`\\` {
				out.write_string('\\\\')
			}
			`\n` {
				out.write_string('\\n')
			}
			`\r` {
				out.write_string('\\r')
			}
			`\t` {
				out.write_string('\\t')
			}
			else {
				if c < 0x20 {
					out.write_string('\\u00')
					out.write_u8('0123456789abcdef'[int(c >> 4)])
					out.write_u8('0123456789abcdef'[int(c & 0xf)])
				} else {
					out.write_u8(c)
				}
			}
		}
		i++
	}
	return out.str()
}
