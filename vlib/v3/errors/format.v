module errors

import encoding.utf8.east_asian
import os
import strings
import v3.flat
import v3.token

const source_context_before = 2
const source_context_after = 2

// formatted_error renders a compiler diagnostic with v1-compatible source context.
pub fn formatted_error(kind string, message string, a &flat.FlatAst, node flat.NodeId, pos token.Pos) string {
	if pos.is_valid() {
		file := a.source_files[pos.id] or { return '${kind} ${message}' }
		action_message := if action := a.template_actions[pos.id] {
			'${message} (veb action: ${action})'
		} else {
			message
		}
		result := formatted_source_error(kind, action_message, file, pos)
		return append_template_call_stack(result, kind, a, pos)
	}
	if int(node) < 0 || int(node) >= a.nodes.len {
		return '${kind} ${message}'
	}
	n := a.nodes[int(node)]
	file := a.source_files[n.pos.id] or { return '${kind} ${message}' }
	action_message := if action := a.template_actions[n.pos.id] {
		'${message} (veb action: ${action})'
	} else {
		message
	}
	result := formatted_source_error(kind, action_message, file, n.pos)
	return append_template_call_stack(result, kind, a, n.pos)
}

// formatted_parser_error renders a parser diagnostic with template call-site context.
pub fn formatted_parser_error(message string, a &flat.FlatAst, pos token.Pos) string {
	return formatted_parser_diagnostic('error:', message, a, pos)
}

// formatted_parser_diagnostic renders a parser diagnostic with template call-site context.
pub fn formatted_parser_diagnostic(kind string, message string, a &flat.FlatAst, pos token.Pos) string {
	file := a.source_files[pos.id] or { return '${kind} ${message}' }
	result := formatted_source_error(kind, message, file, pos)
	return append_template_call_stack(result, kind, a, pos)
}

fn append_template_call_stack(result string, kind string, a &flat.FlatAst, pos token.Pos) string {
	mut output := result
	mut current := pos
	mut visited := map[int]bool{}
	for {
		if current.id in visited {
			break
		}
		visited[current.id] = true
		call_pos := a.template_call_sites[current.id] or { break }
		if call_file := a.source_files[call_pos.id] {
			position := call_file.position(call_pos)
			path := relative_error_path(call_file.name)
			context_full := formatted_source_error(kind, '', call_file, call_pos)
			context := context_full.all_after_first('\n')
			output += '\ncalled from ${path}:${position.line}:${position.column}'
			if context.len > 0 {
				output += '\n${context}'
			}
		}
		current = call_pos
	}
	return output
}

// formatted_source_error renders a diagnostic for a source file and byte span.
pub fn formatted_source_error(kind string, message string, file &token.File, pos token.Pos) string {
	position := file.position(pos)
	path := relative_error_path(file.name)
	mut result := strings.new_builder(message.len + 256)
	reported_column := if pos.reported_column > 0 {
		pos.reported_column
	} else {
		position.column
	}
	result.writeln('${path}:${position.line}:${reported_column}: ${kind} ${message}')
	source := os.read_file(file.name) or { return result.str().trim_right('\n') }
	lines := source.split_into_lines()
	if lines.len == 0 {
		return result.str().trim_right('\n')
	}
	first_line := int_max(1, position.line - source_context_before)
	last_line := int_min(lines.len, position.line + source_context_after)
	for line_number := first_line; line_number <= last_line; line_number++ {
		line := lines[line_number - 1]
		result.writeln('${line_number:5d} | ${line.replace('\t', '    ')}')
		if line_number == position.line {
			line_start := file.line_start(position.line)
			start_byte := int_max(0, int_min(pos.offset - line_start, line.len))
			span_end := int_max(pos.offset + 1, pos.end)
			end_byte := int_min(line.len, int_max(start_byte + 1, int_min(span_end - line_start,
				line.len)))
			mut pointer := strings.new_builder(line.len + 8)
			prefix := line[..start_byte].replace('\t', '    ')
			pointer.write_string(' '.repeat(diagnostic_display_width(prefix)))
			underline_len := int_max(1, diagnostic_display_width(line[start_byte..end_byte]))
			pointer.write_string(if underline_len > 1 {
				'~'.repeat(underline_len)
			} else {
				'^'
			})
			result.writeln('      | ${pointer.str().replace('\t', '    ')}')
		}
	}
	return result.str().trim_right('\n')
}

fn diagnostic_display_width(text string) int {
	mut width := 0
	mut valid_start := 0
	mut i := 0
	for i < text.len {
		sequence_len := valid_utf8_sequence_len(text, i)
		if sequence_len > 0 {
			i += sequence_len
			continue
		}
		if valid_start < i {
			width += east_asian.display_width(text[valid_start..i], 1)
		}
		width++
		i++
		valid_start = i
	}
	if valid_start < text.len {
		width += east_asian.display_width(text[valid_start..], 1)
	}
	return width
}

fn valid_utf8_sequence_len(text string, index int) int {
	first := text[index]
	if first < 0x80 {
		return 1
	}
	mut length := 0
	mut second_min := u8(0x80)
	mut second_max := u8(0xbf)
	if first >= 0xc2 && first <= 0xdf {
		length = 2
	} else if first >= 0xe0 && first <= 0xef {
		length = 3
		if first == 0xe0 {
			second_min = 0xa0
		} else if first == 0xed {
			second_max = 0x9f
		}
	} else if first >= 0xf0 && first <= 0xf4 {
		length = 4
		if first == 0xf0 {
			second_min = 0x90
		} else if first == 0xf4 {
			second_max = 0x8f
		}
	} else {
		return 0
	}
	if index + length > text.len || text[index + 1] < second_min || text[index + 1] > second_max {
		return 0
	}
	for i in index + 2 .. index + length {
		if text[i] < 0x80 || text[i] > 0xbf {
			return 0
		}
	}
	return length
}

fn relative_error_path(path string) string {
	mut normalized := os.real_path(path).replace('\\', '/')
	if os.getenv('VERROR_PATHS') == 'absolute' {
		return normalized
	}
	workdir := os.getwd().replace('\\', '/').trim_right('/') + '/'
	if normalized.starts_with(workdir) {
		normalized = normalized[workdir.len..]
	}
	return normalized
}
