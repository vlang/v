module errors

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
		return formatted_source_error(kind, message, file, pos)
	}
	if int(node) < 0 || int(node) >= a.nodes.len {
		return '${kind} ${message}'
	}
	n := a.nodes[int(node)]
	file := a.source_files[n.pos.id] or { return '${kind} ${message}' }
	return formatted_source_error(kind, message, file, n.pos)
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
			start_column := int_max(0, int_min(position.column - 1, line.len))
			line_start := file.line_start(position.line)
			span_end := int_max(pos.offset + 1, pos.end)
			end_column := int_max(start_column + 1, int_min(span_end - line_start, line.len))
			mut pointer := strings.new_builder(line.len + 8)
			for i := 0; i < start_column; i++ {
				pointer.write_u8(if line[i] == `\t` { `\t` } else { ` ` })
			}
			underline_len := int_max(1, end_column - start_column)
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

fn relative_error_path(path string) string {
	mut normalized := os.real_path(path).replace('\\', '/')
	workdir := os.getwd().replace('\\', '/').trim_right('/') + '/'
	if normalized.starts_with(workdir) {
		normalized = normalized[workdir.len..]
	}
	return normalized
}
