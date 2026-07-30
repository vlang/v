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
	mut i := 0
	for i < text.len {
		sequence_len := valid_utf8_sequence_len(text, i)
		if sequence_len > 0 {
			codepoint := diagnostic_utf8_codepoint(text, i, sequence_len)
			width += if diagnostic_codepoint_is_wide(codepoint) { 2 } else { 1 }
			i += sequence_len
			continue
		}
		width++
		i++
	}
	return width
}

@[direct_array_access]
fn diagnostic_utf8_codepoint(text string, index int, sequence_len int) u32 {
	first := u32(text[index])
	return match sequence_len {
		1 {
			first
		}
		2 {
			((first & 0x1f) << 6) | (u32(text[index + 1]) & 0x3f)
		}
		3 {
			((first & 0x0f) << 12) | ((u32(text[index + 1]) & 0x3f) << 6) | (u32(text[index + 2]) & 0x3f)
		}
		4 {
			((first & 0x07) << 18) | ((u32(text[index + 1]) & 0x3f) << 12) | ((u32(text[index + 2]) & 0x3f) << 6) | (u32(text[
				index + 3]) & 0x3f)
		}
		else {
			0
		}
	}
}

struct DiagnosticWideRange {
	start u32
	end   u32
}

// The formatter uses ambiguous-width 1. Keeping only the full/wide ranges
// avoids pulling the much larger general-purpose East Asian width tables into
// every V3 compiler binary.
const diagnostic_wide_ranges = [
	DiagnosticWideRange{0x1100, 0x1160},
	DiagnosticWideRange{0x231b, 0x231b},
	DiagnosticWideRange{0x232a, 0x232a},
	DiagnosticWideRange{0x23ea, 0x23ed},
	DiagnosticWideRange{0x23f0, 0x23f1},
	DiagnosticWideRange{0x23f3, 0x23f4},
	DiagnosticWideRange{0x25fe, 0x25fe},
	DiagnosticWideRange{0x2614, 0x2616},
	DiagnosticWideRange{0x2649, 0x2653},
	DiagnosticWideRange{0x267f, 0x2680},
	DiagnosticWideRange{0x2693, 0x2694},
	DiagnosticWideRange{0x26a1, 0x26a2},
	DiagnosticWideRange{0x26aa, 0x26ac},
	DiagnosticWideRange{0x26bd, 0x26bf},
	DiagnosticWideRange{0x26c4, 0x26c6},
	DiagnosticWideRange{0x26ce, 0x26cf},
	DiagnosticWideRange{0x26d4, 0x26d5},
	DiagnosticWideRange{0x26f3, 0x26f3},
	DiagnosticWideRange{0x26fb, 0x26fb},
	DiagnosticWideRange{0x26fd, 0x26fe},
	DiagnosticWideRange{0x270b, 0x270b},
	DiagnosticWideRange{0x2729, 0x2729},
	DiagnosticWideRange{0x274c, 0x274f},
	DiagnosticWideRange{0x2753, 0x2756},
	DiagnosticWideRange{0x2796, 0x2797},
	DiagnosticWideRange{0x27c0, 0x27c0},
	DiagnosticWideRange{0x2b1b, 0x2b1d},
	DiagnosticWideRange{0x2b50, 0x2b51},
	DiagnosticWideRange{0x2e80, 0x2ef4},
	DiagnosticWideRange{0x2f00, 0x2fd6},
	DiagnosticWideRange{0x2ff0, 0x2ffc},
	DiagnosticWideRange{0x3000, 0x303f},
	DiagnosticWideRange{0x3041, 0x3097},
	DiagnosticWideRange{0x3099, 0x3100},
	DiagnosticWideRange{0x3105, 0x31e4},
	DiagnosticWideRange{0x31f0, 0x3247},
	DiagnosticWideRange{0x3251, 0x4dbf},
	DiagnosticWideRange{0x4e01, 0xa48d},
	DiagnosticWideRange{0xa490, 0xa4c7},
	DiagnosticWideRange{0xa961, 0xa97d},
	DiagnosticWideRange{0xac00, 0xd7a4},
	DiagnosticWideRange{0xf900, 0xfb00},
	DiagnosticWideRange{0xfe11, 0xfe1a},
	DiagnosticWideRange{0xfe31, 0xfe6c},
	DiagnosticWideRange{0xff01, 0xff61},
	DiagnosticWideRange{0xffe0, 0xffe7},
	DiagnosticWideRange{0x16fe0, 0x16fe5},
	DiagnosticWideRange{0x16ff0, 0x16ff2},
	DiagnosticWideRange{0x17000, 0x187f8},
	DiagnosticWideRange{0x18800, 0x18cd6},
	DiagnosticWideRange{0x18d00, 0x18d09},
	DiagnosticWideRange{0x1b000, 0x1b11f},
	DiagnosticWideRange{0x1b150, 0x1b153},
	DiagnosticWideRange{0x1b164, 0x1b168},
	DiagnosticWideRange{0x1b170, 0x1b2fc},
	DiagnosticWideRange{0x1f0d0, 0x1f0d0},
	DiagnosticWideRange{0x1f192, 0x1f19b},
	DiagnosticWideRange{0x1f200, 0x1f203},
	DiagnosticWideRange{0x1f210, 0x1f23c},
	DiagnosticWideRange{0x1f240, 0x1f249},
	DiagnosticWideRange{0x1f250, 0x1f252},
	DiagnosticWideRange{0x1f260, 0x1f266},
	DiagnosticWideRange{0x1f300, 0x1f321},
	DiagnosticWideRange{0x1f32d, 0x1f394},
	DiagnosticWideRange{0x1f3a0, 0x1f3cb},
	DiagnosticWideRange{0x1f3d0, 0x1f3d3},
	DiagnosticWideRange{0x1f3e1, 0x1f3f0},
	DiagnosticWideRange{0x1f3f9, 0x1f43e},
	DiagnosticWideRange{0x1f441, 0x1f4fd},
	DiagnosticWideRange{0x1f4ff, 0x1f53e},
	DiagnosticWideRange{0x1f54b, 0x1f568},
	DiagnosticWideRange{0x1f596, 0x1f596},
	DiagnosticWideRange{0x1f5fc, 0x1f64f},
	DiagnosticWideRange{0x1f681, 0x1f6c6},
	DiagnosticWideRange{0x1f6cc, 0x1f6cd},
	DiagnosticWideRange{0x1f6d0, 0x1f6d3},
	DiagnosticWideRange{0x1f6d5, 0x1f6d8},
	DiagnosticWideRange{0x1f6eb, 0x1f6ed},
	DiagnosticWideRange{0x1f6f5, 0x1f6fd},
	DiagnosticWideRange{0x1f7e0, 0x1f7ec},
	DiagnosticWideRange{0x1f90d, 0x1f93a},
	DiagnosticWideRange{0x1f93d, 0x1f945},
	DiagnosticWideRange{0x1f948, 0x1fa00},
	DiagnosticWideRange{0x1fa70, 0x1fa75},
	DiagnosticWideRange{0x1fa78, 0x1fa7b},
	DiagnosticWideRange{0x1fa80, 0x1fa87},
	DiagnosticWideRange{0x1fa90, 0x1faa9},
	DiagnosticWideRange{0x1fab0, 0x1fab7},
	DiagnosticWideRange{0x1fac0, 0x1fac3},
	DiagnosticWideRange{0x1fad0, 0x1fad7},
	DiagnosticWideRange{0x20000, 0x2fffe},
	DiagnosticWideRange{0x30000, 0x3fffe},
]

@[direct_array_access]
fn diagnostic_codepoint_is_wide(codepoint u32) bool {
	mut left := 0
	mut right := diagnostic_wide_ranges.len - 1
	for left <= right {
		middle := left + (right - left) / 2
		entry := diagnostic_wide_ranges[middle]
		if codepoint < entry.start {
			right = middle - 1
		} else if codepoint > entry.end {
			left = middle + 1
		} else {
			return true
		}
	}
	return false
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
