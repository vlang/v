module errors

import os
import strings
import v.ansi
import v.flat
import v.token

const source_context_before = 2
const source_context_after = 2

fn formatted_message(kind string, message string) string {
	// Diagnostics name a sum type variant as `Expr.Count`, never by its hidden struct.
	return '${ansi.bold(ansi.color(kind, kind))} ${flat.demangle_named_variants(message)}'
}

// DiagnosticRun holds what the diagnostics of one report share: the working directory, the
// resolved path of each file named so far and the text of the one named last. Resolving a
// path and reading a file take system calls that cost far more than formatting a message,
// and a report names the same few files again and again. A run assumes that neither the working directory nor
// the reported files change while it is used.
pub struct DiagnosticRun {
mut:
	paths_known  bool
	absolute     bool
	workdir      string
	shown_paths  map[string]string
	source_known bool
	source_read  bool
	source_name  string
	source       string
	// Where the line `cursor_line` of `source` starts, 0 when no line was located yet.
	cursor_line   int
	cursor_offset int
}

// formatted_error renders a compiler diagnostic with v1-compatible colors and source context.
pub fn formatted_error(kind string, message string, a &flat.FlatAst, node flat.NodeId, pos token.Pos) string {
	mut run := DiagnosticRun{}
	return run.formatted_error(kind, message, a, node, pos)
}

// formatted_error renders a compiler diagnostic like the function of the same name, sharing
// the work the diagnostics of this run have in common.
pub fn (mut run DiagnosticRun) formatted_error(kind string, message string, a &flat.FlatAst, node flat.NodeId, pos token.Pos) string {
	if pos.is_valid() {
		file := a.source_files[pos.id] or { return formatted_message(kind, message) }
		action_message := if action := a.template_actions[pos.id] {
			'${message} (veb action: ${action})'
		} else {
			message
		}
		result := run.formatted_source_error(kind, action_message, file, pos)
		return run.append_template_call_stack(result, kind, a, pos)
	}
	if int(node) < 0 || int(node) >= a.nodes.len {
		return formatted_message(kind, message)
	}
	n := a.nodes[int(node)]
	file := a.source_files[n.pos.id] or { return formatted_message(kind, message) }
	action_message := if action := a.template_actions[n.pos.id] {
		'${message} (veb action: ${action})'
	} else {
		message
	}
	result := run.formatted_source_error(kind, action_message, file, n.pos)
	return run.append_template_call_stack(result, kind, a, n.pos)
}

// formatted_parser_error renders a parser diagnostic with template call-site context.
pub fn formatted_parser_error(message string, a &flat.FlatAst, pos token.Pos) string {
	return formatted_parser_diagnostic('error:', message, a, pos)
}

// formatted_parser_diagnostic renders a colored parser diagnostic with template call-site context.
pub fn formatted_parser_diagnostic(kind string, message string, a &flat.FlatAst, pos token.Pos) string {
	file := a.source_files[pos.id] or { return formatted_message(kind, message) }
	mut run := DiagnosticRun{}
	result := run.formatted_source_error(kind, message, file, pos)
	return run.append_template_call_stack(result, kind, a, pos)
}

fn (mut run DiagnosticRun) append_template_call_stack(result string, kind string, a &flat.FlatAst, pos token.Pos) string {
	mut output := result
	for call_pos in template_call_positions(a, pos) {
		if call_file := a.source_files[call_pos.id] {
			position := call_file.logical_position(call_pos)
			path := run.relative_path(position.filename)
			context_full := run.formatted_source_error(kind, '', call_file, call_pos)
			context := context_full.all_after_first('\n')
			output += '\ncalled from ${path}:${position.line}:${position.column}'
			if context.len > 0 {
				output += '\n${context}'
			}
		}
	}
	return output
}

// template_call_positions lists the `$tmpl` calls that included the template of pos,
// innermost first.
fn template_call_positions(a &flat.FlatAst, pos token.Pos) []token.Pos {
	mut positions := []token.Pos{}
	mut current := pos
	mut visited := map[int]bool{}
	for {
		if current.id in visited {
			break
		}
		visited[current.id] = true
		call_pos := a.template_call_sites[current.id] or { break }
		positions << call_pos
		current = call_pos
	}
	return positions
}

// formatted_source_error renders a diagnostic with v1-compatible colors for a source byte span.
// Under a `#line` directive, the location is the logical file and line. The excerpt then
// comes from the logical file, when it is another file that can be read, like C compilers
// show it; otherwise it is just the source line of the span, numbered as its logical line.
pub fn formatted_source_error(kind string, message string, file &token.File, pos token.Pos) string {
	mut run := DiagnosticRun{}
	return run.formatted_source_error(kind, message, file, pos)
}

fn (mut run DiagnosticRun) formatted_source_error(kind string, message string, file &token.File, pos token.Pos) string {
	position := file.logical_position(pos)
	path := run.relative_path(position.filename)
	mut result := strings.new_builder(message.len + 256)
	reported_column := if pos.reported_column() > 0 {
		pos.reported_column()
	} else {
		position.column
	}
	location := '${path}:${position.line}:${reported_column}:'
	result.writeln('${ansi.bold(location)} ${formatted_message(kind, message)}')
	// The span, relative to the start of its source line. The column is the same in the
	// logical line.
	line_start := pos.offset - position.column + 1
	span_start := pos.offset - line_start
	span_end := int_max(pos.offset + 1, pos.end) - line_start
	if file.has_line_directives() {
		physical_line := file.find_line(pos.offset)
		if position.filename != file.name || position.line != physical_line {
			if position.filename != file.name {
				if source := run.read_source(position.filename) {
					lines := run.source_lines_through(source, position.line - source_context_before,
						position.line + source_context_after)
					if position.line <= lines.len {
						write_source_excerpt(mut result, kind, lines, position.line, span_start,
							span_end)
						return result.str().trim_right('\n')
					}
				}
			}
			source := run.read_source(file.name) or { return result.str().trim_right('\n') }
			lines := run.source_lines_through(source, physical_line, physical_line)
			if physical_line <= lines.len {
				write_source_line(mut result, kind, lines[physical_line - 1], position.line, true,
					true, span_start, span_end)
			}
			return result.str().trim_right('\n')
		}
	}
	source := run.read_source(file.name) or { return result.str().trim_right('\n') }
	lines := run.source_lines_through(source, position.line - source_context_before,
		position.line + source_context_after)
	if lines.len == 0 {
		return result.str().trim_right('\n')
	}
	write_source_excerpt(mut result, kind, lines, position.line, span_start, span_end)
	return result.str().trim_right('\n')
}

// source_lines_through returns the lines `source.split_into_lines()` holds up to the 1-based
// line `last_line`: all of them when the source is shorter, the first `last_line` otherwise.
// Lines before `first_line` are left empty. A diagnostic prints a few lines around its
// position, so its whole file, often thousands of lines long, is not copied line by line.
fn source_lines_through(source string, first_line int, last_line int) []string {
	return source_lines_from(source, 0, 1, first_line, last_line)
}

// source_lines_from is source_lines_through for a source whose line `start_line`, no later
// than `first_line` or the first line, is known to start at `start_offset`.
@[direct_array_access]
fn source_lines_from(source string, start_offset int, start_line int, first_line int, last_line int) []string {
	mut lines := []string{len: start_line - 1}
	// An empty source has no lines; any other has a first one to count.
	limit := int_max(1, last_line)
	mut line_start := start_offset
	for i := start_offset; i < source.len; i++ {
		if line_start > i {
			continue
		}
		c := source[i]
		if c != `\n` && c != `\r` {
			continue
		}
		lines << if line_start == i || lines.len + 1 < first_line {
			''
		} else {
			source[line_start..i]
		}
		line_start = if c == `\r` && i + 1 < source.len && source[i + 1] == `\n` {
			i + 2
		} else {
			i + 1
		}
		if lines.len >= limit {
			return lines
		}
	}
	if line_start < source.len {
		lines << if lines.len + 1 < first_line { '' } else { source[line_start..] }
	}
	return lines
}

// source_lines_through returns what the function of the same name does for the source
// read last. The diagnostics of one file come in the order of their lines, so the search
// for a line starts where the one before it was found rather than at the top of the file.
@[direct_array_access]
fn (mut run DiagnosticRun) source_lines_through(source string, first_line int, last_line int) []string {
	if !run.source_read || source.str != run.source.str || source.len != run.source.len {
		return source_lines_through(source, first_line, last_line)
	}
	target := int_min(int_max(1, first_line), int_max(1, last_line))
	mut line := 1
	mut line_start := 0
	if run.cursor_line > 0 && run.cursor_line <= target {
		line = run.cursor_line
		line_start = run.cursor_offset
	}
	for i := line_start; line < target && i < source.len; i++ {
		if line_start > i {
			continue
		}
		c := source[i]
		if c != `\n` && c != `\r` {
			continue
		}
		line_start = if c == `\r` && i + 1 < source.len && source[i + 1] == `\n` {
			i + 2
		} else {
			i + 1
		}
		line++
	}
	if line < target {
		// The source ends before the first line asked for.
		return []string{len: if line_start < source.len { line } else { line - 1 }}
	}
	run.cursor_line = line
	run.cursor_offset = line_start
	return source_lines_from(source, line_start, line, first_line, last_line)
}

// write_source_excerpt writes the lines around `target_line` of `lines`, and highlights
// the span `span_start..span_end` (byte offsets relative to the line start) on it.
fn write_source_excerpt(mut result strings.Builder, kind string, lines []string, target_line int, span_start int, span_end int) {
	first_line := int_max(1, target_line - source_context_before)
	last_line := int_min(lines.len, target_line + source_context_after)
	for line_number := first_line; line_number <= last_line; line_number++ {
		write_source_line(mut result, kind, lines[line_number - 1], line_number,
			line_number == target_line, line_number == last_line, span_start, span_end)
	}
}

// write_source_line writes the source line numbered `label` of an excerpt. On the line of
// the diagnostic, the span `span_start..span_end` is highlighted and underlined.
fn write_source_line(mut result strings.Builder, kind string, line string, label int, is_target bool, is_last bool, span_start int, span_end int) {
	mut start_byte := 0
	mut end_byte := 0
	mut highlighted_line := line
	if is_target {
		start_byte = int_max(0, int_min(span_start, line.len))
		end_byte = int_min(line.len, int_max(start_byte + 1, int_min(span_end, line.len)))
		highlighted_line = line[..start_byte] + ansi.color(kind, line[start_byte..end_byte]) +
			line[end_byte..]
	}
	if line.len == 0 && is_last {
		result.writeln('${label:5d} |')
	} else {
		result.writeln('${label:5d} | ${highlighted_line.replace('\t', '    ')}')
	}
	if is_target {
		// Measure the original source, not the ANSI-wrapped text.
		mut pointer := strings.new_builder(line.len + 8)
		prefix := line[..start_byte].replace('\t', '    ')
		pointer.write_string(' '.repeat(diagnostic_display_width(prefix)))
		underline_len := int_max(1, diagnostic_display_width(line[start_byte..end_byte]))
		underline := if underline_len > 1 {
			'~'.repeat(underline_len)
		} else {
			'^'
		}
		pointer.write_string(ansi.bold(ansi.color(kind, underline)))
		result.writeln('      | ${pointer.str().replace('\t', '    ')}')
	}
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
			((first & 0x07) << 18) | ((u32(text[index + 1]) & 0x3f) << 12) | ((u32(text[index + 2]) & 0x3f) << 6) | (u32(text[index + 3]) & 0x3f)
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
	mut run := DiagnosticRun{}
	return run.relative_path(path)
}

// relative_path is the reported spelling of `path`: resolved, and relative to the working
// directory unless VERROR_PATHS asks for absolute paths.
fn (mut run DiagnosticRun) relative_path(path string) string {
	if shown := run.shown_paths[path] {
		return shown
	}
	mut normalized := os.real_path(path).replace('\\', '/')
	if !run.paths_known {
		run.absolute = os.getenv('VERROR_PATHS') == 'absolute'
		if !run.absolute {
			run.workdir = os.getwd().replace('\\', '/').trim_right('/') + '/'
		}
		run.paths_known = true
	}
	if !run.absolute && normalized.starts_with(run.workdir) {
		normalized = normalized[run.workdir.len..]
	}
	run.shown_paths[path] = normalized
	return normalized
}

// read_source reads `name` like os.read_file, once for consecutive diagnostics of one file.
fn (mut run DiagnosticRun) read_source(name string) ?string {
	if !run.source_known || run.source_name != name {
		run.source_name = name
		run.source_known = true
		run.cursor_line = 0
		if source := os.read_file(name) {
			run.source = source
			run.source_read = true
		} else {
			run.source = ''
			run.source_read = false
		}
	}
	if run.source_read {
		return run.source
	}
	return none
}
