// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token
import v.errors
import os
import strings

enum State {
	simple // default - no special interpretation of tags, *at all*!
	// That is suitable for the general case of text template interpolation,
	// for example for interpolating arbitrary source code (even V source) templates.
	//
	html // default, only when the template extension is .html
	css  // <style>
	js   // <script>
	// span // span.{
}

fn (mut state State) update(line string) {
	trimmed_line := line.trim_space()
	if is_html_open_tag('style', line) {
		state = .css
	} else if trimmed_line == '</style>' {
		state = .html
	} else if is_html_open_tag('script', line) {
		state = .js
	} else if trimmed_line == '</script>' {
		state = .html
	}
}

const tmpl_str_end = "')\n"
const tmpl_literal_dollar_marker = '__V_TMPL_LITERAL_DOLLAR__'

// check HTML open tag `<name attr="x" >`
fn is_html_open_tag(name string, s string) bool {
	trimmed_line := s.trim_space()
	mut len := trimmed_line.len

	if len < name.len {
		return false
	}

	mut sub := trimmed_line[0..1]
	if sub != '<' { // not start with '<'
		return false
	}
	sub = trimmed_line[len - 1..len]
	if sub != '>' { // not end with '<'
		return false
	}
	sub = trimmed_line[len - 2..len - 1]
	if sub == '/' { // self-closing
		return false
	}
	sub = trimmed_line[1..len - 1]
	if sub.contains_any('<>') { // `<name <bad> >`
		return false
	}
	if sub == name { // `<name>`
		return true
	} else {
		len = name.len
		if sub.len <= len { // `<nam>` or `<meme>`
			return false
		}
		if sub[..len + 1] != '${name} ' { // not `<name ...>`
			return false
		}
		return true
	}
}

fn is_tmpl_ident_start(c u8) bool {
	return c.is_letter() || c == `_`
}

fn is_tmpl_ident_part(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
}

fn find_tmpl_balanced_end(line string, start int, open u8, close u8) int {
	if start >= line.len || line[start] != open {
		return -1
	}
	mut depth := 0
	mut i := start
	mut in_single_quote := false
	mut in_double_quote := false
	for i < line.len {
		ch := line[i]
		if ch == `\\` {
			i += 2
			continue
		}
		if in_single_quote {
			if ch == `'` {
				in_single_quote = false
			}
			i++
			continue
		}
		if in_double_quote {
			if ch == `"` {
				in_double_quote = false
			}
			i++
			continue
		}
		if ch == `'` {
			in_single_quote = true
			i++
			continue
		}
		if ch == `"` {
			in_double_quote = true
			i++
			continue
		}
		if ch == open {
			depth++
		} else if ch == close {
			depth--
			if depth == 0 {
				return i + 1
			}
		}
		i++
	}
	return -1
}

fn find_tmpl_complex_at_expr_end(line string, start int) int {
	mut i := start
	if i >= line.len || !is_tmpl_ident_start(line[i]) {
		return -1
	}
	i++
	for i < line.len && is_tmpl_ident_part(line[i]) {
		i++
	}
	mut has_complex_suffix := false
	for i + 1 < line.len && line[i] == `.` && is_tmpl_ident_start(line[i + 1]) {
		i += 2
		has_complex_suffix = true
		for i < line.len && is_tmpl_ident_part(line[i]) {
			i++
		}
	}
	for i < line.len {
		if line[i] == `[` {
			expr_end := find_tmpl_balanced_end(line, i, `[`, `]`)
			if expr_end == -1 {
				return -1
			}
			i = expr_end
			has_complex_suffix = true
			continue
		}
		if line[i] == `(` {
			expr_end := find_tmpl_balanced_end(line, i, `(`, `)`)
			if expr_end == -1 {
				return -1
			}
			i = expr_end
			has_complex_suffix = true
			continue
		}
		if i + 1 < line.len && line[i] == `.` && is_tmpl_ident_start(line[i + 1]) {
			i += 2
			for i < line.len && is_tmpl_ident_part(line[i]) {
				i++
			}
			continue
		}
		break
	}
	if !has_complex_suffix {
		return -1
	}
	return i
}

fn rewrite_complex_template_at_expressions(line string) string {
	mut b := strings.new_builder(line.len + 8)
	mut i := 0
	for i < line.len {
		if line[i] != `@` {
			b.write_u8(line[i])
			i++
			continue
		}
		if i > 0 && line[i - 1] == `\\` {
			b.write_u8(`@`)
			i++
			continue
		}
		if i + 1 >= line.len {
			b.write_u8(`@`)
			i++
			continue
		}
		next := line[i + 1]
		if next == `@` || next == `{` {
			b.write_u8(`@`)
			i++
			continue
		}
		if next == `(` {
			expr_end := find_tmpl_balanced_end(line, i + 1, `(`, `)`)
			if expr_end != -1 && expr_end > i + 2 {
				b.write_string('@{')
				b.write_string(line[i + 2..expr_end - 1])
				b.write_u8(`}`)
				i = expr_end
				continue
			}
			b.write_u8(`@`)
			i++
			continue
		}
		expr_end := find_tmpl_complex_at_expr_end(line, i + 1)
		if expr_end != -1 {
			b.write_string('@{')
			b.write_string(line[i + 1..expr_end])
			b.write_u8(`}`)
			i = expr_end
			continue
		}
		b.write_u8(`@`)
		i++
	}
	return b.str()
}

fn escape_bare_tmpl_dollar_interpolations(line string) string {
	mut sb := strings.new_builder(line.len)
	mut i := 0
	for i < line.len {
		if i + 1 < line.len && ((line[i] == `@` && line[i + 1] == `{`)
			|| (line[i] == `$` && line[i + 1] == `{`)) {
			expr_end := find_tmpl_balanced_end(line, i + 1, `{`, `}`)
			if expr_end != -1 {
				sb.write_string(line[i..expr_end])
				i = expr_end
				continue
			}
		}
		if line[i] == `$` && i + 1 < line.len && is_tmpl_ident_start(line[i + 1]) {
			sb.write_string(tmpl_literal_dollar_marker)
			i++
			continue
		}
		sb.write_u8(line[i])
		i++
	}
	return sb.str()
}

fn insert_template_code(fn_name string, tmpl_str_start string, line string) string {
	// HTML, may include `@var`
	// escaped by cgen, unless it's a `veb.RawHtml` string
	trailing_bs := tmpl_str_end + 'sb_${fn_name}.write_u8(92)\n' + tmpl_str_start
	literal_dollar := tmpl_str_end + 'sb_${fn_name}.write_u8(36)\n' + tmpl_str_start
	rewritten_line :=
		escape_bare_tmpl_dollar_interpolations(rewrite_complex_template_at_expressions(line))
	mut sb := strings.new_builder(rewritten_line.len + 16)
	mut i := 0
	for i < rewritten_line.len {
		ch := rewritten_line[i]
		match ch {
			`\\` {
				sb.write_string('\\\\')
				i++
				continue
			}
			`'` {
				sb.write_string("\\'")
				i++
				continue
			}
			`@` {
				if i + 1 < rewritten_line.len && rewritten_line[i + 1] == `@` {
					sb.write_u8(`@`)
					i += 2
					continue
				}
				if i + 1 < rewritten_line.len {
					next := rewritten_line[i + 1]
					if next == `{` {
						sb.write_u8(`$`)
						i++
						continue
					}
					if is_tmpl_ident_start(next) {
						// Bare @ident: find the end of the identifier and wrap with ${}
						mut end := i + 2
						for end < rewritten_line.len && is_tmpl_ident_part(rewritten_line[end]) {
							end++
						}
						sb.write_string('\${')
						sb.write_string(rewritten_line[i + 1..end])
						sb.write_u8(`}`)
						i = end
						continue
					}
				}
				sb.write_u8(`@`)
				i++
				continue
			}
			`$` {
				if i + 1 < rewritten_line.len && rewritten_line[i + 1] == `$` {
					sb.write_string(r'\@')
					i += 2
					continue
				}
			}
			else {}
		}

		sb.write_u8(ch)
		i++
	}
	mut rline := sb.str()
	rline = normalize_keyword_template_interpolations(rline)
	comptime_call_str := rline.find_between('\${', '}')
	if comptime_call_str.contains("\\'") {
		rline = rline.replace(comptime_call_str, comptime_call_str.replace("\\'", r"'"))
	}
	rline = rline.replace(tmpl_literal_dollar_marker, literal_dollar)
	if rline.ends_with('\\') {
		rline = rline[0..rline.len - 2] + trailing_bs
	}
	return rline
}

fn normalize_keyword_template_interpolations(line string) string {
	mut sb := strings.new_builder(line.len)
	mut i := 0
	for i < line.len {
		ch := line[i]
		if ch == `$` && i > 0 && line[i - 1] == `\\` {
			sb.write_u8(ch)
			i++
			continue
		}
		if ch == `$` && i + 1 < line.len && (line[i + 1].is_letter() || line[i + 1] == `_`) {
			mut j := i + 1
			for j < line.len && (line[j].is_letter() || line[j].is_digit() || line[j] == `_`) {
				j++
			}
			name := line[i + 1..j]
			if token.is_key(name) {
				// Force keyword names into the escaped identifier form to avoid parser/scanner issues.
				sb.write_string('\${@${name}}')
				i = j
				continue
			}
		}
		sb.write_u8(ch)
		i++
	}
	return sb.str()
}

struct TmplControlLine {
	header              string
	inline_body         string
	prefix              string
	has_inline_body     bool
	opens_brace_block   bool
	closes_inline_block bool
}

fn parse_tmpl_control_line(line string, directive string) TmplControlLine {
	pos := line.index(directive) or { return TmplControlLine{} }
	remainder := line[pos + directive.len..].trim_space()
	if remainder.len == 0 {
		return TmplControlLine{
			prefix: line[..pos]
		}
	}
	if remainder.ends_with('{') {
		return TmplControlLine{
			header:            remainder[..remainder.len - 1].trim_space()
			prefix:            line[..pos]
			opens_brace_block: true
		}
	}
	if !remainder.ends_with('}') {
		return TmplControlLine{
			header: remainder
			prefix: line[..pos]
		}
	}
	close_pos := remainder.last_index('}') or {
		return TmplControlLine{
			header: remainder
			prefix: line[..pos]
		}
	}
	open_pos := remainder.index('{') or {
		return TmplControlLine{
			header: remainder
			prefix: line[..pos]
		}
	}
	return TmplControlLine{
		header:              remainder[..open_pos].trim_space()
		inline_body:         remainder[open_pos + 1..close_pos].trim_space()
		prefix:              line[..pos]
		has_inline_body:     open_pos + 1 < close_pos
		opens_brace_block:   true
		closes_inline_block: true
	}
}

fn parse_tmpl_else_line(line string) TmplControlLine {
	pos := line.index('@else') or { return TmplControlLine{} }
	remainder := line[pos + '@else'.len..].trim_space()
	if remainder.len == 0 {
		return TmplControlLine{
			header: 'else'
			prefix: line[..pos]
		}
	}
	if remainder.ends_with('{') {
		suffix := remainder[..remainder.len - 1].trim_space()
		return TmplControlLine{
			header:            if suffix.len == 0 { 'else' } else { 'else ${suffix}' }
			prefix:            line[..pos]
			opens_brace_block: true
		}
	}
	if !remainder.ends_with('}') {
		return TmplControlLine{
			header: if remainder.len == 0 { 'else' } else { 'else ${remainder}' }
			prefix: line[..pos]
		}
	}
	close_pos := remainder.last_index('}') or {
		return TmplControlLine{
			header: if remainder.len == 0 { 'else' } else { 'else ${remainder}' }
			prefix: line[..pos]
		}
	}
	open_pos := remainder.index('{') or {
		return TmplControlLine{
			header: if remainder.len == 0 { 'else' } else { 'else ${remainder}' }
			prefix: line[..pos]
		}
	}
	suffix := remainder[..open_pos].trim_space()
	return TmplControlLine{
		header:              if suffix.len == 0 { 'else' } else { 'else ${suffix}' }
		inline_body:         remainder[open_pos + 1..close_pos].trim_space()
		prefix:              line[..pos]
		has_inline_body:     open_pos + 1 < close_pos
		opens_brace_block:   true
		closes_inline_block: true
	}
}

enum TmplBraceBlockKind {
	control
	div
	span
}

fn (mut p Parser) append_tmpl_line_info(template_file string, tmpl_line int, count int) {
	for _ in 0 .. count {
		p.template_line_map << ast.TemplateLineInfo{
			tmpl_path: template_file
			tmpl_line: tmpl_line
		}
	}
}

// struct to track dependecies and cache templates for reuse without io
struct DependencyCache {
pub mut:
	dependencies map[string][]string
	cache        map[string][]string
}

// custom error to handle issues when including template files
struct IncludeError {
	Error
pub:
	calling_file string
	line_nr      int
	position     int
	col          u16
	message      string
}

fn (err IncludeError) msg() string {
	return err.message
}

fn (err IncludeError) line_nr() int {
	return err.line_nr
}

fn (err IncludeError) pos() int {
	return err.position
}

fn (err IncludeError) calling_file() string {
	return err.calling_file
}

fn (err IncludeError) col() u16 {
	return err.col
}

struct TmplInclude {
	path     string
	position int
}

fn tmpl_include_path_error(calling_file string, line_nr int, position int) IError {
	return &IncludeError{
		calling_file: calling_file
		line_nr:      line_nr
		position:     position
		col:          u16(position)
		message:      'path for @include must be quoted with \' or "'
	}
}

fn parse_tmpl_include_path(calling_file string, line_nr int, line string) !TmplInclude {
	include_pos := line.index('@include ') or { 0 }
	mut quote_pos := include_pos + '@include '.len
	for quote_pos < line.len && line[quote_pos].is_space() {
		quote_pos++
	}
	if quote_pos >= line.len || (line[quote_pos] != `'` && line[quote_pos] != `"`) {
		return tmpl_include_path_error(calling_file, line_nr, quote_pos)
	}
	quote := line[quote_pos]
	mut end_pos := quote_pos + 1
	for end_pos < line.len && line[end_pos] != quote {
		end_pos++
	}
	if end_pos >= line.len {
		return tmpl_include_path_error(calling_file, line_nr, quote_pos)
	}
	return TmplInclude{
		path:     line[quote_pos + 1..end_pos]
		position: quote_pos
	}
}

fn (mut p Parser) process_includes(calling_file string, line_number int, line string, mut dc DependencyCache) ![]string {
	base_path := os.dir(calling_file)
	mut tline_number := line_number
	include := parse_tmpl_include_path(calling_file, tline_number, line)!
	mut file_name := include.path
	mut file_ext := os.file_ext(file_name)
	if file_ext == '' {
		file_ext = '.html'
	}
	file_name = file_name.replace(file_ext, '')
	mut file_path := os.real_path(os.join_path_single(base_path, '${file_name}${file_ext}'))

	if !os.exists(file_path) && !file_name.contains('../') {
		// the calling file is probably original way (relative to calling file) and works from the root folder
		path_arr := base_path.split_any('/\\')
		idx := path_arr.index('templates')
		root_path := path_arr[..idx + 1].join('/')
		file_name = file_name.rsplit('../')[0]
		file_path = os.real_path(os.join_path_single(root_path, '${file_name}${file_ext}'))
	}

	// If file hasnt been called before then add to dependency tree
	if file_path !in dc.dependencies {
		dc.dependencies[file_path] = []string{}
	}
	if !dc.dependencies[file_path].contains(calling_file) {
		dc.dependencies[file_path] << calling_file
	}

	// Circular import detection
	for callee in dc.dependencies[file_path] {
		if dc.dependencies[callee].contains(file_path) {
			return &IncludeError{
				calling_file: calling_file
				line_nr:      tline_number
				position:     line.index('@include ') or { 0 }
				message:      'A recursive call is being made on template ${file_name}'
			}
		}
	}
	mut file_content := []string{}
	if file_path in dc.cache {
		file_content = dc.cache[file_path]
	} else {
		file_content = os.read_lines(file_path) or {
			position := include.position
			return &IncludeError{
				calling_file: calling_file
				line_nr:      tline_number // line_number
				position:     position
				message:      'Reading file `${file_name}` from path: ${file_path} failed'
			}
		}
	}
	// no errors detected in calling file - reset tline_number (error reporting)
	tline_number = 1

	// loop over the imported file
	for i, l in file_content {
		if l.contains('@include ') {
			processed := p.process_includes(file_path, tline_number, l, mut dc) or { return err }
			file_content.delete(i) // remove the include line
			for processed_line in processed.reverse() {
				file_content.insert(i, processed_line)
				tline_number--
			}
		}
	}
	// Add template to parser for reloading
	p.template_paths << file_path
	// Add the imported template to the cache
	dc.cache[file_path] = file_content
	return file_content
}

// compile_file compiles the content of a file by the given path as a template
pub fn (mut p Parser) compile_template_file(template_file string, fn_name string) string {
	mut lines := os.read_lines(template_file) or {
		p.error('reading from ${template_file} failed')
		return ''
	}
	p.template_paths << template_file
	// create a new Dependency tree & cache any templates to avoid further io
	mut dc := DependencyCache{}
	lstartlength := lines.len * 30
	tmpl_str_start := "\tsb_${fn_name}.write_string('"
	mut source := strings.new_builder(1000)

	// Reset line mapping for this template compilation
	p.template_line_map = []

	source.writeln('
import strings
// === veb html template for file: ${template_file} ===
fn veb_tmpl_${fn_name}() string {
	mut sb_${fn_name} := strings.new_builder(${lstartlength})\n

')
	// Header adds 8 lines (0: empty, 1: import, 2: comment, 3: fn, 4: builder, 5: empty from \n escape, 6: empty from literal, 7: empty from writeln)
	// Pre-fill the line map with placeholder entries for header lines
	for _ in 0 .. 8 {
		p.template_line_map << ast.TemplateLineInfo{
			tmpl_path: template_file
			tmpl_line: 0
		}
	}

	source.write_string(tmpl_str_start)

	mut state := State.simple
	template_ext := os.file_ext(template_file)
	if template_ext.to_lower_ascii() == '.html' {
		state = .html
	}

	mut in_html_comment := false
	mut brace_block_kinds := []TmplBraceBlockKind{}
	mut end_of_line_pos := 0
	mut start_of_line_pos := 0
	mut tline_number := -1 // keep the original line numbers, even after insert/delete ops on lines; `i` changes
	for i := 0; i < lines.len; i++ {
		line := lines[i]
		trimmed_line := line.trim_space()
		tline_number++
		start_of_line_pos = end_of_line_pos
		end_of_line_pos += line.len + 1
		if state != .simple {
			state.update(line)
		}
		$if trace_tmpl ? {
			eprintln('>>> tfile: ${template_file}, spos: ${start_of_line_pos:6}, epos:${end_of_line_pos:6}, fi: ${tline_number:5}, i: ${i:5}, state: ${state:10}, line: ${line}')
		}
		// Track HTML comments: skip @-interpolation inside <!-- ... -->
		if state == .html {
			if in_html_comment {
				if line.contains('-->') {
					in_html_comment = false
				}
				// Output comment line literally (no @-interpolation)
				escaped := line.replace('\\', '\\\\').replace("'", "\\'")
				source.writeln(escaped)
				p.template_line_map << ast.TemplateLineInfo{
					tmpl_path: template_file
					tmpl_line: tline_number
				}
				continue
			}
			if line.contains('<!--') {
				if !line.contains('-->') {
					in_html_comment = true
				}
				// Single-line or start of multi-line comment: output literally
				escaped := line.replace('\\', '\\\\').replace("'", "\\'")
				source.writeln(escaped)
				p.template_line_map << ast.TemplateLineInfo{
					tmpl_path: template_file
					tmpl_line: tline_number
				}
				continue
			}
		}
		if line.contains('@header') {
			position := line.index('@header') or { 0 }
			p.error_with_error(errors.Error{
				message:   "Please use @include 'header' instead of @header (deprecated)"
				file_path: template_file
				pos:       token.Pos{
					len:       '@header'.len
					line_nr:   tline_number
					pos:       start_of_line_pos + position
					last_line: lines.len
				}
				reporter:  .parser
			})
			continue
		}
		if line.contains('@footer') {
			position := line.index('@footer') or { 0 }
			p.error_with_error(errors.Error{
				message:   "Please use @include 'footer' instead of @footer (deprecated)"
				file_path: template_file
				pos:       token.Pos{
					len:       '@footer'.len
					line_nr:   tline_number
					pos:       start_of_line_pos + position
					last_line: lines.len
				}
				reporter:  .parser
			})
			continue
		}
		if line.contains('@include ') {
			lines.delete(i)
			resolved := p.process_includes(template_file, tline_number, line, mut &dc) or {
				if err is IncludeError {
					p.error_with_error(errors.Error{
						message:   err.msg()
						file_path: err.calling_file()
						pos:       token.Pos{
							len:       '@include '.len
							line_nr:   err.line_nr()
							pos:       start_of_line_pos + err.pos()
							col:       err.col()
							last_line: lines.len
						}
						reporter:  .parser
					})
					[]string{}
				} else {
					p.error_with_error(errors.Error{
						message:   'An unknown error has occurred'
						file_path: template_file
						pos:       token.Pos{
							len:       '@include '.len
							line_nr:   tline_number
							pos:       start_of_line_pos
							last_line: lines.len
						}
						reporter:  .parser
					})
					[]string{}
				}
			}
			for resolved_line in resolved.reverse() {
				tline_number--
				lines.insert(i, resolved_line)
			}

			i--
			continue
		}
		if trimmed_line == '}' && brace_block_kinds.len > 0 && brace_block_kinds.last() == .control {
			source.writeln(tmpl_str_end)
			// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
			p.append_tmpl_line_info(template_file, tline_number, 2)
			source.writeln('}')
			p.append_tmpl_line_info(template_file, tline_number, 1)
			source.write_string(tmpl_str_start)
			brace_block_kinds.delete_last()
			continue
		}
		if line.contains('@if ') {
			control := parse_tmpl_control_line(line, '@if')
			source.writeln(tmpl_str_end)
			// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
			p.append_tmpl_line_info(template_file, tline_number, 2)
			source.writeln('if ${control.header} {')
			p.append_tmpl_line_info(template_file, tline_number, 1)
			source.write_string(tmpl_str_start)
			if control.has_inline_body {
				source.writeln(insert_template_code(fn_name, tmpl_str_start, control.prefix +
					control.inline_body))
				p.append_tmpl_line_info(template_file, tline_number, 1)
			}
			if control.closes_inline_block {
				source.writeln(tmpl_str_end)
				// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
				p.append_tmpl_line_info(template_file, tline_number, 2)
				source.writeln('}')
				p.append_tmpl_line_info(template_file, tline_number, 1)
				source.write_string(tmpl_str_start)
			} else if control.opens_brace_block {
				brace_block_kinds << .control
			}
			continue
		}
		if line.contains('@end') {
			source.writeln(tmpl_str_end)
			// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
			p.append_tmpl_line_info(template_file, tline_number, 2)
			source.writeln('}')
			p.append_tmpl_line_info(template_file, tline_number, 1)
			source.write_string(tmpl_str_start)
			if brace_block_kinds.len > 0 && brace_block_kinds.last() == .control {
				brace_block_kinds.delete_last()
			}
			continue
		}
		if line.contains('@else') {
			control := parse_tmpl_else_line(line)
			source.writeln(tmpl_str_end)
			// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
			p.append_tmpl_line_info(template_file, tline_number, 2)
			source.writeln('} ${control.header} {')
			p.append_tmpl_line_info(template_file, tline_number, 1)
			source.write_string(tmpl_str_start)
			if control.has_inline_body {
				source.writeln(insert_template_code(fn_name, tmpl_str_start, control.prefix +
					control.inline_body))
				p.append_tmpl_line_info(template_file, tline_number, 1)
			}
			if control.closes_inline_block {
				source.writeln(tmpl_str_end)
				// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
				p.append_tmpl_line_info(template_file, tline_number, 2)
				source.writeln('}')
				p.append_tmpl_line_info(template_file, tline_number, 1)
				source.write_string(tmpl_str_start)
				if brace_block_kinds.len > 0 && brace_block_kinds.last() == .control {
					brace_block_kinds.delete_last()
				}
			}
			continue
		}
		if line.contains('@for') {
			control := parse_tmpl_control_line(line, '@for')
			source.writeln(tmpl_str_end)
			// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
			p.append_tmpl_line_info(template_file, tline_number, 2)
			source.writeln('for ${control.header} {')
			p.append_tmpl_line_info(template_file, tline_number, 1)
			source.write_string(tmpl_str_start)
			if control.has_inline_body {
				source.writeln(insert_template_code(fn_name, tmpl_str_start, control.prefix +
					control.inline_body))
				p.append_tmpl_line_info(template_file, tline_number, 1)
			}
			if control.closes_inline_block {
				source.writeln(tmpl_str_end)
				// tmpl_str_end contains '\n', so writeln creates 2 lines: ')' and empty
				p.append_tmpl_line_info(template_file, tline_number, 2)
				source.writeln('}')
				p.append_tmpl_line_info(template_file, tline_number, 1)
				source.write_string(tmpl_str_start)
			} else if control.opens_brace_block {
				brace_block_kinds << .control
			}
			continue
		}
		if state == .simple {
			// by default, just copy 1:1
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
			p.append_tmpl_line_info(template_file, tline_number, 1)
			continue
		}
		// in_write = false
		// The .simple mode ends here. The rest handles .html/.css/.js state transitions.

		if state != .simple {
			if line.contains('@js ') {
				pos := line.index('@js') or { continue }
				source.write_string('<script src="')
				source.write_string(line[pos + 5..line.len - 1])
				source.writeln('"></script>')
				p.template_line_map << ast.TemplateLineInfo{
					tmpl_path: template_file
					tmpl_line: tline_number
				}
				continue
			}
			if line.contains('@css ') {
				pos := line.index('@css') or { continue }
				source.write_string('<link href="')
				source.write_string(line[pos + 6..line.len - 1])
				source.writeln('" rel="stylesheet" type="text/css">')
				p.template_line_map << ast.TemplateLineInfo{
					tmpl_path: template_file
					tmpl_line: tline_number
				}
				continue
			}
		}

		match state {
			.html {
				line_t := line.trim_space()
				if line_t.starts_with('span.') && line.ends_with('{') {
					// `span.header {` => `<span class='header'>`
					class := line.find_between('span.', '{').trim_space()
					source.writeln('<span class="${class}">')
					p.template_line_map << ast.TemplateLineInfo{
						tmpl_path: template_file
						tmpl_line: tline_number
					}
					brace_block_kinds << .span
					continue
				} else if line_t.starts_with('.') && line.ends_with('{') {
					// `.header {` => `<div class='header'>`
					class := line.find_between('.', '{').trim_space()
					trimmed := line.trim_space()
					source.write_string(strings.repeat(`\t`, line.len - trimmed.len)) // add the necessary indent to keep <div><div><div> code clean
					source.writeln('<div class="${class}">')
					p.template_line_map << ast.TemplateLineInfo{
						tmpl_path: template_file
						tmpl_line: tline_number
					}
					brace_block_kinds << .div
					continue
				} else if line_t.starts_with('#') && line.ends_with('{') {
					// `#header {` => `<div id='header'>`
					class := line.find_between('#', '{').trim_space()
					source.writeln('<div id="${class}">')
					p.template_line_map << ast.TemplateLineInfo{
						tmpl_path: template_file
						tmpl_line: tline_number
					}
					brace_block_kinds << .div
					continue
				} else if line_t == '}' {
					source.write_string(strings.repeat(`\t`, line.len - line_t.len)) // add the necessary indent to keep <div><div><div> code clean
					if brace_block_kinds.len > 0 && brace_block_kinds.last() == .span {
						source.writeln('</span>')
						brace_block_kinds.delete_last()
					} else {
						source.writeln('</div>')
						if brace_block_kinds.len > 0 && brace_block_kinds.last() == .div {
							brace_block_kinds.delete_last()
						}
					}
					p.template_line_map << ast.TemplateLineInfo{
						tmpl_path: template_file
						tmpl_line: tline_number
					}
					continue
				}
			}
			.js {
				// if line.contains('//V_TEMPLATE') {
				source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
				p.template_line_map << ast.TemplateLineInfo{
					tmpl_path: template_file
					tmpl_line: tline_number
				}
				//} else {
				// replace `$` to `\$` at first to escape JavaScript template literal syntax
				// source.writeln(line.replace(r'$', r'\$').replace(r'$$', r'@').replace(r'.$',
				// r'.@').replace(r"'", r"\'"))
				//}
				continue
			}
			.css {
				// disable template variable declaration in inline stylesheet
				// because of  some CSS rules prefixed with `@`.
				source.writeln(line.replace(r'.$', r'.@').replace(r"'", r"\'"))
				p.template_line_map << ast.TemplateLineInfo{
					tmpl_path: template_file
					tmpl_line: tline_number
				}
				continue
			}
			else {}
		}

		// %translation_key => ${tr('translation_key')}
		// Process all %key patterns on this line
		mut line_ := line
		mut search_start := 0
		for {
			pos := line_.index_after('%', search_start) or { break }
			is_raw := pos + 4 < line_.len && line_[pos..pos + 5] == '%raw '
			if is_raw {
				// Start reading the key after "raw " (pos + 5)
				mut end := pos + 5
				// valid variable characters
				for end < line_.len && (line_[end].is_letter() || line_[end] == `_`) {
					end++
				}
				// Extract the key
				key := line_[pos + 5..end]
				if key.len > 0 {
					// Replace '%raw key' with just '${key}'
					line_ = line_.replace('%raw ${key}',
						'\${veb.raw(veb.tr(ctx.lang.str(), "${key}"))}')
				}
				search_start = pos + 1
			} else {
				if pos + 1 < line_.len && line_[pos + 1].is_letter() {
					mut end := pos + 1
					for end < line_.len && (line_[end].is_letter() || line_[end] == `_`) {
						end++
					}
					key := line_[pos + 1..end]
					// println('GOT tr key line="${line_}" key="${key}"')
					line_ = line_.replace('%${key}', '\${veb.tr(ctx.lang.str(), "${key}")}')
					search_start = pos + 1
				} else {
					// Not a valid translation key, skip this %
					search_start = pos + 1
				}
			}
		}
		if line_ != line {
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line_))
			p.template_line_map << ast.TemplateLineInfo{
				tmpl_path: template_file
				tmpl_line: tline_number
			}
		} else {
			// by default, just copy 1:1
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
			p.template_line_map << ast.TemplateLineInfo{
				tmpl_path: template_file
				tmpl_line: tline_number
			}
		}
	}

	source.writeln(tmpl_str_end)
	source.writeln('\t_tmpl_res_${fn_name} := sb_${fn_name}.str() ')
	source.writeln('\treturn _tmpl_res_${fn_name}')
	source.writeln('}')
	source.writeln('// === end of veb html template_file: ${template_file} ===')

	mut result := source.str()
	if result.contains('veb.') {
		result = 'import veb\n' + result
	}
	$if trace_tmpl_expansion ? {
		eprintln('>>>>>>> template expanded to:')
		eprintln(result)
		eprintln('-----------------------------')
	}

	return result
}
