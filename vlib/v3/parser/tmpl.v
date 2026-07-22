// Template engine for v3, ported from vlib/v/parser/tmpl.v (v1).
//
// `compile_template_file` reads a `.html` (or other) template and compiles it to
// V source that accumulates the rendered output into a string builder variable.
// Unlike v1 — which emits a standalone `veb_tmpl_<fn>() string` function and
// inlines its body at cgen time — v3 has no closures and no block-expressions, so
// the generated statements are spliced directly into the handler body at parse
// time (see `expand_veb_template_stmt`). To avoid needing an `import strings` in
// the user's file, the builder is a plain `string` accumulated with `+=` rather
// than a `strings.Builder`.
module parser

import os
import strings
import v3.flat
import v3.scanner
import v3.token

enum TmplState {
	simple // no special interpretation of tags
	html   // default, only when the template extension is .html
	css    // <style>
	js     // <script>
}

fn (mut state TmplState) update(line string) {
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

const tmpl_literal_dollar_marker = '__V3_TMPL_LITERAL_DOLLAR__'

// is_html_open_tag checks for an HTML open tag `<name attr="x" >`.
fn is_html_open_tag(name string, s string) bool {
	trimmed_line := s.trim_space()
	mut len := trimmed_line.len
	if len < name.len {
		return false
	}
	mut sub := trimmed_line[0..1]
	if sub != '<' {
		return false
	}
	sub = trimmed_line[len - 1..len]
	if sub != '>' {
		return false
	}
	sub = trimmed_line[len - 2..len - 1]
	if sub == '/' {
		return false
	}
	sub = trimmed_line[1..len - 1]
	if sub.contains_any('<>') {
		return false
	}
	if sub == name {
		return true
	} else {
		len = name.len
		if sub.len <= len {
			return false
		}
		if sub[..len + 1] != '${name} ' {
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

// rewrite_complex_template_at_expressions rewrites `@expr.field`, `@expr(args)`,
// `@expr[idx]` and `@(expr)` into the explicit `@{expr}` interpolation form.
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
		if next == `@` {
			b.write_string('@@')
			i += 2
			continue
		}
		if next == `{` {
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

// tmpl_line_content transforms one template line into the CONTENT of a single
// `bname += '<content>'` write (escaping quotes/backslashes and turning `@{expr}`
// / `@ident` into `${expr}` interpolation). It does not include the wrapper. When
// `escape` is set (a `$veb.html()` template), each interpolation is routed through
// `veb.filter_html` so string values are HTML-escaped against injection.
fn tmpl_line_content(line string, escape bool) string {
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
						if escape {
							expr_end := find_tmpl_balanced_end(rewritten_line, i + 1, `{`,
								`}`)
							if expr_end != -1 {
								sb.write_string('\${veb.filter_html(')
								sb.write_string(rewritten_line[i + 2..expr_end - 1])
								sb.write_string(')}')
								i = expr_end
								continue
							}
						}
						sb.write_u8(`$`)
						i++
						continue
					}
					if is_tmpl_ident_start(next) {
						mut end := i + 2
						for end < rewritten_line.len && is_tmpl_ident_part(rewritten_line[end]) {
							end++
						}
						if escape {
							sb.write_string('\${veb.filter_html(')
							sb.write_string(rewritten_line[i + 1..end])
							sb.write_string(')}')
						} else {
							sb.write_string('\${')
							sb.write_string(rewritten_line[i + 1..end])
							sb.write_u8(`}`)
						}
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
	// A bare literal `$` in the template becomes `\$` inside the generated string
	// literal so it is not treated as interpolation.
	rline = rline.replace(tmpl_literal_dollar_marker, r'\$')
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
			if tmpl_name_is_keyword(name) {
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

fn tmpl_name_is_keyword(name string) bool {
	return name in ['if', 'else', 'for', 'in', 'match', 'fn', 'return', 'mut', 'or', 'is', 'as',
		'type', 'struct', 'enum', 'interface', 'module', 'import', 'pub', 'const', 'true', 'false',
		'none', 'go', 'spawn', 'defer', 'unsafe', 'assert', 'break', 'continue', 'map', 'chan',
		'select', 'lock', 'rlock', 'shared', 'atomic', 'asm', 'goto', 'union']
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

// process_tmpl_includes recursively resolves `@include 'file'` directives into
// the list of lines they expand to. `dir` is the directory of the including file.
fn process_tmpl_includes(dir string, line string, mut seen map[string]bool) []string {
	include_pos := line.index('@include ') or { return [] }
	mut quote_pos := include_pos + '@include '.len
	for quote_pos < line.len && line[quote_pos].is_space() {
		quote_pos++
	}
	if quote_pos >= line.len || (line[quote_pos] != `'` && line[quote_pos] != `"`) {
		return []
	}
	quote := line[quote_pos]
	mut end_pos := quote_pos + 1
	for end_pos < line.len && line[end_pos] != quote {
		end_pos++
	}
	if end_pos >= line.len {
		return []
	}
	mut file_name := line[quote_pos + 1..end_pos]
	mut file_ext := os.file_ext(file_name)
	if file_ext == '' {
		file_ext = '.html'
	}
	file_name = file_name.replace(file_ext, '')
	mut file_path := os.join_path_single(dir, '${file_name}${file_ext}')
	if os.exists(file_path) {
		file_path = os.real_path(file_path)
	}
	if file_path in seen {
		// The file is on the current include stack: a circular include. Stop
		// expanding. (A partial already fully expanded and popped is NOT in `seen`,
		// so the same partial may legitimately be included more than once.)
		return []
	}
	content := os.read_lines(file_path) or { return [] }
	// Mark this file as on the current recursion stack while its own includes are
	// resolved, then pop it so a later sibling include of the same file still expands.
	seen[file_path] = true
	base := os.dir(file_path)
	mut out := []string{}
	for l in content {
		if l.contains('@include ') {
			out << process_tmpl_includes(base, l, mut seen)
		} else {
			out << l
		}
	}
	seen.delete(file_path)
	return out
}

// compile_template_file compiles the template at `template_file` into V source
// that accumulates the rendered string into `bname` (declared here as `mut bname := ''`).
// After the returned source runs, `bname` holds the rendered output. When `escape` is
// set (a `$veb.html()` template), interpolated values are HTML-escaped via
// `veb.filter_html`.
fn (mut p Parser) compile_template_file(template_file string, bname string, escape bool) string {
	mut lines := os.read_lines(template_file) or {
		p.record_diagnostic('reading from template ${template_file} failed', p.tok_pos)
		return 'mut ${bname} := \'\'\n'
	}
	tmpl_str_start := '\t${bname} += \''
	tmpl_str_end := "'\n"
	mut source := strings.new_builder(1024)
	source.writeln('mut ${bname} := \'\'')
	source.write_string(tmpl_str_start)

	mut state := TmplState.simple
	template_ext := os.file_ext(template_file)
	if template_ext.to_lower_ascii() == '.html' {
		state = .html
	}
	mut in_html_comment := false
	mut brace_block_kinds := []TmplBraceBlockKind{}
	mut seen_includes := map[string]bool{}
	base_dir := os.dir(os.real_path(template_file))
	for i := 0; i < lines.len; i++ {
		line := lines[i]
		trimmed_line := line.trim_space()
		if state != .simple {
			state.update(line)
		}
		// HTML comments: emit literally, no @-interpolation.
		if state == .html {
			if in_html_comment {
				if line.contains('-->') {
					in_html_comment = false
				}
				source.writeln(line.replace('\\', '\\\\').replace("'", "\\'"))
				continue
			}
			if line.contains('<!--') {
				if !line.contains('-->') {
					in_html_comment = true
				}
				source.writeln(line.replace('\\', '\\\\').replace("'", "\\'"))
				continue
			}
		}
		if line.contains('@include ') {
			resolved := process_tmpl_includes(base_dir, line, mut seen_includes)
			lines.delete(i)
			for resolved_line in resolved.reverse() {
				lines.insert(i, resolved_line)
			}
			i--
			continue
		}
		if trimmed_line == '}' && brace_block_kinds.len > 0 && brace_block_kinds.last() == .control {
			source.writeln(tmpl_str_end)
			source.writeln('}')
			source.write_string(tmpl_str_start)
			brace_block_kinds.delete_last()
			continue
		}
		if line.contains('@if ') {
			control := parse_tmpl_control_line(line, '@if')
			source.writeln(tmpl_str_end)
			source.writeln('if ${control.header} {')
			source.write_string(tmpl_str_start)
			if control.has_inline_body {
				source.writeln(tmpl_line_content(control.prefix + control.inline_body, escape))
			}
			if control.closes_inline_block {
				source.writeln(tmpl_str_end)
				source.writeln('}')
				source.write_string(tmpl_str_start)
			} else if control.opens_brace_block {
				brace_block_kinds << .control
			}
			continue
		}
		if line.contains('@end') {
			source.writeln(tmpl_str_end)
			source.writeln('}')
			source.write_string(tmpl_str_start)
			if brace_block_kinds.len > 0 && brace_block_kinds.last() == .control {
				brace_block_kinds.delete_last()
			}
			continue
		}
		if line.contains('@else') {
			control := parse_tmpl_else_line(line)
			source.writeln(tmpl_str_end)
			source.writeln('} ${control.header} {')
			source.write_string(tmpl_str_start)
			if control.has_inline_body {
				source.writeln(tmpl_line_content(control.prefix + control.inline_body, escape))
			}
			if control.closes_inline_block {
				source.writeln(tmpl_str_end)
				source.writeln('}')
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
			source.writeln('for ${control.header} {')
			source.write_string(tmpl_str_start)
			if control.has_inline_body {
				source.writeln(tmpl_line_content(control.prefix + control.inline_body, escape))
			}
			if control.closes_inline_block {
				source.writeln(tmpl_str_end)
				source.writeln('}')
				source.write_string(tmpl_str_start)
			} else if control.opens_brace_block {
				brace_block_kinds << .control
			}
			continue
		}
		if state == .simple {
			source.writeln(tmpl_line_content(line, escape))
			continue
		}
		if state != .simple {
			if line.contains('@js ') {
				pos := line.index('@js') or { continue }
				source.write_string('<script src="')
				source.write_string(line[pos + 5..line.len - 1])
				source.writeln('"></script>')
				continue
			}
			if line.contains('@css ') {
				pos := line.index('@css') or { continue }
				source.write_string('<link href="')
				source.write_string(line[pos + 6..line.len - 1])
				source.writeln('" rel="stylesheet" type="text/css">')
				continue
			}
		}
		match state {
			.html {
				line_t := line.trim_space()
				if line_t.starts_with('span.') && line.ends_with('{') {
					class := line.find_between('span.', '{').trim_space()
					source.writeln('<span class="${class}">')
					brace_block_kinds << .span
					continue
				} else if line_t.starts_with('.') && line.ends_with('{') {
					class := line.find_between('.', '{').trim_space()
					source.writeln('<div class="${class}">')
					brace_block_kinds << .div
					continue
				} else if line_t.starts_with('#') && line.ends_with('{') {
					class := line.find_between('#', '{').trim_space()
					source.writeln('<div id="${class}">')
					brace_block_kinds << .div
					continue
				} else if line_t == '}' {
					if brace_block_kinds.len > 0 && brace_block_kinds.last() == .span {
						source.writeln('</span>')
						brace_block_kinds.delete_last()
					} else {
						source.writeln('</div>')
						if brace_block_kinds.len > 0 && brace_block_kinds.last() == .div {
							brace_block_kinds.delete_last()
						}
					}
					continue
				}
			}
			.js {
				source.writeln(tmpl_line_content(line, escape))
				continue
			}
			.css {
				source.writeln(line.replace(r'.$', r'.@').replace(r"'", r"\'"))
				continue
			}
			else {}
		}
		source.writeln(tmpl_line_content(line, escape))
	}
	source.writeln(tmpl_str_end)
	return source.str()
}

// parse_veb_template_expr consumes a `$tmpl('path')` or `$veb.html([...])` call
// (the leading `$` is already consumed) and returns a `.veb_template` placeholder
// node whose `value` is the resolved template file path and whose `typ` is
// 'html' (for veb.html, yields a veb.Result) or 'tmpl' (yields a string).
fn (mut p Parser) parse_veb_template_expr(is_html bool) flat.NodeId {
	if is_html {
		p.next() // skip `veb`
		p.check(.dot)
		p.next() // skip `html`
	} else {
		p.next() // skip `tmpl`
	}
	mut arg := ''
	if p.tok == .lpar {
		p.next()
		if p.tok != .rpar && p.tok != .eof && p.tok != .semicolon {
			// The path may be a compile-time expression (a `const`, a local binding
			// with a literal value, or a `+` concatenation of those), not just a raw
			// string token — e.g. `const p = 'x.html'; $tmpl(p)`. Parse and resolve it.
			arg_id := p.expr(.lowest)
			arg = p.resolve_tmpl_path_arg(arg_id)
		}
		for p.tok != .rpar && p.tok != .eof && p.tok != .semicolon {
			p.next()
		}
		if p.tok == .rpar {
			p.next()
		}
	}
	path := p.resolve_veb_template_path(is_html, arg)
	return p.add_node(flat.Node{
		kind:  .veb_template
		value: path
		typ:   if is_html { 'html' } else { 'tmpl' }
	})
}

// resolve_tmpl_path_arg resolves a `$tmpl(expr)` / `$veb.html(expr)` path argument
// to a compile-time string, mirroring v1's `resolve_tmpl_path_expr` for the forms the
// template tests rely on: string literals, `const`/local bindings whose value is a
// known compile-time string, parenthesised forms, and `+` concatenations of those.
// Returns '' when the argument is not a resolvable compile-time string.
fn (p &Parser) resolve_tmpl_path_arg(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return ''
	}
	node := p.a.nodes[int(id)]
	if node.kind == .infix && node.op == .plus && node.children_count == 2 {
		left := p.resolve_tmpl_path_arg(p.a.children[int(node.children_start)])
		right := p.resolve_tmpl_path_arg(p.a.children[int(node.children_start) + 1])
		if left.len == 0 || right.len == 0 {
			return ''
		}
		return left + right
	}
	// comptime_node_value handles string literals, parenthesised forms, and idents
	// bound to a `const` or local with a known compile-time value; it returns the
	// value in quoted comptime form, so unwrap it back to the raw path string.
	if value := p.comptime_node_value(id) {
		return comptime_cond_value(value)
	}
	return ''
}

// resolve_veb_template_path mirrors v1's lookup: an argument-less `$veb.html()`
// looks for `<fn>.html` next to the source file and then under `templates/`; an
// explicit path (from `$tmpl(path)` or `$veb.html('x.html')`) is resolved relative
// to the source file, then `templates/`.
fn (p &Parser) resolve_veb_template_path(is_html bool, arg string) string {
	dir := os.dir(os.real_path(p.cur_file))
	if is_html && arg.len == 0 {
		fn_name := p.cur_fn.all_after_last('.')
		candidates := [
			os.join_path_single(dir, '${fn_name}.html'),
			os.join_path(dir, 'templates', '${fn_name}.html'),
		]
		for c in candidates {
			if os.exists(c) {
				return c
			}
		}
		return candidates[0]
	}
	if os.is_abs_path(arg) {
		return arg
	}
	direct := os.join_path_single(dir, arg)
	if os.exists(direct) {
		return direct
	}
	in_templates := os.join_path(dir, 'templates', arg)
	if os.exists(in_templates) {
		return in_templates
	}
	return direct
}

// parse_stmts_from_source parses `src` as a statement sequence using a temporary
// sub-scanner, returning the parsed statement node ids. The parser's scanner and
// token state are saved and restored around the call.
fn (mut p Parser) parse_stmts_from_source(src string) []flat.NodeId {
	// The AST owns every source buffer so zero-copy token views stay valid.
	p.a.source_buffers << src
	stable_src := p.a.source_buffers.last()
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('<veb-template>', stable_src.len)
	file.index_lines(stable_src)

	saved_s := p.s
	saved_tok := p.tok
	saved_lit := p.lit
	saved_tok_pos := p.tok_pos
	saved_tok_end := p.tok_end
	saved_prev_tok_end := p.prev_tok_end
	saved_peek_tok := p.peek_tok
	saved_peek_lit := p.peek_lit
	saved_peek_pos := p.peek_pos
	saved_peek_end := p.peek_end
	saved_has_peek := p.has_peek

	p.s = scanner.new_scanner(p.prefs, .normal)
	p.s.init(file, stable_src)
	p.has_peek = false
	p.next()
	mut ids := []flat.NodeId{}
	for p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		id := p.stmt()
		if int(id) >= 0 {
			ids << id
		}
	}

	p.s = saved_s
	p.tok = saved_tok
	p.lit = saved_lit
	p.tok_pos = saved_tok_pos
	p.tok_end = saved_tok_end
	p.prev_tok_end = saved_prev_tok_end
	p.peek_tok = saved_peek_tok
	p.peek_lit = saved_peek_lit
	p.peek_pos = saved_peek_pos
	p.peek_end = saved_peek_end
	p.has_peek = saved_has_peek
	return ids
}

// expand_veb_template_stmt lowers a statement whose value is a `.veb_template`
// placeholder into inline builder statements: `return $veb.html()` becomes
// `mut <b> := ''; <writes>; return ctx.html(<b>)`, and `x := $tmpl(p)` becomes
// `mut <b> := ''; <writes>; x := <b>`. Returns none for any other statement.
fn (mut p Parser) expand_veb_template_stmt(stmt_id flat.NodeId) ?[]flat.NodeId {
	if int(stmt_id) < 0 || int(stmt_id) >= p.a.nodes.len {
		return none
	}
	node := p.a.nodes[int(stmt_id)]
	if node.kind == .return_stmt && node.children_count == 1 {
		child_id := p.a.child(&node, 0)
		child := p.a.nodes[int(child_id)]
		if child.kind == .veb_template {
			bname, mut src := p.veb_template_builder_source(child)
			src += if child.typ == 'html' {
				'\nreturn ctx.html(${bname})\n'
			} else {
				'\nreturn ${bname}\n'
			}
			return p.parse_stmts_from_source(src)
		}
	}
	if node.kind in [.decl_assign, .assign] && node.children_count == 2 {
		rhs_id := p.a.child(&node, 1)
		rhs := p.a.nodes[int(rhs_id)]
		if rhs.kind == .veb_template {
			lhs_id := p.a.child(&node, 0)
			lhs := p.a.nodes[int(lhs_id)]
			bind_op := if node.kind == .decl_assign { ':=' } else { '=' }
			// `mut x := $tmpl(...)` records its mutability on the decl_assign node
			// (see mark_node_mut); re-emit `mut` so the reparsed binding is not
			// lowered as an immutable local and reject later mutation of `x`.
			mut_prefix := if node.kind == .decl_assign && node.is_mut { 'mut ' } else { '' }
			bname, mut src := p.veb_template_builder_source(rhs)
			value_expr := if rhs.typ == 'html' { 'ctx.html(${bname})' } else { bname }
			src += '\n${mut_prefix}${lhs.value} ${bind_op} ${value_expr}\n'
			return p.parse_stmts_from_source(src)
		}
	}
	return none
}

fn (mut p Parser) veb_template_builder_source(tmpl flat.Node) (string, string) {
	bname := 'v3tmpl_${p.veb_tmpl_counter}'
	p.veb_tmpl_counter++
	return bname, p.compile_template_file(tmpl.value, bname, tmpl.typ == 'html')
}
