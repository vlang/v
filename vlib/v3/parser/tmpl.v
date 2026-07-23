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

// tmpl_interp_format_split splits an `@{expr:fmt}` interpolation body at its
// top-level format-specifier `:` (a `:` outside quotes and any `()[]{}` nesting),
// returning `(expr, fmt)`. Returns none when there is no format specifier.
fn tmpl_interp_format_split(content string) ?(string, string) {
	mut depth := 0
	mut in_sq := false
	mut in_dq := false
	mut i := 0
	for i < content.len {
		ch := content[i]
		if ch == `\\` {
			i += 2
			continue
		}
		if in_sq {
			if ch == `'` {
				in_sq = false
			}
			i++
			continue
		}
		if in_dq {
			if ch == `"` {
				in_dq = false
			}
			i++
			continue
		}
		match ch {
			`'` { in_sq = true }
			`"` { in_dq = true }
			`(`, `[`, `{` { depth++ }
			`)`, `]`, `}` { depth-- }
			`:` {
				if depth == 0 {
					return content[..i], content[i + 1..]
				}
			}
			else {}
		}
		i++
	}
	return none
}

// tmpl_format_is_string_safe reports whether a `${expr:fmt}` format specifier can be
// applied to `veb.filter_html`'s string result. Width/precision/flags and the string
// specifier `s` are safe; a numeric/float/char specifier implies a non-string value
// (which v1 does not escape), so those are left unescaped and unwrapped instead.
fn tmpl_format_is_string_safe(fmt string) bool {
	for c in fmt {
		if c in [`b`, `c`, `d`, `o`, `x`, `X`, `e`, `E`, `f`, `F`, `g`, `G`, `p`] {
			return false
		}
	}
	return true
}

// tmpl_write_escaped_interpolation emits an escaped `${...}` interpolation for a
// `$veb.html()` template, routing string values through `veb.filter_html`. A format
// specifier is preserved: it stays outside the helper call for string-safe formats
// (`veb.filter_html(expr):fmt`), and numeric/typed formats are emitted unescaped to
// match v1, which only filters string interpolations.
fn tmpl_write_escaped_interpolation(mut sb strings.Builder, content string) {
	if expr, fmt := tmpl_interp_format_split(content) {
		if tmpl_format_is_string_safe(fmt) {
			sb.write_string('\${veb.filter_html(')
			sb.write_string(expr)
			sb.write_string('):')
			sb.write_string(fmt)
			sb.write_u8(`}`)
		} else {
			sb.write_string('\${')
			sb.write_string(content)
			sb.write_u8(`}`)
		}
		return
	}
	sb.write_string('\${veb.filter_html(')
	sb.write_string(content)
	sb.write_string(')}')
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
								tmpl_write_escaped_interpolation(mut sb, rewritten_line[i + 2..expr_end - 1])
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
							// A bare `@ident` has no format specifier (it stops at the
							// first non-identifier char), so wrap it directly.
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
// A referenced partial that cannot be opened records a diagnostic instead of being
// silently dropped, so a missing/misspelled include fails the compile rather than
// rendering a page without its required partials.
fn (mut p Parser) process_tmpl_includes(dir string, line string, mut seen map[string]bool) []string {
	include_pos := line.index('@include ') or { return [] }
	mut quote_pos := include_pos + '@include '.len
	for quote_pos < line.len && line[quote_pos].is_space() {
		quote_pos++
	}
	if quote_pos >= line.len || (line[quote_pos] != `'` && line[quote_pos] != `"`) {
		p.record_diagnostic('path for @include must be quoted with \' or "', p.tok_pos)
		return []
	}
	quote := line[quote_pos]
	mut end_pos := quote_pos + 1
	for end_pos < line.len && line[end_pos] != quote {
		end_pos++
	}
	if end_pos >= line.len {
		p.record_diagnostic('path for @include must be quoted with \' or "', p.tok_pos)
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
		// The file is on the current include stack: a circular include. Report it and
		// stop expanding, rather than silently dropping the include line. (A partial
		// already fully expanded and popped is NOT in `seen`, so the same partial may
		// legitimately be included more than once.)
		p.record_diagnostic('circular veb template include `${file_name}${file_ext}` (${file_path})',
			p.tok_pos)
		return []
	}
	content := os.read_lines(file_path) or {
		p.record_diagnostic('veb template include `${file_name}${file_ext}` could not be opened (${file_path})',
			p.tok_pos)
		return []
	}
	// Mark this file as on the current recursion stack while its own includes are
	// resolved, then pop it so a later sibling include of the same file still expands.
	seen[file_path] = true
	base := os.dir(file_path)
	mut out := []string{}
	for l in content {
		if l.contains('@include ') {
			out << p.process_tmpl_includes(base, l, mut seen)
		} else {
			out << l
		}
	}
	seen.delete(file_path)
	return out
}

// expand_veb_tr_shorthand rewrites veb's translation shorthand on an HTML template text
// line, mirroring v1: `%key` becomes an interpolation of `veb.tr(ctx.lang.str(), "key")`
// and `%raw key` an interpolation of `veb.raw(veb.tr(ctx.lang.str(), "key"))`. A `%` not
// followed by a valid key (letters/`_`) is left untouched. The result uses the template's
// own `@{...}` interpolation syntax so tmpl_line_content renders it like any other value
// (a non-raw key is HTML-escaped; `veb.raw` yields RawHtml, emitted verbatim). Requires a
// veb request context named `ctx` with a `lang` field, exactly as in v1.
fn expand_veb_tr_shorthand(line string) string {
	mut out := line
	mut search_start := 0
	for {
		pos := out.index_after('%', search_start) or { break }
		is_raw := pos + 4 < out.len && out[pos..pos + 5] == '%raw '
		if is_raw {
			mut end := pos + 5
			for end < out.len && (out[end].is_letter() || out[end] == `_`) {
				end++
			}
			key := out[pos + 5..end]
			if key.len > 0 {
				out = out.replace('%raw ${key}', '@{veb.raw(veb.tr(ctx.lang.str(), "${key}"))}')
			}
			search_start = pos + 1
		} else if pos + 1 < out.len && out[pos + 1].is_letter() {
			mut end := pos + 1
			for end < out.len && (out[end].is_letter() || out[end] == `_`) {
				end++
			}
			key := out[pos + 1..end]
			out = out.replace('%${key}', '@{veb.tr(ctx.lang.str(), "${key}")}')
			search_start = pos + 1
		} else {
			search_start = pos + 1
		}
	}
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
			resolved := p.process_tmpl_includes(base_dir, line, mut seen_includes)
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
		// Only HTML text lines reach here (simple/js/css states are emitted and continue
		// above), so expand veb's `%key` / `%raw key` translation shorthand before the
		// line's `@`-interpolations are rendered, matching v1.
		source.writeln(tmpl_line_content(expand_veb_tr_shorthand(line), escape))
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
		// Only `$veb.html(...)` lowers to a template. A mistyped or unrelated
		// selector such as `$veb.htm()` / `$veb.foo()` must not silently render
		// the handler template; fall back to the unknown-comptime handling
		// (consume to the statement end and yield an empty string literal).
		if p.tok != .name || p.lit != 'html' {
			for p.tok != .semicolon && p.tok != .eof {
				p.next()
			}
			return p.add_val_id(5, '')
		}
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
// looks for the handler's template next to the source file and then under
// `templates/`; an explicit path (from `$tmpl(path)` or `$veb.html('x.html')`) is
// resolved relative to the source file, then `templates/`. Like v1, a handler name
// with underscores also maps to a nested subpath, so `controller_get_all_task`
// resolves `controller/get/all/task.html` in addition to the flat filename.
fn (p &Parser) resolve_veb_template_path(is_html bool, arg string) string {
	dir := os.dir(os.real_path(p.cur_file))
	vmod_dir := nearest_vmod_dir(dir)
	if is_html && arg.len == 0 {
		fn_name := p.cur_fn.all_after_last('.')
		split_name := fn_name.split('_').join(os.path_separator)
		mut candidates := [
			os.join_path_single(dir, '${split_name}.html'),
			os.join_path_single(dir, '${fn_name}.html'),
			os.join_path(dir, 'templates', '${split_name}.html'),
			os.join_path(dir, 'templates', '${fn_name}.html'),
		]
		// A handler in a module subdirectory keeps its templates under the module
		// root's `templates/` (next to `v.mod`); include those before failing.
		if root := vmod_dir {
			if root != dir {
				candidates << os.join_path(root, 'templates', '${split_name}.html')
				candidates << os.join_path(root, 'templates', '${fn_name}.html')
			}
		}
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
	// Fall back to the nearest `v.mod` root: routes kept in subdirectories reference
	// templates at `<module>/<arg>` (e.g. `$veb.html('templates/user.html')`) or
	// `<module>/templates/<arg>`, which are not reachable relative to the source dir.
	if root := vmod_dir {
		if root != dir {
			vmod_direct := os.join_path_single(root, arg)
			if os.exists(vmod_direct) {
				return vmod_direct
			}
			vmod_templates := os.join_path(root, 'templates', arg)
			if os.exists(vmod_templates) {
				return vmod_templates
			}
		}
	}
	return direct
}

// nearest_vmod_dir walks up from `start_dir` to the closest directory that contains a
// `v.mod` file (the module root), or returns none when there is none.
fn nearest_vmod_dir(start_dir string) ?string {
	mut d := start_dir
	for d.len > 0 {
		if os.exists(os.join_path_single(d, 'v.mod')) {
			return d
		}
		parent := os.dir(d)
		if parent == d {
			break
		}
		d = parent
	}
	return none
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
	// Isolate any bindings the re-parsed builder declares (e.g. its `mut <builder> := ''`)
	// so they do not leak into the enclosing function's local-binding scope.
	p.begin_local_binding_scope()
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
	p.end_local_binding_scope()

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
	// Only a plain `:=` / `=` binding is re-emitted here (both carry op `.assign`); a
	// compound assignment like `html += $tmpl(...)` must keep its operator, so it falls
	// through to the general path below, which rewrites the RHS in place and leaves the
	// original assign node (and its `+=`) intact instead of overwriting with `=`.
	if node.kind in [.decl_assign, .assign] && node.children_count == 2 && node.op == .assign {
		rhs_id := p.a.child(&node, 1)
		rhs := p.a.nodes[int(rhs_id)]
		lhs_id := p.a.child(&node, 0)
		lhs := p.a.nodes[int(lhs_id)]
		// Only a plain identifier target can be re-emitted from `lhs.value`; a selector
		// (`page.body`) or index (`items[i]`) LHS would lose its receiver, so those fall
		// through to the general path, which keeps the original assignment node intact.
		if rhs.kind == .veb_template && lhs.kind == .ident {
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
	// A template in a short-circuited condition operand cannot be hoisted (that would
	// evaluate it unconditionally); render it in place with an inline closure first, so
	// it runs only when the guard allows. The hoist collection below skips those
	// operands, so this is what keeps their placeholders from leaking.
	p.replace_short_circuit_templates(stmt_id)
	// General case: a `$tmpl(...)` / `$veb.html()` used as a subexpression (e.g.
	// `ctx.html($tmpl('x.html'))`, `bump() + $tmpl('x.html')`, `f(bump(), $tmpl('x.html'))`).
	// Render each placeholder in place as an immediately-invoked closure rather than
	// hoisting its builder to the front of the statement: hoisting would run the template
	// before earlier side-effecting operands, so an interpolation reading state those
	// operands change would see stale values and the template's own side effects would move
	// ahead of expressions meant to run first. The IIFE keeps every template at its original
	// evaluation position, and no `.veb_template` node leaks past the parser.
	mut tmpl_ids := []flat.NodeId{}
	p.collect_veb_template_node_ids(stmt_id, mut tmpl_ids)
	if tmpl_ids.len == 0 {
		return none
	}
	for tid in tmpl_ids {
		tnode := p.a.nodes[int(tid)]
		repl := p.veb_template_iife_replacement(tnode) or { return none }
		p.a.nodes[int(tid)] = p.a.nodes[int(repl)]
	}
	return [stmt_id]
}

// veb_template_no_descend_kinds are the hard scope boundaries whose contents are never
// reached from the enclosing statement: nested function bodies (`fn_literal`/
// `lambda_expr`/`fn_decl`) and separately-parsed blocks (`block`). A `$tmpl()` inside
// those is handled where the inner scope itself is parsed, not here. Constructs with an
// unconditional header/source but conditional/scoped bodies — `if_expr`/`match_stmt`
// (control expression), `for_stmt` (loop header) and `or_expr` (guarded source
// expression) — are NOT listed here; they are special-cased so their header runs are
// descended into while their bodies/or-blocks are left to be parsed on their own.
const veb_template_no_descend_kinds = [flat.NodeKind.fn_literal, .lambda_expr, .fn_decl,
	.block]

// collect_veb_template_node_ids gathers the `.veb_template` placeholders in the subtree
// rooted at `id` that are safe to hoist into the current statement — i.e. reached only
// through unconditional expression composition (calls, selectors, `+`, struct inits,
// indexing, …) and `if`/`match` control expressions. It stops at scope/branch
// boundaries (see veb_template_no_descend_kinds).
fn (p &Parser) collect_veb_template_node_ids(id flat.NodeId, mut out []flat.NodeId) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	if node.kind == .veb_template {
		out << id
		return
	}
	if node.kind in [.if_expr, .match_stmt] {
		// The control expression (child 0 — an `if` condition/guard or a `match`
		// subject) runs unconditionally in this scope, so a template call there is safe
		// to hoist; the branch bodies/values are conditional and are handled where each
		// branch block is parsed, so they are not descended into.
		if node.children_count > 0 {
			p.collect_veb_template_node_ids(p.a.child(&node, 0), mut out)
		}
		return
	}
	if node.kind == .infix && node.op in [.logical_and, .logical_or]
		&& node.children_count == 2 {
		// Short-circuit operators only evaluate their left operand unconditionally; the
		// right runs solely when the left does not short-circuit (`&&` left true, `||`
		// left false). Hoisting a template from the right operand would render it
		// unconditionally and break that guard, so only descend into the left operand.
		p.collect_veb_template_node_ids(p.a.child(&node, 0), mut out)
		return
	}
	if node.kind == .for_stmt {
		// A `for` header (init/cond/post — children 0..2) runs in this scope, so a
		// template there is safe to hoist; the loop body (children 3+) is a conditional,
		// separately-parsed block and is not descended into.
		header_end := if int(node.children_count) < 3 { int(node.children_count) } else { 3 }
		for i in 0 .. header_end {
			p.collect_veb_template_node_ids(p.a.child(&node, i), mut out)
		}
		return
	}
	if node.kind == .or_expr && node.children_count == 2 {
		// The guarded source expression (child 0) runs unconditionally; the `or {}`
		// block / `?`/`!` propagation (child 1) is conditional and handled on its own.
		p.collect_veb_template_node_ids(p.a.child(&node, 0), mut out)
		return
	}
	if node.kind in veb_template_no_descend_kinds {
		return
	}
	for i in 0 .. node.children_count {
		p.collect_veb_template_node_ids(p.a.child(&node, i), mut out)
	}
}

// replace_short_circuit_templates rewrites, in place, each `$tmpl()`/`$veb.html()` that
// sits in a short-circuited condition operand (the right side of `&&`/`||`) into an
// immediately-invoked closure, so the template renders only when that operand actually
// runs. This must run before collect_veb_template_node_ids, which deliberately skips
// those operands, so they are neither hoisted (which would run unconditionally) nor left
// to leak as `.veb_template` placeholders.
fn (mut p Parser) replace_short_circuit_templates(id flat.NodeId) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	if node.kind == .infix && node.op in [.logical_and, .logical_or]
		&& node.children_count == 2 {
		// Left runs unconditionally (it may hold further short-circuits); the right runs
		// only when the left does not short-circuit, so any template there is inlined.
		p.replace_short_circuit_templates(p.a.child(&node, 0))
		p.inline_templates_as_closures(p.a.child(&node, 1))
		return
	}
	if node.kind in [.if_expr, .match_stmt] {
		if node.children_count > 0 {
			p.replace_short_circuit_templates(p.a.child(&node, 0))
		}
		return
	}
	if node.kind == .for_stmt {
		// Only the header (init/cond/post) runs in this scope; the loop body is a
		// separately-parsed block handled on its own.
		header_end := if int(node.children_count) < 3 { int(node.children_count) } else { 3 }
		for i in 0 .. header_end {
			p.replace_short_circuit_templates(p.a.child(&node, i))
		}
		return
	}
	if node.kind == .or_expr && node.children_count == 2 {
		// Descend into the guarded source expression only; the `or {}` block runs
		// conditionally and is parsed separately.
		p.replace_short_circuit_templates(p.a.child(&node, 0))
		return
	}
	if node.kind in veb_template_no_descend_kinds {
		return
	}
	for i in 0 .. node.children_count {
		p.replace_short_circuit_templates(p.a.child(&node, i))
	}
}

// inline_templates_as_closures replaces every `.veb_template` in the subtree rooted at
// `id` with an inline immediately-invoked closure. Used for conditionally-evaluated
// operands, where the template must render at its own position rather than be hoisted.
fn (mut p Parser) inline_templates_as_closures(id flat.NodeId) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	if node.kind == .veb_template {
		if repl := p.veb_template_iife_replacement(node) {
			p.a.nodes[int(id)] = p.a.nodes[int(repl)]
		}
		return
	}
	if node.kind in veb_template_no_descend_kinds {
		return
	}
	for i in 0 .. node.children_count {
		p.inline_templates_as_closures(p.a.child(&node, i))
	}
}

// veb_template_iife_replacement builds `(fn [captures] () <ret> { <builder>; return
// <value> }())` for a template placeholder, so it renders in place (preserving
// short-circuit / conditional evaluation). The closure explicitly captures the locals
// the template's interpolations reference, since v3 closures do not auto-capture.
fn (mut p Parser) veb_template_iife_replacement(tmpl flat.Node) ?flat.NodeId {
	bname, builder_src := p.veb_template_builder_source(tmpl)
	// Parse the builder once to discover which names it references, so the closure can
	// capture the locals among them (capturing a `const`/`fn` too is harmless).
	builder_ids := p.parse_stmts_from_source(builder_src)
	mut declared := map[string]bool{}
	declared[bname] = true
	mut seen := map[string]bool{}
	mut names := []string{}
	mut mut_names := map[string]bool{}
	for bid in builder_ids {
		p.collect_template_free_idents(bid, mut declared, mut seen, mut names, mut mut_names)
	}
	value_expr := if tmpl.typ == 'html' { 'ctx.html(${bname})' } else { bname }
	ret_type := if tmpl.typ == 'html' { 'veb.Result' } else { 'string' }
	mut captures := []string{}
	if tmpl.typ == 'html' {
		// The html value expression is `ctx.html(...)`, so the closure must capture
		// `ctx` even when the template body never references it — and mutably, since
		// `Context.html` has a mut receiver. Put it first (dropping any plain `ctx`).
		captures << 'mut ctx'
	}
	for name in names {
		if tmpl.typ == 'html' && name == 'ctx' {
			continue
		}
		// A name used mutably (`@{fill(mut buf)}`) must be captured `mut`, or the
		// closure body cannot pass/mutate it.
		captures << if name in mut_names { 'mut ${name}' } else { name }
	}
	cap_part := if captures.len > 0 { '[${captures.join(', ')}] ' } else { '' }
	iife_src := '(fn ${cap_part}() ${ret_type} {\n${builder_src}\nreturn ${value_expr}\n}())'
	return p.parse_veb_template_replacement_expr(iife_src)
}

// collect_template_free_idents accumulates, in first-seen order, the identifiers a
// template builder references that are not declared within it (the builder variable and
// any `for`-loop / `:=` bindings), so they can be captured by an inline closure.
fn (p &Parser) collect_template_free_idents(id flat.NodeId, mut declared map[string]bool, mut seen map[string]bool, mut out []string, mut mut_names map[string]bool) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	match node.kind {
		.ident {
			name := node.value
			// Skip the builder's own bindings and imported module names — a module
			// (`os` in `@{os.base(path)}`) is not a local variable and must not be
			// captured. A module-qualified helper is still reachable inside the closure.
			if name.len > 0 && name != '_' && name !in declared && name !in p.imported_module_names {
				// A mutable use (`mut buf` argument) must be captured `mut`; record it
				// even if the name was already seen through an immutable use.
				if node.is_mut {
					mut_names[name] = true
				}
				if name !in seen {
					seen[name] = true
					out << name
				}
			}
		}
		.for_in_stmt {
			header := if node.value.int() > 0 { node.value.int() } else { 3 }
			// The container (and optional range end) evaluate in the outer scope.
			for i in 2 .. header {
				if i < int(node.children_count) {
					p.collect_template_free_idents(p.a.child(&node, i), mut declared, mut seen, mut
						out, mut mut_names)
				}
			}
			// The key/value loop variables and any `:=` bindings inside the body are locals
			// of the loop body, not of the enclosing template. Snapshot the outer declared
			// names so a later OUTER use of a shadowed name (`@item` after the loop) is still
			// collected as a capture, then drop the body-local names once the body ends —
			// otherwise the loop var would stay "declared" and the outer use is omitted.
			mut outer_declared := map[string]bool{}
			for name in declared.keys() {
				outer_declared[name] = true
			}
			if node.children_count > 0 {
				p.declare_template_ident(p.a.child(&node, 0), mut declared)
			}
			if node.children_count > 1 {
				p.declare_template_ident(p.a.child(&node, 1), mut declared)
			}
			for i in header .. int(node.children_count) {
				p.collect_template_free_idents(p.a.child(&node, i), mut declared, mut seen, mut
					out, mut mut_names)
			}
			mut body_local := []string{}
			for name in declared.keys() {
				if name !in outer_declared {
					body_local << name
				}
			}
			for name in body_local {
				declared.delete(name)
			}
		}
		.call {
			// The callee (child 0) names the function being invoked. A bare-ident callee is
			// captured only when it is an actual in-scope local binding (a function-valued
			// parameter/local, e.g. `render` in `@{render(row)}` where `render` is a handler
			// parameter) — v3 closures do not auto-capture. A bare ident that is not a local
			// binding is a module/top-level function, which is globally reachable and must
			// not be captured. A non-ident callee (a `recv.method` selector) is descended so
			// its receiver local is captured.
			if node.children_count > 0 {
				callee := p.a.child(&node, 0)
				if int(callee) >= 0 && int(callee) < p.a.nodes.len {
					callee_node := p.a.nodes[int(callee)]
					callee_is_local_binding := callee_node.kind == .ident
						&& p.is_local_binding(callee_node.value)
					if callee_node.kind != .ident || callee_is_local_binding {
						p.collect_template_free_idents(callee, mut declared, mut seen, mut out,
							mut mut_names)
					}
				}
			}
			for i in 1 .. node.children_count {
				p.collect_template_free_idents(p.a.child(&node, i), mut declared, mut seen, mut
					out, mut mut_names)
			}
		}
		.decl_assign {
			if node.children_count > 0 {
				p.declare_template_ident(p.a.child(&node, 0), mut declared)
			}
			for i in 0 .. node.children_count {
				p.collect_template_free_idents(p.a.child(&node, i), mut declared, mut seen, mut
					out, mut mut_names)
			}
		}
		else {
			for i in 0 .. node.children_count {
				p.collect_template_free_idents(p.a.child(&node, i), mut declared, mut seen, mut
					out, mut mut_names)
			}
		}
	}
}

fn (p &Parser) declare_template_ident(id flat.NodeId, mut declared map[string]bool) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	if node.kind == .ident && node.value.len > 0 {
		declared[node.value] = true
	}
}

// parse_veb_template_replacement_expr parses `src` as an expression (via a throwaway
// `_ = <src>` assignment) and returns the RHS node id, used to substitute a rendered
// template value in place of a `.veb_template` placeholder.
fn (mut p Parser) parse_veb_template_replacement_expr(src string) ?flat.NodeId {
	stmts := p.parse_stmts_from_source('_ = ${src}\n')
	if stmts.len != 1 {
		return none
	}
	stmt := p.a.nodes[int(stmts[0])]
	if stmt.kind == .assign && stmt.children_count == 2 {
		return p.a.child(&stmt, 1)
	}
	return none
}

fn (mut p Parser) veb_template_builder_source(tmpl flat.Node) (string, string) {
	bname := 'v3tmpl_${p.veb_tmpl_counter}'
	p.veb_tmpl_counter++
	return bname, p.compile_template_file(tmpl.value, bname, tmpl.typ == 'html')
}
