module yaml

import strconv
import strings

struct Parser {
mut:
	lines           []string
	idx             int
	anchors         map[string]Any
	directives_done bool
}

fn (mut p Parser) parse() !Any {
	p.skip_ignorable()
	if p.idx >= p.lines.len {
		return null
	}
	indent := p.line_indent(p.idx)!
	if indent < 0 {
		return null
	}
	return p.parse_node(indent)
}

fn (mut p Parser) parse_node(indent int) !Any {
	p.skip_ignorable()
	if p.idx >= p.lines.len {
		return null
	}
	current_indent := p.line_indent(p.idx)!
	if current_indent < indent {
		return null
	}
	content := p.current_content()!
	if content.starts_with('-') && (content.len == 1 || content[1] == ` `) {
		return p.parse_sequence(current_indent)
	}
	if split_mapping_entry(content).ok {
		return p.parse_mapping(current_indent)
	}
	d := extract_decorators(content)
	if d.alias != '' && d.rest == '' {
		p.idx++
		return p.resolve_alias(d.alias)
	}
	if is_block_scalar(d.rest) {
		p.idx++
		return p.register_anchor(d.anchor, Any(p.parse_block_scalar(current_indent, d.rest)!))
	}
	if d.rest.starts_with('[') || d.rest.starts_with('{') {
		p.idx++
		full := p.collect_flow_continuation(d.rest)!
		return p.register_anchor(d.anchor, parse_flow_value(full)!)
	}
	p.idx++
	if d.rest.len > 0 && (d.rest[0] == `"` || d.rest[0] == `'`) {
		quoted := p.gather_quoted_continuation(d.rest)!
		return p.register_anchor(d.anchor, parse_scalar(quoted)!)
	}
	folded := p.gather_plain_continuation(d.rest, current_indent)
	return p.register_anchor(d.anchor, parse_scalar(folded)!)
}

// gather_plain_continuation extends a plain scalar with subsequent lines that
// belong to the same node per YAML 1.2 §6.5.1: adjacent non-blank lines fold
// to a single space, blank lines contribute literal `\n`s. The scan stops at
// document markers, structural indicators, or a less-indented line.
fn (mut p Parser) gather_plain_continuation(initial string, base_indent int) string {
	mut sb := strings.new_builder(initial.len * 2)
	sb.write_string(initial.trim_space())
	mut blanks := 0
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		trimmed_raw := line.trim_space()
		if trimmed_raw == '' {
			blanks++
			p.idx++
			continue
		}
		trimmed := strip_comments(line).trim_space()
		if trimmed == '' {
			blanks++
			p.idx++
			continue
		}
		if trimmed == '---' || trimmed == '...' {
			break
		}
		line_indent := p.line_indent(p.idx) or { break }
		if line_indent < base_indent {
			break
		}
		if trimmed.starts_with('- ') || trimmed == '-' {
			break
		}
		if split_mapping_entry(trimmed).ok {
			break
		}
		if blanks > 0 {
			for _ in 0 .. blanks {
				sb.write_u8(`\n`)
			}
			blanks = 0
		} else {
			sb.write_u8(` `)
		}
		sb.write_string(trimmed)
		p.idx++
	}
	return sb.str()
}

fn (mut p Parser) parse_mapping(indent int) !Any {
	mut result := map[string]Any{}
	for p.idx < p.lines.len {
		p.skip_ignorable()
		if p.idx >= p.lines.len {
			break
		}
		current_indent := p.line_indent(p.idx)!
		if current_indent < indent {
			break
		}
		if current_indent > indent {
			return error('yaml: unexpected indentation on line ${p.idx + 1}')
		}
		content := p.current_content()!
		entry := split_mapping_entry(content)
		if !entry.ok {
			return error('yaml: expected a mapping entry on line ${p.idx + 1}')
		}
		p.idx++
		result[entry.key] = p.parse_mapping_value(entry.rest, indent)!
	}
	return Any(result)
}

fn (mut p Parser) parse_mapping_value(rest_in string, indent int) !Any {
	d := extract_decorators(rest_in)
	if d.alias != '' && d.rest == '' {
		return p.resolve_alias(d.alias)
	}
	value := if d.rest == '' {
		next_indent := p.peek_next_indent()
		if next_indent > indent {
			p.parse_node(next_indent)!
		} else {
			null
		}
	} else if is_block_scalar(d.rest) {
		Any(p.parse_block_scalar(indent, d.rest)!)
	} else if d.rest.starts_with('[') || d.rest.starts_with('{') {
		full := p.collect_flow_continuation(d.rest)!
		parse_flow_value(full)!
	} else if d.rest.len > 0 && (d.rest[0] == `"` || d.rest[0] == `'`) {
		quoted := p.gather_quoted_continuation(d.rest)!
		parse_scalar(quoted)!
	} else {
		parse_scalar(d.rest)!
	}
	return p.register_anchor(d.anchor, value)
}

fn (mut p Parser) parse_sequence(indent int) !Any {
	mut items := []Any{}
	for p.idx < p.lines.len {
		p.skip_ignorable()
		if p.idx >= p.lines.len {
			break
		}
		current_indent := p.line_indent(p.idx)!
		if current_indent < indent {
			break
		}
		if current_indent > indent {
			return error('yaml: unexpected indentation on line ${p.idx + 1}')
		}
		content := p.current_content()!
		if !content.starts_with('-') || (content.len > 1 && content[1] != ` `) {
			break
		}
		rest := if content.len == 1 { '' } else { content[1..].trim_space() }
		p.idx++
		items << p.parse_sequence_item(rest, indent)!
	}
	return Any(items)
}

fn (mut p Parser) parse_sequence_item(rest_in string, indent int) !Any {
	d := extract_decorators(rest_in)
	if d.alias != '' && d.rest == '' {
		return p.resolve_alias(d.alias)
	}
	value := if d.rest == '' {
		next_indent := p.peek_next_indent()
		if next_indent > indent {
			p.parse_node(next_indent)!
		} else {
			null
		}
	} else if is_block_scalar(d.rest) {
		Any(p.parse_block_scalar(indent, d.rest)!)
	} else if d.rest.starts_with('[') || d.rest.starts_with('{') {
		full := p.collect_flow_continuation(d.rest)!
		parse_flow_value(full)!
	} else {
		entry := split_mapping_entry(d.rest)
		if entry.ok {
			mut result := map[string]Any{}
			child_indent := indent + 2
			result[entry.key] = p.parse_mapping_value(entry.rest, child_indent)!
			for p.idx < p.lines.len {
				p.skip_ignorable()
				if p.idx >= p.lines.len {
					break
				}
				current_indent := p.line_indent(p.idx)!
				if current_indent <= indent {
					break
				}
				if current_indent != child_indent {
					return error('yaml: unexpected indentation on line ${p.idx + 1}')
				}
				content := p.current_content()!
				next_entry := split_mapping_entry(content)
				if !next_entry.ok {
					break
				}
				p.idx++
				result[next_entry.key] = p.parse_mapping_value(next_entry.rest, child_indent)!
			}
			Any(result)
		} else if d.rest.len > 0 && (d.rest[0] == `"` || d.rest[0] == `'`) {
			quoted := p.gather_quoted_continuation(d.rest)!
			parse_scalar(quoted)!
		} else {
			parse_scalar(d.rest)!
		}
	}
	return p.register_anchor(d.anchor, value)
}

fn (p &Parser) resolve_alias(name string) Any {
	return p.anchors[name] or { null }
}

// register_anchor associates `value` with `anchor` when the latter is set,
// then returns `value` so call sites can `return p.register_anchor(...)`
// in a single statement instead of branching.
fn (mut p Parser) register_anchor(anchor string, value Any) Any {
	if anchor != '' {
		p.anchors[anchor] = value
	}
	return value
}

// parse_block_scalar parses a `|` (literal) or `>` (folded) block, honoring
// the optional chomp indicator from the header (`-` strip, `+` keep, default
// clip). Indent indicators (`|2`) are tolerated but ignored — the block's
// indentation is auto-detected from the first non-empty content line.
fn (mut p Parser) parse_block_scalar(parent_indent int, header string) !string {
	style, chomp := parse_block_header(header)
	start := p.idx
	mut min_indent := -1
	for i := start; i < p.lines.len; i++ {
		line := p.lines[i]
		if line.trim_space() == '' {
			continue
		}
		line_indent := p.line_indent(i)!
		if line_indent <= parent_indent {
			break
		}
		if min_indent == -1 || line_indent < min_indent {
			min_indent = line_indent
		}
	}
	if min_indent == -1 {
		// Body is entirely empty/blank. Keep chomp (`+`) still preserves the
		// implicit trailing line breaks; strip and clip yield the empty string.
		if chomp != `+` {
			return ''
		}
		mut blanks := 0
		for p.idx < p.lines.len {
			line := p.lines[p.idx]
			if line.trim_space() != '' {
				break
			}
			blanks++
			p.idx++
		}
		if blanks == 0 {
			blanks = 1
		}
		return '\n'.repeat(blanks)
	}
	mut lines := []string{}
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		if line.trim_space() == '' {
			lines << ''
			p.idx++
			continue
		}
		line_indent := p.line_indent(p.idx)!
		if line_indent <= parent_indent {
			break
		}
		if line.len <= min_indent {
			lines << ''
		} else {
			lines << line[min_indent..]
		}
		p.idx++
	}
	mut stripped_trailing := 0
	for lines.len > 0 && lines[lines.len - 1] == '' {
		stripped_trailing++
		lines.delete(lines.len - 1)
	}
	body := if style == `|` { lines.join('\n') } else { fold_block_scalar(lines) }
	return apply_chomp(body, chomp, stripped_trailing)
}

struct FlowBalance {
mut:
	bracket   int
	brace     int
	in_single bool
	in_double bool
	escape    bool
}

// collect_flow_continuation gathers subsequent lines into `initial` until the
// `[` / `{` brackets and the active quoted strings are all balanced. YAML 1.2
// allows flow collections to span lines; without this the parser would reject
// anything that wraps. The returned text is what `parse_flow_value` receives.
fn (mut p Parser) collect_flow_continuation(initial string) !string {
	mut bal := FlowBalance{}
	bal.scan(initial)
	if !bal.unbalanced() {
		return initial
	}
	mut sb := strings.new_builder(initial.len * 2)
	sb.write_string(initial)
	for p.idx < p.lines.len && bal.unbalanced() {
		line := p.lines[p.idx]
		segment := if bal.in_single || bal.in_double { line } else { strip_comments(line) }
		trimmed := segment.trim_space()
		if trimmed == '' && !bal.in_single && !bal.in_double {
			p.idx++
			continue
		}
		sb.write_u8(` `)
		sb.write_string(trimmed)
		bal.scan(trimmed)
		p.idx++
	}
	if bal.unbalanced() {
		return error('yaml: unterminated flow collection')
	}
	return sb.str()
}

fn (b &FlowBalance) unbalanced() bool {
	return b.bracket > 0 || b.brace > 0 || b.in_single || b.in_double
}

fn (mut b FlowBalance) scan(s string) {
	for i := 0; i < s.len; i++ {
		ch := s[i]
		if b.in_double {
			if b.escape {
				b.escape = false
			} else if ch == `\\` {
				b.escape = true
			} else if ch == `"` {
				b.in_double = false
			}
			continue
		}
		if b.in_single {
			if ch == `'` {
				if i + 1 < s.len && s[i + 1] == `'` {
					i++
					continue
				}
				b.in_single = false
			}
			continue
		}
		match ch {
			`"` { b.in_double = true }
			`'` { b.in_single = true }
			`[` { b.bracket++ }
			`]` { b.bracket-- }
			`{` { b.brace++ }
			`}` { b.brace-- }
			else {}
		}
	}
}

// parse_block_header reads the `|`/`>` style and the optional `+`/`-` chomp
// indicator from a block-scalar header like `|`, `|-`, `|+`, `>2-`.
fn parse_block_header(s string) (u8, u8) {
	if s.len == 0 {
		return `|`, 0
	}
	style := s[0]
	mut chomp := u8(0)
	for i := 1; i < s.len; i++ {
		c := s[i]
		if c == `+` || c == `-` {
			chomp = c
		}
	}
	return style, chomp
}

// apply_chomp rewrites the block body's trailing whitespace per RFC 9.1.1.2:
// strip removes all trailing newlines, clip keeps a single trailing newline
// when the body is non-empty, keep preserves every original trailing newline.
fn apply_chomp(body string, chomp u8, stripped_trailing int) string {
	return match chomp {
		`-` {
			body
		}
		`+` {
			clipped := if body == '' { '' } else { body + '\n' }
			clipped + '\n'.repeat(stripped_trailing)
		}
		else {
			if body == '' {
				''
			} else {
				body + '\n'
			}
		}
	}
}

// is_ignorable_line returns true when `trimmed` is content the parser must
// skip over: blank lines, document markers, and directive lines that occur
// before the first body line. Directives (`%YAML`, `%TAG`, …) become plain
// text once `directives_done` flips, so the skip is conditional on that flag.
fn (p &Parser) is_ignorable_line(trimmed string) bool {
	return trimmed == '' || trimmed == '---' || trimmed == '...'
		|| (trimmed.starts_with('%') && !p.directives_done)
}

fn (mut p Parser) skip_ignorable() {
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		trimmed := strip_comments(line).trim_space()
		if p.is_ignorable_line(trimmed) {
			p.idx++
			continue
		}
		// `--- <inline>` (or `---\t…`) folds the document marker away and
		// keeps the inline content as the document body at column 0.
		if (line.starts_with('--- ') || line.starts_with('---\t')) && line.len > 4 {
			p.lines[p.idx] = line[4..]
		}
		p.directives_done = true
		break
	}
}

fn (p &Parser) peek_next_indent() int {
	mut i := p.idx
	for i < p.lines.len {
		line := p.lines[i]
		trimmed := strip_comments(line).trim_space()
		if p.is_ignorable_line(trimmed) {
			i++
			continue
		}
		return p.line_indent(i) or { -1 }
	}
	return -1
}

fn (p &Parser) current_content() !string {
	line := p.lines[p.idx]
	indent := p.line_indent(p.idx)!
	if line.len <= indent {
		return ''
	}
	return strip_comments(line[indent..]).trim_space()
}

fn (p &Parser) line_indent(index int) !int {
	line := p.lines[index]
	mut indent := 0
	for indent < line.len && line[indent] == ` ` {
		indent++
	}
	if indent < line.len && line[indent] == `\t` {
		return error('yaml: tabs are not supported for indentation on line ${index + 1}')
	}
	return indent
}

struct MappingEntry {
	key  string
	rest string
	ok   bool
}

fn split_mapping_entry(content string) MappingEntry {
	mut in_single := false
	mut in_double := false
	mut escape := false
	mut bracket_depth := 0
	mut brace_depth := 0
	mut i := 0
	for i < content.len {
		ch := content[i]
		if in_double {
			if escape {
				escape = false
			} else if ch == `\\` {
				escape = true
			} else if ch == `"` {
				in_double = false
			}
			i++
			continue
		}
		if in_single {
			if ch == `'` {
				if i + 1 < content.len && content[i + 1] == `'` {
					i += 2
					continue
				}
				in_single = false
			}
			i++
			continue
		}
		match ch {
			`"` {
				in_double = true
			}
			`'` {
				in_single = true
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			`{` {
				brace_depth++
			}
			`}` {
				if brace_depth > 0 {
					brace_depth--
				}
			}
			`:` {
				if bracket_depth == 0 && brace_depth == 0
					&& (i + 1 == content.len || content[i + 1].is_space()) {
					key_text := content[..i].trim_space()
					if key_text == '' {
						return MappingEntry{}
					}
					return MappingEntry{
						key:  parse_key(key_text) or { return MappingEntry{} }
						rest: if i + 1 < content.len {
							content[i + 1..].trim_space()
						} else {
							''
						}
						ok:   true
					}
				}
			}
			else {}
		}

		i++
	}
	return MappingEntry{}
}

// strip_node_decorators removes a leading anchor (`&id`), a tag (`!Type` or
// `!!Type`), or a sequence of both, from a YAML node's text. The semantics of
// anchors and tags are intentionally not implemented: stripping them lets the
// underlying scalar/collection still parse, which matches the common practical
// case where the document carries decorators but does not rely on them. A
// stand-alone alias (`*name`) cannot be resolved without anchor tracking and
// is therefore left untouched, so the caller still sees that something was
// referenced.
fn strip_node_decorators(s string) string {
	return extract_decorators(s).rest
}

// Decorators carries the anchor / alias / remaining content split off a node.
struct Decorators {
	anchor string
	alias  string
	rest   string
}

// extract_decorators peels leading anchor / alias / tag decorators off `s` and
// returns the anchor name, alias name, and remaining content. At most one
// anchor and one alias are recognized; tags are stripped without being
// returned.
fn extract_decorators(s string) Decorators {
	mut anchor := ''
	mut alias := ''
	mut out := s.trim_left(' \t')
	for {
		if out.len < 2 {
			break
		}
		c := out[0]
		if (c == `&` || c == `*`) && out[1] != ` ` && out[1] != `\t` {
			mut i := 1
			for i < out.len && out[i] != ` ` && out[i] != `\t` {
				i++
			}
			name := out[1..i]
			if c == `&` {
				anchor = name
			} else {
				alias = name
			}
			out = out[i..].trim_left(' \t')
			continue
		}
		if c == `!` {
			mut i := 1
			if i < out.len && out[i] == `!` {
				i++
			}
			for i < out.len && out[i] != ` ` && out[i] != `\t` {
				i++
			}
			out = out[i..].trim_left(' \t')
			continue
		}
		break
	}
	return Decorators{
		anchor: anchor
		alias:  alias
		rest:   out
	}
}

// parse_key resolves a mapping-key token: drops anchor / tag decorators,
// unquotes the result if surrounded by matching `"` or `'` quotes, and
// returns the cleaned key string otherwise.
fn parse_key(src string) !string {
	cleaned := strip_node_decorators(src)
	if cleaned.len >= 2 && ((cleaned[0] == `"` && cleaned[cleaned.len - 1] == `"`)
		|| (cleaned[0] == `'` && cleaned[cleaned.len - 1] == `'`)) {
		return parse_quoted_string(cleaned)
	}
	return cleaned.trim_space()
}

// parse_scalar resolves a scalar token to its YAML 1.2 typed value: quoted
// strings unquote, the case-insensitive keywords `null`/`~`, `true`/`yes`/`on`
// and `false`/`no`/`off` produce the matching constants, integer and float
// literals (with `_` digit separators) parse to `i64`/`u64`/`f64`, and any
// other text falls back to a plain string.
fn parse_scalar(text string) !Any {
	value := strip_node_decorators(text).trim_space()
	if value == '' {
		return Any('')
	}
	if value.len >= 2 && ((value[0] == `"` && value[value.len - 1] == `"`)
		|| (value[0] == `'` && value[value.len - 1] == `'`)) {
		return Any(parse_quoted_string(value)!)
	}
	// Keyword check: only strings of length 1..5 can match `~`, `null`, `true`,
	// `yes`, `on`, `false`, `no`, `off`. Length-bound first to skip the
	// allocation of `to_lower()` for every plain scalar (the overwhelmingly
	// common case in real documents).
	if value.len <= 5 {
		if value.len == 1 && value[0] == `~` {
			return null
		}
		if equals_ascii_ci(value, 'null') {
			return null
		}
		if equals_ascii_ci(value, 'true') || equals_ascii_ci(value, 'yes')
			|| equals_ascii_ci(value, 'on') {
			return Any(true)
		}
		if equals_ascii_ci(value, 'false') || equals_ascii_ci(value, 'no')
			|| equals_ascii_ci(value, 'off') {
			return Any(false)
		}
	}
	numeric := strip_underscores(value)
	if is_integer(numeric) {
		if numeric.starts_with('-') {
			return Any(numeric.parse_int(0, 64)!)
		}
		if numeric.starts_with('+') {
			return Any(numeric[1..].parse_uint(0, 64)!)
		}
		return Any(numeric.parse_uint(0, 64)!)
	}
	if is_float(numeric) {
		return Any(strconv.atof64(numeric)!)
	}
	return Any(value)
}

// parse_quoted_string unquotes a YAML scalar wrapped in matching `"` or `'`
// quotes. Single-quoted strings only undouble `''` to `'`. Double-quoted
// strings honor the YAML 1.2 §5.7 escape set (`\b \f \n \r \t \" \\ \/
// \uXXXX`); any other backslash sequence is rejected as malformed.
fn parse_quoted_string(src string) !string {
	if src.len < 2 {
		return error('yaml: invalid quoted string')
	}
	quote := src[0]
	inner := fold_quoted_inner(src[1..src.len - 1])
	if quote == `'` {
		// Single-quoted strings only undouble `''`. When the body has no
		// doubled quote, return the slice as-is — `inner` already shares the
		// source buffer, so this avoids a `replace` allocation.
		if !inner.contains("''") {
			return inner
		}
		return inner.replace("''", "'")
	}
	// Double-quoted fast path: if there's no `\` in the body, no escape
	// resolution is needed — `inner` is the final value verbatim.
	if !inner.contains_u8(`\\`) {
		return inner
	}
	mut out := []u8{cap: inner.len}
	mut i := 0
	for i < inner.len {
		ch := inner[i]
		if ch != `\\` {
			out << ch
			i++
			continue
		}
		i++
		if i >= inner.len {
			return error('yaml: invalid escape sequence')
		}
		esc := inner[i]
		match esc {
			`"`, `\\`, `/` {
				out << esc
			}
			`b` {
				out << `\b`
			}
			`f` {
				out << `\f`
			}
			`n` {
				out << `\n`
			}
			`r` {
				out << `\r`
			}
			`t` {
				out << `\t`
			}
			`u` {
				if i + 4 >= inner.len {
					return error('yaml: invalid unicode escape')
				}
				code := inner[i + 1..i + 5]
				r := rune(code.parse_uint(16, 32)!)
				out << r.str().bytes()
				i += 4
			}
			else {
				return error('yaml: unknown escape sequence \\${rune(esc).str()}')
			}
		}

		i++
	}
	return out.bytestr()
}

// fold_quoted_inner applies YAML 1.2 §7.3 line folding rules to the body of a
// quoted scalar (the chars between the opening and closing quote): adjacent
// non-blank content lines fold to a single space, runs of N consecutive line
// breaks collapse to N-1 literal newlines, and leading whitespace inside
// continuation lines is stripped. A leading or trailing empty line folds to a
// single space (§7.3.1).
fn fold_quoted_inner(inner string) string {
	if !inner.contains_u8(`\n`) {
		return inner
	}
	lines := inner.split('\n')
	n := lines.len
	mut trimmed := []string{cap: n}
	for i := 0; i < n; i++ {
		line := lines[i]
		if i == 0 && i != n - 1 {
			trimmed << line.trim_right(' \t')
		} else if i == n - 1 && i != 0 {
			trimmed << line.trim_left(' \t')
		} else {
			trimmed << line.trim(' \t')
		}
	}
	has_pre := trimmed[0] == ''
	has_post := trimmed[n - 1] == ''
	mut sb := strings.new_builder(inner.len)
	if has_pre {
		sb.write_u8(` `)
	}
	start := if has_pre { 1 } else { 0 }
	end := if has_post { n - 1 } else { n }
	mut blanks := 0
	mut wrote := false
	for i := start; i < end; i++ {
		f := trimmed[i]
		if f == '' {
			blanks++
			continue
		}
		if wrote {
			if blanks == 0 {
				sb.write_u8(` `)
			} else {
				for _ in 0 .. blanks {
					sb.write_u8(`\n`)
				}
			}
		}
		blanks = 0
		sb.write_string(f)
		wrote = true
	}
	if has_post {
		sb.write_u8(` `)
	}
	return sb.str()
}

// quoted_terminated reports whether `s` (which starts with a `quote` byte)
// ends with the matching closing quote, taking single-quote `''` doubling and
// double-quote `\` escapes into account.
fn quoted_terminated(s string, quote u8) bool {
	if s.len < 2 || s[0] != quote {
		return false
	}
	mut i := 1
	if quote == `'` {
		for i < s.len {
			if s[i] == `'` {
				if i + 1 < s.len && s[i + 1] == `'` {
					i += 2
					continue
				}
				return i == s.len - 1
			}
			i++
		}
		return false
	}
	mut esc := false
	for i < s.len {
		ch := s[i]
		if esc {
			esc = false
			i++
			continue
		}
		if ch == `\\` {
			esc = true
			i++
			continue
		}
		if ch == `"` {
			return i == s.len - 1
		}
		i++
	}
	return false
}

// gather_quoted_continuation accumulates subsequent lines into a quoted scalar
// that doesn't terminate on its first line. The returned string still wraps
// the original line breaks; `fold_quoted_inner` collapses them later.
fn (mut p Parser) gather_quoted_continuation(initial string) !string {
	if initial.len == 0 {
		return initial
	}
	quote := initial[0]
	if quote != `"` && quote != `'` {
		return initial
	}
	if quoted_terminated(initial, quote) {
		return initial
	}
	mut buf := initial.bytes()
	for p.idx < p.lines.len {
		buf << `\n`
		buf << p.lines[p.idx].bytes()
		p.idx++
		snap := buf.bytestr()
		if quoted_terminated(snap, quote) {
			return snap
		}
	}
	return error('yaml: unterminated quoted string')
}

fn strip_comments(line string) string {
	// Fast path: the overwhelming majority of YAML lines have no `#`.
	if !line.contains_u8(`#`) {
		return line.trim_right(' \t')
	}
	mut in_single := false
	mut in_double := false
	mut escape := false
	mut bracket_depth := 0
	mut brace_depth := 0
	mut i := 0
	for i < line.len {
		ch := line[i]
		if in_double {
			if escape {
				escape = false
			} else if ch == `\\` {
				escape = true
			} else if ch == `"` {
				in_double = false
			}
			i++
			continue
		}
		if in_single {
			if ch == `'` {
				if i + 1 < line.len && line[i + 1] == `'` {
					i += 2
					continue
				}
				in_single = false
			}
			i++
			continue
		}
		match ch {
			`"` {
				in_double = true
			}
			`'` {
				in_single = true
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			`{` {
				brace_depth++
			}
			`}` {
				if brace_depth > 0 {
					brace_depth--
				}
			}
			`#` {
				if bracket_depth == 0 && brace_depth == 0 {
					return line[..i].trim_right(' \t')
				}
			}
			else {}
		}

		i++
	}
	return line.trim_right(' \t')
}

fn fold_block_scalar(lines []string) string {
	mut out := ''
	mut pending_newlines := 0
	mut started := false
	for line in lines {
		if line == '' {
			pending_newlines++
			continue
		}
		if !started {
			out = '\n'.repeat(pending_newlines) + line
			started = true
		} else if pending_newlines > 0 {
			out += '\n'.repeat(pending_newlines) + line
		} else {
			out += ' ' + line
		}
		pending_newlines = 0
	}
	if pending_newlines > 0 {
		out += '\n'.repeat(pending_newlines)
	}
	return out
}

fn is_block_scalar(value string) bool {
	if value.len == 0 || (value[0] != `|` && value[0] != `>`) {
		return false
	}
	// Allow `|`, `>`, `|-`, `|+`, `>2`, `|3-`, etc. Any other char rules it out
	// (e.g. `> something` is just a plain scalar starting with `>`).
	for i := 1; i < value.len; i++ {
		c := value[i]
		if c != `+` && c != `-` && !(c >= `0` && c <= `9`) {
			return false
		}
	}
	return true
}

fn is_integer(value string) bool {
	if value.len == 0 {
		return false
	}
	if value[0] in [`+`, `-`] {
		if value.len == 1 {
			return false
		}
		if value[0] == `-` {
			value.parse_int(0, 64) or { return false }
			return true
		}
		value[1..].parse_uint(0, 64) or { return false }
		return true
	}
	value.parse_uint(0, 64) or { return false }
	return true
}

fn is_float(value string) bool {
	if value.len == 0 {
		return false
	}
	if !value.contains('.') && !value.contains('e') && !value.contains('E') {
		return false
	}
	strconv.atof64(value) or { return false }
	return true
}

// equals_ascii_ci reports whether `s` equals `lower_ref` byte-for-byte once
// ASCII letters in `s` are lower-cased. `lower_ref` MUST already be lowercase
// ASCII; mixing case in it silently breaks the comparison. Used by
// `parse_scalar` to recognize boolean / null keywords without allocating a
// lower-cased copy of every plain scalar in the document.
fn equals_ascii_ci(s string, lower_ref string) bool {
	if s.len != lower_ref.len {
		return false
	}
	for i := 0; i < s.len; i++ {
		mut c := s[i]
		if c >= `A` && c <= `Z` {
			c |= 0x20
		}
		if c != lower_ref[i] {
			return false
		}
	}
	return true
}

// strip_underscores removes `_` digit separators from a numeric literal in a
// single pass. Returns `value` unchanged when no `_` is present, avoiding an
// allocation on the common case.
fn strip_underscores(value string) string {
	if !value.contains_u8(`_`) {
		return value
	}
	mut out := []u8{cap: value.len}
	for c in value {
		if c != `_` {
			out << c
		}
	}
	return out.bytestr()
}
