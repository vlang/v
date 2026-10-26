// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import strings
import encoding.html as ehtml

// block_level_tags lists HTML tags that start an HTML block (type 6).
// vfmt off
const block_level_tags = [
	'address', 'article', 'aside', 'base', 'basefont', 'blockquote', 'body', 'caption',	'center',
	'col', 'colgroup', 'dd', 'details', 'dialog', 'dir', 'div', 'dl', 'dt', 'fieldset',
	'figcaption', 'figure', 'footer', 'form', 'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5',
	'h6', 'head', 'header', 'hr', 'html', 'iframe', 'legend', 'li', 'link', 'main', 'menu',
	'menuitem', 'meta', 'nav', 'noframes', 'ol', 'optgroup', 'option', 'p', 'param', 'search',
	'section', 'summary', 'table', 'tbody', 'td', 'tfoot', 'th', 'thead', 'title', 'tr', 'track',
	'ul'
]!
// vfmt on

// BlockParser parses markdown block structure line by line into an AST.
// After block parsing, inline content is parsed for every leaf node.
struct BlockParser {
	opts Options
mut:
	lines   []string
	pos     int
	ref_map map[string]LinkRef
	fn_defs map[string]&Node
}

// BlockParser.new creates a BlockParser for the given source.
fn BlockParser.new(src string, opts Options, ref_map map[string]LinkRef) BlockParser {
	normalized := src.replace('\r\n', '\n').replace('\r', '\n')
	mut lines := normalized.split('\n')
	// A trailing newline in the source is not an extra blank line; drop the
	// empty artifact produced by split.
	if lines.len > 0 && lines[lines.len - 1] == '' {
		lines = lines[..lines.len - 1]
	}
	mut refs := map[string]LinkRef{}
	for k, v in ref_map {
		refs[k] = v
	}
	return BlockParser{
		opts:    opts
		lines:   lines
		ref_map: refs
		fn_defs: map[string]&Node{}
	}
}

// nested_block_parser creates a nested parser that inherits the current options
// and reference definitions.
fn (p &BlockParser) nested_block_parser(lines []string) BlockParser {
	mut refs := map[string]LinkRef{}
	for k, v in p.ref_map {
		refs[k] = v
	}
	return BlockParser{
		opts:    p.opts
		lines:   lines
		ref_map: refs
		fn_defs: map[string]&Node{}
	}
}

// merge_nested_state propagates nested parser state back to the parent parser.
fn (mut p BlockParser) merge_nested_state(inner BlockParser) {
	for k, v in inner.ref_map {
		p.ref_map[k] = v
	}
	if p.opts.footnotes {
		for k, v in inner.fn_defs {
			if k !in p.fn_defs {
				p.fn_defs[k] = v
			}
		}
	}
}

// parse parses the full document and returns the AST root node.
fn (mut p BlockParser) parse() &Node {
	mut doc := new_node(.document)
	p.parse_blocks(mut doc, 0)
	// Attach collected footnote definitions as children of the document.
	if p.opts.footnotes {
		for _, fn_node in p.fn_defs {
			doc.append_child(fn_node)
		}
	}
	return doc
}

// parse_blocks fills parent with block-level children parsed from p.lines[p.pos..].
// indent is the minimum leading-space indent already consumed by a container.
fn (mut p BlockParser) parse_blocks(mut parent Node, indent int) {
	for p.pos < p.lines.len {
		line_raw := p.lines[p.pos]
		line := expand_tabs(line_raw)
		trimmed := line.trim_left(' \t')

		// --- blank line ---
		if is_blank(line) {
			p.pos++
			continue
		}

		stripped := trim_indent(line, indent)
		sp := leading_spaces(stripped)
		content := trim_indent(stripped, sp)

		// --- blockquote (>) ---
		// sp < 4 here (indented code handled above), so content is safe.
		if sp <= 3 && content.starts_with('>') {
			bq := p.parse_blockquote(indent)
			parent.append_child(bq)
			continue
		}

		// --- indented code block (4 spaces) ---
		if sp >= 4 {
			cb := p.parse_indented_code(indent)
			parent.append_child(cb)
			continue
		}

		// --- thematic break (---, ***, ___) ---
		if is_thematic_break(stripped) {
			node := new_node(.thematic_break)
			parent.append_child(node)
			p.pos++
			continue
		}

		// --- ATX heading (# ... ######) ---
		// sp < 4 here; content has container/leading indent removed.
		if heading := p.try_atx_heading(content) {
			parent.append_child(heading)
			p.pos++
			continue
		}

		// --- fenced code block (``` or ~~~) ---
		if fenced := p.try_fenced_code(stripped, indent) {
			parent.append_child(fenced)
			continue
		}

		// --- HTML block ---
		if html_blk := p.try_html_block(stripped, indent) {
			parent.append_child(html_blk)
			continue
		}

		// --- link reference definition ---
		// CommonMark allows 0-3 leading spaces after container indentation.
		if sp <= 3 && p.try_link_ref_def(indent) {
			continue
		}

		// --- footnote definition (if footnotes extension enabled) ---
		if p.opts.footnotes {
			if p.try_footnote_def(stripped, indent) {
				continue
			}
		}

		// --- list (bullet or ordered) ---
		if is_list_marker(stripped) {
			lst := p.parse_list(indent)
			parent.append_child(lst)
			continue
		}

		// --- GFM table (if tables extension enabled) ---
		if p.opts.tables {
			if tbl := p.try_table(indent) {
				parent.append_child(tbl)
				continue
			}
		}

		// --- definition list (if extension enabled) ---
		if p.opts.definition_list {
			if dl := p.try_definition_list(indent) {
				parent.append_child(dl)
				continue
			}
		}

		// --- paragraph (including setext headings) ---
		para := p.parse_paragraph(indent)
		if para.kind == .heading || para.kind == .paragraph {
			parent.append_child(para)
		}
		_ = trimmed
		_ = content
	}
}

// is_thematic_break returns true if line is a valid thematic break
// (three or more -, *, or _ with optional spaces).
fn is_thematic_break(line string) bool {
	trimmed := line.trim_space()
	if trimmed.len < 3 {
		return false
	}
	mut c := trimmed[0]
	if c != `-` && c != `*` && c != `_` {
		return false
	}
	mut count := 0
	for i := 0; i < trimmed.len; i++ {
		ch := trimmed[i]
		if ch == c {
			count++
		} else if ch != ` ` && ch != `\t` {
			return false
		}
	}
	return count >= 3
}

// try_atx_heading attempts to parse an ATX heading from line.
// Returns the heading node on success.
fn (mut p BlockParser) try_atx_heading(line string) ?&Node {
	if line.len == 0 || line[0] != `#` {
		return none
	}
	mut level := 0
	for level < line.len && line[level] == `#` {
		level++
	}
	if level > 6 {
		return none
	}
	if level < line.len && line[level] != ` ` && line[level] != `\t` {
		return none
	}
	mut content := line[level..].trim_space()
	// Strip trailing # sequence.
	for content.ends_with('#') {
		stripped := content.trim_right('#')
		if stripped.len == 0 || stripped.ends_with(' ') || stripped.ends_with('\t') {
			content = stripped.trim_right(' \t')
			break
		}
		break
	}
	mut node := new_node(.heading)
	node.level = level
	node.literal = content
	if p.opts.parser_opts.auto_heading_id {
		node.id = heading_id_from_text(content)
	}
	return node
}

// try_fenced_code attempts to parse a fenced code block starting at p.pos.
fn (mut p BlockParser) try_fenced_code(line string, indent int) ?&Node {
	// Up to 3 spaces of indentation are allowed before the fence.
	fence_offset := leading_spaces(line)
	off_line := trim_indent(line, fence_offset)
	fence_char, fence_len := detect_fence(off_line)
	if fence_len < 3 {
		return none
	}
	mut info := off_line[fence_len..].trim_space()
	// info string must not contain a backtick when using backtick fence.
	if fence_char == 96 && info.contains('`') {
		return none
	}
	// The info string is the first word only; decode escapes and entities.
	info = decode_link_string(info)
	p.pos++
	mut code_lines := []string{}
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		// Check for closing fence (up to 3 spaces of indentation allowed).
		close_offset := leading_spaces(stripped)
		if close_offset > 3 {
			code_lines << trim_indent(stripped, fence_offset)
			p.pos++
			continue
		}
		close_line := trim_indent(stripped, close_offset)
		close_char, close_len := detect_fence(close_line)
		if close_char == fence_char && close_len >= fence_len {
			rest := close_line[close_len..].trim_space()
			if rest.len == 0 {
				p.pos++
				break
			}
		}
		code_lines << trim_indent(stripped, fence_offset)
		p.pos++
	}
	mut node := new_node(.fenced_code)
	node.fence_info = info
	mut literal := ''
	if code_lines.len > 0 {
		literal = code_lines.join('\n') + '\n'
	}
	node.literal = literal
	return node
}

// detect_fence returns (fence_char, fence_length) if line starts with a valid
// code-fence sequence, or (0, 0) if not.
fn detect_fence(line string) (u8, int) {
	if line.len < 3 {
		return 0, 0
	}
	c := line[0]
	if c != 96 && c != `~` {
		return 0, 0
	}
	mut n := 0
	for n < line.len && line[n] == c {
		n++
	}
	if n >= 3 {
		return c, n
	}
	return 0, 0
}

// parse_indented_code collects lines that are indented by at least (indent+4)
// spaces (or blank) into an indented code block.
fn (mut p BlockParser) parse_indented_code(indent int) &Node {
	mut lines := []string{}
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		if is_blank(raw) {
			// Blank line may be included, but only if followed by more indented code.
			// Whitespace beyond the code indent is content (e.g. 6 spaces
			// contribute 2 spaces); trailing blank lines are trimmed below.
			stripped := trim_indent(raw, indent)
			sp := leading_spaces(stripped)
			lines << if sp >= 4 { stripped[4..] } else { stripped[sp..] }
			p.pos++
			continue
		}
		stripped := trim_indent(raw, indent)
		sp := leading_spaces(stripped)
		if sp < 4 {
			break
		}
		lines << trim_indent(stripped, 4)
		p.pos++
	}
	// Trim trailing blank lines.
	for lines.len > 0 && is_blank(lines[lines.len - 1]) {
		lines = lines[..lines.len - 1]
	}
	mut node := new_node(.code_block)
	node.literal = lines.join('\n') + '\n'
	return node
}

// try_html_block attempts to parse an HTML block starting at p.pos.
fn (mut p BlockParser) try_html_block(line string, indent int) ?&Node {
	html_type := detect_html_block_type(line)
	if html_type == 0 {
		return none
	}
	mut raw_lines := []string{}
	raw_lines << p.lines[p.pos]
	p.pos++
	// Types 1-5 may already end on the start line.
	if html_block_ends_on_line(html_type, line) {
		mut node := new_node(.html_block)
		node.literal = raw_lines.join('\n') + '\n'
		return node
	}
	// Types 1-5 end at specific end patterns; type 6-7 end at blank line.
	for p.pos < p.lines.len {
		raw := p.lines[p.pos]
		expanded := expand_tabs(raw)
		stripped := trim_indent(expanded, indent)
		match html_type {
			1 {
				raw_lines << raw
				p.pos++
				low := stripped.to_lower()
				if low.contains('</script>') || low.contains('</pre>') || low.contains('</style>')
					|| low.contains('</textarea>') {
					break
				}
			}
			2 {
				raw_lines << raw
				p.pos++
				if stripped.contains('-->') {
					break
				}
			}
			3 {
				raw_lines << raw
				p.pos++
				if stripped.contains('?>') {
					break
				}
			}
			4 {
				raw_lines << raw
				p.pos++
				if stripped.contains('>') {
					break
				}
			}
			5 {
				raw_lines << raw
				p.pos++
				if stripped.contains(']]>') {
					break
				}
			}
			6, 7 {
				if is_blank(stripped) {
					break
				}
				raw_lines << raw
				p.pos++
			}
			else {}
		}
	}
	mut node := new_node(.html_block)
	node.literal = raw_lines.join('\n') + '\n'
	return node
}

// detect_html_block_type returns the HTML block type (1-7) or 0 if the line
// does not start an HTML block.
fn detect_html_block_type(line string) int {
	stripped := line.trim_left(' \t')
	if stripped.len == 0 || stripped[0] != `<` {
		return 0
	}
	low := stripped.to_lower()
	// Type 2: HTML comment
	if low.starts_with('<!--') {
		return 2
	}
	// Type 3: processing instruction
	if low.starts_with('<?') {
		return 3
	}
	// Type 5: CDATA
	if low.starts_with('<![cdata[') {
		return 5
	}
	// Type 4: <!X
	if low.len > 2 && low[1] == `!` && low[2] >= `a` && low[2] <= `z` {
		return 4
	}
	// Type 1: script/pre/style/textarea
	for _, tag in ['<script', '<pre', '<style', '<textarea'] {
		if low.starts_with(tag) {
			rest := low[tag.len..]
			if rest.len == 0 || rest[0] == ` ` || rest[0] == `\t` || rest[0] == `>`
				|| rest[0] == `\n` {
				return 1
			}
		}
	}
	// Type 6: block-level tag (`<tag` or `</tag`, followed by space, tab,
	// end of line, `>` or `/>`).
	tag_name, tag_end := extract_tag_name(low[1..])
	if tag_name.len > 0 {
		after := low[1 + tag_end..]
		if after.len == 0 || after[0] == ` ` || after[0] == `\t` || after[0] == `>`
			|| after[0] == `/` {
			for bt in block_level_tags {
				if tag_name == bt {
					return 6
				}
			}
		}
	}
	// Type 7: complete open/close tag not in type 6
	if is_complete_html_tag(stripped) {
		return 7
	}
	return 0
}

// html_block_ends_on_line reports whether an HTML block of the given type
// already ends on its start line (the end condition is met there).
fn html_block_ends_on_line(html_type int, line string) bool {
	low := line.to_lower()
	match html_type {
		1 {
			return low.contains('</script>') || low.contains('</pre>')
				|| low.contains('</style>') || low.contains('</textarea>')
		}
		2 {
			// A comment ends at `-->`, or abruptly at `-->`... no: at `<!-->`
			// or `<!--->` (which are complete comments by themselves).
			if line.contains('<!-->') || line.contains('<!--->') {
				return true
			}
			return line.index_after_('-->', 4) >= 0
		}
		3 {
			return line.contains('?>')
		}
		4 {
			return line.contains('>')
		}
		5 {
			return line.contains(']]>')
		}
		else {
			return false
		}
	}
}

// extract_tag_name extracts the tag name from the beginning of s (after
// `<`), allowing an optional leading `/` for closing tags. It also returns
// the index just past the name, for follower validation.
fn extract_tag_name(s string) (string, int) {
	mut start := 0
	if start < s.len && s[start] == `/` {
		start++
	}
	mut end := start
	for end < s.len && (is_alnum(s[end]) || s[end] == `-`) {
		end++
	}
	return s[start..end].to_lower(), end
}

// is_complete_html_tag returns true if s is a complete open or close tag
// (with only trailing whitespace after it), for HTML block type 7.
fn is_complete_html_tag(s string) bool {
	tag_end := scan_html_tag(s.trim_left(' \t'))
	if tag_end < 0 {
		return false
	}
	return s.trim_left(' \t')[tag_end..].trim_space().len == 0
}

// try_link_ref_def attempts to parse a link reference definition at p.pos.
// CommonMark allows the title to appear on the next line when the destination
// is alone on the first line.  Returns true and advances p.pos if successful.
fn (mut p BlockParser) try_link_ref_def(indent int) bool {
	first_raw := expand_tabs(p.lines[p.pos])
	first := trim_indent(first_raw, indent).trim_left(' \t')
	if !first.starts_with('[') {
		return false
	}
	// Find closing bracket of the label: the first `]` on the line.
	// The label may span lines (each continuation line appended, up to a
	// few lines, no blanks). Labels may not contain unescaped `[`.
	mut label_parts := []string{}
	mut end_off := 0
	mut end_col := -1
	mut end_text := ''
	mut li_scan := 0
	for li_scan < 3 {
		cur := if li_scan == 0 {
			first[1..]
		} else {
			if p.pos + li_scan >= p.lines.len {
				break
			}
			trim_indent(expand_tabs(p.lines[p.pos + li_scan]), indent).trim_right(' \t')
		}
		if li_scan > 0 && is_blank(cur) {
			break
		}
		mut c := 0
		mut closed := false
		for c < cur.len {
			if cur[c] == `\\` {
				c += 2
				continue
			}
			if cur[c] == `[` {
				// Labels may not contain an unescaped `[`.
				return false
			}
			if cur[c] == `]` {
				closed = true
				break
			}
			c++
		}
		if closed {
			label_parts << cur[..c]
			end_off = li_scan
			end_col = c + if li_scan == 0 { 1 } else { 0 }
			end_text = if li_scan == 0 { first } else { cur }
			break
		}
		label_parts << cur
		li_scan++
	}
	if end_col < 0 {
		return false
	}
	label := normalize_label(label_parts.join('\n'))
	if label.len == 0 {
		return false
	}
	// The `]:` must close on the same line where `]` was found.
	// end_col is relative to end_text (both use the same trimmed form).
	if end_col + 1 >= end_text.len || end_text[end_col + 1] != `:` {
		return false
	}
	rest := end_text[end_col + 2..].trim_left(' \t')
	// The destination may start on the next line (no blank line between).
	mut li := p.pos + end_off
	mut dest := ''
	mut after_dest := ''
	if rest.len > 0 {
		dest, after_dest = parse_link_dest(rest)
		if after_dest == rest {
			return false
		}
	} else {
		if li + 1 >= p.lines.len {
			return false
		}
		next := expand_tabs(p.lines[li + 1]).trim_left(' \t')
		if is_blank(next) {
			return false
		}
		dest, after_dest = parse_link_dest(next)
		if after_dest == next {
			return false
		}
		li++
	}
	// Optional title: remainder of the destination line, else following lines.
	// The title may span multiple lines but not contain a blank line.
	// When both destination and title are present they must be separated.
	mut title := ''
	title_first := after_dest.trim_left(' \t')
	if title_first.len > 0 {
		if after_dest.len == title_first.len {
			// Title abuts the destination with no separation: invalid.
			return false
		}
		t, trem, used := parse_title_spanning(title_first, p.lines, li + 1, indent)
		if trem.trim_space().len > 0 {
			// Trailing content after the title: invalid definition.
			return false
		}
		title = t
		li += used
	} else if li + 1 < p.lines.len {
		cand := trim_indent(expand_tabs(p.lines[li + 1]), indent).trim_left(' \t')
		if cand.len > 0 && (cand[0] == `"` || cand[0] == `'` || cand[0] == `(`) {
			t, trem, used := parse_title_spanning(cand, p.lines, li + 2, indent)
			if trem.trim_space().len == 0 {
				title = t
				li += 1 + used
			}
			// A failed next-line title does not invalidate the definition.
		}
	}
	if label !in p.ref_map {
		p.ref_map[label] = LinkRef{
			dest:  dest
			title: title
		}
	}
	p.pos = li + 1
	return true
}

// parse_title_spanning parses a link title starting at frag (which must begin
// with `"`, `'` or `(`), extending over following lines if the closing quote
// is not on the first fragment. Returns (title, rest, extra_lines_used).
// A blank line before the title closes fails the parse.
fn parse_title_spanning(frag string, lines []string, start int, indent int) (string, string, int) {
	cur := frag.trim_left(' \t')
	if cur.len == 0 || (cur[0] != `"` && cur[0] != `'` && cur[0] != `(`) {
		return '', frag, 0
	}
	mut acc := cur
	mut used := 0
	for {
		t, rest := parse_link_title(acc)
		if rest != acc {
			return t, rest, used
		}
		if start + used >= lines.len {
			return '', frag, 0
		}
		next := trim_indent(expand_tabs(lines[start + used]), indent).trim_right(' \t')
		if is_blank(next) {
			return '', frag, 0
		}
		acc += '\n' + next
		used++
	}
	return '', frag, 0
}

// parse_link_dest parses a link destination from s and returns (dest, rest).
fn parse_link_dest(s string) (string, string) {
	if s.len == 0 {
		return '', s
	}
	if s[0] == `<` {
		// Angle-bracket form: no line endings, no unescaped `<` or `>`.
		mut i := 1
		for i < s.len {
			c := s[i]
			if c == `\n` || c == `\r` || c == `<` {
				return '', s
			}
			if c == `\\` && i + 1 < s.len && is_ascii_punct(rune(s[i + 1])) {
				i += 2
				continue
			}
			if c == `>` {
				return decode_link_string(s[1..i]), s[i + 1..]
			}
			i++
		}
		return '', s
	}
	// Regular form: no spaces, no control characters, balanced parentheses.
	mut parens := 0
	mut i := 0
	for i < s.len {
		c := s[i]
		if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			break
		}
		if c == `(` {
			parens++
		} else if c == `)` {
			if parens == 0 {
				break
			}
			parens--
		} else if c == `\\` && i + 1 < s.len {
			i += 2
			continue
		}
		i++
	}
	if i == 0 {
		return '', s
	}
	if parens != 0 {
		return '', s
	}
	return decode_link_string(s[..i]), s[i..]
}

// parse_link_title parses an optional link title from s and returns (title, rest).
// Returns empty string if no valid title is found.
fn parse_link_title(s string) (string, string) {
	if s.len == 0 {
		return '', s
	}
	open := s[0]
	mut close := u8(0)
	match open {
		`"` { close = `"` }
		`'` { close = `'` }
		`(` { close = `)` }
		else { return '', s }
	}

	mut i := 1
	for i < s.len {
		c := s[i]
		if c == close {
			return decode_link_string(s[1..i]), s[i + 1..]
		}
		if c == `\\` && i + 1 < s.len {
			i += 2
			continue
		}
		i++
	}
	return '', s
}

// is_para_text_line reports whether s could be paragraph continuation text,
// i.e. it does not start a block construct (in container-relative coords).
// Used to decide lazy continuation: laziness needs an open paragraph.
fn is_para_text_line(s string) bool {
	if is_blank(s) {
		return false
	}
	// Indented code is a block start, not paragraph text.
	if leading_spaces(s) >= 4 {
		return false
	}
	t := s.trim_left(' \t')
	if t.len == 0 {
		return false
	}
	// A nested blockquote marker defers to nested parsing, which re-evaluates
	// laziness at each level (e.g. `> > > foo` followed by `bar`).
	ls := leading_spaces(s)
	if ls <= 3 && s[ls..].starts_with('>') {
		return true
	}
	if t.starts_with('```') || t.starts_with('~~~') {
		return false
	}
	if t[0] == `#` && atx_heading_level(t) > 0 {
		return false
	}
	if is_thematic_break(t) {
		return false
	}
	if t[0] == `>` {
		return false
	}
	if t[0] == `<` {
		ht := detect_html_block_type(t)
		if ht >= 1 && ht <= 6 {
			return false
		}
	}
	if setext_underline_level(t) > 0 {
		return false
	}
	_, mok := parse_list_marker(t)
	if mok {
		// A list marker keeps a paragraph open only if content follows it
		// on the same line (otherwise no paragraph is open yet).
		return marker_has_content(t)
	}
	return true
}

// marker_has_content reports whether the list marker starting t is followed
// by non-blank content on the same line (which opens a paragraph).
fn marker_has_content(t string) bool {
	mut i := 0
	for i < t.len && t[i] == ` ` {
		i++
	}
	// Skip bullet or ordered marker.
	if t[i] == `-` || t[i] == `*` || t[i] == `+` {
		i++
	} else {
		for i < t.len && is_digit(t[i]) {
			i++
		}
		if i < t.len && (t[i] == `.` || t[i] == `)`) {
			i++
		} else {
			return false
		}
	}
	// Skip one separator space/tab.
	if i < t.len && (t[i] == ` ` || t[i] == `\t`) {
		i++
	}
	return i < t.len && !is_blank(t[i..])
}

// escape_setext_underline escapes a lazily-absorbed setext underline so the
// nested parse keeps it as literal text: a setext underline cannot form a
// heading from a lazy continuation line, but the line stays in the paragraph.
fn escape_setext_underline(s string) string {
	idx := s.index('=') or { return s }
	return s[..idx] + '\\' + s[idx..]
}

// strip_bq_marker consumes one blockquote marker (`>`) at the start of s
// (which must already be stripped of container indent, with up to 3 leading
// spaces) plus one optional separator space/tab, and returns the content.
// base_col is the column of `>` in the original line (for tab stops).
// Tabs are expanded with tab stops relative to the line start: the separator
// accounts for a single column, and any remaining expanded columns become
// content indent (preserving column alignment for nested blocks). The rest
// of the line after leading whitespace is kept byte-identical so that tabs
// inside code content survive.
fn strip_bq_marker(s string, base_col int) (string, bool) {
	if s.len == 0 || s[0] != `>` {
		return s, false
	}
	mut i := 1
	mut col := base_col + 1
	mut prefix := ''
	if i < s.len && s[i] == ` ` {
		i++
		col++
	} else if i < s.len && s[i] == `\t` {
		// Separator tab: one column is the separator, the rest is content.
		n := 4 - (col % 4)
		for _ in 0 .. n - 1 {
			prefix += ' '
		}
		col += n
		i++
	}
	// Expand further leading tabs (column-accurate), keep the rest raw.
	for i < s.len && (s[i] == ` ` || s[i] == `\t`) {
		if s[i] == ` ` {
			prefix += ' '
			col++
			i++
		} else {
			n := 4 - (col % 4)
			for _ in 0 .. n {
				prefix += ' '
			}
			col += n
			i++
		}
	}
	return prefix + s[i..], true
}

// parse_blockquote parses a blockquote block and returns a blockquote node.
fn (mut p BlockParser) parse_blockquote(indent int) &Node {
	mut bq_lines := []string{}
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		if is_blank(stripped) {
			// Lazy continuation stops at blank.
			p.pos++
			break
		}
		// A blockquote marker allows up to 3 leading spaces past the
		// container indent; consume the marker plus one optional space.
		// Tabs are expanded with tab stops: the separator is one column,
		// and the rest of an expanded tab belongs to the content (this
		// matters for indented code inside quotes).
		ls := leading_spaces(stripped)
		if ls <= 3 {
			rest := stripped[ls..]
			if rest.starts_with('>') {
				content, ok := strip_bq_marker(rest, ls)
				if ok {
					bq_lines << content
					p.pos++
					continue
				}
			}
		}
		// Lazy continuation line: only paragraph continuation text with no
		// blank line intervening can be lazy, and the quote must have an
		// open paragraph (the previous line is paragraph text).
		if interrupts_paragraph(stripped) {
			break
		}
		if bq_lines.len == 0 || !is_para_text_line(bq_lines[bq_lines.len - 1]) {
			break
		}
		lazy_line := if setext_underline_level(stripped) == 1 {
			escape_setext_underline(stripped)
		} else {
			stripped
		}
		bq_lines << lazy_line
		p.pos++
	}
	mut node := new_node(.blockquote)
	// Recursively parse the blockquote content.
	mut inner := p.nested_block_parser(bq_lines)
	inner.parse_blocks(mut node, 0)
	p.merge_nested_state(inner)
	return node
}

// ListMarker holds parsed list marker information.
struct ListMarker {
	is_ordered  bool
	bullet_char u8
	start       int
	indent      int    // content indent in columns (leading indent included)
	content     string // content after the marker (whitespace run processed)
}

// is_list_marker returns true if line starts with a bullet or ordered list marker.
fn is_list_marker(line string) bool {
	if line.len == 0 {
		return false
	}
	_, ok := parse_list_marker(line)
	return ok
}

// parse_list_marker parses a list marker from the beginning of line.
fn parse_list_marker(line string) (ListMarker, bool) {
	sp := leading_spaces(line)
	rest := line[sp..]
	if rest.len == 0 {
		return ListMarker{}, false
	}
	// Bullet list: -, *, +
	if rest[0] == `-` || rest[0] == `*` || rest[0] == `+` {
		if rest.len < 2 || (rest[1] != ` ` && rest[1] != `\t`) {
			// Only a bare marker with no space is not valid (except empty item for -)
			if rest.len == 1 {
				return ListMarker{
					is_ordered:  false
					bullet_char: rest[0]
					indent:      sp + 2
					content:     ''
				}, true
			}
			return ListMarker{}, false
		}
		ci, cs := marker_content(rest, sp, 1)
		return ListMarker{
			is_ordered:  false
			bullet_char: rest[0]
			indent:      ci
			content:     cs
		}, true
	}
	// Ordered list: 1. or 1) with at most 9 digits (spec limit).
	mut num_end := 0
	for num_end < rest.len && is_digit(rest[num_end]) {
		num_end++
	}
	if num_end > 0 && num_end <= 9 && num_end < rest.len
		&& (rest[num_end] == `.` || rest[num_end] == `)`) {
		marker_end := num_end + 1
		if marker_end < rest.len && rest[marker_end] != ` ` && rest[marker_end] != `\t` {
			return ListMarker{}, false
		}
		num_str := rest[..num_end]
		start := num_str.int()
		ci, cs := marker_content(rest, sp, marker_end)
		return ListMarker{
			is_ordered:  true
			start:       start
			bullet_char: rest[num_end]
			indent:      ci
			content:     cs
		}, true
	}
	return ListMarker{}, false
}

// marker_content computes the content indent (in columns) and the content
// string for a marker ending at marker_end (index in rest, where rest starts
// after sp leading spaces). The whitespace run after the marker is measured
// with tab stops: if it spans 5+ columns, only one column separates the
// marker from the content and the rest (as spaces) belongs to the content
// (matters for indented code); otherwise the run counts toward the indent.
// An empty run means an empty item: content follows the marker.
fn marker_content(rest string, sp int, marker_end int) (int, string) {
	mut wi := marker_end
	mut wcol := sp + marker_end
	for wi < rest.len && (rest[wi] == ` ` || rest[wi] == `\t`) {
		if rest[wi] == ` ` {
			wcol++
		} else {
			wcol += 4 - (wcol % 4)
		}
		wi++
	}
	w := wcol - (sp + marker_end)
	if wi >= rest.len {
		// Empty item (nothing but whitespace after the marker): trailing
		// spaces do not count, content follows the marker.
		return sp + marker_end + 1, ''
	}
	if w >= 5 {
		return sp + marker_end + 1, ' '.repeat(w - 1) + rest[wi..]
	}
	return wcol, rest[wi..]
}

// parse_list parses a list (bullet or ordered) and returns a list node.
fn (mut p BlockParser) parse_list(indent int) &Node {
	// Determine list type from the first item's marker.
	if p.pos >= p.lines.len {
		return new_node(.list)
	}

	first_raw := expand_tabs(p.lines[p.pos])
	first_line := trim_indent(first_raw, indent)
	marker, ok := parse_list_marker(first_line)
	if !ok {
		return new_node(.list)
	}

	mut list := new_node(.list)
	list.is_ordered = marker.is_ordered
	list.list_start = if marker.is_ordered { marker.start } else { 1 }
	list.is_tight = true

	mut had_blank := false

	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)

		if is_blank(stripped) {
			had_blank = true
			p.pos++
			continue
		}

		cur_marker, marker_ok := parse_list_marker(stripped)
		if !marker_ok {
			break
		}
		// List markers allow 0-3 leading spaces; deeper markers cannot
		// start an item (they are indented code or item content).
		if leading_spaces(stripped) > 3 {
			break
		}
		// A thematic break ends the list (it takes precedence over a
		// same-marker list continuation).
		if is_thematic_break(stripped) {
			break
		}
		// Different list type → stop.
		if cur_marker.is_ordered != marker.is_ordered {
			break
		}
		if !cur_marker.is_ordered && cur_marker.bullet_char != marker.bullet_char {
			break
		}
		// Ordered lists with different delimiters (`.` vs `)`) are separate lists.
		if cur_marker.is_ordered && cur_marker.bullet_char != marker.bullet_char {
			break
		}

		if had_blank {
			list.is_tight = false
		}
		had_blank = false

		item, item_loose := p.parse_list_item(indent)
		if item_loose {
			list.is_tight = false
		}
		list.append_child(item)
	}

	return list
}

// parse_list_item parses a single list item and returns a list_item node.
// The second return value reports whether a blank line was consumed inside
// the item (between content lines), which makes the parent list loose.
fn (mut p BlockParser) parse_list_item(base_indent int) (&Node, bool) {
	if p.pos >= p.lines.len {
		return new_node(.list_item), false
	}

	mut item := new_node(.list_item)

	// Get first line of the item
	first_raw := expand_tabs(p.lines[p.pos])
	first_stripped := trim_indent(first_raw, base_indent)

	// Extract content after marker. The item carries its own indentation
	// (up to 3 spaces for a top-level list), so skip it before matching the
	// marker: otherwise a `-` after the indent is misread and the marker
	// text leaks into the content, which then parses as a nested list.
	// first_leading below adds the skipped width back into content_indent.
	first_leading := leading_spaces(first_stripped)
	marker_line := first_stripped[first_leading..]
	// Exactly one column separates the marker from the content when the
	// whitespace run spans 5+ columns; otherwise the run counts toward
	// the indent. parse_list_marker computes both forms.
	m, mok := parse_list_marker(marker_line)
	// m.indent is the content indent in columns; m.content is the content
	// after the marker (whitespace run processed per spec).
	mut marker_idx := if mok { m.indent } else { 0 }
	mut first_content := if mok { m.content } else { marker_line }

	// Detect task list checkbox: [ ] or [x] or [X] at the start of content
	// (leading spaces after an extra-wide marker are tolerated).
	trimmed_fc := first_content.trim_left(' \t')
	mut task_checked := false
	mut has_task := false
	mut task_content_start := 0
	if p.opts.task_list && trimmed_fc.len >= 3 && trimmed_fc[0] == `[` {
		if (trimmed_fc[1] == ` ` || trimmed_fc[1] == `x` || trimmed_fc[1] == `X`)
			&& trimmed_fc[2] == `]` {
			if trimmed_fc.len == 3 || trimmed_fc[3] == ` ` || trimmed_fc[3] == `\t` {
				has_task = true
				task_checked = trimmed_fc[1] != ` `
				// Skip the checkbox and one optional following whitespace char.
				task_content_start = if trimmed_fc.len > 3 { 4 } else { 3 }
			}
		}
	}
	actual_first_content := if has_task {
		trimmed_fc[task_content_start..]
	} else {
		first_content
	}

	// Collect lines belonging to this item
	mut item_lines := [actual_first_content]
	// item_sps parallels item_lines with each line's indent (after container
	// indent removal, before content-indent removal); -1 marks blank lines.
	// It is used to attribute blank lines to the nesting level they separate.
	mut item_sps := [0]
	p.pos++

	// Content indent is where subsequent lines must be indented to
	content_indent := first_leading + marker_idx

	mut saw_inner_blank := false
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped_base := trim_indent(raw, base_indent)

		if is_blank(stripped_base) {
			// An item that starts with a blank line (empty first content)
			// cannot swallow a second blank: `-` followed by a blank line
			// ends the (empty) item.
			if item_lines.len == 1 && item_lines[0] == '' {
				break
			}
			// Only consume blank lines that separate more item content.
			// Peek at the next non-blank line: if it is indented enough
			// to continue this item, the blank belongs to the item;
			// otherwise leave it (and what follows) for the outer parser,
			// which decides tight/loose between items.
			mut k := p.pos + 1
			for k < p.lines.len {
				peek_raw := expand_tabs(p.lines[k])
				peek_stripped := trim_indent(peek_raw, base_indent)
				if !is_blank(peek_stripped) {
					break
				}
				k++
			}
			if k >= p.lines.len {
				for p.pos < p.lines.len {
					p.pos++
				}
				break
			}
			peek_raw := expand_tabs(p.lines[k])
			peek_stripped := trim_indent(peek_raw, base_indent)
			if leading_spaces(peek_stripped) >= content_indent {
				item_lines << ''
				item_sps << -1
				saw_inner_blank = true
				p.pos++
				continue
			}
			break
		}

		sp := leading_spaces(stripped_base)

		// If line has enough indent, include it
		if sp >= content_indent {
			// Remove the content indent
			item_lines << trim_indent(stripped_base, content_indent)
			item_sps << sp - content_indent
			p.pos++
			continue
		}

		// A list marker indented 3 or less ends this item (a same-list item
		// or a new list is decided by the caller). A deeper marker cannot
		// start an item, so it falls through to lazy continuation below.
		_, is_mk := parse_list_marker(stripped_base)
		if is_mk && sp <= 3 {
			break
		}

		// Check if it might be a sub-block (less indented but meaningful)
		if is_thematic_break(stripped_base) || stripped_base.starts_with('#') {
			break
		}

		// Lazy continuation: only paragraph continuation text with no
		// blank line intervening can be lazy, and the item must have an
		// open paragraph (the previous line is paragraph text).
		if saw_inner_blank {
			break
		}
		if item_lines.len == 0 || !is_para_text_line(item_lines[item_lines.len - 1]) {
			break
		}
		lazy_line := if setext_underline_level(stripped_base) == 1 {
			escape_setext_underline(stripped_base)
		} else {
			stripped_base
		}
		item_lines << lazy_line
		item_sps << sp
		p.pos++
	}

	// Trim trailing blank lines
	for item_lines.len > 0 && item_lines[item_lines.len - 1] == '' {
		item_lines = item_lines[..item_lines.len - 1]
		item_sps = item_sps[..item_sps.len - 1]
	}
	// A blank line makes the parent list loose only when it directly separates
	// two blocks of this item. Blanks fully inside nested content (a nested
	// list, quote or code) loosen that nested structure instead: the blank is
	// nested when the preceding line opens nested content (a list marker) that
	// the following line continues (a list marker, or indented content).
	mut inner_loose := false
	// Fence state for skipping blanks swallowed by fenced code blocks.
	mut fence_ch := u8(0)
	mut fence_len := 0
	for idx := 0; idx < item_lines.len; idx++ {
		ln := item_lines[idx]
		if ln != '' {
			ls := leading_spaces(ln)
			if ls <= 3 {
				rest := ln[ls..]
				if rest.starts_with('```') || rest.starts_with('~~~') {
					c := rest[0]
					mut n := 0
					for n < rest.len && rest[n] == c {
						n++
					}
					if fence_ch == 0 {
						fence_ch = c
						fence_len = n
					} else if c == fence_ch && n >= fence_len
						&& rest[n..].trim_space().len == 0 {
						fence_ch = 0
						fence_len = 0
					}
				}
			}
			continue
		}
		if fence_ch != 0 {
			continue
		}
		mut pb := idx - 1
		for pb >= 0 && item_lines[pb] == '' {
			pb--
		}
		mut nb := idx + 1
		for nb < item_lines.len && item_lines[nb] == '' {
			nb++
		}
		if pb < 0 || nb >= item_lines.len {
			continue
		}
		_, prev_mk := parse_list_marker(item_lines[pb])
		if !prev_mk {
			inner_loose = true
			break
		}
		_, peek_mk := parse_list_marker(item_lines[nb])
		if peek_mk || item_sps[nb] > 0 {
			continue
		}
		inner_loose = true
		break
	}

	// Recursively parse the item's content with fresh parser
	mut inner := p.nested_block_parser(item_lines)
	inner.parse_blocks(mut item, 0)

	// Merge back any new link references and footnote definitions.
	p.merge_nested_state(inner)

	// Prepend task checkbox node if detected (must be first child).
	if has_task {
		mut chk := new_node(.task_checkbox)
		chk.checked = task_checked
		mut new_children := [chk]
		for child in item.children {
			new_children << child
		}
		item.children = new_children
	}

	return item, inner_loose
}

// try_table attempts to parse a GFM table starting at p.pos.
// A table requires a header row, an alignment row (|---|), then data rows.
fn (mut p BlockParser) try_table(indent int) ?&Node {
	if p.pos + 1 >= p.lines.len {
		return none
	}
	header_raw := expand_tabs(p.lines[p.pos])
	header_line := trim_indent(header_raw, indent)
	sep_raw := expand_tabs(p.lines[p.pos + 1])
	sep_line := trim_indent(sep_raw, indent)

	if !is_table_separator(sep_line) {
		return none
	}
	if !header_line.contains('|') {
		return none
	}

	aligns := parse_table_alignments(sep_line)
	if aligns.len == 0 {
		return none
	}

	mut tbl := new_node(.table)

	// Header row.
	mut head := new_node(.table_head)
	header_row := parse_table_row(header_line, aligns)
	head.append_child(header_row)
	tbl.append_child(head)

	p.pos += 2

	// Body rows.
	mut body := new_node(.table_body)
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		if is_blank(stripped) || !stripped.contains('|') {
			break
		}
		body.append_child(parse_table_row(stripped, aligns))
		p.pos++
	}
	if body.children.len > 0 {
		tbl.append_child(body)
	}
	return tbl
}

// is_table_separator returns true if line is a table alignment separator row.
fn is_table_separator(line string) bool {
	trimmed := line.trim('| \t')
	if trimmed.len == 0 {
		return false
	}
	cells := split_table_cells(line)
	if cells.len == 0 {
		return false
	}
	for cell in cells {
		c := cell.trim_space()
		if c.len == 0 {
			continue
		}
		inner := if c.starts_with(':') && c.ends_with(':') {
			c[1..c.len - 1]
		} else if c.starts_with(':') {
			c[1..]
		} else if c.ends_with(':') {
			c[..c.len - 1]
		} else {
			c
		}
		for ch in inner.runes() {
			if ch != `-` {
				return false
			}
		}
		if inner.len == 0 {
			return false
		}
	}
	return true
}

// parse_table_alignments returns the alignment for each column from a separator line.
fn parse_table_alignments(line string) []Alignment {
	cells := split_table_cells(line)
	mut aligns := []Alignment{}
	for cell in cells {
		c := cell.trim_space()
		if c.len == 0 {
			continue
		}
		left := c.starts_with(':')
		right := c.ends_with(':')
		if left && right {
			aligns << .center
		} else if left {
			aligns << .left
		} else if right {
			aligns << .right
		} else {
			aligns << .none_
		}
	}
	return aligns
}

// parse_table_row parses a table row line into a table_row node.
fn parse_table_row(line string, aligns []Alignment) &Node {
	cells := split_table_cells(line)
	mut row := new_node(.table_row)
	for i, cell_text in cells {
		mut cell := new_node(.table_cell)
		cell.align = if i < aligns.len { aligns[i] } else { .none_ }
		cell.literal = cell_text.trim_space()
		row.append_child(cell)
	}
	return row
}

// split_table_cells splits a table row line by pipe characters.
fn split_table_cells(line string) []string {
	trimmed := line.trim_space()
	// Strip leading/trailing pipe.
	inner := if trimmed.starts_with('|') && trimmed.ends_with('|') && trimmed.len > 1 {
		trimmed[1..trimmed.len - 1]
	} else if trimmed.starts_with('|') {
		trimmed[1..]
	} else if trimmed.ends_with('|') {
		trimmed[..trimmed.len - 1]
	} else {
		trimmed
	}
	mut cells := []string{}
	mut current := strings.new_builder(32)
	for i := 0; i < inner.len; i++ {
		if inner[i] == `\\` && i + 1 < inner.len && inner[i + 1] == `|` {
			current.write_u8(`|`)
			i++
		} else if inner[i] == `|` {
			cells << current.str()
			current = strings.new_builder(32)
		} else {
			current.write_u8(inner[i])
		}
	}
	last := current.str()
	cells << last
	return cells
}

// try_definition_list attempts to parse a definition list starting at p.pos.
fn (mut p BlockParser) try_definition_list(indent int) ?&Node {
	if p.pos + 1 >= p.lines.len {
		return none
	}
	next_raw := expand_tabs(p.lines[p.pos + 1])
	next_stripped := trim_indent(next_raw, indent)
	if !next_stripped.starts_with(':') {
		return none
	}
	mut dl := new_node(.definition_list)
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		if is_blank(stripped) {
			p.pos++
			break
		}
		// Collect term line.
		mut term := new_node(.definition_term)
		term.literal = stripped
		p.pos++
		// Collect one or more definitions (:).
		for p.pos < p.lines.len {
			def_raw := expand_tabs(p.lines[p.pos])
			def_stripped := trim_indent(def_raw, indent)
			if def_stripped.starts_with(':') {
				mut desc := new_node(.definition_desc)
				desc.literal = def_stripped[1..].trim_left(' \t')
				term.append_child(desc)
				p.pos++
			} else {
				break
			}
		}
		dl.append_child(term)
	}
	if dl.children.len == 0 {
		return none
	}
	return dl
}

// try_footnote_def attempts to parse a footnote definition starting at p.pos.
fn (mut p BlockParser) try_footnote_def(line string, indent int) bool {
	if !line.starts_with('[^') {
		return false
	}
	end := line.index_after_(']', 2)
	if end < 0 || end + 1 >= line.len || line[end + 1] != `:` {
		return false
	}
	label := line[2..end]
	if label.len == 0 {
		return false
	}
	content := line[end + 2..].trim_left(' \t')
	mut fn_node := new_node(.footnote_def)
	fn_node.fn_label = label
	fn_node.literal = content
	p.pos++
	// Collect continuation lines (indented by at least 4 spaces).
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		if is_blank(stripped) || leading_spaces(stripped) >= 4 {
			fn_node.literal += '\n' + stripped.trim_left('    ')
			p.pos++
		} else {
			break
		}
	}
	if label !in p.fn_defs {
		p.fn_defs[label] = fn_node
	}
	return true
}

// parse_paragraph parses a paragraph block, upgrading it to a setext heading
// if the immediately following line is a setext underline (=== or ---).
fn (mut p BlockParser) parse_paragraph(indent int) &Node {
	mut para_lines := []string{}
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		ls := leading_spaces(stripped)

		if is_blank(stripped) {
			p.pos++
			break
		}
		// Check for setext underline (0-3 leading spaces) before checking
		// block starts: a setext underline takes precedence.
		if para_lines.len > 0 && ls <= 3 {
			level := setext_underline_level(stripped)
			if level > 0 {
				content := para_lines.join('\n').trim_space()
				mut node := new_node(.heading)
				node.level = level
				node.literal = content
				if p.opts.parser_opts.auto_heading_id {
					node.id = heading_id_from_text(content)
				}
				p.pos++
				return node
			}
		}
		// Other block starters interrupt a paragraph (0-3 leading spaces).
		if para_lines.len > 0 && interrupts_paragraph(stripped) {
			break
		}
		// Continuation lines: leading whitespace is consumed.
		para_lines << stripped.trim_left(' \t')
		p.pos++
	}
	mut node := new_node(.paragraph)
	node.literal = para_lines.join('\n').trim_space()
	return node
}

// setext_underline_level returns 1 if line is a setext underline of `=`s and
// 2 if it is one of `-`s, 0 otherwise. Only an unbroken run of a single
// character (plus trailing whitespace) qualifies.
fn setext_underline_level(line string) int {
	trimmed := line.trim_space()
	if trimmed.len == 0 {
		return 0
	}
	c := trimmed[0]
	if c != `=` && c != `-` {
		return 0
	}
	for i := 0; i < trimmed.len; i++ {
		if trimmed[i] != c {
			return 0
		}
	}
	return if c == `=` { 1 } else { 2 }
}

// is_setext_underline returns true if line is a setext heading underline
// (0-3 leading spaces, then one or more = or - with optional trailing spaces).
fn is_setext_underline(line string) bool {
	return setext_underline_level(line) > 0
}

// interrupts_paragraph reports whether stripped (after container indent
// removal) starts a block that can interrupt an open paragraph.
fn interrupts_paragraph(stripped string) bool {
	if is_blank(stripped) {
		return false
	}
	// 4+ leading spaces: indented code, which cannot interrupt a paragraph.
	if leading_spaces(stripped) >= 4 {
		return false
	}
	t := stripped.trim_left(' \t')
	if is_thematic_break(t) {
		return true
	}
	if t[0] == `#` {
		// Only a valid ATX heading start interrupts.
		return atx_heading_level(t) > 0
	}
	if t.starts_with('```') || t.starts_with('~~~') {
		return true
	}
	if t[0] == `>` {
		return true
	}
	// HTML block types 1-6 interrupt a paragraph (type 7 does not).
	if t[0] == `<` {
		ht := detect_html_block_type(t)
		return ht >= 1 && ht <= 6
	}
	// List items interrupt paragraphs, with restrictions: the content after
	// the marker must be non-blank, and ordered markers must start at 1.
	marker, ok := parse_list_marker(t)
	if ok {
		return list_marker_interrupts(t, marker)
	}
	return false
}

// list_marker_interrupts applies the paragraph-interruption restrictions:
// non-blank content after the marker, and ordered markers start at 1.
fn list_marker_interrupts(stripped string, marker ListMarker) bool {
	if marker.is_ordered && marker.start != 1 {
		return false
	}
	t := stripped.trim_left(' \t')
	if t.len == 0 {
		return false
	}
	mut end := 0
	if t[0] == `-` || t[0] == `*` || t[0] == `+` {
		end = 1
	} else {
		for end < t.len && is_digit(t[end]) {
			end++
		}
		if end >= t.len || (t[end] != `.` && t[end] != `)`) {
			return false
		}
		end++
	}
	// Require non-blank content after the marker.
	for end < t.len && (t[end] == ` ` || t[end] == `\t`) {
		end++
	}
	return end < t.len
}

// atx_heading_level returns the ATX heading level (1-6) or 0 if line does not
// start a valid ATX heading.
fn atx_heading_level(line string) int {
	if line.len == 0 || line[0] != `#` {
		return 0
	}
	mut level := 0
	for level < line.len && line[level] == `#` {
		level++
	}
	if level > 6 {
		return 0
	}
	if level < line.len && line[level] != ` ` && line[level] != `\t` {
		return 0
	}
	return level
}

// unescape_string decodes CommonMark backslash escapes in s.
fn unescape_string(s string) string {
	if !s.contains('\\') {
		return s
	}
	mut out := strings.new_builder(s.len)
	mut i := 0
	for i < s.len {
		if s[i] == `\\` && i + 1 < s.len && is_ascii_punct(s[i + 1]) {
			out.write_u8(s[i + 1])
			i += 2
		} else {
			out.write_u8(s[i])
			i++
		}
	}
	return out.str()
}

// decode_link_string applies CommonMark unescaping and HTML entity decoding
// to a link destination or title. Backslash escapes take precedence; entity
// references are decoded afterwards as the reference implementation does.
fn decode_link_string(s string) string {
	return ehtml.unescape(unescape_string(s), all: true)
}
