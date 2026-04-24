// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import strings

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
]
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
	lines := normalized.split('\n')
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

		// --- thematic break (---, ***, ___) ---
		if is_thematic_break(stripped) {
			node := new_node(.thematic_break)
			parent.append_child(node)
			p.pos++
			continue
		}

		// --- ATX heading (# ... ######) ---
		if heading := p.try_atx_heading(stripped) {
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
		if sp <= 3 && p.try_link_ref_def(content) {
			continue
		}

		// --- footnote definition (if footnotes extension enabled) ---
		if p.opts.footnotes {
			if p.try_footnote_def(stripped, indent) {
				continue
			}
		}

		// --- blockquote (>) ---
		if stripped.starts_with('>') {
			bq := p.parse_blockquote(indent)
			parent.append_child(bq)
			continue
		}

		// --- indented code block (4 spaces) ---
		if sp >= 4 && !is_blank(stripped) {
			cb := p.parse_indented_code(indent)
			parent.append_child(cb)
			continue
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
	fence_char, fence_len := detect_fence(line)
	if fence_len < 3 {
		return none
	}
	info := line[fence_len..].trim_space()
	// info string must not contain a backtick when using backtick fence.
	if fence_char == 96 && info.contains('`') {
		return none
	}
	p.pos++
	mut code_lines := []string{}
	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped := trim_indent(raw, indent)
		// Check for closing fence.
		close_char, close_len := detect_fence(stripped)
		if close_char == fence_char && close_len >= fence_len {
			rest := stripped[close_len..].trim_space()
			if rest.len == 0 {
				p.pos++
				break
			}
		}
		code_lines << trim_indent(raw, indent)
		p.pos++
	}
	mut node := new_node(.fenced_code)
	node.fence_info = info
	node.literal = code_lines.join('\n') + '\n'
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
			lines << ''
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
	for lines.len > 0 && lines[lines.len - 1] == '' {
		lines = unsafe { lines[..lines.len - 1] }
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
	// Type 6: block-level tag
	tag_name := extract_tag_name(low[1..])
	if tag_name.len > 0 {
		for bt in block_level_tags {
			if tag_name == bt {
				return 6
			}
		}
	}
	// Type 7: complete open/close tag not in type 6
	if is_complete_html_tag(stripped) {
		return 7
	}
	return 0
}

// extract_tag_name extracts the tag name from the beginning of s.
fn extract_tag_name(s string) string {
	mut end := 0
	for end < s.len && (is_alnum(s[end]) || s[end] == `-`) {
		end++
	}
	return s[..end].to_lower()
}

// is_complete_html_tag returns true if s looks like a complete open or close tag.
fn is_complete_html_tag(s string) bool {
	if s.len < 3 {
		return false
	}
	// Must start with < and end with >
	if s[0] != `<` {
		return false
	}
	end := s.index('>') or { return false }
	return end == s.len - 1 || s[end + 1..].trim_space().len == 0
}

// try_link_ref_def attempts to parse a link reference definition at p.pos.
// CommonMark allows the title to appear on the next line when the destination
// is alone on the first line.  Returns true and advances p.pos if successful.
fn (mut p BlockParser) try_link_ref_def(line string) bool {
	if !line.starts_with('[') {
		return false
	}
	// Find closing bracket.
	mut i := 1
	for i < line.len && line[i] != `]` {
		if line[i] == `\\` {
			i++
		}
		i++
	}
	if i >= line.len || line[i] != `]` || i + 1 >= line.len || line[i + 1] != `:` {
		return false
	}
	label := normalize_label(line[1..i])
	rest := line[i + 2..].trim_left(' \t')
	if label.len == 0 {
		return false
	}
	// Parse destination.
	dest, after_dest := parse_link_dest(rest)
	if after_dest == rest {
		return false
	}
	// Parse optional title.  The title may appear on the same line or, if the
	// destination is the only content on the first line, on the very next line.
	mut title := ''
	mut extra_lines := 0 // number of additional lines consumed for the title
	title_str := after_dest.trim_left(' \t')
	if title_str.len > 0 {
		// Title (or unwanted content) is on the same line as the destination.
		parsed_title, title_rest := parse_link_title(title_str)
		if title_rest == title_str {
			// Not a valid title; reject the whole definition.
			return false
		}
		if title_rest.trim_space().len > 0 {
			// Trailing content after the title – invalid.
			return false
		}
		title = parsed_title
	} else {
		// Destination was alone on its line; look for a title on the next line.
		next_idx := p.pos + 1
		if next_idx < p.lines.len {
			next_line := expand_tabs(p.lines[next_idx]).trim_left(' \t')
			if next_line.len > 0 {
				parsed_title, title_rest := parse_link_title(next_line)
				if title_rest != next_line && title_rest.trim_space().len == 0 {
					// Valid title on the next line; consume it.
					title = parsed_title
					extra_lines = 1
				}
			}
		}
	}
	if label !in p.ref_map {
		p.ref_map[label] = LinkRef{
			dest:  dest
			title: title
		}
	}
	p.pos += 1 + extra_lines
	return true
}

// parse_link_dest parses a link destination from s and returns (dest, rest).
fn parse_link_dest(s string) (string, string) {
	if s.len == 0 {
		return '', s
	}
	if s[0] == `<` {
		// Angle-bracket form: <url>
		end := s.index_after_('>', 1)
		if end < 0 {
			return '', s
		}
		return s[1..end], s[end + 1..]
	}
	// Regular form: no spaces, no control characters, balanced parentheses.
	mut parens := 0
	mut i := 0
	for i < s.len {
		c := s[i]
		if c == ` ` || c == `\t` || c == `\n` {
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
	return s[..i], s[i..]
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
			return unescape_string(s[1..i]), s[i + 1..]
		}
		if c == `\\` && i + 1 < s.len {
			i += 2
			continue
		}
		i++
	}
	return '', s
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
		if stripped.starts_with('> ') {
			bq_lines << stripped[2..]
			p.pos++
		} else if stripped.starts_with('>') {
			bq_lines << stripped[1..]
			p.pos++
		} else {
			// Lazy continuation line.
			bq_lines << stripped
			p.pos++
		}
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
	indent      int // total indent of content after marker
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
				}, true
			}
			return ListMarker{}, false
		}
		content_indent := sp + 1 + (if rest.len > 1 && rest[1] == `\t` { 3 } else { 1 })
		return ListMarker{
			is_ordered:  false
			bullet_char: rest[0]
			indent:      content_indent
		}, true
	}
	// Ordered list: 1. or 1)
	mut num_end := 0
	for num_end < rest.len && is_digit(rest[num_end]) {
		num_end++
	}
	if num_end > 0 && num_end < rest.len && (rest[num_end] == `.` || rest[num_end] == `)`) {
		marker_end := num_end + 1
		if marker_end < rest.len && rest[marker_end] != ` ` && rest[marker_end] != `\t` {
			return ListMarker{}, false
		}
		num_str := rest[..num_end]
		start := num_str.int()
		mut content_indent := sp + marker_end + 1
		if marker_end < rest.len && rest[marker_end] == `\t` {
			content_indent = sp + marker_end + (4 - ((sp + marker_end) % 4))
		}
		return ListMarker{
			is_ordered:  true
			start:       start
			bullet_char: rest[num_end]
			indent:      content_indent
		}, true
	}
	return ListMarker{}, false
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
		// Different list type → stop.
		if cur_marker.is_ordered != marker.is_ordered {
			break
		}
		if !cur_marker.is_ordered && cur_marker.bullet_char != marker.bullet_char {
			break
		}

		if had_blank {
			list.is_tight = false
		}
		had_blank = false

		item := p.parse_list_item(indent)
		list.append_child(item)
	}

	return list
}

// parse_list_item parses a single list item and returns a list_item node.
fn (mut p BlockParser) parse_list_item(base_indent int) &Node {
	if p.pos >= p.lines.len {
		return new_node(.list_item)
	}

	mut item := new_node(.list_item)

	// Get first line of the item
	first_raw := expand_tabs(p.lines[p.pos])
	first_stripped := trim_indent(first_raw, base_indent)

	// Extract content after marker
	marker_line := first_stripped
	mut marker_idx := 0

	// Find where marker ends in stripped line
	if marker_line.len > 0
		&& (marker_line[0] == `-` || marker_line[0] == `*` || marker_line[0] == `+`) {
		// Bullet marker: skip marker and whitespace
		marker_idx = 1
		for marker_idx < marker_line.len
			&& (marker_line[marker_idx] == ` ` || marker_line[marker_idx] == `\t`) {
			marker_idx++
		}
	} else {
		// Ordered marker: skip number and . or )
		for marker_idx < marker_line.len && is_digit(marker_line[marker_idx]) {
			marker_idx++
		}
		if marker_idx < marker_line.len
			&& (marker_line[marker_idx] == `.` || marker_line[marker_idx] == `)`) {
			marker_idx++
		}
		// Skip whitespace after marker
		for marker_idx < marker_line.len
			&& (marker_line[marker_idx] == ` ` || marker_line[marker_idx] == `\t`) {
			marker_idx++
		}
	}

	// Get content after marker
	first_content := if marker_idx < marker_line.len {
		marker_line[marker_idx..]
	} else {
		''
	}

	// Detect task list checkbox: [ ] or [x] or [X] at the start of content.
	mut task_checked := false
	mut has_task := false
	mut task_content_start := 0
	if p.opts.task_list && first_content.len >= 3 && first_content[0] == `[` {
		if (first_content[1] == ` ` || first_content[1] == `x` || first_content[1] == `X`)
			&& first_content[2] == `]` {
			if first_content.len == 3 || first_content[3] == ` ` || first_content[3] == `\t` {
				has_task = true
				task_checked = first_content[1] != ` `
				// Skip the checkbox and one optional following whitespace char.
				task_content_start = if first_content.len > 3 { 4 } else { 3 }
			}
		}
	}
	actual_first_content := if has_task {
		first_content[task_content_start..]
	} else {
		first_content
	}

	// Collect lines belonging to this item
	mut item_lines := [actual_first_content]
	p.pos++

	// Content indent is where subsequent lines must be indented to
	first_leading := leading_spaces(first_stripped)
	content_indent := first_leading + marker_idx

	for p.pos < p.lines.len {
		raw := expand_tabs(p.lines[p.pos])
		stripped_base := trim_indent(raw, base_indent)

		if is_blank(stripped_base) {
			item_lines << ''
			p.pos++
			continue
		}

		sp := leading_spaces(stripped_base)

		// If line has enough indent, include it
		if sp >= content_indent {
			// Remove the content indent
			item_lines << trim_indent(stripped_base, content_indent)
			p.pos++
			continue
		}

		// If line starts a new list item at base indent level, stop
		if sp < 2 {
			_, mk := parse_list_marker(stripped_base)
			if mk {
				break
			}
		}

		// Check if it might be a sub-block (less indented but meaningful)
		if is_thematic_break(stripped_base) || stripped_base.starts_with('#') {
			break
		}

		item_lines << stripped_base
		p.pos++
	}

	// Trim trailing blank lines
	for item_lines.len > 0 && item_lines[item_lines.len - 1] == '' {
		item_lines = unsafe { item_lines[..item_lines.len - 1] }
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

	return item
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
		for ch in inner.bytes() {
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

		if is_blank(stripped) {
			p.pos++
			break
		}
		// Check for setext underline on the next line while para_lines is non-empty.
		if para_lines.len > 0 {
			if is_setext_underline(stripped) {
				level := if stripped.trim_left(' \t')[0] == `=` { 1 } else { 2 }
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
		// Other block starters interrupt a paragraph.
		if para_lines.len > 0 {
			if is_thematic_break(stripped) || stripped.starts_with('#') || stripped.starts_with('>')
				|| stripped.starts_with('```') || stripped.starts_with('~~~') {
				break
			}
			if is_list_marker(stripped) {
				break
			}
		}
		para_lines << stripped
		p.pos++
	}
	mut node := new_node(.paragraph)
	node.literal = para_lines.join('\n').trim_space()
	return node
}

// is_setext_underline returns true if line is a setext heading underline
// (0-3 leading spaces, then one or more = or - with optional trailing spaces).
fn is_setext_underline(line string) bool {
	if line.len == 0 {
		return false
	}
	// Allow up to 3 leading spaces (CommonMark spec rule 80).
	mut start := 0
	for start < 3 && start < line.len && line[start] == ` ` {
		start++
	}
	if start >= line.len {
		return false
	}
	c := line[start]
	if c != `=` && c != `-` {
		return false
	}
	for i := start; i < line.len; i++ {
		if line[i] != c && line[i] != ` ` && line[i] != `\t` {
			return false
		}
	}
	return true
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
