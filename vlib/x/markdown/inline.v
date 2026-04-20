// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import encoding.html as ehtml

// parse_inline parses src as inline content and returns a slice of inline nodes.
pub fn parse_inline(src string, opts Options, ref_map map[string]LinkRef) []&Node {
	mut p := InlineParser{
		src:     src
		opts:    opts
		ref_map: ref_map
	}
	return p.parse()
}

// InlineParser parses inline markdown content.
struct InlineParser {
	opts    Options
	ref_map map[string]LinkRef
mut:
	src string
	pos int
}

struct EmphDelim {
mut:
	node_idx  int
	ch        u8
	length    int
	orig_len  int
	can_open  bool
	can_close bool
	active    bool = true
}

// parse parses the full inline source and returns a node slice.
fn (mut p InlineParser) parse() []&Node {
	mut result := []&Node{}
	mut delims := []EmphDelim{}
	for p.pos < p.src.len {
		if p.src[p.pos] == `*` || p.src[p.pos] == `_` {
			start := p.pos
			ch := p.src[p.pos]
			for p.pos < p.src.len && p.src[p.pos] == ch {
				p.pos++
			}
			run := p.src[start..p.pos]
			before := if start > 0 { p.src[start - 1] } else { u8(` `) }
			after := if p.pos < p.src.len { p.src[p.pos] } else { u8(` `) }
			can_open := can_open_emphasis(ch, before, after)
			can_close := can_close_emphasis(ch, before, after)
			result << text_node(run)
			if can_open || can_close {
				delims << EmphDelim{
					node_idx:  result.len - 1
					ch:        ch
					length:    run.len
					orig_len:  run.len
					can_open:  can_open
					can_close: can_close
				}
			}
			continue
		}
		nodes := p.parse_one()
		for n in nodes {
			result << n
		}
	}
	if delims.len > 0 {
		resolve_emphasis(mut result, mut delims)
		result = compact_empty_text_nodes(result)
	}
	return merge_text_nodes(result)
}

fn resolve_emphasis(mut nodes []&Node, mut delims []EmphDelim) {
	mut i := 0
	for i < delims.len {
		if !delims[i].active || !delims[i].can_close || delims[i].length == 0 {
			i++
			continue
		}
		mut opener := i - 1
		for opener >= 0 {
			if !delims[opener].active || !delims[opener].can_open || delims[opener].length == 0 {
				opener--
				continue
			}
			if delims[opener].ch != delims[i].ch {
				opener--
				continue
			}
			if (delims[i].can_open || delims[opener].can_close)
				&& (delims[opener].orig_len + delims[i].orig_len) % 3 == 0
				&& (delims[opener].orig_len % 3 != 0 || delims[i].orig_len % 3 != 0) {
				opener--
				continue
			}
			if delims[opener].node_idx + 1 >= delims[i].node_idx {
				opener--
				continue
			}
			break
		}
		if opener < 0 {
			i++
			continue
		}

		use_len := if delims[opener].length >= 2 && delims[i].length >= 2 { 2 } else { 1 }
		opener_idx := delims[opener].node_idx
		closer_idx := delims[i].node_idx
		if opener_idx < 0 || closer_idx < 0 || opener_idx >= nodes.len || closer_idx >= nodes.len
			|| opener_idx >= closer_idx {
			delims[i].active = false
			i++
			continue
		}
		if nodes[opener_idx].literal.len < use_len || nodes[closer_idx].literal.len < use_len {
			delims[i].active = false
			i++
			continue
		}

		nodes[opener_idx].literal = nodes[opener_idx].literal[..nodes[opener_idx].literal.len - use_len]
		nodes[closer_idx].literal = nodes[closer_idx].literal[use_len..]
		delims[opener].length -= use_len
		delims[i].length -= use_len

		mut emph := new_node(if use_len == 2 { .strong } else { .emphasis })
		for child in nodes[opener_idx + 1..closer_idx] {
			emph.append_child(child)
		}

		n_inner := closer_idx - opener_idx - 1
		if n_inner > 0 {
			nodes.delete_many(opener_idx + 1, n_inner)
			nodes.insert(opener_idx + 1, emph)
		}

		mut delta := n_inner - 1
		if delta < 0 {
			delta = 0
		}
		for j := 0; j < delims.len; j++ {
			if !delims[j].active {
				continue
			}
			if delims[j].node_idx > opener_idx && delims[j].node_idx < closer_idx {
				delims[j].active = false
				continue
			}
			if delta > 0 && delims[j].node_idx >= closer_idx {
				delims[j].node_idx -= delta
			}
		}

		if delims[opener].length == 0 {
			delims[opener].can_open = false
		}
		if delims[i].length == 0 {
			delims[i].can_close = false
		}
		i = opener + 1
	}
}

fn compact_empty_text_nodes(nodes []&Node) []&Node {
	mut out := []&Node{}
	for n in nodes {
		if n.kind == .text && n.literal.len == 0 {
			continue
		}
		out << n
	}
	return out
}

// parse_one parses one or more inline elements at the current position.
fn (mut p InlineParser) parse_one() []&Node {
	if p.pos >= p.src.len {
		return []
	}
	c := p.src[p.pos]
	match c {
		`\\` {
			return [p.parse_backslash()]
		}
		96 { // backtick
			if node := p.try_code_span() {
				return [node]
			}
			p.pos++
			return [text_node('`')]
		}
		`*`, `_` {
			p.pos++
			return [text_node(c.ascii_str())]
		}
		`~` {
			if p.opts.strikethrough {
				if node := p.try_strikethrough() {
					return [node]
				}
			}
			p.pos++
			return [text_node('~')]
		}
		`[` {
			if nodes := p.try_link_or_footnote() {
				return nodes
			}
			p.pos++
			return [text_node('[')]
		}
		`!` {
			if p.pos + 1 < p.src.len && p.src[p.pos + 1] == `[` {
				saved := p.pos
				p.pos += 2
				if nodes := p.try_image_after_bang() {
					return nodes
				}
				p.pos = saved
				p.pos++
				return [text_node('!')]
			}
			p.pos++
			return [text_node('!')]
		}
		`<` {
			if node := p.try_autolink_or_html() {
				return [node]
			}
			p.pos++
			return [text_node('<')]
		}
		`&` {
			if node := p.try_entity() {
				return [node]
			}
			p.pos++
			return [text_node('&')]
		}
		`\n` {
			return [p.parse_newline()]
		}
		else {
			if p.opts.linkify {
				if node := p.try_linkify() {
					return [node]
				}
			}
			p.pos++
			return [text_node(c.ascii_str())]
		}
	}
}

// text_node creates a text node with the given literal string.
fn text_node(s string) &Node {
	mut n := new_node(.text)
	n.literal = s
	return n
}

// merge_text_nodes merges consecutive text nodes into one.
fn merge_text_nodes(nodes []&Node) []&Node {
	if nodes.len <= 1 {
		return nodes
	}
	mut result := []&Node{}
	for node in nodes {
		if result.len > 0 && result[result.len - 1].kind == .text && node.kind == .text {
			result[result.len - 1].literal += node.literal
		} else {
			result << node
		}
	}
	return result
}

// can_open_emphasis reports whether a delimiter run can open emphasis.
fn can_open_emphasis(delim u8, before u8, after u8) bool {
	left_flanking := !is_unicode_space(after) && (!is_ascii_punct(after) || is_unicode_space(before)
		|| is_ascii_punct(before))
	right_flanking := !is_unicode_space(before)
		&& (!is_ascii_punct(before) || is_unicode_space(after)
		|| is_ascii_punct(after))
	if delim == `*` {
		return left_flanking
	}
	if delim == `_` {
		return left_flanking && (!right_flanking || is_ascii_punct(before))
	}
	return false
}

// can_close_emphasis reports whether a delimiter run can close emphasis.
fn can_close_emphasis(delim u8, before u8, after u8) bool {
	left_flanking := !is_unicode_space(after) && (!is_ascii_punct(after) || is_unicode_space(before)
		|| is_ascii_punct(before))
	right_flanking := !is_unicode_space(before)
		&& (!is_ascii_punct(before) || is_unicode_space(after)
		|| is_ascii_punct(after))
	if delim == `*` {
		return right_flanking
	}
	if delim == `_` {
		return right_flanking && (!left_flanking || is_ascii_punct(after))
	}
	return false
}

// parse_backslash handles backslash escapes and hard line breaks.
fn (mut p InlineParser) parse_backslash() &Node {
	p.pos++ // consume '\'
	if p.pos >= p.src.len {
		return text_node('\\')
	}
	ch := p.src[p.pos]
	if ch == `\n` {
		p.pos++
		return new_node(.hard_break)
	}
	if is_ascii_punct(ch) {
		p.pos++
		return text_node(ch.ascii_str())
	}
	return text_node('\\')
}

// try_code_span attempts to parse a backtick code span.
fn (mut p InlineParser) try_code_span() ?&Node {
	start := p.pos
	mut n := 0
	for p.pos < p.src.len && p.src[p.pos] == 96 {
		n++
		p.pos++
	}
	content_start := p.pos
	mut search := content_start
	for search < p.src.len {
		if p.src[search] == 96 {
			close_start := search
			mut close_n := 0
			for search < p.src.len && p.src[search] == 96 {
				close_n++
				search++
			}
			if close_n == n {
				code_raw := p.src[content_start..close_start]
				mut code := code_raw.replace('\n', ' ')
				if code.len >= 2 && code[0] == ` ` && code[code.len - 1] == ` `
					&& code.trim_space().len > 0 {
					code = code[1..code.len - 1]
				}
				mut node := new_node(.code_span)
				node.literal = code
				p.pos = search
				return node
			}
		} else {
			search++
		}
	}
	p.pos = start
	return none
}

// try_emphasis attempts to parse *em*, **strong**, _em_, __strong__.
fn (mut p InlineParser) try_emphasis(c u8) ?&Node {
	start := p.pos
	mut run := 0
	for p.pos < p.src.len && p.src[p.pos] == c {
		run++
		p.pos++
	}

	// Prevent splitting an intraword __ run into a synthetic single-underscore opener.
	if c == `_` && run == 1 && start > 1 && p.src[start - 1] == `_` {
		before2 := p.src[start - 2]
		after1 := if start + run < p.src.len { p.src[start + run] } else { u8(` `) }
		if is_wordish(before2) && is_wordish(after1) {
			p.pos = start
			return none
		}
	}

	before := if start > 0 { p.src[start - 1] } else { u8(` `) }
	after := if start + run < p.src.len { p.src[start + run] } else { u8(` `) }
	opener_can_open := can_open_emphasis(c, before, after)
	opener_can_close := can_close_emphasis(c, before, after)

	if !opener_can_open {
		p.pos = start
		return none
	}

	// Prefer em first for odd runs (e.g. ***foo*** -> <em><strong>foo</strong></em>).
	if run % 2 == 1 {
		p.pos = start + 1
		if node := p.match_close_delim(c, 1, run, opener_can_close) {
			return node
		}
		if run >= 2 {
			p.pos = start + 2
			if node := p.match_close_delim(c, 2, run, opener_can_close) {
				return node
			}
		}
	} else {
		if run >= 2 {
			p.pos = start + 2
			if node := p.match_close_delim(c, 2, run, opener_can_close) {
				return node
			}
		}
		p.pos = start + 1
		if node := p.match_close_delim(c, 1, run, opener_can_close) {
			return node
		}
	}

	p.pos = start
	return none
}

// is_wordish reports whether c behaves like a word character for emphasis
// boundary checks (includes non-ASCII bytes used in UTF-8 sequences).
@[inline]
fn is_wordish(c u8) bool {
	return !is_unicode_space(c) && !is_ascii_punct(c)
}

// match_close_delim parses content after the opening delimiter run and finds
// a matching closing delimiter of exactly `count` characters.
fn (mut p InlineParser) match_close_delim(c u8, count int, opener_run int, opener_can_close bool) ?&Node {
	content_start := p.pos
	mut content_nodes := []&Node{}

	for p.pos < p.src.len {
		loop_start_pos := p.pos
		ch := p.src[p.pos]
		// Check for closing delimiter.
		if ch == c {
			close_pos := p.pos
			mut close_run := 0
			for p.pos < p.src.len && p.src[p.pos] == c {
				close_run++
				p.pos++
			}
			if close_run >= count {
				// Verify right-flanking.
				before_close := if close_pos > 0 { p.src[close_pos - 1] } else { u8(` `) }
				after_close := if p.pos < p.src.len { p.src[p.pos] } else { u8(` `) }
				closer_can_close := can_close_emphasis(c, before_close, after_close)
				closer_can_open := can_open_emphasis(c, before_close, after_close)
				if closer_can_close {
					if count == 1 && opener_run == 1 && close_run > 1 && closer_can_open {
						p.pos = close_pos
					} else if count == 1 && opener_run > 1 && close_run > 1 && closer_can_open {
						// Keep extra delimiters inside the emphasis span so nested
						// strong parsing can consume them (e.g. foo***bar***baz).
						inner_end := close_pos + (close_run - count)
						if inner_end > content_start && inner_end <= p.src.len {
							inner_nodes := parse_inline(p.src[content_start..inner_end], p.opts,
								p.ref_map)
							mut node := new_node(.emphasis)
							for child in inner_nodes {
								node.append_child(child)
							}
							p.pos = close_pos + close_run
							return node
						}
					} else {
						if opener_can_close && closer_can_open && (opener_run + close_run) % 3 == 0
							&& (opener_run % 3 != 0 || close_run % 3 != 0) {
							p.pos = close_pos
						} else {
							if content_nodes.len == 0 {
								content_nodes << text_node(c.ascii_str())
								p.pos = close_pos + 1
								continue
							}
							// Rewind extra closing chars beyond `count`.
							p.pos = close_pos + count
							kind := if count == 2 { NodeKind.strong } else { NodeKind.emphasis }
							mut node := new_node(kind)
							for child in content_nodes {
								node.append_child(child)
							}
							return node
						}
					}
				}
			}
			p.pos = close_pos
			if count == 1 && opener_run > 1 && opener_can_close {
				content_nodes << text_node(c.ascii_str())
				p.pos++
				continue
			}
		}
		if ch == `\n` {
			// Newlines stop emphasis search.
			break
		}
		inner := p.parse_one()
		if p.pos <= loop_start_pos {
			// Safety net: force progress to avoid recursive delimiter stalls.
			content_nodes << text_node(p.src[loop_start_pos].ascii_str())
			p.pos = loop_start_pos + 1
			continue
		}
		content_nodes << inner
	}
	// Not found; reset.
	p.pos = content_start
	return none
}

// try_strikethrough parses ~~text~~.
fn (mut p InlineParser) try_strikethrough() ?&Node {
	if p.pos + 1 >= p.src.len || p.src[p.pos + 1] != `~` {
		return none
	}
	p.pos += 2
	close := p.src.index_after_('~~', p.pos)
	if close < 0 {
		p.pos -= 2
		return none
	}
	inner := p.src[p.pos..close]
	inner_nodes := parse_inline(inner, p.opts, p.ref_map)
	mut node := new_node(.strikethrough)
	for child in inner_nodes {
		node.append_child(child)
	}
	p.pos = close + 2
	return node
}

// try_link_or_footnote handles [ and attempts to parse a link or footnote ref.
fn (mut p InlineParser) try_link_or_footnote() ?[]&Node {
	saved := p.pos
	p.pos++ // consume '['
	// Footnote reference [^label].
	if p.opts.footnotes && p.pos < p.src.len && p.src[p.pos] == `^` {
		fn_start := p.pos + 1
		fn_close := p.src.index_after_(']', fn_start)
		if fn_close > fn_start {
			label := p.src[fn_start..fn_close]
			mut fn_ref := new_node(.footnote_ref)
			fn_ref.fn_label = label
			p.pos = fn_close + 1
			return [fn_ref]
		}
	}
	text_start := p.pos
	close := find_bracket_close(p.src, p.pos)
	if close < 0 {
		p.pos = saved
		return none
	}
	link_text := p.src[text_start..close]
	p.pos = close + 1

	// Inline link (url).
	if p.pos < p.src.len && p.src[p.pos] == `(` {
		dest, title, end := parse_inline_link_dest_from(p.src, p.pos + 1)
		if end >= 0 {
			inner_nodes := parse_inline(link_text, p.opts, p.ref_map)
			mut node := new_node(.link)
			node.dest = unescape_string(dest)
			node.title = unescape_string(title)
			for child in inner_nodes {
				node.append_child(child)
			}
			p.pos = end + 1
			return [node]
		}
	}
	// Full reference [text][label].
	if p.pos < p.src.len && p.src[p.pos] == `[` {
		ref_start := p.pos + 1
		ref_close := p.src.index_after_(']', ref_start)
		if ref_close >= 0 {
			raw_label := p.src[ref_start..ref_close]
			label := normalize_label(if raw_label.len > 0 { raw_label } else { link_text })
			if label in p.ref_map {
				ref := p.ref_map[label]
				mut node := new_node(.link)
				node.dest = ref.dest
				node.title = ref.title
				node.label = label
				inner_nodes := parse_inline(link_text, p.opts, p.ref_map)
				for child in inner_nodes {
					node.append_child(child)
				}
				p.pos = ref_close + 1
				return [node]
			}
			if raw_label.len > 0 {
				// Do not downgrade explicit [text][label] to shortcut [text].
				p.pos = saved
				return none
			}
		}
	}
	// Shortcut reference [label].
	label := normalize_label(link_text)
	if label in p.ref_map {
		ref := p.ref_map[label]
		mut node := new_node(.link)
		node.dest = ref.dest
		node.title = ref.title
		node.label = label
		inner_nodes := parse_inline(link_text, p.opts, p.ref_map)
		for child in inner_nodes {
			node.append_child(child)
		}
		return [node]
	}
	p.pos = saved
	return none
}

// try_image_after_bang parses the [alt](url) part of an image after '![' was consumed.
fn (mut p InlineParser) try_image_after_bang() ?[]&Node {
	text_start := p.pos
	close := find_bracket_close(p.src, p.pos)
	if close < 0 {
		return none
	}
	alt_text := p.src[text_start..close]
	p.pos = close + 1

	if p.pos < p.src.len && p.src[p.pos] == `(` {
		dest, title, end := parse_inline_link_dest_from(p.src, p.pos + 1)
		if end >= 0 {
			mut node := new_node(.image)
			node.dest = unescape_string(dest)
			node.title = unescape_string(title)
			inner_nodes := parse_inline(alt_text, p.opts, p.ref_map)
			for child in inner_nodes {
				node.append_child(child)
			}
			p.pos = end + 1
			return [node]
		}
	}
	if p.pos < p.src.len && p.src[p.pos] == `[` {
		ref_start := p.pos + 1
		ref_close := p.src.index_after_(']', ref_start)
		if ref_close >= 0 {
			raw_label := p.src[ref_start..ref_close]
			label := normalize_label(if raw_label.len > 0 { raw_label } else { alt_text })
			if label in p.ref_map {
				ref := p.ref_map[label]
				mut node := new_node(.image)
				node.dest = ref.dest
				node.title = ref.title
				inner_nodes := parse_inline(alt_text, p.opts, p.ref_map)
				for child in inner_nodes {
					node.append_child(child)
				}
				p.pos = ref_close + 1
				return [node]
			}
		}
	}
	return none
}

// find_bracket_close finds the ] matching the [ at start, handling nesting and escapes.
fn find_bracket_close(s string, start int) int {
	mut depth := 1
	mut i := start
	for i < s.len {
		if s[i] == `\\` && i + 1 < s.len {
			i += 2
			continue
		}
		if s[i] == `[` {
			depth++
		} else if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
		i++
	}
	return -1
}

// parse_inline_link_dest_from parses (url) or (url "title") starting at s[start]
// (start is after the opening paren). Returns (dest, title, end_paren_pos) or ("","", -1).
fn parse_inline_link_dest_from(s string, start int) (string, string, int) {
	i := skip_ws(s, start)
	if i >= s.len {
		return '', '', -1
	}
	dest, after_dest := parse_link_dest(s[i..])
	j := i + (s[i..].len - after_dest.len)
	k := skip_ws(s, j)
	if k < s.len && s[k] == `)` {
		return dest, '', k
	}
	title, after_title := parse_link_title(s[k..])
	l := k + (s[k..].len - after_title.len)
	m := skip_ws(s, l)
	if m < s.len && s[m] == `)` {
		return dest, title, m
	}
	return '', '', -1
}

// skip_ws returns the position in s after skipping whitespace from start.
fn skip_ws(s string, start int) int {
	mut i := start
	for i < s.len && (s[i] == ` ` || s[i] == `\t` || s[i] == `\n`) {
		i++
	}
	return i
}

// try_autolink_or_html handles <...> for autolinks and raw HTML.
fn (mut p InlineParser) try_autolink_or_html() ?&Node {
	rest := p.src[p.pos..]
	auto_end := try_autolink(rest)
	if auto_end >= 0 {
		content := rest[1..auto_end]
		mut node := new_node(.autolink)
		node.literal = content
		if content.contains('@') && !content.contains('://') {
			node.dest = 'mailto:' + content
		} else {
			node.dest = content
		}
		p.pos += auto_end + 1
		return node
	}
	raw_end := try_raw_html_tag(rest)
	if raw_end >= 0 {
		mut node := new_node(.raw_html)
		node.literal = rest[..raw_end + 1]
		p.pos += raw_end + 1
		return node
	}
	return none
}

// try_autolink matches an <autolink> returning the position of '>' or -1.
fn try_autolink(s string) int {
	if s.len < 3 || s[0] != `<` {
		return -1
	}
	end := s.index_after_('>', 1)
	if end < 0 {
		return -1
	}
	inner := s[1..end]
	if inner.contains(' ') || inner.contains('<') {
		return -1
	}
	if inner.contains('://') {
		return end
	}
	if inner.contains('@') && !inner.starts_with('@') {
		return end
	}
	return -1
}

// try_raw_html_tag matches a raw HTML tag starting with '<' and returns the '>' position.
fn try_raw_html_tag(s string) int {
	if s.len < 3 || s[0] != `<` {
		return -1
	}
	if s.starts_with('<!--') {
		end := s.index_after_('-->', 4)
		if end >= 0 {
			return end + 2
		}
		return -1
	}
	if s.starts_with('<?') {
		end := s.index_after_('?>', 2)
		if end >= 0 {
			return end + 1
		}
		return -1
	}
	low := s.to_lower()
	if low.starts_with('<![cdata[') {
		end := s.index_after_(']]>', 9)
		if end >= 0 {
			return end + 2
		}
		return -1
	}
	end := s.index_after_('>', 1)
	if end < 0 {
		return -1
	}
	inner := s[1..end]
	if inner.len == 0 {
		return -1
	}
	if !is_alpha(inner[0]) && inner[0] != `/` && inner[0] != `!` {
		return -1
	}
	return end
}

// try_entity parses an HTML entity reference &name; or &#n; or &#xn'
fn (mut p InlineParser) try_entity() ?&Node {
	rest := p.src[p.pos..]
	semi := rest.index(';') or { return none }
	if semi > 32 || semi < 2 {
		return none
	}
	candidate := rest[..semi + 1]
	decoded := ehtml.unescape(candidate, all: true)
	if decoded == candidate {
		return none
	}
	p.pos += semi + 1
	return text_node(decoded)
}

// parse_newline handles a newline character.
fn (mut p InlineParser) parse_newline() &Node {
	// Hard break if preceded by two or more spaces.
	if p.pos >= 2 && p.src[p.pos - 1] == ` ` && p.src[p.pos - 2] == ` ` {
		p.pos++
		return new_node(.hard_break)
	}
	p.pos++
	return new_node(.soft_break)
}

// try_linkify matches a bare URL (linkify extension).
fn (mut p InlineParser) try_linkify() ?&Node {
	rest := p.src[p.pos..]
	for _, scheme in ['https://', 'http://', 'ftp://', 'mailto:'] {
		if rest.starts_with(scheme) {
			mut end := scheme.len
			for end < rest.len {
				ch := rest[end]
				if ch == ` ` || ch == `<` || ch == `>` || ch == `"` || ch == `\n` || ch == `\t` {
					break
				}
				end++
			}
			for end > scheme.len {
				last := rest[end - 1]
				if last == `.` || last == `,` || last == `;` || last == `!` || last == `?` {
					end--
				} else {
					break
				}
			}
			url := rest[..end]
			mut node := new_node(.autolink)
			node.literal = url
			node.dest = url
			p.pos += end
			return node
		}
	}
	return none
}
