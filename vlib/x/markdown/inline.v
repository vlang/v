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

@[inline]
fn rune_at_or_space(s string, pos int) rune {
	if pos < 0 || pos >= s.len {
		return ` `
	}
	char_len := utf8_char_len(s[pos])
	if char_len <= 0 || pos + char_len > s.len {
		return rune(s[pos])
	}
	return s[pos..pos + char_len].runes()[0]
}

@[inline]
fn prev_rune_or_space(s string, pos int) rune {
	if pos <= 0 {
		return ` `
	}
	mut i := pos - 1
	for i > 0 && (s[i] & u8(0xC0)) == u8(0x80) {
		i--
	}
	return rune_at_or_space(s, i)
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
			before := prev_rune_or_space(p.src, start)
			after := rune_at_or_space(p.src, p.pos)
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
	mut merged := merge_text_nodes(result)
	strip_before_hardbreak(mut merged)
	return merged
}

// strip_before_hardbreak removes trailing spaces/tabs from a text node that
// is immediately followed by a hard or soft line break (they are insignificant
// there and must not appear in the output). Code spans are atomic nodes, so
// significant spaces inside them are never touched. Applies at every level.
fn strip_before_hardbreak(mut nodes []&Node) {
	for i := 0; i < nodes.len; i++ {
		if nodes[i].children.len > 0 {
			strip_before_hardbreak(mut nodes[i].children)
		}
	}
	for i := 0; i + 1 < nodes.len; i++ {
		if nodes[i + 1].kind == .hard_break || nodes[i + 1].kind == .soft_break {
			// Walk back over consecutive all-whitespace text nodes: inline
			// parsing emits one node per character, so trailing spaces may
			// span several nodes (notably inside emphasis children, which
			// are never merged).
			mut j := i
			for j >= 0 && nodes[j].kind == .text {
				stripped := nodes[j].literal.trim_right(' \t')
				if stripped.len == nodes[j].literal.len {
					break
				}
				nodes[j].literal = stripped
				if stripped.len > 0 {
					break
				}
				j--
			}
		}
	}
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
		for child_idx in opener_idx + 1 .. closer_idx {
			emph.append_child(nodes[child_idx])
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
			// No match: the whole run is literal text. Skipping past it
			// (instead of one byte) keeps a trailing run from pairing up
			// with an earlier one, e.g. ```foo`` stays literal.
			start := p.pos
			for p.pos < p.src.len && p.src[p.pos] == 96 {
				p.pos++
			}
			return [text_node(p.src[start..p.pos])]
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
			r := rune_at_or_space(p.src, p.pos)
			step := utf8_char_len(p.src[p.pos])
			if p.opts.linkify {
				if node := p.try_linkify() {
					return [node]
				}
			}
			p.pos += step
			return [text_node(r.str())]
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
fn can_open_emphasis(delim u8, before rune, after rune) bool {
	left_flanking := !is_unicode_space(after) && (!is_unicode_punct(after) || is_unicode_space(before)
		|| is_unicode_punct(before))
	right_flanking := !is_unicode_space(before)
		&& (!is_unicode_punct(before) || is_unicode_space(after)
			|| is_unicode_punct(after))
	if delim == `*` {
		return left_flanking
	}
	if delim == `_` {
		return left_flanking && (!right_flanking || is_unicode_punct(before))
	}
	return false
}

// can_close_emphasis reports whether a delimiter run can close emphasis.
fn can_close_emphasis(delim u8, before rune, after rune) bool {
	left_flanking := !is_unicode_space(after) && (!is_unicode_punct(after) || is_unicode_space(before)
		|| is_unicode_punct(before))
	right_flanking := !is_unicode_space(before)
		&& (!is_unicode_punct(before) || is_unicode_space(after)
			|| is_unicode_punct(after))
	if delim == `*` {
		return right_flanking
	}
	if delim == `_` {
		return right_flanking && (!left_flanking || is_unicode_punct(after))
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
		before2 := prev_rune_or_space(p.src, start - 1)
		after1 := rune_at_or_space(p.src, start + run)
		if is_wordish(before2) && is_wordish(after1) {
			p.pos = start
			return none
		}
	}

	before := prev_rune_or_space(p.src, start)
	after := rune_at_or_space(p.src, start + run)
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
fn is_wordish(c rune) bool {
	return !is_unicode_space(c) && !is_unicode_punct(c)
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
				before_close := prev_rune_or_space(p.src, close_pos)
				after_close := rune_at_or_space(p.src, p.pos)
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
			fallback := rune_at_or_space(p.src, loop_start_pos)
			content_nodes << text_node(fallback.str())
			p.pos = loop_start_pos + utf8_char_len(p.src[loop_start_pos])
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
			if contains_link(inner_nodes) {
				// A link cannot contain another link.
				p.pos = saved
				return none
			}
			mut node := new_node(.link)
			node.dest = dest
			node.title = title
			for child in inner_nodes {
				node.append_child(child)
			}
			p.pos = end + 1
			return [node]
		}
	}
	// Full reference [text][label]. Labels end at the first `]` and may
	// not contain unescaped `[` (matching the reference definitions).
	if p.pos < p.src.len && p.src[p.pos] == `[` {
		ref_start := p.pos + 1
		ref_close := p.src.index_after_(']', ref_start)
		if ref_close >= 0 {
			raw_label := p.src[ref_start..ref_close]
			label := normalize_label(if raw_label.len > 0 { raw_label } else { link_text })
			if label in p.ref_map {
				ref := p.ref_map[label]
				inner_nodes := parse_inline(link_text, p.opts, p.ref_map)
				if contains_link(inner_nodes) {
					// A link cannot contain another link.
					p.pos = saved
					return none
				}
				mut node := new_node(.link)
				node.dest = ref.dest
				node.title = ref.title
				node.label = label
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
		inner_nodes := parse_inline(link_text, p.opts, p.ref_map)
		if contains_link(inner_nodes) {
			// A link cannot contain another link.
			p.pos = saved
			return none
		}
		mut node := new_node(.link)
		node.dest = ref.dest
		node.title = ref.title
		node.label = label
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
			node.dest = dest
			node.title = title
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
		return none
	}
	// Shortcut reference ![label].
	label := normalize_label(alt_text)
	if label in p.ref_map {
		ref := p.ref_map[label]
		mut node := new_node(.image)
		node.dest = ref.dest
		node.title = ref.title
		inner_nodes := parse_inline(alt_text, p.opts, p.ref_map)
		for child in inner_nodes {
			node.append_child(child)
		}
		return [node]
	}
	return none
}

// contains_link reports whether nodes contain a link node at any depth,
// except inside image alt text (which is flattened and cannot block an
// outer link). A link cannot contain another link: if the text of a
// candidate link contains a link, the outer brackets stay literal.
fn contains_link(nodes []&Node) bool {
	for node in nodes {
		if node.kind == .link {
			return true
		}
		if node.kind == .image {
			continue
		}
		if contains_link(node.children) {
			return true
		}
	}
	return false
}

// find_bracket_close finds the ] matching the [ at start, handling nesting and escapes.
// Code spans and `<...>` (autolinks, raw HTML tags) take precedence: a `]`
// inside them does not close the link text.
fn find_bracket_close(s string, start int) int {
	mut depth := 1
	mut i := start
	for i < s.len {
		if s[i] == `\\` && i + 1 < s.len {
			i += 2
			continue
		}
		if s[i] == 96 {
			// Skip a code span: only a run of the same length closes it.
			mut run := 1
			for i + run < s.len && s[i + run] == 96 {
				run++
			}
			close_run := find_code_close(s, i + run, run)
			if close_run >= 0 {
				i = close_run
				continue
			}
			i += run
			continue
		}
		if s[i] == `<` {
			auto := try_autolink(s[i..])
			if auto >= 0 {
				i += auto + 1
				continue
			}
			tag_end := scan_html_tag(s[i..])
			if tag_end > 0 {
				i += tag_end
				continue
			}
			raw := try_raw_html_tag(s[i..])
			if raw >= 0 {
				i += raw + 1
				continue
			}
			i++
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

// find_code_close finds a closing backtick run of exactly `run` backticks
// at or after start, returning the position just past it, or -1.
fn find_code_close(s string, start int, run int) int {
	mut i := start
	for i < s.len {
		if s[i] == 96 {
			mut j := i
			for j < s.len && s[j] == 96 {
				j++
			}
			if j - i == run {
				return j
			}
			i = j
			continue
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
	// Separator between destination and title: spaces/tabs and at most one
	// line ending. Required when both destination and title are present.
	mut k := j
	mut seps := 0
	mut nl := 0
	for k < s.len {
		c := s[k]
		if c == ` ` || c == `\t` {
			seps++
			k++
		} else if c == `\n` {
			nl++
			seps++
			if nl > 1 {
				break
			}
			k++
		} else if c == `\r` {
			nl++
			seps++
			if nl > 1 {
				break
			}
			k++
			if k < s.len && s[k] == `\n` {
				k++
			}
		} else {
			break
		}
	}
	if k < s.len && s[k] == `)` {
		return dest, '', k
	}
	if dest.len > 0 && seps == 0 {
		// Destination and title must be separated.
		return '', '', -1
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
	for i < s.len {
		ch := rune_at_or_space(s, i)
		if ch !in unicode_space {
			break
		}
		i += utf8_char_len(s[i])
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
		if content.to_lower().starts_with('mailto:') {
			node.dest = content
		} else if content.contains('@') && !content.contains('://') {
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
// Absolute URIs need a valid scheme; bare `a@b` style text needs a valid
// email shape. No whitespace (including line endings) is allowed inside.
fn try_autolink(s string) int {
	if s.len < 3 || s[0] != `<` {
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
	for i := 0; i < inner.len; i++ {
		c := inner[i]
		if c <= 0x20 || c == 0x7f || c == `<` {
			return -1
		}
	}
	// Absolute URI: scheme ':' + zero or more non-control characters.
	idx := inner.index(':') or { -1 }
	if idx > 0 {
		scheme := inner[..idx]
		if scheme.len >= 2 && scheme.len <= 32 && is_alpha(scheme[0]) {
			mut ok := true
			for i := 1; i < scheme.len; i++ {
				c := scheme[i]
				if !(is_alpha(c) || is_digit(c) || c == `+` || c == `-` || c == `.`) {
					ok = false
					break
				}
			}
			if ok {
				return end
			}
		}
		return -1
	}
	if idx == 0 || inner.contains(':') {
		return -1
	}
	if is_autolink_email(inner) {
		return end
	}
	return -1
}

// is_autolink_email reports whether s has the shape local@domain.
fn is_autolink_email(s string) bool {
	at := s.index('@') or { return false }
	if at <= 0 || at == s.len - 1 {
		return false
	}
	if s.index_after_('@', at + 1) >= 0 {
		return false
	}
	for i := 0; i < at; i++ {
		c := s[i]
		if !(is_alnum(c) || c == `.` || c == `!` || c == `#` || c == `$` || c == `%` || c == `&` || c == `'` || c == `*` || c == `+` || c == `/` || c == `=` || c == `?` || c == `^` || c == `_` || c == 96 || c == `{` || c == `|` || c == `}` || c == `~` || c == `-`) {
			return false
		}
	}
	domain := s[at + 1..]
	mut start := 0
	for i := 0; i <= domain.len; i++ {
		if i == domain.len || domain[i] == `.` {
			if !is_domain_label(domain[start..i]) {
				return false
			}
			start = i + 1
		}
	}
	return true
}

// is_domain_label reports whether s is a valid DNS label.
fn is_domain_label(s string) bool {
	if s.len == 0 || s.len > 63 {
		return false
	}
	if !is_alnum(s[0]) || !is_alnum(s[s.len - 1]) {
		return false
	}
	for i := 1; i + 1 < s.len; i++ {
		if !(is_alnum(s[i]) || s[i] == `-`) {
			return false
		}
	}
	return true
}

// scan_html_tag scans an HTML open or close tag starting at s[0] == `<`.
// It implements the CommonMark tag grammar (tag name, attributes with
// unquoted/single/double-quoted values, optional `/` self-close).
// Returns the index just past the closing `>` or -1 if s is not a valid tag.
fn scan_html_tag(s string) int {
	if s.len < 3 || s[0] != `<` {
		return -1
	}
	mut i := 1
	is_close := s[i] == `/`
	if is_close {
		i++
	}
	// Tag name: ASCII letter followed by letters, digits, hyphens.
	if i >= s.len || !is_alpha(s[i]) {
		return -1
	}
	for i < s.len && (is_alpha(s[i]) || is_digit(s[i]) || s[i] == `-`) {
		i++
	}
	for i < s.len {
		c := s[i]
		if c == `>` {
			return i + 1
		}
		if c == `/` {
			if is_close || i + 1 >= s.len || s[i + 1] != `>` {
				return -1
			}
			return i + 2
		}
		// A whitespace separator is required here (between the tag name
		// and the first attribute, or between attributes).
		if c != ` ` && c != `\t` && c != `\n` && c != `\r` {
			return -1
		}
		for i < s.len
			&& (s[i] == ` ` || s[i] == `\t` || s[i] == `\n` || s[i] == `\r`) {
			i++
		}
		if i >= s.len {
			return -1
		}
		if s[i] == `>` {
			return i + 1
		}
		if s[i] == `/` {
			if is_close || i + 1 >= s.len || s[i + 1] != `>` {
				return -1
			}
			return i + 2
		}
		// Closing tags take no attributes.
		if is_close {
			return -1
		}
		// Attribute name must start with a letter, `_` or `:`.
		if !(is_alpha(s[i]) || s[i] == `_` || s[i] == `:`) {
			return -1
		}
		for i < s.len
			&& (is_alpha(s[i]) || is_digit(s[i]) || s[i] == `-` || s[i] == `_` || s[i] == `:` || s[i] == `.`) {
			i++
		}
		// Optional whitespace before `=`; peek without consuming, so that
		// a valueless attribute keeps its trailing separator for the next
		// loop iteration (which requires whitespace before attributes).
		mut j := i
		for j < s.len && (s[j] == ` ` || s[j] == `\t` || s[j] == `\n` || s[j] == `\r`) {
			j++
		}
		if j < s.len && s[j] == `=` {
			i = j + 1
			for i < s.len && (s[i] == ` ` || s[i] == `\t` || s[i] == `\n` || s[i] == `\r`) {
				i++
			}
			if i >= s.len {
				return -1
			}
			if s[i] == `"` || s[i] == `'` {
				q := s[i]
				i++
				for i < s.len && s[i] != q {
					i++
				}
				if i >= s.len {
					return -1
				}
				i++
			} else {
				vstart := i
				for i < s.len
					&& s[i] != ` ` && s[i] != `\t` && s[i] != `\n` && s[i] != `\r` && s[i] != `"` && s[i] != `'` && s[i] != `=` && s[i] != `<` && s[i] != `>` && s[i] != 96 {
					i++
				}
				if i == vstart {
					return -1
				}
			}
		}
	}
	return -1
}

// try_raw_html_tag matches a raw HTML tag starting with '<' and returns the '>' position.
fn try_raw_html_tag(s string) int {
	if s.len < 3 || s[0] != `<` {
		return -1
	}
	if s.starts_with('<!--') {
		// Abrupt closings of an empty comment are complete comments.
		if s.starts_with('<!-->') {
			return 4
		}
		if s.starts_with('<!--->') {
			return 5
		}
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
	// Declaration: `<!` + ASCII letter, then anything up to `>`.
	if s.len > 2 && s[1] == `!` && is_alpha(s[2]) {
		end := s.index_after_('>', 2)
		if end >= 0 {
			return end
		}
		return -1
	}
	tag_end := scan_html_tag(s)
	if tag_end > 0 {
		return tag_end - 1 // index of '>'
	}
	return -1
}

// try_entity parses an HTML entity reference &name; or &#n; or &#xn'
fn (mut p InlineParser) try_entity() ?&Node {
	rest := p.src[p.pos..]
	semi := rest.index(';') or { return none }
	if semi > 32 || semi < 2 {
		return none
	}
	candidate := rest[..semi + 1]
	// Multi-code-point entities (e.g. &ngE;) cannot be represented in the
	// rune-based standard table; resolve the known ones locally.
	if candidate.len > 3 && candidate[1] != `#` {
		inner := candidate[1..candidate.len - 1]
		if multi := multi_rune_entities[inner] {
			p.pos += semi + 1
			return text_node(multi)
		}
	}
	// CommonMark limits the digit count, not the numeric value. Invalid
	// Unicode values, zero, and surrogates decode to U+FFFD below.
	if candidate.len > 3 && candidate[1] == `#` {
		if numeric_entity_too_long(candidate[2..candidate.len - 1]) {
			return none
		}
	}
	decoded := ehtml.unescape(candidate, all: true)
	if decoded == candidate {
		return none
	}
	p.pos += semi + 1
	return text_node(decoded)
}

// multi_rune_entities holds HTML named entities that expand to more than one
// Unicode code point and are therefore absent from the rune-based table.
const multi_rune_entities = {
	'ngE': '≧̸'
}

// numeric_entity_too_long checks CommonMark's decimal and hexadecimal digit limits.
// Malformed bodies within these limits are rejected by the entity decoder.
fn numeric_entity_too_long(body string) bool {
	if body.len > 0 && (body[0] == `x` || body[0] == `X`) {
		return body.len - 1 > 6
	}
	return body.len > 7
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
