module strings

// wrap greedily wraps s to the given column width. Wrapping is rune-aware
// (never splits a multi-byte character), preserves explicit newlines, and
// breaks words longer than width. It is the V-idiomatic equivalent of
// Python's textwrap.wrap (joined with newlines).
pub fn wrap(s string, width int) string {
	if width <= 0 {
		return s
	}
	paragraphs := s.split('\n')
	mut out := []string{cap: paragraphs.len}
	for p in paragraphs {
		out << wrap_paragraph(p, width)
	}
	return out.join('\n')
}

fn wrap_paragraph(p string, width int) string {
	words := p.fields()
	if words.len == 0 {
		return ''
	}
	mut lines := []string{}
	mut cur := []rune{}
	for w in words {
		wr := w.runes()
		if wr.len > width {
			if cur.len > 0 {
				lines << cur.string()
				cur = []rune{}
			}
			mut i := 0
			for i < wr.len {
				mut end := i + width
				if end > wr.len {
					end = wr.len
				}
				lines << wr[i..end].string()
				i = end
			}
			continue
		}
		need := if cur.len == 0 { wr.len } else { wr.len + 1 }
		if cur.len + need > width {
			lines << cur.string()
			cur = []rune{}
		}
		if cur.len > 0 {
			cur << rune(32)
		}
		cur << wr
	}
	if cur.len > 0 {
		lines << cur.string()
	}
	return lines.join('\n')
}

// indent prefixes every line of s with prefix, except the first one when
// skip_first is true.
pub fn indent(s string, prefix string, skip_first bool) string {
	lines := s.split('\n')
	mut out := []string{cap: lines.len}
	for i, line in lines {
		if i == 0 && skip_first {
			out << line
		} else {
			out << prefix + line
		}
	}
	return out.join('\n')
}

// dedent removes the longest common leading whitespace (spaces/tabs) from all
// non-blank lines.
pub fn dedent(s string) string {
	lines := s.split('\n')
	mut min_indent := -1
	for line in lines {
		b := line.bytes()
		mut n := 0
		for n < b.len && (b[n] == 32 || b[n] == 9) {
			n++
		}
		if n == b.len {
			continue // blank line: does not count
		}
		if min_indent == -1 || n < min_indent {
			min_indent = n
		}
	}
	if min_indent <= 0 {
		return s
	}
	mut out := []string{cap: lines.len}
	for line in lines {
		if line.len >= min_indent {
			out << line[min_indent..]
		} else {
			out << line
		}
	}
	return out.join('\n')
}
