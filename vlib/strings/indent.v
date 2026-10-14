module strings

// indent prefixes every line of s with prefix, except the first one when
// skip_first is true. It complements the builtin margin/indent helpers
// (string.strip_margin, string.trim_indent), which remove indentation,
// while indent adds it — e.g. for nested code generation or quoted blocks.
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
