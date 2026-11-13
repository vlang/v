// Module path implements lexical manipulation of slash-separated paths.
// Use the path helpers in os for native filesystem paths.
module path

import encoding.utf8

// is_abs reports whether p is an absolute path (starts with '/').
pub fn is_abs(p string) bool {
	return p.starts_with('/')
}

// clean returns the shortest path name equivalent to p by purely lexical
// processing, using '/' as separator. It collapses redundant separators and
// removes '.' elements and resolves '..' against preceding ordinary elements.
// Leading '..' elements are preserved in relative paths and discarded at the root.
pub fn clean(p string) string {
	if p == '' {
		return '.'
	}
	mut rooted := p.starts_with('/')
	mut parts := []string{}
	for seg in p.split('/') {
		if seg == '' || seg == '.' {
			continue
		}
		if seg == '..' {
			if parts.len > 0 && parts[parts.len - 1] != '..' {
				parts = parts[..parts.len - 1]
				continue
			}
			if !rooted {
				parts << seg
			}
			continue
		}
		parts << seg
	}
	if parts.len == 0 {
		if rooted {
			return '/'
		}
		return '.'
	}
	mut res := ''
	if rooted {
		res = '/'
	}
	res += parts.join('/')
	return res
}

// join joins any number of path elements into a single path, cleaning the
// result.
pub fn join(elem ...string) string {
	mut parts := []string{}
	for e in elem {
		if e != '' {
			parts << e
		}
	}
	if parts.len == 0 {
		return ''
	}
	return clean(parts.join('/'))
}

// split splits p into dir and file. If p has no slash, dir is ''.
pub fn split(p string) (string, string) {
	if idx := p.last_index('/') {
		return p[..idx + 1], p[idx + 1..]
	}
	return '', p
}

// base returns the last element of p after removing trailing slashes.
// It returns '.' for an empty path and '/' for a path consisting only of slashes.
pub fn base(p string) string {
	if p == '' {
		return '.'
	}
	// strip trailing slashes
	mut q := p
	for q.len > 1 && q.ends_with('/') {
		q = q[..q.len - 1]
	}
	if q == '/' {
		return '/'
	}
	if idx := q.last_index('/') {
		return q[idx + 1..]
	}
	return q
}

// dir returns all but the last element of p.
pub fn dir(p string) string {
	d, _ := split(p)
	if d == '' {
		return '.'
	}
	// remove trailing slash
	mut res := d
	if res.len > 1 && res.ends_with('/') {
		res = res[..res.len - 1]
	}
	if res == '' {
		res = '.'
	}
	return clean(res)
}

// ext returns the suffix starting at the final dot in p's final slash-separated element.
// Leading dots count as extensions; a trailing slash or an element without a dot yields ''.
pub fn ext(p string) string {
	for i := p.len - 1; i >= 0; i-- {
		if p[i] == `/` {
			break
		}
		if p[i] == `.` {
			return p[i..]
		}
	}
	return ''
}

// match reports whether the entire name matches pattern.
// '*' matches any sequence without '/', and '?' matches one non-'/' UTF-8 character.
// Nonempty '[...]' classes support character ranges and leading '^' or '!' negation.
// A backslash escapes the following character, including inside a class.
// Malformed patterns return an error even if the name does not match their prefix.
pub fn match(pattern string, name string) !bool {
	tokens := parse_pattern(pattern)!
	if !tokens.any(it.kind == .star) {
		mut index := 0
		for token in tokens {
			index = token.match_at(name, index)
			if index < 0 {
				return false
			}
		}
		return index == name.len
	}
	// Track reachable byte offsets rather than recursively trying every star expansion.
	// Each token visits each offset at most once, bounding the work by pattern * name.
	mut reachable := []bool{len: name.len + 1}
	reachable[0] = true
	for token in tokens {
		if token.kind == .star {
			for i in 0 .. name.len {
				if reachable[i] && name[i] != `/` {
					reachable[i + 1] = true
				}
			}
			continue
		}
		mut found := false
		// Descending offsets let us reuse the same buffer: tokens only advance the name.
		for i := name.len; i >= 0; i-- {
			if !reachable[i] {
				continue
			}
			reachable[i] = false
			next := token.match_at(name, i)
			if next >= 0 {
				reachable[next] = true
				found = true
			}
		}
		if !found {
			return false
		}
	}
	return reachable[name.len]
}

enum PatternKind {
	literal
	star
	any
	class
}

struct CharacterRange {
	lo rune
	hi rune
}

struct PatternToken {
	kind   PatternKind
	byte   u8
	negate bool
	ranges []CharacterRange
}

fn (token PatternToken) match_at(name string, index int) int {
	if index >= name.len {
		return -1
	}
	if token.kind == .literal {
		return if name[index] == token.byte { index + 1 } else { -1 }
	}
	r, size := utf8.decode_rune_at(name, index)
	if token.kind == .any {
		return if r != `/` { index + size } else { -1 }
	}
	mut matched := false
	for range in token.ranges {
		if r >= range.lo && r <= range.hi {
			matched = true
			break
		}
	}
	return if matched != token.negate { index + size } else { -1 }
}

fn parse_pattern(pattern string) ![]PatternToken {
	mut tokens := []PatternToken{}
	mut i := 0
	for i < pattern.len {
		c := pattern[i]
		i++
		match c {
			`*` {
				if tokens.len == 0 || tokens.last().kind != .star {
					tokens << PatternToken{ kind: .star }
				}
			}
			`?` {
				tokens << PatternToken{ kind: .any }
			}
			`[` {
				mut negate := false
				if i < pattern.len && pattern[i] in [`!`, `^`] {
					negate = true
					i++
				}
				mut ranges := []CharacterRange{}
				for {
					if i >= pattern.len {
						return error('path: invalid pattern')
					}
					if pattern[i] == `]` && ranges.len > 0 {
						i++
						break
					}
					lo, next := class_character(pattern, i)!
					i = next
					mut hi := lo
					if i < pattern.len && pattern[i] == `-` {
						end, after := class_character(pattern, i + 1)!
						hi = end
						i = after
					}
					ranges << CharacterRange{lo, hi}
				}
				tokens << PatternToken{ kind: .class, negate: negate, ranges: ranges }
			}
			`\\` {
				if i >= pattern.len {
					return error('path: invalid pattern')
				}
				tokens << PatternToken{ kind: .literal, byte: pattern[i] }
				i++
			}
			else {
				tokens << PatternToken{ kind: .literal, byte: c }
			}
		}
	}
	return tokens
}

fn class_character(pattern string, start int) !(rune, int) {
	mut i := start
	if i >= pattern.len || pattern[i] in [`-`, `]`] {
		return error('path: invalid pattern')
	}
	if pattern[i] == `\\` {
		i++
		if i >= pattern.len {
			return error('path: invalid pattern')
		}
	}
	r, size := utf8.decode_rune_at(pattern, i)
	if r == 0xfffd && size == 1 {
		return error('path: invalid pattern')
	}
	return r, i + size
}
