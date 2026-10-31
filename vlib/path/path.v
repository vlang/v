// Module path implements slash-separated path manipulation, similar to Go's
// `path` package (as opposed to OS-specific `os.filepath`).
module path

// is_abs reports whether p is an absolute path (starts with '/').
pub fn is_abs(p string) bool {
	return p.starts_with('/')
}

// clean returns the shortest path name equivalent to p by purely lexical
// processing, using '/' as separator. It collapses redundant separators and
// removes '.' and '..' elements.
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

// base returns the last element of p.
pub fn base(p string) string {
	if p == '' {
		return '.'
	}
	// strip trailing slashes
	mut q := p
	for q.len > 1 && q.ends_with('/') {
		q = q[..q.len - 1]
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

// ext returns the file name extension (including the dot).
pub fn ext(p string) string {
	b := base(p)
	if idx := b.last_index('.') {
		if idx > 0 {
			return b[idx..]
		}
	}
	return ''
}

// match reports whether name matches the shell pattern.
// Pattern syntax: '*' matches any sequence, '?' matches single char,
// '[...]' character class.
pub fn match(pattern string, name string) !bool {
	return match_pattern(pattern, name)
}

fn match_pattern(pattern string, name string) !bool {
	// recursive glob matcher
	if pattern == '' {
		return name == ''
	}
	// handle '*'
	if pattern[0] == `*` {
		// try matching rest at every position
		for i in 0 .. name.len + 1 {
			if match_pattern(pattern[1..], name[i..]) or { false } {
				return true
			}
		}
		return false
	}
	if name == '' {
		return false
	}
	if pattern[0] == `?` {
		return match_pattern(pattern[1..], name[1..])
	}
	if pattern[0] == `[` {
		// find closing bracket
		mut j := 1
		mut negate := false
		if j < pattern.len && (pattern[j] == `!` || pattern[j] == `^`) {
			negate = true
			j++
		}
		mut matched := false
		for j < pattern.len && pattern[j] != `]` {
			if j + 2 < pattern.len && pattern[j + 1] == `-` {
				lo := pattern[j]
				hi := pattern[j + 2]
				if name[0] >= lo && name[0] <= hi {
					matched = true
				}
				j += 3
				continue
			}
			if pattern[j] == name[0] {
				matched = true
			}
			j++
		}
		if j >= pattern.len {
			return error('path: invalid pattern')
		}
		j++ // skip ']'
		if negate {
			matched = !matched
		}
		if !matched {
			return false
		}
		return match_pattern(pattern[j..], name[1..])
	}
	if pattern[0] == name[0] {
		return match_pattern(pattern[1..], name[1..])
	}
	return false
}
