module strings

// cut slices s around the first instance of sep, returning the text before and
// after sep. The found result reports whether sep appears in s. If sep does not
// appear, cut returns s, "", false. Mirrors Go's strings.Cut.
pub fn cut(s string, sep string) (string, string, bool) {
	if sep == '' {
		return '', s, true
	}
	if idx := s.index(sep) {
		return s[..idx], s[idx + sep.len..], true
	}
	return s, '', false
}

// cut_prefix returns s without the provided leading prefix and reports whether
// it found the prefix. Mirrors Go's strings.CutPrefix.
pub fn cut_prefix(s string, prefix string) (string, bool) {
	if s.starts_with(prefix) {
		return s[prefix.len..], true
	}
	return s, false
}

// cut_suffix returns s without the provided trailing suffix and reports whether
// it found the suffix. Mirrors Go's strings.CutSuffix.
pub fn cut_suffix(s string, suffix string) (string, bool) {
	if s.ends_with(suffix) {
		return s[..s.len - suffix.len], true
	}
	return s, false
}
