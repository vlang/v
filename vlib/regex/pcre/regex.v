// Compatibility facade for the historical `regex.pcre` module name.
//
// The implementation is a pure-V non-backtracking automata VM and now lives
// in `regex.meta`, which is the name used by ripgrep's default matcher port.
module pcre

import regex.meta

pub type Regex = meta.Regex
pub type Match = meta.Match
pub type Machine = meta.Machine

pub fn compile(pattern string) !Regex {
	return meta.compile(pattern)
}

pub fn new_regex(pattern string, flags int) !Regex {
	return meta.new_regex(pattern, flags)
}
