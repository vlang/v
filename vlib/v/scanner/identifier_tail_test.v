module scanner

import v.pref
import v.token

fn test_identifier_tail_preserves_every_byte_boundary() {
	for i in 0 .. 256 {
		c := u8(i)
		source := [u8(`a`), c, `Z`].bytestr()
		mut files := token.FileSet.new()
		file := files.add_file('identifier_tail.v', source.len)
		mut s := new_scanner(&pref.Preferences{}, .normal)
		s.init(file, source)
		assert s.scan() == .name
		tail := (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_`
		assert s.lit == if tail { source } else { 'a' }, 'byte ${i}'
		assert s.pos == 0
		assert s.offset == if tail { 3 } else { 1 }, 'byte ${i}'
		assert s.diagnostics.len == 0, 'byte ${i}'
	}
}

fn test_identifier_tail_keeps_prefixes_keywords_and_semicolons() {
	source := "_a0Z abc123 return_value @type r'raw' c'x' js'js'\nreturn"
	mut files := token.FileSet.new()
	file := files.add_file('identifier_tokens.v', source.len)
	mut s := new_scanner(&pref.Preferences{ backend: 'fastc' }, .normal)
	s.init(file, source)
	for expected in [token.Token.name, .name, .name, .name, .string, .char, .string, .semicolon,
		.key_return, .semicolon, .eof] {
		assert s.scan() == expected
	}
	assert s.diagnostics.len == 0
}
