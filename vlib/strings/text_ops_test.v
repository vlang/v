module strings

import io

fn test_split_after() {
	assert split_after('a.b.c', '.') == ['a.', 'b.', 'c']
	assert split_after('a.b.', '.') == ['a.', 'b.', ''], 'trailing separator yields a final empty piece, like Go'
	assert split_after('abc', '.') == ['abc']
	assert split_after('', '.') == ['']
	assert split_after('a::b::c', '::') == ['a::', 'b::', 'c']
	assert split_after('a-b', 'xyz') == ['a-b']
	assert split_after('abc', '') == ['abc']
	assert split_after('...', '.') == ['.', '.', '.', '']
}

fn test_equal_fold() {
	assert equal_fold('Go', 'GO')
	assert equal_fold('go', 'GO')
	assert equal_fold('', '')
	assert equal_fold('Abc123!', 'aBC123!')
	assert !equal_fold('Go', 'Go!')
	assert !equal_fold('abc', 'abd')
	assert !equal_fold('Ä', 'ä'), 'non-ASCII is not folded'
	assert equal_fold('Ä', 'Ä')
}

fn test_last_index_any() {
	assert last_index_any('abcabc', 'bc') == 5
	assert last_index_any('hello', 'lo') == 4
	assert last_index_any('hello', 'z') == -1
	assert last_index_any('', 'a') == -1
	assert last_index_any('abc', '') == -1
	assert last_index_any('aaa', 'a') == 2
	assert last_index_any('a.b.c', '.') == 3
}

fn test_contains_rune() {
	assert contains_rune('héllo', `é`)
	assert !contains_rune('hello', `é`)
	assert contains_rune('abc', `b`)
	assert !contains_rune('', `a`)
	assert contains_rune('日本語', `本`)
}

fn test_text_ops_boundaries_and_unicode_contracts() {
	assert split_after('é::文::', '::') == ['é::', '文::', '']
	assert split_after('', '') == ['']
	assert split_after('aba', 'aba') == ['aba', '']
	assert last_index_any('aéz', 'é') == 2
	assert last_index_any('é', 'ø') == 0, 'this API intentionally searches bytes'
	assert contains_rune('x😀y', `😀`)
	assert !contains_rune('é', `ø`)
	assert equal_fold('Vé', 'vé')
	assert !equal_fold('VÉ', 'vé')
}

fn test_title() {
	assert title('hello world') == 'Hello World'
	assert title('already Title') == 'Already Title'
	assert title('') == ''
}

fn test_fields() {
	assert fields('  a b  c ') == ['a', 'b', 'c']
	assert fields('') == []
	assert fields('   ') == []
}

fn test_fields_func() {
	f := fn (r rune) bool { return r == `,` }
	assert fields_func('a,b,,c', f) == ['a', 'b', 'c']
}

fn test_count_rune() {
	assert count_rune('hello', `l`) == 2
}

fn test_index_rune() {
	assert index_rune('hello', `e`) == 1
	assert index_rune('hello', `z`) == -1
}

fn test_last_index_rune() {
	assert last_index_rune('hello', `l`) == 3
	assert last_index_rune('hello', `z`) == -1
}

fn test_reader() {
	mut r := new_reader('hello')
	mut buf := []u8{len: 3}
	n := r.read(mut buf) or { 0 }
	assert n == 3
	assert buf.bytestr() == 'hel'
	assert r.len() == 2
	r.reset('world')
	assert r.len() == 5
}

fn test_to_valid_utf8() {
	assert to_valid_utf8('abc', '') == 'abc'
}

fn test_to_valid_utf8_preserves_valid_sequences() {
	mut raw := []u8{}
	raw << [u8(0x63), 0x61, 0x66, 0xc3, 0xa9, 0xff, 0xe9, 0x9b, 0xaa]
	s := raw.bytestr()
	mut want_raw := []u8{}
	want_raw << [u8(0x63), 0x61, 0x66, 0xc3, 0xa9, u8(`?`), 0xe9, 0x9b, 0xaa]
	assert to_valid_utf8(s, '?') == want_raw.bytestr()
	assert to_valid_utf8('café', '?') == 'café'
	assert to_valid_utf8('abc', '?') == 'abc'
}

fn test_fields_func_decodes_runes() {
	f := fn (r rune) bool { return r == `é` }
	assert fields_func('uév', f) == ['u', 'v']
	f_c3 := fn (r rune) bool { return r == rune(0xc3) }
	assert fields_func('é', f_c3) == ['é']
}

fn test_index_rune_tracks_consumed_bytes() {
	mut ib := []u8{}
	ib << [u8(0xff), u8(0x61)]
	ss := ib.bytestr()
	assert index_rune(ss, `a`) == 1
	assert last_index_rune(ss, `a`) == 1
	assert index_rune('héllo', `é`) == 1
	assert last_index_rune('héllo', `l`) == 4
}

fn test_reader_returns_eof_when_exhausted() {
	mut r := new_reader('ab')
	mut buf := []u8{len: 1}
	assert r.read(mut buf)! == 1
	assert buf[0] == `a`
	assert r.read(mut buf)! == 1
	assert buf[0] == `b`
	r.read(mut buf) or {
		assert err is io.Eof
		return
	}
	assert false, 'expected io.Eof after Reader is exhausted'
}

fn test_reader_composes_with_multi_reader_and_read_all() {
	mut m := io.multi_reader([io.Reader(new_reader('a')), io.Reader(new_reader('b'))])
	data := io.read_all(io.ReadAllConfig{ reader: m })!
	assert data.bytestr() == 'ab'
}
