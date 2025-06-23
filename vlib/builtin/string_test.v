// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

struct Foo {
	bar int
mut:
	str string
}

fn test_add() {
	mut a := 'a'
	a += 'b'
	assert a == ('ab')
	a = 'a'
	for i := 1; i < 1000; i++ {
		a += 'b'
	}
	assert a.len == 1000
	assert a.ends_with('bbbbb')
	a += '123'
	assert a.ends_with('3')
}

fn test_len_utf8() {
	assert 'Vlang'.len_utf8() == 5
	assert 'María'.len_utf8() == 5
	assert '姓名'.len_utf8() == 2
	assert 'Слово'.len_utf8() == 5
	assert 'Λέξη'.len_utf8() == 4
}

fn test_is_pure_ascii() {
	assert 'Vlang'.is_pure_ascii()
	assert !'María'.is_pure_ascii()
	assert !'姓名'.is_pure_ascii()
	assert !'Слово'.is_pure_ascii()
	assert !'Λέξη'.is_pure_ascii()
}

fn test_ends_with() {
	a := 'browser.v'
	assert a.ends_with('.v')

	s := 'V Programming Language'
	assert s.ends_with('guage') == true
	assert s.ends_with('Language') == true
	assert s.ends_with('Programming Language') == true
	assert s.ends_with('V') == false
}

fn test_between() {
	s := 'hello [man] how you doing'
	assert s.find_between('[', ']') == 'man'
	assert s.find_between('[', 'A') == ''
	assert s.find_between('A', ']') == ''
}

fn test_compare() {
	a := 'Music'
	b := 'src'
	assert b >= a
}

fn test_lt() {
	a := ''
	b := 'a'
	c := 'a'
	d := 'b'
	e := 'aa'
	f := 'ab'
	assert a < b
	assert !(b < c)
	assert c < d
	assert !(d < e)
	assert c < e
	assert e < f
}

fn test_ge() {
	a := 'aa'
	b := 'aa'
	c := 'ab'
	d := 'abc'
	e := 'aaa'
	assert b >= a
	assert c >= b
	assert d >= c
	assert !(c >= d)
	assert e >= a
}

fn test_compare_strings() {
	a := 'aa'
	b := 'aa'
	c := 'ab'
	d := 'abc'
	e := 'aaa'
	assert compare_strings(a, b) == 0
	assert compare_strings(b, c) == -1
	assert compare_strings(c, d) == -1
	assert compare_strings(d, e) == 1
	assert compare_strings(a, e) == -1
	assert compare_strings(e, a) == 1
}

fn test_sort() {
	mut vals := [
		'arr',
		'an',
		'a',
		'any',
	]
	len := vals.len
	vals.sort()
	assert len == vals.len
	assert vals[0] == 'a'
	assert vals[1] == 'an'
	assert vals[2] == 'any'
	assert vals[3] == 'arr'
}

fn test_sort_reverse() {
	mut vals := [
		'arr',
		'an',
		'a',
		'any',
	]
	len := vals.len
	vals.sort(b > a)
	assert len == vals.len
	assert vals[0] == 'a'
	assert vals[1] == 'an'
	assert vals[2] == 'any'
	assert vals[3] == 'arr'
}

fn test_ranges() {
	s := 'test'
	s1 := s[0..20] or { 'both' }
	s2 := s[..20] or { 'last' }
	s3 := s[10..] or { 'first' }
	s4 := ranges_propagate_both(s) or { 'both' }
	s5 := ranges_propagate_last(s) or { 'last' }
	s6 := ranges_propagate_first(s) or { 'first' }
	assert s1 == 'both'
	assert s2 == 'last'
	assert s3 == 'first'
	assert s4 == 'both'
	assert s5 == 'last'
	assert s6 == 'first'
}

fn ranges_propagate_first(s string) !string {
	return s[10..]!
}

fn ranges_propagate_last(s string) !string {
	return s[..20]!
}

fn ranges_propagate_both(s string) !string {
	return s[1..20]!
}

fn test_split_nth() {
	a := '1,2,3'
	assert a.split(',').len == 3
	assert a.split_nth(',', -1).len == 3
	assert a.split_nth(',', 0).len == 3
	assert a.split_nth(',', 1).len == 1
	assert a.split_nth(',', 2).len == 2
	assert a.split_nth(',', 10).len == 3
	b := '1::2::3'
	assert b.split('::').len == 3
	assert b.split_nth('::', -1).len == 3
	assert b.split_nth('::', 0).len == 3
	assert b.split_nth('::', 1).len == 1
	assert b.split_nth('::', 2).len == 2
	assert b.split_nth('::', 10).len == 3
	c := 'ABCDEF'
	assert c.split('').len == 6
	assert c.split_nth('', 3).len == 3
	assert c.split_nth('BC', -1).len == 2
	d := ','
	assert d.split(',').len == 2
	assert d.split_nth('', 3).len == 1
	assert d.split_nth(',', -1).len == 2
	assert d.split_nth(',', 3).len == 2
	e := ',,,0,,,,,a,,b,'
	assert e.split(',,').len == 5
	assert e.split_nth(',,', 3).len == 3
	assert e.split_nth(',', -1).len == 12
	assert e.split_nth(',', 3).len == 3
	f := '1:2:3'
	assert f.split_nth(':', 2) == ['1', '2:3']
	assert f.rsplit_nth(':', 2) == ['3', '1:2']
	g := '123'
	assert g.split_nth('', 2) == ['1', '23']
	assert g.rsplit_nth('', 2) == ['3', '12']
	h := ''
	assert h.split_nth('', 2) == []
	assert h.rsplit_nth('', 2) == []
}

fn test_rsplit_nth() {
	a := '1,2,3'
	assert a.rsplit(',').len == 3
	assert a.rsplit_nth(',', -1).len == 3
	assert a.rsplit_nth(',', 0).len == 3
	assert a.rsplit_nth(',', 1).len == 1
	assert a.rsplit_nth(',', 2).len == 2
	assert a.rsplit_nth(',', 10).len == 3
	b := '1::2::3'
	assert b.rsplit('::').len == 3
	assert b.rsplit_nth('::', -1).len == 3
	assert b.rsplit_nth('::', 0).len == 3
	assert b.rsplit_nth('::', 1).len == 1
	assert b.rsplit_nth('::', 2).len == 2
	assert b.rsplit_nth('::', 10).len == 3
	c := 'ABCDEF'
	assert c.rsplit('').len == 6
	assert c.rsplit_nth('', 3).len == 3
	assert c.rsplit_nth('BC', -1).len == 2
	d := ','
	assert d.rsplit(',').len == 2
	assert d.rsplit_nth('', 3).len == 1
	assert d.rsplit_nth(',', -1).len == 2
	assert d.rsplit_nth(',', 3).len == 2
	e := ',,,0,,,,,a,,b,'
	assert e.rsplit(',,').len == 5
	assert e.rsplit_nth(',,', 3).len == 3
	assert e.rsplit_nth(',', -1).len == 12
	assert e.rsplit_nth(',', 3).len == 3
}

fn test_split_nth_values() {
	line := 'CMD=eprintln(phase=1)'

	a0 := line.split_nth('=', 0)
	assert a0.len == 3
	assert a0[0] == 'CMD'
	assert a0[1] == 'eprintln(phase'
	assert a0[2] == '1)'

	a1 := line.split_nth('=', 1)
	assert a1.len == 1
	assert a1[0] == 'CMD=eprintln(phase=1)'

	a2 := line.split_nth('=', 2)
	assert a2.len == 2
	assert a2[0] == 'CMD'
	assert a2[1] == 'eprintln(phase=1)'

	a3 := line.split_nth('=', 3)
	assert a3.len == 3
	assert a3[0] == 'CMD'
	assert a3[1] == 'eprintln(phase'
	assert a3[2] == '1)'

	a4 := line.split_nth('=', 4)
	assert a4.len == 3
	assert a4[0] == 'CMD'
	assert a4[1] == 'eprintln(phase'
	assert a4[2] == '1)'
}

fn test_rsplit_nth_values() {
	line := 'CMD=eprintln(phase=1)'

	a0 := line.rsplit_nth('=', 0)
	assert a0.len == 3
	assert a0[0] == '1)'
	assert a0[1] == 'eprintln(phase'
	assert a0[2] == 'CMD'

	a1 := line.rsplit_nth('=', 1)
	assert a1.len == 1
	assert a1[0] == 'CMD=eprintln(phase=1)'

	a2 := line.rsplit_nth('=', 2)
	assert a2.len == 2
	assert a2[0] == '1)'
	assert a2[1] == 'CMD=eprintln(phase'

	a3 := line.rsplit_nth('=', 3)
	assert a3.len == 3
	assert a0[0] == '1)'
	assert a0[1] == 'eprintln(phase'
	assert a0[2] == 'CMD'

	a4 := line.rsplit_nth('=', 4)
	assert a4.len == 3
	assert a0[0] == '1)'
	assert a0[1] == 'eprintln(phase'
	assert a0[2] == 'CMD'
}

fn test_split() {
	mut s := 'volt/twitch.v:34'
	mut vals := s.split(':')
	assert vals.len == 2
	assert vals[0] == 'volt/twitch.v'
	assert vals[1] == '34'
	// /////////
	s = '2018-01-01z13:01:02'
	vals = s.split('z')
	assert vals.len == 2
	assert vals[0] == '2018-01-01'
	assert vals[1] == '13:01:02'
	// //////////
	s = '4627a862c3dec29fb3182a06b8965e0025759e18___1530207969___blue'
	vals = s.split('___')
	assert vals.len == 3
	assert vals[0] == '4627a862c3dec29fb3182a06b8965e0025759e18'
	assert vals[1] == '1530207969'
	assert vals[2] == 'blue'
	// /////////
	s = 'lalala'
	vals = s.split('a')
	assert vals.len == 4
	assert vals[0] == 'l'
	assert vals[1] == 'l'
	assert vals[2] == 'l'
	assert vals[3] == ''
	// /////////
	s = 'awesome'
	a := s.split('')
	assert a.len == 7
	assert a[0] == 'a'
	assert a[1] == 'w'
	assert a[2] == 'e'
	assert a[3] == 's'
	assert a[4] == 'o'
	assert a[5] == 'm'
	assert a[6] == 'e'
	// /////////
	s = 'wavy turquoise bags'
	vals = s.split(' bags')
	assert vals.len == 2
	assert vals[0] == 'wavy turquoise'
	assert vals[1] == ''
	// /////////
	s = 'aaaa'
	vals = s.split('aa')
	assert vals.len == 3
	assert vals[0] == ''
	assert vals[1] == ''
	assert vals[2] == ''
}

fn test_rsplit() {
	mut s := 'volt/twitch.v:34'
	mut vals := s.rsplit(':')
	assert vals.len == 2
	assert vals[0] == '34'
	assert vals[1] == 'volt/twitch.v'
	// /////////
	s = '2018-01-01z13:01:02'
	vals = s.rsplit('z')
	assert vals.len == 2
	assert vals[0] == '13:01:02'
	assert vals[1] == '2018-01-01'
	// //////////
	s = '4627a862c3dec29fb3182a06b8965e0025759e18___1530207969___blue'
	vals = s.rsplit('___')
	assert vals.len == 3
	assert vals[0] == 'blue'
	assert vals[1] == '1530207969'
	assert vals[2] == '4627a862c3dec29fb3182a06b8965e0025759e18'
	// /////////
	s = 'lalala'
	vals = s.rsplit('a')
	assert vals.len == 4
	assert vals[0] == ''
	assert vals[1] == 'l'
	assert vals[2] == 'l'
	assert vals[3] == 'l'
	// /////////
	s = 'awesome'
	a := s.rsplit('')
	assert a.len == 7
	assert a[0] == 'e'
	assert a[1] == 'm'
	assert a[2] == 'o'
	assert a[3] == 's'
	assert a[4] == 'e'
	assert a[5] == 'w'
	assert a[6] == 'a'
	// /////////
	s = 'wavy turquoise bags'
	vals = s.rsplit('wavy ')
	assert vals.len == 2
	assert vals[0] == 'turquoise bags'
	assert vals[1] == ''
}

fn test_split_any() {
	assert 'ABC'.split_any('') == ['A', 'B', 'C']
	assert ''.split_any(' ') == []
	assert ' '.split_any(' ') == ['']
	assert '  '.split_any(' ') == ['', '']
	assert 'Ciao come stai? '.split_any(' ') == ['Ciao', 'come', 'stai?']
	assert 'Ciao+come*stai? '.split_any('+*') == ['Ciao', 'come', 'stai? ']
	assert 'Ciao+come*stai? '.split_any('+* ') == ['Ciao', 'come', 'stai?']
	assert 'first row\nsecond row'.split_any(' \n') == ['first', 'row', 'second', 'row']
}

fn test_rsplit_any() {
	assert 'ABC'.rsplit_any('') == ['C', 'B', 'A']
	assert ''.rsplit_any(' ') == []
	assert ' '.rsplit_any(' ') == ['']
	assert '  '.rsplit_any(' ') == ['', '']
	assert ' Ciao come stai?'.rsplit_any(' ') == ['stai?', 'come', 'Ciao']
	assert ' Ciao+come*stai?'.rsplit_any('+*') == ['stai?', 'come', ' Ciao']
	assert ' Ciao+come*stai?'.rsplit_any('+* ') == ['stai?', 'come', 'Ciao']
	assert 'first row\nsecond row'.rsplit_any(' \n') == ['row', 'second', 'row', 'first']
}

fn test_split_once() ? {
	path1, ext1 := 'home/dir/lang.zip'.split_once('.')?
	assert path1 == 'home/dir/lang'
	assert ext1 == 'zip'
	path2, ext2 := 'home/dir/lang.ts.dts'.split_once('.')?
	assert path2 == 'home/dir/lang'
	assert ext2 == 'ts.dts'
	path3, ext3 := 'home/dir'.split_once('.') or { '', '' }
	assert path3 == ''
	assert ext3 == ''
}

fn test_rsplit_once() ? {
	path1, ext1 := 'home/dir/lang.zip'.rsplit_once('.')?
	assert path1 == 'home/dir/lang'
	assert ext1 == 'zip'
	path2, ext2 := 'home/dir/lang.ts.dts'.rsplit_once('.')?
	assert path2 == 'home/dir/lang.ts'
	assert ext2 == 'dts'
	path3, ext3 := 'home/dir'.rsplit_once('.') or { '', '' }
	assert path3 == ''
	assert ext3 == ''
}

fn test_split_by_space() {
	assert 'a     b    c'.split_by_space() == ['a', 'b', 'c']
	assert '  a\t\tb\tc'.split_by_space() == ['a', 'b', 'c']
	assert 'a b c \n\r'.split_by_space() == ['a', 'b', 'c']
	assert '\ta b \t \tc \r\n'.split_by_space() == ['a', 'b', 'c']
}

fn test_is_bin() {
	assert ''.is_bin() == false
	assert '0b1'.is_bin() == true
	assert '0b0'.is_bin() == true
	assert '0b'.is_bin() == false
	assert '-0b'.is_bin() == false
	assert '-0b1'.is_bin() == true
	assert '-0b0'.is_bin() == true
	assert '-0b1101'.is_bin() == true
	assert '-0b0101'.is_bin() == true
	assert '-324'.is_bin() == false
	assert '-0'.is_bin() == false
	assert '0x1'.is_bin() == false
	assert '0x1A'.is_bin() == false
	assert '+0b1101'.is_bin() == true
	assert '+0b1'.is_bin() == true
	assert '-0x1'.is_bin() == false
	assert '-0x1A'.is_bin() == false
	assert '0x'.is_bin() == false
	assert '0'.is_bin() == false
	assert '0xFF'.is_bin() == false
	assert '0xG'.is_bin() == false
	assert '-'.is_bin() == false
	assert '+'.is_bin() == false
	assert '-0xFF'.is_bin() == false
	assert '0.34'.is_bin() == false
	assert '0o23'.is_bin() == false
	assert 'vlang'.is_bin() == false
}

fn test_is_oct() {
	assert ''.is_oct() == false
	assert '0o0'.is_oct() == true
	assert '0o1'.is_oct() == true
	assert '0o2'.is_oct() == true
	assert '-0o0'.is_oct() == true
	assert '-0o1'.is_oct() == true
	assert '-0o2'.is_oct() == true

	assert '0o04'.is_oct() == true
	assert '0o16'.is_oct() == true
	assert '0o23'.is_oct() == true
	assert '-0o05'.is_oct() == true
	assert '-0o13'.is_oct() == true
	assert '-0o22'.is_oct() == true

	assert '0o8'.is_oct() == false
	assert '0o9'.is_oct() == false
	assert '-0o8'.is_oct() == false
	assert '-0o9'.is_oct() == false
	assert '0o84'.is_oct() == false
	assert '0o96'.is_oct() == false
	assert '-0o83'.is_oct() == false
	assert '-0o2923'.is_oct() == false
	assert '0b1'.is_oct() == false
	assert '0b0'.is_oct() == false
	assert '0b'.is_oct() == false
	assert '-0b'.is_oct() == false
	assert '-0b1'.is_oct() == false
	assert '-0b0'.is_oct() == false
	assert '-0b1101'.is_oct() == false
	assert '+0o234'.is_oct() == true
	assert '+0o432'.is_oct() == true
	assert '-'.is_oct() == false
	assert '+'.is_oct() == false
	assert '-0b0101'.is_oct() == false
	assert '-324'.is_oct() == false
	assert '-0'.is_oct() == false
	assert '0x1'.is_oct() == false
	assert '0x1A'.is_oct() == false
	assert '-0x1'.is_oct() == false
	assert '-0x1A'.is_oct() == false
	assert '0x'.is_oct() == false
	assert '0'.is_oct() == false
	assert '0xFF'.is_oct() == false
	assert '0xG'.is_oct() == false
	assert '-0xFF'.is_oct() == false
	assert '0.34'.is_oct() == false
	assert 'vlang'.is_oct() == false
}

fn test_is_hex() {
	assert ''.is_hex() == false
	assert '-324'.is_hex() == false
	assert '-0'.is_hex() == false
	assert '0x1'.is_hex() == true
	assert '0x1A'.is_hex() == true
	assert '-0x1'.is_hex() == true
	assert '-0x1A'.is_hex() == true
	assert '+0x1'.is_hex() == true
	assert '+0x1A'.is_hex() == true
	assert '0x'.is_hex() == false
	assert '0'.is_hex() == false
	assert '-'.is_hex() == false
	assert '+'.is_hex() == false
	assert '0xFF'.is_hex() == true
	assert '0xG'.is_hex() == false
	assert '-0xFF'.is_hex() == true
	assert '0b1101'.is_hex() == false
	assert '0.34'.is_hex() == false
	assert '0o23'.is_hex() == false
	assert 'vlang'.is_hex() == false
}

fn test_is_int() {
	assert ''.is_int() == false
	assert '-324'.is_int() == true
	assert '234'.is_int() == true
	assert '-0'.is_int() == true
	assert '-b'.is_int() == false
	assert '-123456789'.is_int() == true
	assert '123456789'.is_int() == true
	assert '0x1'.is_int() == false
	assert '0b1101'.is_int() == false
	assert '0.34'.is_int() == false
	assert '0o23'.is_int() == false
	assert 'vlang'.is_int() == false

	assert '0o0'.is_int() == false
	assert '0o1'.is_int() == false
	assert '0o2'.is_int() == false
	assert '-0o0'.is_int() == false
	assert '-0o1'.is_int() == false
	assert '-0o2'.is_int() == false

	assert '0o04'.is_int() == false
	assert '0o16'.is_int() == false
	assert '0o23'.is_int() == false
	assert '-0o05'.is_int() == false
	assert '-0o13'.is_int() == false
	assert '-0o22'.is_int() == false

	assert '0o8'.is_int() == false
	assert '0o9'.is_int() == false
	assert '-0o8'.is_int() == false
	assert '-0o9'.is_int() == false
	assert '0o84'.is_int() == false
	assert '0o96'.is_int() == false
	assert '-0o83'.is_int() == false
	assert '-0o2923'.is_int() == false
	assert '0b1'.is_int() == false
	assert '0b0'.is_int() == false
	assert '0b'.is_int() == false
	assert '-0b'.is_int() == false
	assert '-0b1'.is_int() == false
	assert '-0b0'.is_int() == false
	assert '-0b1101'.is_int() == false
	assert '-0b0101'.is_int() == false
	assert '-324'.is_int() == true
	assert '-0'.is_int() == true
	assert '0x1'.is_int() == false
	assert '0x1A'.is_int() == false
	assert '-0x1'.is_int() == false
	assert '-0x1A'.is_oct() == false
	assert '0x'.is_int() == false
	assert '0'.is_int() == true
	assert '0xFF'.is_int() == false
	assert '0xG'.is_int() == false
	assert '-0xFF'.is_int() == false
	assert '0.34'.is_int() == false
	assert '-'.is_int() == false
	assert '+'.is_int() == false
	assert '+3'.is_int() == true
	assert '+3232'.is_int() == true
}

fn test_trim_space() {
	a := ' a '
	assert a.trim_space() == 'a'
	assert a.trim_space_left() == 'a '
	assert a.trim_space_right() == ' a'

	code := '
\t
fn main() {
        println(2)
}

'
	code_clean := 'fn main() {
        println(2)
}'
	code_trim_right := '
\t
fn main() {
        println(2)
}'
	code_trim_left := 'fn main() {
        println(2)
}

'
	assert code.trim_space() == code_clean
	assert code.trim_space_right() == code_trim_right
	assert code.trim_space_left() == code_trim_left
}

fn test_join() {
	mut strs := ['a', 'b', 'c']
	mut s := strs.join(' ')
	assert s == 'a b c'
	strs = [
		'one
two ',
		'three!
four!',
	]
	s = strs.join(' ')
	assert s.contains('one') && s.contains('two ') && s.contains('four')
	empty := []string{len: 0}
	assert empty.join('A') == ''
}

fn test_clone() {
	mut a := 'a'
	a += 'a'
	a += 'a'
	b := a
	c := a.clone()
	assert c == a
	assert c == 'aaa'
	assert b == 'aaa'
}

fn test_replace() {
	a := 'hello man!'
	mut b := a.replace('man', 'world')
	assert b == ('hello world!')
	b = b.replace('!', '')
	assert b == ('hello world')
	b = b.replace('h', 'H')
	assert b == ('Hello world')
	b = b.replace('foo', 'bar')
	assert b == ('Hello world')
	s := 'hey man how are you'
	assert s.replace('man ', '') == 'hey how are you'
	lol := 'lol lol lol'
	assert lol.replace('lol', 'LOL') == 'LOL LOL LOL'
	b = 'oneBtwoBBthree'
	assert b.replace('B', '') == 'onetwothree'
	b = '*charptr'
	assert b.replace('charptr', 'byteptr') == '*byteptr'
	c := 'abc'
	assert c.replace('', '-') == c
	v := 'a   b c d'
	assert v.replace('  ', ' ') == 'a  b c d'
}

fn test_replace_each() {
	s := 'hello man man :)'
	q := s.replace_each([
		'man',
		'dude',
		'hello',
		'hey',
	])
	assert q == 'hey dude dude :)'
	bb := '[b]bold[/b] [code]code[/code]'
	assert bb.replace_each([
		'[b]',
		'<b>',
		'[/b]',
		'</b>',
		'[code]',
		'<code>',
		'[/code]',
		'</code>',
	]) == '<b>bold</b> <code>code</code>'
	bb2 := '[b]cool[/b]'
	assert bb2.replace_each([
		'[b]',
		'<b>',
		'[/b]',
		'</b>',
	]) == '<b>cool</b>'
	t := 'aaaaaaaa'
	y := t.replace_each([
		'aa',
		'b',
	])
	assert y == 'bbbb'
	s2 := 'hello_world hello'
	assert s2.replace_each(['hello_world', 'aaa', 'hello', 'bbb']) == 'aaa bbb'
}

fn test_replace_char() {
	assert 'azert'.replace_char(`z`, `s`, 2) == 'assert'
	assert '\rHello!\r'.replace_char(`\r`, `\n`, 1) == '\nHello!\n'
	assert 'Hello!'.replace_char(`l`, `e`, 4) == 'Heeeeeeeeeo!'
	assert '1141'.replace_char(`1`, `8`, 2) == '8888488'
}

fn test_normalize_tabs() {
	assert '\t\tHello!'.normalize_tabs(4) == '        Hello!'
	assert '\t\tHello!\t; greeting'.normalize_tabs(1) == '  Hello! ; greeting'
}

fn test_expand_tabs() {
	assert 'AB\tHello!'.expand_tabs(4) == 'AB  Hello!'
	assert 'AB\t\tHello!\t; greeting'.expand_tabs(4) == 'AB      Hello!  ; greeting'
}

fn test_itoa() {
	num := 777
	assert num.str() == '777'
	big := 7779998
	assert big.str() == '7779998'
	a := 3
	assert a.str() == '3'
	b := 5555
	assert b.str() == '5555'
	zero := 0
	assert zero.str() == '0'
	neg := -7
	assert neg.str() == '-7'
}

fn test_reassign() {
	a := 'hi'
	mut b := a
	b += '!'
	assert a == 'hi'
	assert b == 'hi!'
}

fn test_runes() {
	s := 'привет'
	assert s.len == 12
	s2 := 'privet'
	assert s2.len == 6
	u := s.runes()
	assert u.len == 6
	assert s2.substr(1, 4).len == 3
	assert s2.substr(1, 4) == 'riv'
	assert s2[1..4].len == 3
	assert s2[1..4] == 'riv'
	assert s2[..4].len == 4
	assert s2[..4] == 'priv'
	assert s2[2..].len == 4
	assert s2[2..] == 'ivet'
	assert u[1..4].string().len == 6
	assert u[1..4].string() == 'рив'
	assert s2.substr(1, 2) == 'r'
	assert u[1..2].string() == 'р'
	assert s2.runes()[1] == `r`
	assert u[1] == `р`
	first := u[0]
	last := u[u.len - 1]
	assert first.str().len == 2
	assert last.str().len == 2
}

fn test_contains() {
	s := 'view.v'
	assert s.contains('vi')
	assert !s.contains('random')
	assert ''.contains('')
	assert 'abc'.contains('')
}

fn test_contains_any() {
	assert !'team'.contains_any('i')
	assert 'fail'.contains_any('ui')
	assert 'ure'.contains_any('ui')
	assert 'failure'.contains_any('ui')
	assert !'foo'.contains_any('')
	assert !''.contains_any('')
}

fn test_contains_only() {
	assert '23885'.contains_only('0123456789')
	assert '23gg885'.contains_only('01g23456789')
	assert !'hello;'.contains_only('hello')
	assert !''.contains_only('')
}

fn test_contains_any_substr() {
	s := 'Some random text'
	assert s.contains_any_substr(['false', 'not', 'rand'])
	assert !s.contains_any_substr(['ABC', 'invalid'])
	assert ''.contains_any_substr([])
	assert 'abc'.contains_any_substr([''])
}

fn test_arr_contains() {
	a := ['a', 'b', 'c']
	assert a.contains('b')
	ints := [1, 2, 3]
	assert ints.contains(2)
}

fn test_to_num() {
	s := '7'
	assert s.int() == 7
	assert s.u8() == 7
	assert s.u64() == 7
	f := '71.5 hasdf'
	// QTODO
	assert f.f32() == 71.5
	vals := ['9']
	assert vals[0].int() == 9
	big := '93993993939322'
	assert big.u64() == 93993993939322
	assert big.i64() == 93993993939322
}

fn test_to_u8_array() {
	// empty string
	assert ''.u8_array() == []
	assert '0x'.u8_array() == []
	assert '0X'.u8_array() == []
	assert '0b'.u8_array() == []
	assert '0B'.u8_array() == []
	// invalid digit
	assert '-123'.u8_array() == []
	assert '1_2xt'.u8_array() == []
	assert 'd1_2xt'.u8_array() == []

	// ---------------------------------
	// hex test
	// invalid hex digit
	assert '0X-123'.u8_array() == []
	assert '0O12'.u8_array() == []
	// odd number of digits
	assert '0x1'.u8_array() == [u8(0x01)]
	assert '0x123'.u8_array() == [u8(0x01), 0x23]
	assert '0x1_23'.u8_array() == [u8(0x01), 0x23]

	// long digits
	long_u8 := [u8(0x00), 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc,
		0xdd, 0xee, 0xff]
	assert '0x00112233445566778899aabbccddeeff'.u8_array() == long_u8
	assert '0x00_112_2334455667788_99aabbccddeeff'.u8_array() == long_u8
	assert '0x00112233445566778899AABBCCDDEEFF'.u8_array() == long_u8
	assert '0x001_12233445566778899A_ABBCCDDEEFF'.u8_array() == long_u8

	// ---------------------------------
	// bin test
	// invalid bin digit
	assert '0b-123'.u8_array() == []
	assert '0B12'.u8_array() == []
	// not enough length
	assert '0b0'.u8_array() == [u8(0x00)]
	assert '0b1'.u8_array() == [u8(0x01)]
	assert '0b101'.u8_array() == [u8(0x05)]
	assert '0b0101'.u8_array() == [u8(0x05)]
	// long digits
	assert '0b0101_0101'.u8_array() == [u8(0x55)]
	assert '0b0101010110101010'.u8_array() == [u8(0x55), 0xaa]
	assert '0b0101010110101010_0101010110101010'.u8_array() == [u8(0x55), 0xaa, 0x55, 0xaa]
}

fn test_inter_format_string() {
	float_num := 1.52345
	float_num_string := '-${float_num:.3f}-'
	assert float_num_string == '-1.523-'
	int_num := 7
	int_num_string := '-${int_num:03d}-'
	assert int_num_string == '-007-'
	ch := `a`
	ch_string := '-${ch:c}-'
	assert ch_string == '-a-'
	hex_n := 192
	hex_n_string := '-${hex_n:x}-'
	assert hex_n_string == '-c0-'
	oct_n := 192
	oct_n_string := '-${oct_n:o}-'
	assert oct_n_string == '-300-'
	str := 'abc'
	str_string := '-${str:s}-'
	assert str_string == '-abc-'
}

fn test_hash() {
	s := '10000'
	assert s.hash() == 46730161
	s2 := '24640'
	assert s2.hash() == 47778736
	s3 := 'Content-Type'
	assert s3.hash() == 949037134
	s4 := 'bad_key'
	assert s4.hash() == -346636507
	s5 := '24640'
	// From a map collision test
	assert s5.hash() % ((1 << 20) - 1) == s.hash() % ((1 << 20) - 1)
	assert s5.hash() % ((1 << 20) - 1) == 592861
}

fn test_trim() {
	assert 'banana'.trim('bna') == ''
	assert 'abc'.trim('ac') == 'b'
	assert 'aaabccc'.trim('ac') == 'b'
	assert 'あいうえお'.trim('あい') == 'うえお'
}

fn test_trim_indexes() {
	mut left, mut right := 0, 0
	left, right = '- -- - '.trim_indexes(' -')
	assert left == 0 && right == 0
	left, right = '- hello-world!\t'.trim_indexes(' -\t')
	assert left == 2 && right == 14
	left, right = 'abc'.trim_indexes('ac')
	assert left == 1 && right == 2
}

fn test_trim_left() {
	mut s := 'module main'
	assert s.trim_left(' ') == 'module main'
	s = ' module main'
	assert s.trim_left(' ') == 'module main'
	// test cutset
	s = 'banana'
	assert s.trim_left('ba') == 'nana'
	assert s.trim_left('ban') == ''
	assert 'あいうえお'.trim_left('あい') == 'うえお'
}

fn test_trim_right() {
	mut s := 'module main'
	assert s.trim_right(' ') == 'module main'
	s = 'module main '
	assert s.trim_right(' ') == 'module main'
	// test cutset
	s = 'banana'
	assert s.trim_right('na') == 'b'
	assert s.trim_right('ban') == ''
	assert 'あいうえお'.trim_right('うえお') == 'あい'
}

fn test_all_before() {
	s := 'fn hello fn'
	assert s.all_before(' ') == 'fn'
	assert s.all_before('2') == s
	assert s.all_before('') == s
}

fn test_all_before_last() {
	s := 'fn hello fn'
	assert s.all_before_last(' ') == 'fn hello'
	assert s.all_before_last('2') == s
	assert s.all_before_last('') == s
}

fn test_all_after() {
	s := 'fn hello'
	assert s.all_after('fn ') == 'hello'
	assert s.all_after('test') == s
	assert s.all_after('') == s
	assert s.after('e') == 'llo'
	x := s.after('e')
	assert x == 'llo'
}

fn test_reverse() {
	assert 'hello'.reverse() == 'olleh'
	assert ''.reverse() == ''
	assert 'a'.reverse() == 'a'
}

fn test_bytes_to_string() {
	mut buf := vcalloc(10)
	unsafe {
		buf[0] = `h`
		buf[1] = `e`
		buf[2] = `l`
		buf[3] = `l`
		buf[4] = `o`
	}
	assert unsafe { buf.vstring() } == 'hello'
	assert unsafe { buf.vstring_with_len(2) } == 'he'
	bytes := [u8(`h`), `e`, `l`, `l`, `o`]
	assert bytes.bytestr() == 'hello'
}

fn test_charptr() {
	foo := &char(c'VLANG')
	assert typeof(foo).name == '&char'
	assert unsafe { foo.vstring() } == 'VLANG'
	assert unsafe { foo.vstring_with_len(3) } == 'VLA'
}

fn test_count() {
	assert ''.count('') == 0
	assert ''.count('a') == 0
	assert 'a'.count('') == 0
	assert 'aa'.count('a') == 2
	assert 'aa'.count('aa') == 1
	assert 'aabbaa'.count('aa') == 2
	assert 'bbaabb'.count('aa') == 1
}

fn test_lower() {
	mut s := 'A'
	assert !s.is_lower()
	assert s.to_lower() == 'a'
	assert s.to_lower().len == 1
	s = 'HELLO'
	assert !s.is_lower()
	assert s.to_lower() == 'hello'
	assert s.to_lower().len == 5
	s = 'Aloha'
	assert !s.is_lower()
	assert s.to_lower() == 'aloha'
	s = 'Have A nice Day!'
	assert !s.is_lower()
	assert s.to_lower() == 'have a nice day!'
	s = 'hi'
	assert s.is_lower()
	assert s.to_lower() == 'hi'
	assert 'aloha!'[0] == `a`
	assert 'aloha!'[5] == `!`
	s = '123'
	assert !s.is_lower()
	assert s.to_lower() == '123'
	s = ''
	assert !s.is_lower()
}

fn test_lower_ascii() {
	mut s := 'A'
	assert !s.is_lower()
	assert s.to_lower_ascii() == 'a'
	assert s.to_lower_ascii().len == 1
	s = 'HELLO'
	assert !s.is_lower()
	assert s.to_lower_ascii() == 'hello'
	assert s.to_lower_ascii().len == 5
	s = 'Aloha'
	assert !s.is_lower()
	assert s.to_lower_ascii() == 'aloha'
	s = 'Have A nice Day!'
	assert !s.is_lower()
	assert s.to_lower_ascii() == 'have a nice day!'
	s = 'hi'
	assert s.is_lower()
	assert s.to_lower_ascii() == 'hi'
	assert 'aloha!'[0] == `a`
	assert 'aloha!'[5] == `!`
	s = '123'
	assert !s.is_lower()
	assert s.to_lower_ascii() == '123'
	s = ''
	assert !s.is_lower()
}

fn test_upper() {
	mut s := 'a'
	assert !s.is_upper()
	assert s.to_upper() == 'A'
	assert s.to_upper().len == 1
	s = 'hello'
	assert !s.is_upper()
	assert s.to_upper() == 'HELLO'
	assert s.to_upper().len == 5
	s = 'Aloha'
	assert !s.is_upper()
	assert s.to_upper() == 'ALOHA'
	s = 'have a nice day!'
	assert !s.is_upper()
	assert s.to_upper() == 'HAVE A NICE DAY!'
	s = 'HI'
	assert s.is_upper()
	assert s.to_upper() == 'HI'
	s = '123'
	assert !s.is_upper()
	assert s.to_upper() == '123'
	s = ''
	assert !s.is_upper()
}

fn test_upper_ascii() {
	mut s := 'a'
	assert !s.is_upper()
	assert s.to_upper_ascii() == 'A'
	assert s.to_upper_ascii().len == 1
	s = 'hello'
	assert !s.is_upper()
	assert s.to_upper_ascii() == 'HELLO'
	assert s.to_upper_ascii().len == 5
	s = 'Aloha'
	assert !s.is_upper()
	assert s.to_upper_ascii() == 'ALOHA'
	s = 'have a nice day!'
	assert !s.is_upper()
	assert s.to_upper_ascii() == 'HAVE A NICE DAY!'
	s = 'HI'
	assert s.is_upper()
	assert s.to_upper_ascii() == 'HI'
	s = '123'
	assert !s.is_upper()
	assert s.to_upper_ascii() == '123'
	s = ''
	assert !s.is_upper()
}

fn test_runes_to_upper() {
	assert 'água'.to_upper() == 'ÁGUA'
}

fn test_runes_to_lower() {
	assert 'ÁGUA'.to_lower() == 'água'
}

fn test_capitalize() {
	mut s := 'hello'
	assert !s.is_capital()
	assert s.capitalize() == 'Hello'
	s = 'test'
	assert !s.is_capital()
	assert s.capitalize() == 'Test'
	s = 'i am ray'
	assert !s.is_capital()
	assert s.capitalize() == 'I am ray'
	s = ''
	assert !s.is_capital()
	assert s.capitalize() == ''
	s = 'TEST IT'
	assert !s.is_capital()
	assert s.capitalize() == 'TEST IT'
	s = 'Test it'
	assert s.is_capital()
	assert s.capitalize() == 'Test it'
	assert 'GameMission_t'.capitalize() == 'GameMission_t'
}

fn test_title() {
	mut s := 'hello world'
	assert !s.is_title()
	assert s.title() == 'Hello World'
	s = 'HELLO WORLD'
	assert !s.is_title()
	assert s.title() == 'HELLO WORLD'
	s = 'Hello World'
	assert s.is_title()
	assert s.title() == 'Hello World'
}

fn test_for_loop() {
	mut i := 0
	s := 'abcd'

	for c in s {
		assert c == s[i]
		i++
	}
}

fn test_for_loop_two() {
	s := 'abcd'

	for i, c in s {
		assert c == s[i]
	}
}

fn test_quote() {
	a := `'`
	b := 'hi'
	assert b == 'hi'
	assert a.str() == "'"
}

fn test_limit() {
	s := 'hello'
	assert s.limit(2) == 'he'
	assert s.limit(9) == s
	assert s.limit(0) == ''
	// assert s.limit(-1) == ''
}

fn test_repeat() {
	s1 := 'V! '
	assert s1.repeat(5) == 'V! V! V! V! V! '
	assert s1.repeat(1) == s1
	assert s1.repeat(0) == ''
	s2 := ''
	assert s2.repeat(5) == s2
	assert s2.repeat(1) == s2
	assert s2.repeat(0) == s2
	// TODO: Add test for negative values
}

fn test_starts_with() {
	s := 'V Programming Language'
	assert s.starts_with('V') == true
	assert s.starts_with('V Programming') == true
	assert s.starts_with('Language') == false
}

fn test_starts_with_capital() {
	assert 'A sentence'.starts_with_capital()
	assert 'A paragraph. It also does.'.starts_with_capital()
	assert ''.starts_with_capital() == false
	assert 'no'.starts_with_capital() == false
	assert ' No'.starts_with_capital() == false
}

fn test_trim_string_left() {
	s := 'V Programming Language'
	assert s.trim_string_left('V ') == 'Programming Language'
	assert s.trim_string_left('V Programming ') == 'Language'
	assert s.trim_string_left('Language') == s

	s2 := 'TestTestTest'
	assert s2.trim_string_left('Test') == 'TestTest'
	assert s2.trim_string_left('TestTest') == 'Test'

	s3 := '123Test123Test'
	assert s3.trim_string_left('123') == 'Test123Test'
	assert s3.trim_string_left('123Test') == '123Test'
}

fn test_trim_string_right() {
	s := 'V Programming Language'
	assert s.trim_string_right(' Language') == 'V Programming'
	assert s.trim_string_right(' Programming Language') == 'V'
	assert s.trim_string_right('V') == s

	s2 := 'TestTestTest'
	assert s2.trim_string_right('Test') == 'TestTest'
	assert s2.trim_string_right('TestTest') == 'Test'

	s3 := '123Test123Test'
	assert s3.trim_string_right('123') == s3
	assert s3.trim_string_right('123Test') == '123Test'
}

fn test_raw() {
	raw := r'raw\nstring'
	lines := raw.split('\n')
	assert lines.len == 1

	raw2 := r'Hello V\0'
	assert raw2[7] == `\\`
	assert raw2[8] == `0`

	raw3 := r'Hello V\x00'
	assert raw3[7] == `\\`
	assert raw3[8] == `x`
	assert raw3[9] == `0`
	assert raw3[10] == `0`
}

fn test_raw_with_quotes() {
	raw := r"some'" + r'"thing' // " should be escaped in the generated C code
	assert raw[0] == `s`
	assert raw[5] == `"`
	assert raw[6] == `t`
}

fn test_escape() {
	a := 10
	assert "\"${a}" == '"10'
}

fn test_atoi() {
	assert '234232'.int() == 234232
	assert '-9009'.int() == -9009
	assert '0'.int() == 0
	for n in -10000 .. 100000 {
		s := n.str()
		assert s.int() == n
	}
}

fn test_raw_inter() {
	world := 'world'
	s := r'hello\n$world'
	assert s == r'hello\n$world'
	assert s.contains('$')
}

fn test_c_r() {
	// This used to break because of r'' and c''
	c := 42
	cs := '${c}'
	r := 50
	rs := '${r}'
}

fn test_inter_before_comptime_if() {
	s := '123'
	// This used to break ('123 $....')
	$if linux {
	}
	assert s == '123'
}

fn test_double_quote_inter() {
	a := 1
	b := 2
	assert '${a} ${b}' == '1 2'
	assert '${a} ${b}' == '1 2'
}

fn foo(b u8) u8 {
	return b - 10
}

fn filter(b u8) bool {
	return b != `a`
}

fn test_split_into_lines() {
	line_content := 'line content'

	text_cr := '${line_content}\r${line_content}\r${line_content}'
	lines_cr := text_cr.split_into_lines()

	assert lines_cr.len == 3
	for line in lines_cr {
		assert line == line_content
	}

	text_crlf := '${line_content}\r\n${line_content}\r\n${line_content}'
	lines_crlf := text_crlf.split_into_lines()

	assert lines_crlf.len == 3
	for line in lines_crlf {
		assert line == line_content
	}

	text_lf := '${line_content}\n${line_content}\n${line_content}'
	lines_lf := text_lf.split_into_lines()

	assert lines_lf.len == 3
	for line in lines_lf {
		assert line == line_content
	}

	text_mixed := '${line_content}\n${line_content}\r${line_content}'
	lines_mixed := text_mixed.split_into_lines()

	assert lines_mixed.len == 3
	for line in lines_mixed {
		assert line == line_content
	}

	text_mixed_trailers := '${line_content}\n${line_content}\r${line_content}\r\r\r\n\n\n\r\r'
	lines_mixed_trailers := text_mixed_trailers.split_into_lines()

	assert lines_mixed_trailers.len == 9
	for line in lines_mixed_trailers {
		assert line == line_content || line == ''
	}
}

const single_backslash = '\\'
const double_backslash = '\\\\'
const newline = '\n'

// vfmt off
fn test_string_literal_with_backslash_followed_by_newline() {
	// Note `\` is followed *directly* by a newline, then some more whitespace, then a non whitespace string.
	// In this case, the \ is treated as line breaking, and the whitespace after that on the new line,
	// should be just ignored.
	//
	// See also https://doc.rust-lang.org/reference/tokens.html#string-literals
	// >> Both byte sequences are normally translated to U+000A, but as a special exception,
	// when an unescaped U+005C character occurs immediately before the line-break,
	// the U+005C character, the line-break, and all whitespace at the beginning of the
	// next line are ignored.
	a := 'Hello\
             World'
	assert a == 'HelloWorld'

	// Here, `\\\` means `\\` followed by `\`, followed by a newline.
	// the first is a single escaped \, that should go into the literal, the second together with
	// the newline and the whitespace after it, is a line-break, and should be simply ignored.
	// Same with `\\\\\`, which is `\\\\`, followed by `\`, i.e. an escaped double backslash,
	// and a line-break after it:
	b := 'One \
	         Two Three \\\
             Four \\\\
    Five \\\\\
    end'
	assert b == 'One Two Three ${single_backslash}Four ${double_backslash}${newline}    Five ${double_backslash}end'

	// Note `\\` is followed *directly* by a newline, but `\\` is just an escape for `\`,
	// and thus the newline has no special meaning, and should go into the string literal.
	c := 'Hello\\
        World'
	assert c == 'Hello\\\n        World'

	d := 'One\\
    Two Three \\
    Four'
	assert d == 'One\\\n    Two Three \\\n    Four'
}
// vfmt on

type MyString = string

fn test_string_alias() {
	s := MyString('hi')
	ss := s + '!'
	assert ss == 'hi!'
}

// sort an array of structs, by their string field values

struct Ka {
	s string
	i int
}

fn test_sorter() {
	mut arr := [
		Ka{
			s: 'bbb'
			i: 100
		},
		Ka{
			s: 'aaa'
			i: 101
		},
		Ka{
			s: 'ccc'
			i: 102
		},
	]
	cmp := fn (a &Ka, b &Ka) int {
		return compare_strings(a.s, b.s)
	}
	arr.sort_with_compare(cmp)
	assert arr[0].s == 'aaa'
	assert arr[0].i == 101
	assert arr[1].s == 'bbb'
	assert arr[1].i == 100
	assert arr[2].s == 'ccc'
	assert arr[2].i == 102
}

fn test_fields() {
	assert 'a bcde'.fields() == ['a', 'bcde']
	assert '  sss \t  ssss '.fields() == ['sss', 'ssss']
	assert '\n xyz \t abc   def'.fields() == ['xyz', 'abc', 'def']
	assert 'hello'.fields() == ['hello']
	assert ''.fields() == []
}

fn test_interpolation_after_quoted_variable_still_works() {
	rr := 'abc'
	tt := 'xyz'

	// Basic interpolation, no internal quotes
	yy := 'Replacing ${rr} with ${tt}'
	assert yy == 'Replacing abc with xyz'

	// Interpolation after quoted variable ending with 'r'quote
	// that may be mistaken with the start of a raw string,
	// ensure that it is not.
	ss := 'Replacing "${rr}" with "${tt}"'
	assert ss == 'Replacing "abc" with "xyz"'
	zz := "Replacing '${rr}' with '${tt}'"
	assert zz == "Replacing 'abc' with 'xyz'"

	// Interpolation after quoted variable ending with 'c'quote
	// may be mistaken with the start of a c string, so
	// check it is not.
	cc := 'abc'
	ccc := "Replacing '${cc}' with '${tt}'"
	assert ccc == "Replacing 'abc' with 'xyz'"
	cccq := 'Replacing "${cc}" with "${tt}"'
	assert cccq == 'Replacing "abc" with "xyz"'
}

fn test_emoji_to_runes() {
	x := '👋'
	assert x.runes()[0] == `👋`
}

fn test_string_to_rune() {
	x := 'Hello World 👋'
	assert x.runes().len == 13
}

fn test_index_any() {
	x := 'abcdefghij'
	assert x.index_any('ef') == 4
	assert x.index_any('fe') == 4
}

fn test_string_f64() {
	assert ''.f64() == 0
	assert '123'.f64() == 123
	assert '-123'.f64() == -123
	assert '-123.456'.f64() == -123.456
}

const f32_epsilon = 0.0000000001

fn test_string_f32() {
	assert ''.f32() - 0 <= f32_epsilon
	assert '123'.f32() - 123 < f32_epsilon
	assert '-123'.f32() - (-123) < f32_epsilon
	assert '-123.456'.f32() - (-123.456) <= f32_epsilon
}

fn test_string_is_ascii() {
	assert ''.is_ascii() == true
	assert ' '.is_ascii() == true
	assert '~~'.is_ascii() == true
	assert ' Az~'.is_ascii() == true
	assert ' Aö~'.is_ascii() == false
	assert '👋'.is_ascii() == false
	assert 'a👋bc'.is_ascii() == false
}

fn test_string_is_identifier() {
	assert ''.is_identifier() == false
	assert ' '.is_identifier() == false
	assert '~~'.is_identifier() == false
	assert '_Az~'.is_identifier() == false
	assert '_Aö~'.is_identifier() == false
	assert '👋'.is_identifier() == false
	assert 'a👋bc'.is_identifier() == false
	assert '9'.is_identifier() == false
	assert '_9'.is_identifier() == true
	assert 'a 9'.is_identifier() == false
	assert 't'.is_identifier() == true
}

fn test_string_with_zero_byte_escape() {
	assert '\x00'.bytes() == [u8(0)]
}

fn test_is_blank() {
	assert ''.is_blank()
	assert ' '.is_blank()
	assert ' \t'.is_blank()
	assert ' \t

'.is_blank()
	assert ' \t\r'.is_blank()
	assert ' \t\r

'.is_blank()
}

fn test_indent_width() {
	assert 'abc'.indent_width() == 0
	assert ' abc'.indent_width() == 1
	assert '  abc'.indent_width() == 2
	assert '\tabc'.indent_width() == 1
	assert '\t abc'.indent_width() == 2
	assert '\t\tabc'.indent_width() == 2
	assert '\t\t abc'.indent_width() == 3
}

fn test_index_u8() {
	assert 'abcabca'.index_u8(`a`) == 0
	assert 'abcabca'.index_u8(`b`) == 1
	assert 'abcabca'.index_u8(`c`) == 2

	assert 'abc'.index_u8(`d`) == -1
	assert 'abc'.index_u8(`A`) == -1
	assert 'abc'.index_u8(`B`) == -1
	assert 'abc'.index_u8(`C`) == -1
}

fn test_last_index() {
	assert 'abcabca'.last_index('ca')? == 5
	assert 'abcabca'.last_index('ab')? == 3
	assert 'abcabca'.last_index('b')? == 4
	assert 'Zabcabca'.last_index('Z')? == 0
	x := 'Zabcabca'.last_index('Y')
	assert x == none
	assert 'Zabcabca'.last_index('Y') == none
}

fn test_last_index_u8() {
	assert 'abcabca'.last_index_u8(`a`) == 6
	assert 'abcabca'.last_index_u8(`c`) == 5
	assert 'abcabca'.last_index_u8(`b`) == 4
	assert 'Zabcabca'.last_index_u8(`Z`) == 0
	//
	assert 'abc'.last_index_u8(`d`) == -1
	assert 'abc'.last_index_u8(`A`) == -1
	assert 'abc'.last_index_u8(`B`) == -1
	assert 'abc'.last_index_u8(`C`) == -1
}

fn test_contains_byte() {
	assert 'abc abca'.contains_u8(`a`)
	assert 'abc abca'.contains_u8(`b`)
	assert 'abc abca'.contains_u8(`c`)
	assert 'abc abca'.contains_u8(` `)
	assert !'abc abca'.contains_u8(`A`)
}

fn test_camel_to_snake() {
	assert 'Abcd'.camel_to_snake() == 'abcd'
	assert 'aBcd'.camel_to_snake() == 'a_bcd'
	assert 'AAbb'.camel_to_snake() == 'aa_bb'
	assert 'aaBB'.camel_to_snake() == 'aa_bb'
	assert 'aaBbCcDD'.camel_to_snake() == 'aa_bb_cc_dd'
	assert 'AAbbCC'.camel_to_snake() == 'aa_bb_cc'
	assert 'aaBBcc'.camel_to_snake() == 'aa_bb_cc'
	assert 'aa_BB'.camel_to_snake() == 'aa_bb'
	assert 'aa__BB'.camel_to_snake() == 'aa__bb'
	assert 'JVM_PUBLIC_ACC'.camel_to_snake() == 'jvm_public_acc'
	assert '_ISspace'.camel_to_snake() == '_is_space'
	assert '_aBcd'.camel_to_snake() == '_a_bcd'
	assert '_a_Bcd'.camel_to_snake() == '_a_bcd'
	assert '_AbCDe_'.camel_to_snake() == '_ab_cd_e_'
}

fn test_snake_to_camel() {
	assert 'abcd'.snake_to_camel() == 'Abcd'
	assert 'ab_cd'.snake_to_camel() == 'AbCd'
	assert 'ab_cd_efg'.snake_to_camel() == 'AbCdEfg'
	assert '_abcd'.snake_to_camel() == 'Abcd'
	assert '_abcd_'.snake_to_camel() == 'Abcd'
}

fn test_string_wrap() {
	assert 'Hello World'.wrap(width: 10) == 'Hello\nWorld'
	assert 'Hello World'.wrap(width: 10, end: '<linea-break>') == 'Hello<linea-break>World'
	assert 'The V programming language'.wrap(width: 20, end: '|') == 'The V programming|language'
	assert 'Hello, my name is Carl and I am a delivery'.wrap(width: 20) == 'Hello, my name is\nCarl and I am a\ndelivery'
}

fn test_hex() {
	assert 'Hello World!'.hex() == '48656c6c6f20576f726c6421'
	assert 'VLANG'.hex() == '564c414e47'
	assert 'VLANG'.hex() == 'VLANG'.bytes().hex()
	for c in u8(0) .. 255 {
		assert c.ascii_str().hex() == [c].hex()
	}
}
