import strings

// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
	println(c.split('').len)
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

fn test_trim_space() {
	a := ' a '
	assert a.trim_space() == 'a'
	code := '

fn main() {
        println(2)
}

'
	code_clean := 'fn main() {
        println(2)
}'
	assert code.trim_space() == code_clean
}

fn test_join() {
	mut strings := ['a', 'b', 'c']
	mut s := strings.join(' ')
	assert s == 'a b c'
	strings = [
		'one
two ',
		'three!
four!',
	]
	s = strings.join(' ')
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
	foo := &char('VLANG'.str)
	println(typeof(foo).name)
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
	println('testing double quotes')
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
	// TODO Add test for negative values
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
	println(lines)
	assert lines.len == 1
	println('raw string: "$raw"')

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
	println("\"$a")
	assert "\"$a" == '"10'
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
	println(world)
	s := r'hello\n$world'
	assert s == r'hello\n$world'
	assert s.contains('$')
}

fn test_new_inter() {
	// world := 'world'
	// assert 'hello {world}' == 'hello world'
}

fn test_c_r() {
	// This used to break because of r'' and c''
	c := 42
	println('$c')
	r := 50
	println('$r')
}

fn test_inter_before_comptime_if() {
	s := '123'
	// This used to break ('123 $....')
	$if linux {
		println(s)
	}
	assert s == '123'
}

fn test_double_quote_inter() {
	a := 1
	b := 2
	println('$a $b')
	assert '$a $b' == '1 2'
	assert '$a $b' == '1 2'
}

fn foo(b u8) u8 {
	return b - 10
}

fn filter(b u8) bool {
	return b != `a`
}

fn test_split_into_lines() {
	line_content := 'Line'
	text_crlf := '$line_content\r\n$line_content\r\n$line_content'
	lines_crlf := text_crlf.split_into_lines()

	assert lines_crlf.len == 3
	for line in lines_crlf {
		assert line == line_content
	}

	text_lf := '$line_content\n$line_content\n$line_content'
	lines_lf := text_lf.split_into_lines()

	assert lines_lf.len == 3
	for line in lines_lf {
		assert line == line_content
	}
}

fn test_string_literal_with_backslash() {
	a := 'HelloWorld'
	assert a == 'HelloWorld'

	b := 'OneTwoThree'
	assert b == 'OneTwoThree'
}

/*
type MyString = string

fn test_string_alias() {
	s := MyString('hi')
	ss := s + '!'
}
*/

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
	yy := 'Replacing $rr with $tt'
	assert yy == 'Replacing abc with xyz'

	// Interpolation after quoted variable ending with 'r'quote
	// that may be mistaken with the start of a raw string,
	// ensure that it is not.
	ss := 'Replacing "$rr" with "$tt"'
	assert ss == 'Replacing "abc" with "xyz"'
	zz := "Replacing '$rr' with '$tt'"
	assert zz == "Replacing 'abc' with 'xyz'"

	// Interpolation after quoted variable ending with 'c'quote
	// may be mistaken with the start of a c string, so
	// check it is not.
	cc := 'abc'
	ccc := "Replacing '$cc' with '$tt'"
	assert ccc == "Replacing 'abc' with 'xyz'"
	cccq := 'Replacing "$cc" with "$tt"'
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

fn test_string_with_zero_byte_escape() {
	assert '\x00'.bytes() == [u8(0)]
}
