// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

fn test_add() {
	mut a := 'a'
	a += 'b'
	assert a==('ab')
	a = 'a'
	for i := 1; i < 1000; i++ {
		a += 'b'
	}
	assert a.len == 1000
	assert a.ends_with('bbbbb')
	a += '123'
	assert a.ends_with('3')
}

fn test_ends_with() {
	a := 'browser.v'
	assert a.ends_with('.v')
}

fn test_between() {
	 s := 'hello [man] how you doing'
	assert s.find_between('[', ']') == 'man'
}

fn test_compare() {
	a := 'Music'
	b := 'src'
	assert b>=(a)
}

fn test_lt() {
	a := ''
	b := 'a'
	c := 'a'
	d := 'b'
	e := 'aa'
	f := 'ab'
	assert a < (b)
	assert !(b < c)
	assert c < (d)
	assert !(d < e)
	assert c < (e)
	assert e < (f)
}

fn test_ge() {
	a := 'aa'
	b := 'aa'
	c := 'ab'
	d := 'abc'
	e := 'aaa'
	assert b >= (a)
	assert c >= (b)
	assert d >= (c)
	assert !(c >= d)
	assert e >= (a)
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
		'arr', 'an', 'a', 'any'
	]
	len := vals.len
	vals.sort()
	assert len == vals.len
	assert vals[0] == 'a'
	assert vals[1] == 'an'
	assert vals[2] == 'any'
	assert vals[3] == 'arr'
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
	assert vals[0] =='2018-01-01'
	assert vals[1] == '13:01:02'
	// //////////
	s = '4627a862c3dec29fb3182a06b8965e0025759e18___1530207969___blue'
	vals = s.split('___')
	assert vals.len == 3
	assert vals[0]== '4627a862c3dec29fb3182a06b8965e0025759e18'
	assert vals[1]=='1530207969'
	assert vals[2]== 'blue'
	// /////////
	s = 'lalala'
	vals = s.split('a')
	assert vals.len == 3
	assert vals[0] == 'l'
	assert vals[1] == 'l'
	assert vals[2] == 'l'
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
	mut strings := [ 'a', 'b', 'c' ]
	mut s := strings.join(' ')
	assert s == 'a b c'
	strings = ['one
two ',
	'three!
four!']
	s = strings.join(' ')
	assert s.contains('one') && s.contains('two ') && s.contains('four')
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
	assert b==('hello world!')
	b = b.replace('!', '')
	assert b==('hello world')
	b = b.replace('h', 'H')
	assert b==('Hello world')
	b = b.replace('foo', 'bar')
	assert b==('Hello world')
	s := 'hey man how are you'
	assert s.replace('man ', '') == 'hey how are you'
	lol := 'lol lol lol'
	assert lol.replace('lol', 'LOL') == 'LOL LOL LOL'
	b = 'oneBtwoBBthree'
	assert b.replace('B', '') == 'onetwothree'
	b = '**char'
	assert b.replace('*char', 'byteptr') == '*byteptr'
	mut c :='abc'
	assert c.replace('','-') == c
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
	u := s.ustring()
	assert u.len == 6
	assert s2.substr(1, 4).len == 3
	assert s2.substr(1, 4) == 'riv'
	assert u.substr(1, 4).len == 6
	assert u.substr(1, 4) == 'рив'
	assert s2.substr(1, 2) == 'r'
	assert u.substr(1, 2) == 'р'
	assert s2.ustring().at(1) == 'r'
	assert u.at(1) == 'р'
	first := u.at(0)
	last := u.at(u.len - 1)
	assert first.len == 2
	assert last.len == 2
}

fn test_lower() {
	mut s := 'A'
	assert s.to_lower() == 'a'
	assert s.to_lower().len == 1
	s = 'HELLO'
	assert s.to_lower() == 'hello'
	assert s.to_lower().len == 5
	s = 'Aloha'
	assert s.to_lower() == 'aloha'
	s = 'Have A nice Day!'
	assert s.to_lower() == 'have a nice day!'
	s = 'hi'
	assert s.to_lower() == 'hi'
}

fn test_upper() {
	mut s := 'a'
	assert s.to_upper() == 'A'
	assert s.to_upper().len == 1
	s = 'hello'
	assert s.to_upper() == 'HELLO'
	assert s.to_upper().len == 5
	s = 'Aloha'
	assert s.to_upper() == 'ALOHA'
	s = 'have a nice day!'
	assert s.to_upper() == 'HAVE A NICE DAY!'
	s = 'hi'
	assert s.to_upper() == 'HI'

}

fn test_left_right() {
	s := 'ALOHA'
	assert s.left(3) == 'ALO'
	assert s.right(3) == 'HA'
	u := s.ustring()
	assert u.left(3) == 'ALO'
	assert u.right(3) == 'HA'
}

fn test_contains() {
	s := 'view.v'
	assert s.contains('vi')
	assert !s.contains('random')
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
	f := '71.5 hasdf'
	assert f.f32() == 71.5
	b := 1.52345
	mut a := '${b:.03f}'
	assert a == '1.523'
	num := 7
	a = '${num:03d}'
	vals := ['9']
	assert vals[0].int() == 9
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
	assert s5.hash() % ((1 << 20) -1) == s.hash() % ((1 << 20) -1)
	assert s5.hash() % ((1 << 20) -1) == 592861
}

fn test_trim() {
	assert 'banana'.trim('bna') == ''
	assert 'abc'.trim('ac') == 'b'
	assert 'aaabccc'.trim('ac') == 'b'
}

fn test_trim_left() {
	mut s := 'module main'
	assert s.trim_left(' ') == 'module main'
	s = ' module main'
	assert s.trim_left(' ') == 'module main'
	// test cutset
	s = 'banana'
	assert s.trim_left('ba') == 'nana'
}

fn test_trim_right() {
	mut s := 'module main'
	assert s.trim_right(' ') == 'module main'
	s = 'module main '
	assert s.trim_right(' ') == 'module main'
	// test cutset
	s = 'banana'
	assert s.trim_right('na') == 'b'
}

fn test_all_after() {
	s := 'fn hello'
	q := s.all_after('fn ')
	assert q == 'hello'
}

fn test_reverse() {
	s := 'hello'
	assert s.reverse() == 'olleh'
	t := ''
	assert t.reverse() == t
}


struct Foo {
	bar int
}

fn (f Foo) baz() string {
	return 'baz'
}

fn test_interpolation() {
	num := 7
	mut s := 'number=$num'
	assert s == 'number=7'
	foo := Foo{}
	s = 'baz=${foo.baz()}'
	assert s == 'baz=baz'

}

fn test_bytes_to_string() {
	mut buf := calloc(10)
	buf[0] = `h`
	buf[1] = `e`
	buf[2] = `l`
	buf[3] = `l`
	buf[4] = `o`
	assert string(buf) == 'hello'
	assert string(buf, 2) == 'he'
	bytes := [`h`, `e`, `l`, `l`, `o`]
	assert string(bytes, 5) == 'hello'
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

fn test_capitalize() {
	mut s := 'hello'
	assert s.capitalize() == 'Hello'
	s = 'test'
	assert s.capitalize() == 'Test'
    s = 'i am ray'
	assert s.capitalize() == 'I am ray'
}

fn test_title() {
	mut s := 'hello world'
	assert s.title() == 'Hello World'
	s.to_upper()
	assert s.title() == 'Hello World'
	s.to_lower()
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
	println("testing double quotes")
	b := "hi"
	assert b == 'hi'
	assert a.str() == '\''
}

fn test_ustring_comparisons() {
	assert ('h€llô !'.ustring() == 'h€llô !'.ustring()) == true
	assert ('h€llô !'.ustring() == 'h€llô'.ustring()) == false
	assert ('h€llô !'.ustring() == 'h€llo !'.ustring()) == false

	assert ('h€llô !'.ustring() != 'h€llô !'.ustring()) == false
	assert ('h€llô !'.ustring() != 'h€llô'.ustring()) == true

	assert ('h€llô'.ustring() < 'h€llô!'.ustring()) == true
	assert ('h€llô'.ustring() < 'h€llo'.ustring()) == false
	assert ('h€llo'.ustring() < 'h€llô'.ustring()) == true

	assert ('h€llô'.ustring() <= 'h€llô!'.ustring()) == true
	assert ('h€llô'.ustring() <= 'h€llô'.ustring()) == true
	assert ('h€llô!'.ustring() <= 'h€llô'.ustring()) == false

	assert ('h€llô!'.ustring() > 'h€llô'.ustring()) == true
	assert ('h€llô'.ustring() > 'h€llô'.ustring()) == false

	assert ('h€llô!'.ustring() >= 'h€llô'.ustring()) == true
	assert ('h€llô'.ustring() >= 'h€llô'.ustring()) == true
	assert ('h€llô'.ustring() >= 'h€llô!'.ustring()) == false
}

fn test_ustring_count() {
	a := 'h€llôﷰ h€llô ﷰ'.ustring()
	assert (a.count('l'.ustring())) == 4
	assert (a.count('€'.ustring())) == 2
	assert (a.count('h€llô'.ustring())) == 2
	assert (a.count('ﷰ'.ustring())) == 2
	assert (a.count('a'.ustring())) == 0
}

fn test_repeat() {
	s := 'V! '
	assert s.repeat(5) == 'V! V! V! V! V! '
}
