// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

fn test_add() {
	mut a := 'a'
	a += 'b'
	assert a.eq('ab')
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
	assert b.ge(a)
}

fn test_lt() {
	a := ''
	b := 'a'
	c := 'a'
	d := 'b'
	e := 'aa'
	f := 'ab'
	assert a.lt(b)
	assert b.lt(c) == false
	assert c.lt(d)
	assert d.lt(e) == false
	assert c.lt(e)
	assert e.lt(f)
}

fn test_ge() {
	a := 'aa'
	b := 'aa'
	c := 'ab'
	d := 'abc'
	e := 'aaa'
	assert b.ge(a)
	assert c.ge(b)
	assert d.ge(c)
	assert c.ge(d) == false
	assert e.ge(a)
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
	assert vals[0]== 'volt/twitch.v'
	assert vals[1]== '34'
	// /////////
	s = '2018-01-01z13:01:02'
	vals = s.split('z')
	assert vals.len == 2
	assert vals[0]=='2018-01-01'
	assert vals[1]== '13:01:02'
	// /////////////
	s = '4627a862c3dec29fb3182a06b8965e0025759e18___1530207969___blue'
	vals = s.split('___')
	assert vals.len == 3
	assert vals[0]== '4627a862c3dec29fb3182a06b8965e0025759e18'
	assert vals[1]=='1530207969'
	assert vals[2]== 'blue'
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
	mut b := a
	mut c := a.clone()
	assert c == a
	assert c == 'aaa'
	assert b == 'aaa'
}

fn test_replace() {
	a := 'hello man!'
	mut b := a.replace('man', 'world')
	assert b.eq('hello world!')
	b = b.replace('!', '')
	assert b.eq('hello world')
	b = b.replace('h', 'H')
	assert b.eq('Hello world')
	b = b.replace('kek', 'lul')
	assert b.eq('Hello world')
	s := 'hey man how are you'
	assert s.replace('man ', '') == 'hey how are you'
	lol := 'lol lol lol'
	assert lol.replace('lol', 'kek') == 'kek kek kek'
	b = 'oneBtwoBBthree'
	assert b.replace('B', '') == 'onetwothree'
	b = '**char'
	assert b.replace('*char', 'byteptr') == '*byteptr'
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
	// ///////
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
	assert s.to_i() == 7
	f := '71.5 hasdf'
	assert f.to_float() == 71.5
	b := 1.52345
	mut a := '${b:.03f}'
	assert a == '1.523'
	num := 7
	a = '${num:03d}'
	vals := ['9']
	assert vals[0].to_i() == 9
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

fn test_trim_left() {
	mut s := 'module main'
	assert s.trim_left(' ') == 'module main'
	s = ' module main'
	assert s.trim_left(' ') == 'module main'
}

fn test_all_after() {
	s := 'fn hello'
	q := s.all_after('fn ')
	assert q == 'hello'
}