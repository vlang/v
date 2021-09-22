struct Foo {
	bar int
mut:
	str string
}

fn main() {
	{
		// test add
		mut a := 'a'
		a += 'b'
		println(a)
		a = 'a'
		for i := 1; i < 1000; i++ {
			a += 'b'
		}
		println(a.len)
		println(a.ends_with('bbbbb'))
		a += '123'
		println(a.ends_with('3'))
	}
	{
		// test ends with
		a := 'browser.v'
		println(a.ends_with('.v'))

		s := 'V Programming Language'
		assert s.ends_with('guage') == true
		assert s.ends_with('Language') == true
		assert s.ends_with('Programming Language') == true
		assert s.ends_with('V') == false
	}
	{
		// test between
		s := 'hello [man] how you doing'
		println(s.find_between('[', ']'))
	}
	{
		// test compare
		a := 'Music'
		b := 'src'
		println(b >= a)
	}
	{
		// test lt
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
	{
		// test ge
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
	{
		// test compare strings
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
	{
		// test sort
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
	{
		// todo(playX): sort codegen
		/*// test sort reverse
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
		assert vals[3] == 'arr'*/
	}
	{
		// todo: split nth
		/*
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
		*/
	}
	{
		// test split
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
}
