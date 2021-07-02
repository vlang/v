// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import x.toml.token
// import x.toml.util

pub type Key = Bare | Quoted

pub fn (k Key) str() string {
	return k.text
}

pub type Value = Bool | Date | DateTime | Number | Quoted | Time | []Value | map[string]Value

pub struct Comment {
pub:
	text string
	pos  token.Position
}

pub fn (c Comment) str() string {
	mut s := typeof(c).name + '{\n'
	s += '  text:  \'$c.text\'\n'
	s += '  pos:  $c.pos\n'
	s += '}'
	return s
}

pub struct Quoted {
pub:
	text string
	pos  token.Position
}

pub fn (q Quoted) str() string {
	mut str := typeof(q).name + '{\n'
	str += '  text:  \'$q.text\'\n'
	str += '  pos:  $q.pos\n'
	str += '}'
	return str
}

pub struct Bare {
pub:
	text string
	pos  token.Position
}

pub fn (b Bare) str() string {
	mut str := typeof(b).name + '{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

pub struct Bool {
pub:
	text string
	pos  token.Position
}

pub fn (b Bool) str() string {
	mut str := typeof(b).name + '{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

pub struct Number {
pub:
	text string
	pos  token.Position
}

pub fn (n Number) str() string {
	mut str := typeof(n).name + '{\n'
	str += '  text:  \'$n.text\'\n'
	str += '  pos:  $n.pos\n'
	str += '}'
	return str
}

pub struct Date {
pub:
	text string
	pos  token.Position
}

pub fn (d Date) str() string {
	mut str := typeof(d).name + '{\n'
	str += '  text:  \'$d.text\'\n'
	str += '  pos:  $d.pos\n'
	str += '}'
	return str
}

pub struct Time {
pub:
	text   string
	offset int
	pos    token.Position
}

pub fn (t Time) str() string {
	mut str := typeof(t).name + '{\n'
	str += '  text:  \'$t.text\'\n'
	str += '  offset:  \'$t.offset\'\n'
	str += '  pos:  $t.pos\n'
	str += '}'
	return str
}

pub struct DateTime {
pub:
	date Date
	time Time
	pos  token.Position
}

pub fn (dt DateTime) str() string {
	mut str := typeof(dt).name + '{\n'
	str += '  date:  \'$dt.date\'\n'
	str += '  time:  \'$dt.time\'\n'
	str += '  pos:  $dt.pos\n'
	str += '}'
	return str
}

pub struct EOF {
pub:
	pos token.Position
}

pub fn (e EOF) str() string {
	mut str := typeof(e).name + '{\n'
	str += '  pos:  $e.pos\n'
	str += '}'
	return str
}
