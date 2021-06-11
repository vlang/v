// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import x.toml.token

pub struct Comment {
pub:
	text     string
	pos      token.Position
pub mut:
	children []&Node
}

pub fn (c Comment) str() string {
	mut s := typeof(c).name+'{\n'
	s += '  text:  \'$c.text\'\n'
	s += '  pos:  $c.pos\n'
	s += '  children:  $c.children\n'
	s += '}'
	return s
}

pub struct Identifier {
pub:
	text     string
	pos      token.Position
pub mut:
	children []&Node
}

pub fn (i Identifier) str() string {
	mut s := typeof(i).name+'{\n'
	s += '  text:  \'$i.text\'\n'
	s += '  pos:  $i.pos\n'
	s += '  children:  $i.children\n'
	s += '}'
	return s
}

pub struct Assign {
pub:
	text     string
	pos      token.Position
pub mut:
	children []&Node
}

pub fn (a Assign) str() string {
	mut s := typeof(a).name+'{\n'
	s += '  text:  \'$a.text\'\n'
	s += '  pos:  $a.pos\n'
	s += '  children:  $a.children\n'
	s += '}'
	return s
}

pub struct String {
pub:
	text     string
	pos      token.Position
pub mut:
	children []&Node
}

pub fn (s String) str() string {
	mut str := typeof(s).name+'{\n'
	str += '  text:  \'$s.text\'\n'
	str += '  pos:  $s.pos\n'
	str += '  children:  $s.children\n'
	str += '}'
	return str
}

pub struct EOF {
pub:
	pos      token.Position
pub mut:
	children []&Node
}

pub fn (e EOF) str() string {
	mut str := typeof(e).name+'{\n'
	str += '  pos:  $e.pos\n'
	str += '  children:  $e.children\n'
	str += '}'
	return str
}
