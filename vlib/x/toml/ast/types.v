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
