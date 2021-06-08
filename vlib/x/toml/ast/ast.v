// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import x.toml.input
//import x.toml.token

interface Node {
	children []Node
}

//pub type Node = Root | Comment

// Root represents the root structure of any parsed TOML text snippet or file.
[heap]
pub struct Root {
pub:
	input              input.Config // User input configuration
pub mut:
	children           []Node
//	scope              &Scope
	//errors           []errors.Error    // all the checker errors in the file
}

pub fn (r Root) str() string {
	mut s := '${typeof(r).name}{\n'
	s += '  input:  $r.input\n'
	s += '  children:  $r.children\n'
	s += '}'
	return s
}

/*
pub fn (n Node) children() []Node {
	mut children := []Node{}
	return children
}
*/
