// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

// Code generation for while loop.
fn (p mut Parser) while_st() {
	p.check(.key_while)
	p.loop_expr_cnt++
	if p.tok != .lcbr {
		p.fspace()
	}
	p.open_scope()
	if p.tok == .lcbr {
		// Infinite loop
		p.gen('while (1) {')
	}
	else {
		// `while condition {`
		p.gen('while (')
		p.check_types(p.bool_expression(), 'bool')
		p.genln(') {')
	}
	p.fspace()
	p.check(.lcbr)
	p.statements()
	p.close_scope()
	p.loop_expr_cnt--
	p.returns = false // TODO handle loops that are guaranteed to return
}

