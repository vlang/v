// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import v.ast

pub fn (mut g Gen) asm_template(parent ast.AsmStmt, node ast.AsmTemplate) {
	if node.is_label || node.is_directive {
		g.v_error("`asm wasm` doesn't support labels or directives", node.pos)
	}

	match node.name {
		'i32.const' {
			g.expr()
			eprintln(node)
		}
		else {
			g.v_error('unknown opcode', node.pos)
		}
	}
}

pub fn (mut g Gen) asm_stmt(node ast.AsmStmt) {
	for tmpl in node.templates {
		g.asm_template(node, tmpl)
	}
}