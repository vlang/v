module analysis 

import v.ast 
import v.ast.walker

struct HasBreakVisitor {
mut: 
	has_break bool 
}

pub fn has_break(n &ast.Node) bool {
	mut v := HasBreakVisitor{has_break: false}
	walker.walk(mut v, n)
	return v.has_break
}

fn (mut v HasBreakVisitor) visit(node &ast.Node) ! {
	if v.has_break {
		return 
	}

	if node is ast.Stmt {
		match node {
			ast.BranchStmt {
				if node.kind == .key_break && node.label.len > 0 {
					v.has_break = true 
					return
				}
			}
			else {}
		}
	}
}

