module main

type Stmt = ForCStmt | ForInStmt

struct ForCStmt {
	label string
}

struct ForInStmt {
	label string
}

struct Gen {
mut:
	inner_loop    &Stmt = unsafe { nil }
	labeled_loops map[string]&Stmt
}

fn update_loop(mut g Gen, node Stmt) {
	node_ref := &node
	match node {
		ForCStmt {
			saved := g.inner_loop
			g.inner_loop = node_ref
			if node.label != '' {
				unsafe {
					g.labeled_loops[node.label] = node_ref
				}
			}
			g.inner_loop = saved
		}
		else {}
	}
}

fn main() {
	mut g := Gen{}
	update_loop(mut g, ForCStmt{
		label: 'outer'
	})
}
