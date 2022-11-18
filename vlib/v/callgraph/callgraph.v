module callgraph

import v.ast
import v.ast.walker
import v.pref
import v.dotgraph

// callgraph.show walks the AST, starting at main() and prints a DOT output describing the calls
// that function make transitively
pub fn show(mut table ast.Table, pref &pref.Preferences, ast_files []&ast.File) {
	mut mapper := &Mapper{
		pref: pref
		table: table
		dg: dotgraph.new('CallGraph', 'CallGraph for ${pref.path}', 'green')
	}
	// Node14 [shape="box",label="PrivateBase",URL="$classPrivateBase.html"];
	// Node15 -> Node9 [dir=back,color="midnightblue",fontsize=10,style="solid"];
	for afile in ast_files {
		walker.walk(mut mapper, afile)
	}
	mapper.dg.finish()
}

[heap]
struct Mapper {
	pos int
mut:
	pref            &pref.Preferences = unsafe { nil }
	table           &ast.Table        = unsafe { nil }
	file            &ast.File   = unsafe { nil }
	node            &ast.Node   = unsafe { nil }
	fn_decl         &ast.FnDecl = unsafe { nil }
	caller_name     string
	dot_caller_name string
	is_caller_used  bool
	dg              dotgraph.DotGraph
}

fn (mut m Mapper) dot_normalise_node_name(name string) string {
	res := name.replace_each([
		'.',
		'_',
		'==',
		'op_eq',
		'>=',
		'op_greater_eq',
		'<=',
		'op_lesser_eq',
		'>',
		'op_greater',
		'<',
		'op_lesser',
		'+',
		'op_plus',
		'-',
		'op_minus',
		'/',
		'op_divide',
		'*',
		'op_multiply',
		'^',
		'op_xor',
		'|',
		'op_or',
		'&',
		'op_and',
	])
	return res
}

fn (mut m Mapper) fn_name(fname string, receiver_type ast.Type, is_method bool) string {
	if !is_method {
		return fname
	}
	rec_sym := m.table.sym(receiver_type)
	return '${rec_sym.name}.${fname}'
}

fn (mut m Mapper) dot_fn_name(fname string, recv_type ast.Type, is_method bool) string {
	if is_method {
		return 'Node_method_' + int(recv_type).str() + '_' + m.dot_normalise_node_name(fname)
	}
	return 'Node_fn_' + m.dot_normalise_node_name(fname)
}

fn (mut m Mapper) visit(node &ast.Node) ! {
	m.node = unsafe { node }
	match node {
		ast.File {
			m.file = unsafe { &node }
		}
		ast.Stmt {
			match node {
				ast.FnDecl {
					m.is_caller_used = true
					if m.pref.skip_unused {
						m.is_caller_used = m.table.used_fns[node.fkey()]
					}
					m.fn_decl = unsafe { &node }
					m.caller_name = m.fn_name(node.name, node.receiver.typ, node.is_method)
					m.dot_caller_name = m.dot_fn_name(node.name, node.receiver.typ, node.is_method)
					if m.is_caller_used {
						m.dg.new_node(m.caller_name,
							node_name: m.dot_caller_name
							should_highlight: m.caller_name == 'main.main'
						)
					}
				}
				else {}
			}
		}
		ast.Expr {
			match node {
				ast.CallExpr {
					if m.is_caller_used {
						dot_called_name := m.dot_fn_name(node.name, node.receiver_type,
							node.is_method)
						// Node15 -> Node9 [dir=back,color="midnightblue",fontsize=10,style="solid"];
						m.dg.new_edge(m.dot_caller_name, dot_called_name,
							should_highlight: m.caller_name == 'main.main'
						)
					}
				}
				else {}
			}
		}
		else {}
	}
}
