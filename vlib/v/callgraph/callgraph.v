module callgraph

import v.ast
import v.ast.walker
import v.pref
import strings

// callgraph.show walks the AST, starting at main() and prints a DOT output describing the calls
// that function make transitively
pub fn show(mut table ast.Table, pref &pref.Preferences, ast_files []&ast.File) {
	mut mapper := &Mapper{
		pref: pref
		table: table
	}
	mapper.sb.writeln('digraph G {')
	mapper.sb.writeln('\tedge [fontname="Helvetica",fontsize="10",labelfontname="Helvetica",labelfontsize="10",style="solid",color="black"];')
	mapper.sb.writeln('\tnode [fontname="Helvetica",fontsize="10",style="filled",fontcolor="black",fillcolor="white",color="black",shape="box"];')
	mapper.sb.writeln('\trankdir="LR";')
	// Node14 [shape="box",label="PrivateBase",URL="$classPrivateBase.html"];
	// Node15 -> Node9 [dir=back,color="midnightblue",fontsize=10,style="solid"];
	for afile in ast_files {
		walker.walk(mapper, afile)
	}
	mapper.sb.writeln('}')
	println(mapper.sb.str())
}

[heap]
struct Mapper {
	pos int
mut:
	pref            &pref.Preferences
	table           &ast.Table
	file            &ast.File   = 0
	node            &ast.Node   = 0
	fn_decl         &ast.FnDecl = 0
	caller_name     string
	dot_caller_name string
	is_caller_used  bool
	sb              strings.Builder = strings.new_builder(1024)
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
	rec_sym := m.table.get_type_symbol(receiver_type)
	return '${rec_sym.name}.$fname'
}

fn (mut m Mapper) dot_fn_name(fname string, recv_type ast.Type, is_method bool) string {
	if is_method {
		return 'Node_method_' + int(recv_type).str() + '_' + m.dot_normalise_node_name(fname)
	}
	return 'Node_fn_' + m.dot_normalise_node_name(fname)
}

fn (mut m Mapper) visit(node &ast.Node) ? {
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
						if m.caller_name == 'main.main' {
							m.sb.writeln('\t$m.dot_caller_name [label="fn main()",color="blue",height=0.2,width=0.4,fillcolor="#00FF00",tooltip="The main program entry point.",shape=oval];')
						} else {
							m.sb.writeln('\t$m.dot_caller_name [shape="box",label="$m.caller_name"];')
						}
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
						if m.caller_name == 'main.main' {
							m.sb.writeln('\t$m.dot_caller_name -> $dot_called_name [color="blue"];')
						} else {
							m.sb.writeln('\t$m.dot_caller_name -> $dot_called_name')
						}
					}
				}
				else {}
			}
		}
		else {}
	}
}

/*
mut fpath := ''
	if m.file != 0 {
		fpath = m.file.path
	}
	node_pos := node.position()
	fpos := '$fpath:$node_pos.line_nr:$node_pos.col:'
	println('$fpos $node.type_name() | $node')
*/
