module main

import v.token
import v.ast

// do not support yet by vlang
// fn (t Tree) array_node1<T>(nodes []T, method_name string) &Node {
// 	mut arr := new_array()

// 	// call method dynamically, V do not support yet
// 	// error: todo: not a string literal

// 	// for node in nodes {
// 	// 	arr.add_item(t.$method_name(node))
// 	// }

// 	// temp
// 	$for method in Tree.methods {
// 		if method.name == method_name {
// 			for node in nodes {
// 				res := t.$method(node)
// 				arr.add_item(res) // TODO,waiting for bug fixed
// 			}
// 		}
// 	}
// 	return arr
// }

// do not support yet by vlang
// fn (t Tree) array_node2<T>(nodes []T) &Node {
// 	mut arr := new_array()

// 	for node in nodes {
// 		match node {
// 			string {
// 				arr.add_item(t.string_node(node))
// 			}
// 			ast.Comment {
// 				arr.add_item(t.comment(node))
// 			}
// 			ast.ConstField {
// 				arr.add_item(t.const_field(node))
// 			}
// 			else {
// 				panic('unknown array type')
// 			}
// 		}
// 	}

// 	return arr
// }

// temporary
fn (t Tree) array_node_string(nodes []string) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.string_node(node))
	}
	return arr
}

fn (t Tree) array_node_position(nodes []token.Position) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.position(node))
	}
	return arr
}

fn (t Tree) array_node_if_branch(nodes []ast.IfBranch) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.if_branch(node))
	}
	return arr
}

fn (t Tree) array_node_fn_decl(nodes []ast.FnDecl) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.fn_decl(node))
	}
	return arr
}

fn (t Tree) array_node_generic_fns(nodes []&ast.FnDecl) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.fn_decl(node))
	}
	return arr
}

fn (t Tree) array_node_embed_file(nodes []ast.EmbeddedFile) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.embed_file(node))
	}
	return arr
}

fn (t Tree) array_node_attr(nodes []ast.Attr) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.attr(node))
	}
	return arr
}

fn (t Tree) array_node_scope_struct_field(nodes []ast.ScopeStructField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.scope_struct_field(node))
	}
	return arr
}

fn (t Tree) array_node_type(nodes []ast.Type) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.type_node(node))
	}
	return arr
}

fn (t Tree) array_node_import_symbol(nodes []ast.ImportSymbol) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.import_symbol(node))
	}
	return arr
}

fn (t Tree) array_node_comment(nodes []ast.Comment) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.comment(node))
	}
	return arr
}

fn (t Tree) array_node_const_field(nodes []ast.ConstField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.const_field(node))
	}
	return arr
}

fn (t Tree) array_node_arg(nodes []ast.Param) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.arg(node))
	}
	return arr
}

fn (t Tree) array_node_stmt(nodes []ast.Stmt) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.stmt(node))
	}
	return arr
}

fn (t Tree) array_node_defer_stmt(nodes []ast.DeferStmt) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.defer_stmt(node))
	}
	return arr
}

fn (t Tree) array_node_struct_field(nodes []ast.StructField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.struct_field(node))
	}
	return arr
}

fn (t Tree) array_node_embed(nodes []ast.Embed) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.embed(node))
	}
	return arr
}

fn (t Tree) array_node_enum_field(nodes []ast.EnumField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.enum_field(node))
	}
	return arr
}

fn (t Tree) array_node_global_field(nodes []ast.GlobalField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.global_field(node))
	}
	return arr
}

fn (t Tree) array_node_expr(nodes []ast.Expr) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.expr(node))
	}
	return arr
}

fn (t Tree) array_node_call_arg(nodes []ast.CallArg) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.call_arg(node))
	}
	return arr
}

fn (t Tree) array_node_int(nodes []int) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.number_node(node))
	}
	return arr
}

fn (t Tree) array_node_byte(nodes []byte) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.number_node(node))
	}
	return arr
}

fn (t Tree) array_node_bool(nodes []bool) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.bool_node(node))
	}
	return arr
}

fn (t Tree) array_node_struct_init_field(nodes []ast.StructInitField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.struct_init_field(node))
	}
	return arr
}

fn (t Tree) array_node_struct_init_embed(nodes []ast.StructInitEmbed) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.struct_init_embed(node))
	}
	return arr
}

fn (t Tree) array_node_match_branch(nodes []ast.MatchBranch) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.match_branch(node))
	}
	return arr
}

fn (t Tree) array_node_ident(nodes []ast.Ident) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.ident(node))
	}
	return arr
}

fn (t Tree) array_node_select_branch(nodes []ast.SelectBranch) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.select_branch(node))
	}
	return arr
}

fn (t Tree) array_node_asm_clobbered(nodes []ast.AsmClobbered) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_clobbered(node))
	}
	return arr
}

fn (t Tree) array_node_asm_template(nodes []ast.AsmTemplate) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_template(node))
	}
	return arr
}

fn (t Tree) array_node_asm_io(nodes []ast.AsmIO) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_io(node))
	}
	return arr
}

fn (t Tree) array_node_asm_arg(nodes []ast.AsmArg) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_arg(node))
	}
	return arr
}

fn (t Tree) array_node_sql_stmt_line(nodes []ast.SqlStmtLine) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.sql_stmt_line(node))
	}
	return arr
}

fn (t Tree) array_node_interface_embedding(nodes []ast.InterfaceEmbedding) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.interface_embedding(node))
	}
	return arr
}
