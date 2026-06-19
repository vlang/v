module flat

import v3.ast
import v3.token

pub fn flatten(files []ast.File) FlatAst {
	mut a := FlatAst.new()
	for file in files {
		flatten_file(mut a, file)
	}
	return a
}

fn add_children(mut a FlatAst, ids []NodeId) int {
	start := a.children.len
	for id in ids {
		a.children << id
	}
	return start
}

fn flatten_file(mut a FlatAst, file ast.File) NodeId {
	mut ids := []NodeId{}
	for stmt in file.stmts {
		id := flatten_stmt(mut a, stmt)
		if int(id) >= 0 {
			ids << id
		}
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .file
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_stmt(mut a FlatAst, stmt ast.Stmt) NodeId {
	match stmt {
		ast.FnDecl {
			return flatten_fn_decl(mut a, stmt)
		}
		ast.ExprStmt {
			child := flatten_expr(mut a, stmt.expr)
			start := add_children(mut a, [child])
			return a.add_node(Node{
				kind:           .expr_stmt
				children_start: start
				children_count: 1
			})
		}
		ast.AssignStmt {
			return flatten_assign(mut a, stmt)
		}
		ast.ReturnStmt {
			mut ids := []NodeId{}
			for expr in stmt.exprs {
				ids << flatten_expr(mut a, expr)
			}
			start := add_children(mut a, ids)
			return a.add_node(Node{
				kind:           .return_stmt
				children_start: start
				children_count: child_count(ids.len)
			})
		}
		ast.ForStmt {
			return flatten_for(mut a, stmt)
		}
		ast.FlowControlStmt {
			match stmt.op {
				.key_break { return a.add(NodeKind.break_stmt) }
				.key_continue { return a.add(NodeKind.continue_stmt) }
				else { return empty_node }
			}
		}
		ast.BlockStmt {
			return flatten_block(mut a, stmt.stmts)
		}
		ast.StructDecl {
			return flatten_struct_decl(mut a, stmt)
		}
		ast.GlobalDecl {
			return flatten_global_decl(mut a, stmt)
		}
		ast.ConstDecl {
			return empty_node
		}
		ast.ImportStmt, ast.ModuleStmt, []ast.Attribute {
			return empty_node
		}
		ast.EnumDecl, ast.TypeDecl, ast.InterfaceDecl {
			return empty_node
		}
		else {
			return empty_node
		}
	}
}

fn flatten_fn_decl(mut a FlatAst, decl ast.FnDecl) NodeId {
	if decl.language == .c {
		return a.add_node(Node{
			kind:  .c_fn_decl
			value: decl.name
			typ:   return_type_name(decl.typ)
		})
	}
	mut ids := []NodeId{}

	for param in decl.typ.params {
		ids << a.add_node(Node{
			kind:  .param
			value: param.name
			typ:   type_name(param.typ)
		})
	}

	for stmt in decl.stmts {
		id := flatten_stmt(mut a, stmt)
		if int(id) >= 0 {
			ids << id
		}
	}

	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .fn_decl
		value:          decl.name
		typ:            return_type_name(decl.typ)
		children_start: start
		children_count: child_count(ids.len)
		pos:            decl.pos
	})
}

fn flatten_assign(mut a FlatAst, stmt ast.AssignStmt) NodeId {
	mut ids := []NodeId{}
	for i in 0 .. stmt.lhs.len {
		lhs := unwrap_modifier(stmt.lhs[i])
		ids << flatten_expr(mut a, lhs)
		ids << flatten_expr(mut a, stmt.rhs[i])
	}
	mut kind := if stmt.op == .decl_assign {
		NodeKind.decl_assign
	} else {
		NodeKind.assign
	}
	if stmt.lhs.len > 0 && unwrap_modifier(stmt.lhs[0]) is ast.SelectorExpr {
		kind = .selector_assign
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           kind
		op:             token_to_op(stmt.op)
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_for(mut a FlatAst, stmt ast.ForStmt) NodeId {
	mut ids := []NodeId{}

	// child 0: init
	if stmt.init is ast.EmptyStmt {
		ids << a.add(NodeKind.empty)
	} else {
		ids << flatten_stmt(mut a, stmt.init)
	}
	// child 1: cond
	if stmt.cond is ast.EmptyExpr {
		ids << a.add(NodeKind.empty)
	} else {
		ids << flatten_expr(mut a, stmt.cond)
	}
	// child 2: post
	if stmt.post is ast.EmptyStmt {
		ids << a.add(NodeKind.empty)
	} else {
		ids << flatten_stmt(mut a, stmt.post)
	}
	// children 3..n: body statements
	for s in stmt.stmts {
		id := flatten_stmt(mut a, s)
		if int(id) >= 0 {
			ids << id
		}
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .for_stmt
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_block(mut a FlatAst, stmts []ast.Stmt) NodeId {
	mut ids := []NodeId{}
	for stmt in stmts {
		id := flatten_stmt(mut a, stmt)
		if int(id) >= 0 {
			ids << id
		}
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .block
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_struct_decl(mut a FlatAst, decl ast.StructDecl) NodeId {
	mut ids := []NodeId{}
	for field in decl.fields {
		ids << a.add_node(Node{
			kind:  .field_decl
			value: field.name
			typ:   type_name(field.typ)
		})
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .struct_decl
		value:          decl.name
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_global_decl(mut a FlatAst, decl ast.GlobalDecl) NodeId {
	mut ids := []NodeId{}
	for field in decl.fields {
		ids << a.add_node(Node{
			kind:  .field_decl
			value: field.name
			typ:   type_name(field.typ)
		})
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .global_decl
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_match(mut a FlatAst, expr ast.MatchExpr) NodeId {
	match_expr := flatten_expr(mut a, expr.expr)
	mut ids := []NodeId{cap: expr.branches.len + 1}
	ids << match_expr
	for branch in expr.branches {
		mut branch_ids := []NodeId{}
		for cond in branch.cond {
			branch_ids << flatten_expr(mut a, cond)
		}
		for stmt in branch.stmts {
			id := flatten_stmt(mut a, stmt)
			if int(id) >= 0 {
				branch_ids << id
			}
		}
		bstart := add_children(mut a, branch_ids)
		is_else := branch.cond.len == 0 || (branch.cond.len == 1 && branch.cond[0] is ast.Keyword)
		ids << a.add_node(Node{
			kind:           .match_branch
			value:          if is_else { 'else' } else { '' }
			children_start: bstart
			children_count: child_count(branch_ids.len)
		})
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .match_stmt
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn flatten_expr(mut a FlatAst, expr ast.Expr) NodeId {
	match expr {
		ast.BasicLiteral {
			kind := match expr.kind {
				.number { NodeKind.int_literal }
				.key_true, .key_false { NodeKind.bool_literal }
				.char { NodeKind.char_literal }
				else { NodeKind.int_literal }
			}

			return a.add_val(kind, expr.value)
		}
		ast.StringLiteral {
			return a.add_val(.string_literal, strip_quotes(expr.value))
		}
		ast.StringInterLiteral {
			mut ids := []NodeId{}
			for i, val in expr.values {
				if val.len > 0 {
					ids << a.add_val(.string_literal, strip_quotes(val))
				}
				if i < expr.inters.len {
					ids << flatten_expr(mut a, expr.inters[i].expr)
				}
			}
			start := add_children(mut a, ids)
			return a.add_node(Node{
				kind:           .string_literal
				value:          '_interp'
				children_start: start
				children_count: child_count(ids.len)
			})
		}
		ast.Ident {
			return a.add_val(.ident, expr.name)
		}
		ast.CallExpr {
			mut ids := []NodeId{}
			ids << flatten_expr(mut a, expr.lhs)
			for arg in expr.args {
				ids << flatten_expr(mut a, arg)
			}
			start := add_children(mut a, ids)
			return a.add_node(Node{
				kind:           .call
				children_start: start
				children_count: child_count(ids.len)
			})
		}
		ast.CallOrCastExpr {
			lhs := flatten_expr(mut a, expr.lhs)
			arg := flatten_expr(mut a, expr.expr)
			start := add_children(mut a, [lhs, arg])
			return a.add_node(Node{
				kind:           .call
				children_start: start
				children_count: 2
			})
		}
		ast.InfixExpr {
			lhs := flatten_expr(mut a, expr.lhs)
			rhs := flatten_expr(mut a, expr.rhs)
			start := add_children(mut a, [lhs, rhs])
			return a.add_node(Node{
				kind:           .infix
				op:             token_to_op(expr.op)
				children_start: start
				children_count: 2
			})
		}
		ast.PrefixExpr {
			child := flatten_expr(mut a, expr.expr)
			start := add_children(mut a, [child])
			return a.add_node(Node{
				kind:           .prefix
				op:             token_to_op(expr.op)
				children_start: start
				children_count: 1
			})
		}
		ast.PostfixExpr {
			child := flatten_expr(mut a, expr.expr)
			start := add_children(mut a, [child])
			return a.add_node(Node{
				kind:           .postfix
				op:             token_to_op(expr.op)
				children_start: start
				children_count: 1
			})
		}
		ast.ParenExpr {
			child := flatten_expr(mut a, expr.expr)
			start := add_children(mut a, [child])
			return a.add_node(Node{
				kind:           .paren
				children_start: start
				children_count: 1
			})
		}
		ast.SelectorExpr {
			child := flatten_expr(mut a, expr.lhs)
			start := add_children(mut a, [child])
			return a.add_node(Node{
				kind:           .selector
				value:          expr.rhs.name
				children_start: start
				children_count: 1
			})
		}
		ast.IndexExpr {
			lhs := flatten_expr(mut a, expr.lhs)
			idx := flatten_expr(mut a, expr.expr)
			start := add_children(mut a, [lhs, idx])
			return a.add_node(Node{
				kind:           .index
				children_start: start
				children_count: 2
			})
		}
		ast.InitExpr {
			mut ids := []NodeId{}
			for field in expr.fields {
				val := flatten_expr(mut a, field.value)
				ids << a.add_node(Node{
					kind:           .field_init
					value:          field.name
					children_start: add_children(mut a, [val])
					children_count: 1
				})
			}
			start := add_children(mut a, ids)
			return a.add_node(Node{
				kind:           .struct_init
				value:          expr.typ.name()
				children_start: start
				children_count: child_count(ids.len)
			})
		}
		ast.MatchExpr {
			return flatten_match(mut a, expr)
		}
		ast.IfExpr {
			return flatten_if(mut a, expr)
		}
		ast.ModifierExpr {
			return flatten_expr(mut a, expr.expr)
		}
		ast.EmptyExpr {
			return a.add(NodeKind.empty)
		}
		else {
			return a.add(NodeKind.empty)
		}
	}
}

fn flatten_if(mut a FlatAst, expr ast.IfExpr) NodeId {
	mut ids := []NodeId{}
	// child 0: condition
	if expr.cond is ast.EmptyExpr {
		ids << a.add(NodeKind.empty)
	} else {
		ids << flatten_expr(mut a, expr.cond)
	}
	// child 1: then block
	ids << flatten_block(mut a, expr.stmts)
	// child 2: else
	if expr.else_expr is ast.IfExpr {
		ids << flatten_if(mut a, expr.else_expr)
	} else if expr.else_expr !is ast.EmptyExpr {
		if expr.else_expr is ast.IfExpr {
			ids << flatten_if(mut a, expr.else_expr)
		}
	}
	start := add_children(mut a, ids)
	return a.add_node(Node{
		kind:           .if_expr
		children_start: start
		children_count: child_count(ids.len)
	})
}

fn unwrap_modifier(expr ast.Expr) ast.Expr {
	if expr is ast.ModifierExpr {
		return expr.expr
	}
	return expr
}

fn type_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.Type {
			match expr {
				ast.OptionType { return '?${type_name(expr.base_type)}' }
				ast.PointerType { return '&${type_name(expr.base_type)}' }
				ast.ResultType { return '!${type_name(expr.base_type)}' }
				else { return '' }
			}
		}
		else {
			return ''
		}
	}
}

fn return_type_name(typ ast.FnType) string {
	if typ.return_type is ast.EmptyExpr {
		return 'void'
	}
	return type_name(typ.return_type)
}

fn strip_quotes(s string) string {
	if s.len >= 2 && ((s[0] == `'` && s[s.len - 1] == `'`) || (s[0] == `"` && s[s.len - 1] == `"`)) {
		return s[1..s.len - 1]
	}
	return s
}

fn token_to_op(tok token.Token) Op {
	return match tok {
		.plus { Op.plus }
		.minus { Op.minus }
		.mul { Op.mul }
		.div { Op.div }
		.mod { Op.mod }
		.eq { Op.eq }
		.ne { Op.ne }
		.lt { Op.lt }
		.gt { Op.gt }
		.le { Op.le }
		.ge { Op.ge }
		.amp { Op.amp }
		.pipe { Op.pipe }
		.xor { Op.xor }
		.left_shift { Op.left_shift }
		.right_shift { Op.right_shift }
		.and { Op.logical_and }
		.logical_or { Op.logical_or }
		.not { Op.not }
		.bit_not { Op.bit_not }
		.assign { Op.assign }
		.plus_assign { Op.plus_assign }
		.minus_assign { Op.minus_assign }
		.mul_assign { Op.mul_assign }
		.div_assign { Op.div_assign }
		.inc { Op.inc }
		.dec { Op.dec }
		.decl_assign { Op.assign }
		else { Op.none }
	}
}
