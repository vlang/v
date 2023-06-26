// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast

fn (mut g Gen) expr(node ast.Expr) {
	match node {
		ast.ParExpr {
			g.expr(node.expr)
		}
		ast.ArrayInit {
			pos := g.allocate_array('_anonarray', 8, node.exprs.len)
			g.code_gen.init_array(LocalVar{ offset: pos, typ: node.typ }, node)
			g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), pos)
		}
		ast.BoolLiteral {
			g.code_gen.mov64(g.code_gen.main_reg(), if node.val {
				1
			} else {
				0
			})
		}
		ast.CallExpr {
			if node.name == 'C.syscall' {
				g.code_gen.gen_syscall(node)
			} else if node.name == 'exit' {
				g.code_gen.gen_exit(node.args[0].expr)
			} else if node.name in ['println', 'print', 'eprintln', 'eprint'] {
				expr := node.args[0].expr
				typ := node.args[0].typ
				g.gen_print_from_expr(expr, typ, node.name)
			} else {
				g.code_gen.call_fn(node)
			}
		}
		ast.FloatLiteral {
			val := g.eval.expr(node, ast.float_literal_type_idx).float_val()
			g.code_gen.load_fp(val)
		}
		ast.Ident {
			var := g.get_var_from_ident(node)
			// XXX this is intel specific
			match var {
				LocalVar {
					if g.is_register_type(var.typ) {
						g.code_gen.mov_var_to_reg(g.code_gen.main_reg(), node as ast.Ident)
					} else if var.typ.is_pure_float() {
						g.code_gen.load_fp_var(node as ast.Ident)
					} else {
						ts := g.table.sym(var.typ)
						match ts.info {
							ast.Struct {
								g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), g.get_var_offset(node.name))
							}
							ast.Enum {
								g.code_gen.mov_var_to_reg(g.code_gen.main_reg(), node as ast.Ident,
									typ: ast.int_type_idx
								)
							}
							else {
								g.n_error('Unsupported variable type')
							}
						}
					}
				}
				else {
					g.n_error('Unsupported variable kind')
				}
			}
		}
		ast.IfExpr {
			g.if_expr(node)
		}
		ast.InfixExpr {
			g.code_gen.infix_expr(node)
		}
		ast.IntegerLiteral {
			g.code_gen.mov64(g.code_gen.main_reg(), i64(node.val.u64()))
		}
		ast.Nil {
			g.code_gen.mov64(g.code_gen.main_reg(), 0)
		}
		ast.PostfixExpr {
			g.postfix_expr(node)
		}
		ast.PrefixExpr {
			g.code_gen.prefix_expr(node)
		}
		ast.StringLiteral {
			str := g.eval_str_lit_escape_codes(node)
			g.allocate_string(str, 3, .rel32)
		}
		ast.CharLiteral {
			bytes := g.eval_escape_codes(node.val)
				.bytes()
			mut val := rune(0)
			for i, v in bytes {
				val |= v << (i * 8)
				if i >= sizeof(rune) {
					g.n_error('runes are only 4 bytes wide')
				}
			}
			g.code_gen.movabs(g.code_gen.main_reg(), i64(val))
		}
		ast.StructInit {
			pos := g.allocate_by_type('_anonstruct', node.typ)
			g.code_gen.init_struct(LocalVar{ offset: pos, typ: node.typ }, node)
			g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), pos)
		}
		ast.GoExpr {
			g.v_error('native backend doesnt support threads yet', node.pos)
		}
		ast.MatchExpr {
			g.code_gen.gen_match_expr(node)
		}
		ast.SelectorExpr {
			g.code_gen.gen_selector_expr(node)
		}
		ast.CastExpr {
			g.code_gen.gen_cast_expr(node)
		}
		ast.EnumVal {
			type_name := g.table.get_type_name(node.typ)
			g.code_gen.mov(g.code_gen.main_reg(), g.enum_vals[type_name].fields[node.val])
		}
		ast.UnsafeExpr {
			g.expr(node.expr)
		}
		ast.ConcatExpr {
			g.code_gen.gen_concat_expr(node)
		}
		else {
			g.n_error('expr: unhandled node type: ${node.type_name()}')
		}
	}
}

fn (mut g Gen) condition(expr ast.Expr, neg bool) int {
	g.expr(expr)
	g.code_gen.cmp_zero(g.code_gen.main_reg())
	return g.code_gen.cjmp(if neg { .jne } else { .je })
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_comptime {
		if stmts := g.comptime_conditional(node) {
			g.stmts(stmts)
		}
		return
	}
	if node.branches.len == 0 {
		return
	}
	mut endif_label := 0
	has_endif := node.branches.len > 1
	if has_endif {
		endif_label = g.labels.new_label()
	}
	for idx in 0 .. node.branches.len {
		branch := node.branches[idx]
		if idx == node.branches.len - 1 && node.has_else {
			g.stmts(branch.stmts)
		} else {
			if branch.cond is ast.BoolLiteral {
				if branch.cond.val {
					g.stmts(branch.stmts)
				}
				continue
			}
			expr := branch.cond
			label := g.labels.new_label()
			cjmp_addr := g.condition(expr, false)
			g.labels.patches << LabelPatch{
				id: label
				pos: cjmp_addr
			}
			g.println('; jump to label ${label}')
			g.stmts(branch.stmts)
			if has_endif {
				jump_addr := g.code_gen.jmp(0)
				g.labels.patches << LabelPatch{
					id: endif_label
					pos: jump_addr
				}
				g.println('; jump to label ${endif_label}')
			}
			// println('after if g.pos=$g.pos() jneaddr=$cjmp_addr')
			g.labels.addrs[label] = g.pos()
			g.println('; label ${label}')
		}
	}
	if has_endif {
		g.labels.addrs[endif_label] = g.pos()
		g.println('; label ${endif_label}')
	}
}

fn (mut g Gen) postfix_expr(node ast.PostfixExpr) {
	if node.expr !is ast.Ident {
		return
	}
	ident := node.expr as ast.Ident
	match node.op {
		.inc {
			g.code_gen.inc_var(ident)
		}
		.dec {
			g.code_gen.dec_var(ident)
		}
		else {}
	}
}

fn (mut g Gen) gen_typeof_expr(it ast.TypeOf, newline bool) {
	nl := if newline { '\n' } else { '' }
	r := g.typ(it.typ).name
	g.code_gen.learel(g.code_gen.main_reg(), g.allocate_string('${r}${nl}', 3, .rel32))
}

fn (mut g Gen) gen_print_from_expr(expr ast.Expr, typ ast.Type, name string) {
	newline := name in ['println', 'eprintln']
	fd := if name in ['eprint', 'eprintln'] { 2 } else { 1 }
	match expr {
		ast.StringLiteral {
			str := g.eval_str_lit_escape_codes(expr)
			if newline {
				g.code_gen.gen_print(str + '\n', fd)
			} else {
				g.code_gen.gen_print(str, fd)
			}
		}
		ast.Nil {
			str := '0x0'
			if newline {
				g.code_gen.gen_print(str + '\n', fd)
			} else {
				g.code_gen.gen_print(str, fd)
			}
		}
		ast.CharLiteral {
			str := g.eval_escape_codes(expr.val)
			if newline {
				g.code_gen.gen_print(str + '\n', fd)
			} else {
				g.code_gen.gen_print(str, fd)
			}
		}
		ast.Ident {
			vo := g.try_var_offset(expr.name)

			reg := g.code_gen.main_reg()
			if vo != -1 {
				g.gen_var_to_string(reg, expr, expr as ast.Ident)
				g.code_gen.gen_print_reg(reg, -1, fd)
				if newline {
					g.code_gen.gen_print('\n', fd)
				}
			} else {
				g.code_gen.gen_print_reg(reg, -1, fd)
			}
		}
		ast.IntegerLiteral {
			if newline {
				g.code_gen.gen_print('${expr.val}\n', fd)
			} else {
				g.code_gen.gen_print('${expr.val}', fd)
			}
		}
		ast.BoolLiteral {
			// register 'true' and 'false' strings // g.expr(expr)
			// XXX mov64 shuoldnt be used for addressing
			nl := if newline { '\n' } else { '' }

			if expr.val {
				g.code_gen.gen_print('true' + nl, fd)
			} else {
				g.code_gen.gen_print('false' + nl, fd)
			}
		}
		ast.SizeOf {
			size := g.get_type_size(expr.typ)
			if newline {
				g.code_gen.gen_print('${size}\n', fd)
			} else {
				g.code_gen.gen_print('${size}', fd)
			}
		}
		ast.OffsetOf {
			styp := g.typ(expr.struct_type)
			field_name := expr.field
			if styp.kind == .struct_ {
				off := g.get_field_offset(expr.struct_type, field_name)
				if newline {
					g.code_gen.gen_print('${off}\n', fd)
				} else {
					g.code_gen.gen_print('${off}', fd)
				}
			} else {
				g.v_error('_offsetof expects a struct Type as first argument', expr.pos)
			}
		}
		ast.None {
			if newline {
				g.code_gen.gen_print('<none>\n', fd)
			} else {
				g.code_gen.gen_print('<none>', fd)
			}
		}
		ast.AtExpr {
			if newline {
				g.code_gen.gen_print(g.comptime_at(expr) + '\n', fd)
			} else {
				g.code_gen.gen_print(g.comptime_at(expr), fd)
			}
		}
		ast.StringInterLiteral {
			g.n_error('Interlaced string literals are not yet supported in the native backend.') // , expr.pos)
		}
		ast.IfExpr {
			if expr.is_comptime {
				if stmts := g.comptime_conditional(expr) {
					for i, stmt in stmts {
						if i + 1 == stmts.len && stmt is ast.ExprStmt {
							g.gen_print_from_expr(stmt.expr, stmt.typ, name)
						} else {
							g.stmt(stmt)
						}
					}
				} else {
					g.n_error('nothing to print')
				}
			} else {
				g.n_error('non-comptime conditionals are not implemented yet.')
			}
		}
		else {
			g.expr(expr)
			g.gen_to_string(g.code_gen.main_reg(), typ)
			g.code_gen.gen_print_reg(g.code_gen.main_reg(), -1, fd)
			if newline {
				g.code_gen.gen_print('\n', fd)
			}
		}
	}
}
