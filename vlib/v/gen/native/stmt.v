// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast

fn C.strtol(str &char, endptr &&char, base int) int

pub fn (mut g Gen) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.stmt(stmt)
	}
}

fn (mut g Gen) stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			g.code_gen.assign_stmt(node)
		}
		ast.Block {
			g.stmts(node.stmts)
		}
		ast.BranchStmt {
			label_name := node.label
			for i := g.labels.branches.len - 1; i >= 0; i-- {
				branch := g.labels.branches[i]
				if label_name == '' || label_name == branch.name {
					label := if node.kind == .key_break {
						branch.end
					} else { // continue
						branch.start
					}
					jump_addr := g.code_gen.jmp(0)
					g.labels.patches << LabelPatch{
						id: label
						pos: jump_addr
					}
					g.println('; jump to ${label}: ${node.kind}')
					break
				}
			}
		}
		ast.ConstDecl {}
		ast.DeferStmt {
			name := '_defer${g.defer_stmts.len}'
			defer_var := g.get_var_offset(name)
			g.code_gen.mov_int_to_var(LocalVar{defer_var, ast.i64_type_idx, name}, 1)
			g.defer_stmts << node
			g.defer_stmts[g.defer_stmts.len - 1].idx_in_fn = g.defer_stmts.len - 1
		}
		ast.ExprStmt {
			g.expr(node.expr)
		}
		ast.FnDecl {
			g.fn_decl(node)
		}
		ast.ForCStmt {
			g.gen_forc_stmt(node)
		}
		ast.ForInStmt {
			if node.stmts.len == 0 {
				// if no statements, just dont make it
				return
			}
			g.code_gen.for_in_stmt(node)
		}
		ast.ForStmt {
			g.for_stmt(node)
		}
		ast.HashStmt {
			words := node.val.split(' ')
			for word in words {
				if word.len != 2 {
					g.n_error('opcodes format: xx xx xx xx\nhash statements are not allowed with the native backend, use the C backend for extended C interoperability.')
				}
				b := unsafe { C.strtol(&char(word.str), 0, 16) }
				// b := word.u8()
				// println('"$word" $b')
				g.write8(b)
			}
		}
		ast.Module {}
		ast.Return {
			g.code_gen.return_stmt(node)
		}
		ast.AsmStmt {
			g.code_gen.gen_asm_stmt(node)
		}
		ast.AssertStmt {
			g.code_gen.gen_assert(node)
		}
		ast.Import {} // do nothing here
		ast.StructDecl {}
		ast.EnumDecl {}
		else {
			eprintln('native.stmt(): bad node: ' + node.type_name())
		}
	}
}

fn (mut g Gen) gen_forc_stmt(node ast.ForCStmt) {
	if node.has_init {
		g.stmts([node.init])
	}
	start := g.pos()
	start_label := g.labels.new_label()
	mut jump_addr := i64(0)
	if node.has_cond {
		cond := node.cond
		match cond {
			ast.InfixExpr {
				match cond.left {
					ast.Ident {
						lit := cond.right as ast.IntegerLiteral
						g.code_gen.cmp_var(cond.left as ast.Ident, lit.val.int())
						match cond.op {
							.gt {
								jump_addr = g.code_gen.cjmp(.jle)
							}
							.lt {
								jump_addr = g.code_gen.cjmp(.jge)
							}
							else {
								g.n_error('unsupported conditional in for-c loop')
							}
						}
					}
					else {
						g.n_error('unhandled infix.left')
					}
				}
			}
			else {}
		}
		// dump(node.cond)
		g.expr(node.cond)
	}
	end_label := g.labels.new_label()
	g.labels.patches << LabelPatch{
		id: end_label
		pos: int(jump_addr)
	}
	g.println('; jump to label ${end_label}')
	g.labels.branches << BranchLabel{
		name: node.label
		start: start_label
		end: end_label
	}
	g.stmts(node.stmts)
	g.labels.addrs[start_label] = g.pos()
	g.println('; label ${start_label}')
	if node.has_inc {
		g.stmts([node.inc])
	}
	g.labels.branches.pop()
	g.code_gen.jmp_back(start)
	g.labels.addrs[end_label] = g.pos()
	g.println('; jump to label ${end_label}')

	// loop back
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	if node.is_inf {
		if node.stmts.len == 0 {
			g.code_gen.infloop()
			return
		}
		// infinite loop
		start := g.pos()
		start_label := g.labels.new_label()
		g.labels.addrs[start_label] = start
		g.println('; label ${start_label}')
		end_label := g.labels.new_label()
		g.labels.branches << BranchLabel{
			name: node.label
			start: start_label
			end: end_label
		}
		g.stmts(node.stmts)
		g.labels.branches.pop()
		g.code_gen.jmp_back(start)
		g.println('jmp after infinite for')
		g.labels.addrs[end_label] = g.pos()
		g.println('; label ${end_label}')
		return
	}
	infix_expr := node.cond as ast.InfixExpr
	mut jump_addr := 0 // location of `jne *00 00 00 00*`
	start := g.pos()
	start_label := g.labels.new_label()
	g.labels.addrs[start_label] = start
	g.println('; label ${start_label}')
	match infix_expr.left {
		ast.Ident {
			match infix_expr.right {
				ast.Ident {
					reg := g.code_gen.main_reg()
					g.code_gen.mov_var_to_reg(reg, infix_expr.right as ast.Ident)
					g.code_gen.cmp_var_reg(infix_expr.left as ast.Ident, reg)
				}
				ast.IntegerLiteral {
					lit := infix_expr.right as ast.IntegerLiteral
					g.code_gen.cmp_var(infix_expr.left as ast.Ident, lit.val.int())
				}
				else {
					g.n_error('unhandled expression type')
				}
			}
			match infix_expr.left.tok_kind {
				.lt {
					jump_addr = g.code_gen.cjmp(.jge)
				}
				.gt {
					jump_addr = g.code_gen.cjmp(.jle)
				}
				.le {
					jump_addr = g.code_gen.cjmp(.jg)
				}
				.ge {
					jump_addr = g.code_gen.cjmp(.jl)
				}
				.ne {
					jump_addr = g.code_gen.cjmp(.je)
				}
				.eq {
					jump_addr = g.code_gen.cjmp(.jne)
				}
				else {
					g.n_error('unhandled infix cond token')
				}
			}
		}
		else {
			g.n_error('unhandled infix.left')
		}
	}
	end_label := g.labels.new_label()
	g.labels.patches << LabelPatch{
		id: end_label
		pos: jump_addr
	}
	g.println('; jump to label ${end_label}')
	g.labels.branches << BranchLabel{
		name: node.label
		start: start_label
		end: end_label
	}
	g.stmts(node.stmts)
	g.labels.branches.pop()
	// Go back to `cmp ...`
	g.code_gen.jmp_back(start)
	// Update the jump addr to current pos
	g.labels.addrs[end_label] = g.pos()
	g.println('; label ${end_label}')
	g.println('jmp after for')
}
