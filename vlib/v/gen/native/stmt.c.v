// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast

fn C.strtol(str &char, endptr &&char, base i32) i32

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
			g.for_in_stmt(node)
		}
		ast.ForStmt {
			g.for_stmt(node)
		}
		ast.HashStmt {
			if !g.should_emit_hash_stmt(node) {
				return
			}

			match node.kind {
				'include', 'preinclude', 'define', 'insert' {
					g.v_error('#${node.kind} is not supported with the native backend',
						node.pos)
				}
				'flag' {
					// do nothing; flags are already handled when dispatching extern dependencies
				}
				else {
					g.gen_native_hash_stmt(node)
				}
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
			g.gen_assert(node)
		}
		ast.GlobalDecl {
			if !g.pref.experimental {
				g.warning('globals are not supported yet', node.pos)
			}
		}
		ast.Import {} // do nothing here
		ast.StructDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		ast.InterfaceDecl {}
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
	mut jump_addr := i32(0)
	if node.has_cond {
		cond := node.cond
		match cond {
			ast.InfixExpr {
				match cond.left {
					ast.Ident {
						lit := cond.right as ast.IntegerLiteral
						g.code_gen.cmp_var(cond.left as ast.Ident, i32(lit.val.int()))
						match cond.op {
							.gt {
								jump_addr = g.code_gen.cjmp(.jle)
							}
							.ge {
								jump_addr = g.code_gen.cjmp(.jl)
							}
							.lt {
								jump_addr = g.code_gen.cjmp(.jge)
							}
							.le {
								jump_addr = g.code_gen.cjmp(.jg)
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
		pos: jump_addr
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
	mut jump_addr := i32(0) // location of `jne *00 00 00 00*`
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
					g.code_gen.cmp_var(infix_expr.left as ast.Ident, i32(lit.val.int()))
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

fn (mut g Gen) for_in_stmt(node ast.ForInStmt) { // Work on that
	if node.is_range {
		// for a in node.cond .. node.high {
		i := g.code_gen.allocate_var(node.val_var, 8, 0) // iterator variable
		g.expr(node.cond) // outputs the lower loop bound (initial value) to the main reg
		main_reg := g.code_gen.main_reg()
		g.code_gen.mov_reg_to_var(LocalVar{i, ast.i64_type_idx, node.val_var}, main_reg) // i = node.cond // initial value

		start := g.pos() // label-begin:
		start_label := g.labels.new_label()
		g.code_gen.mov_var_to_reg(main_reg, LocalVar{i, ast.i64_type_idx, node.val_var})
		g.code_gen.push(main_reg) // put the iterator on the stack
		g.expr(node.high) // final value (upper bound) to the main reg
		g.code_gen.cmp_to_stack_top(main_reg)
		jump_addr := g.code_gen.cjmp(.jge) // leave loop i >= upper bound

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
		g.stmts(node.stmts) // writes the actual body of the loop
		g.labels.addrs[start_label] = g.pos()
		g.println('; label ${start_label}')
		g.code_gen.inc_var(LocalVar{i, ast.i64_type_idx, node.val_var})
		g.labels.branches.pop()
		g.code_gen.jmp_back(start) // loops
		g.labels.addrs[end_label] = g.pos()
		g.println('; label ${end_label}')
		/*
		} else if node.kind == .array {
	} else if node.kind == .array_fixed {
	} else if node.kind == .map {
	} else if node.kind == .string {
	} else if node.kind == .struct_ {
	} else if it.kind in [.array, .string] || it.cond_type.has_flag(.variadic) {
	} else if it.kind == .map {
		*/
	} else {
		g.v_error('for-in statement is not yet implemented', node.pos)
	}
}

fn (mut g Gen) gen_assert(assert_node ast.AssertStmt) {
	mut cjmp_addr := i32(0)
	ane := assert_node.expr
	label := g.labels.new_label()
	cjmp_addr = g.condition(ane, true)
	g.labels.patches << LabelPatch{
		id: label
		pos: cjmp_addr
	}
	g.println('; jump to label ${label}')
	g.expr(assert_node.expr)
	g.code_gen.trap()
	g.labels.addrs[label] = g.pos()
	g.println('; label ${label}')
}

fn (mut g Gen) gen_flag_hash_stmt(node ast.HashStmt) {
	if node.main.contains('-l') {
		g.linker_libs << node.main.all_after('-l').trim_space()
	} else if node.main.contains('-L') {
		g.linker_include_paths << node.main.all_after('-L').trim_space()
	} else if node.main.contains('-D') || node.main.contains('-I') {
		g.v_error('`-D` and `-I` flags are not supported with the native backend', node.pos)
	} else {
		g.v_error('unknown `#flag` format: `${node.main}`', node.pos)
	}
}

fn (mut g Gen) gen_native_hash_stmt(node ast.HashStmt) {
	words := node.val.split(' ')
	mut unsupported := false
	for word in words {
		if word.len != 2 {
			unsupported = true
			break
		}
		b := unsafe { C.strtol(&char(word.str), 0, 16) }
		// b := word.u8()
		// println('"$word" $b')
		g.write8(b)
	}

	if unsupported {
		if !g.pref.experimental {
			g.warning('opcodes format: xx xx xx xx\nhash statements are not allowed with the native backend, use the C backend for extended C interoperability.',
				node.pos)
		}
	}
}
