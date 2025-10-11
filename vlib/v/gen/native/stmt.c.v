// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast
import v.util
import v.errors

fn C.strtol(str &char, endptr &&char, base i32) i32

pub fn (mut g Gen) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.stmt(stmt)
	}
}

fn (mut g Gen) stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			g.assign_stmt(node)
		}
		ast.Block {
			g.stmts(node.stmts)
		}
		ast.BranchStmt {
			label_name := node.label
			// break / continue statements in for loops
			for i := g.labels.branches.len - 1; i >= 0; i-- {
				branch := g.labels.branches[i]
				if label_name == '' || label_name == branch.name {
					label := if node.kind == .key_break {
						branch.end
					} else { // continue
						branch.start
					}
					jump_addr := g.cg.cg_jmp(0)
					g.labels.patches << LabelPatch{
						id:  label
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
			g.cg.cg_mov_int_to_var(LocalVar{defer_var, ast.i64_type_idx, name}, 1)
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
				'include' {}
				'preinclude', 'postinclude', 'define', 'insert' {
					util.show_compiler_message('notice', errors.CompilerMessage{
						message:   '#${node.kind} is not supported with the native backend'
						file_path: node.source_file
						pos:       node.pos
					})
				}
				'flag' {
					// do nothing; flags are already handled when dispatching extern dependencies
				}
				else {
					g.gen_native_hash_stmt(node)
				}
			}
		}
		ast.Module {
			g.is_builtin_mod = util.module_is_builtin(node.name)
		}
		ast.Return {
			g.cg.cg_return_stmt(node)
		}
		ast.AsmStmt {
			g.cg.cg_gen_asm_stmt(node)
		}
		ast.AssertStmt {
			g.gen_assert(node)
		}
		ast.GlobalDecl {
			if g.pref.os == .linux {
				// handled in elf generator
			} else if !g.is_builtin_mod && !g.pref.experimental {
				g.warning('global variables are not supported', node.pos)
			}
		}
		ast.Import {} // do nothing here
		ast.StructDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		ast.InterfaceDecl {}
		else {
			g.n_error('${@LOCATION} bad node: ' + node.type_name())
		}
	}
}

fn (mut g Gen) assign_stmt(node ast.AssignStmt) {
	g.cg.cg_assign_stmt(node)
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
						match cond.right {
							ast.IntegerLiteral {
								lit := cond.right as ast.IntegerLiteral
								g.cg.cg_cmp_var(cond.left as ast.Ident, i32(lit.val.int()))
							}
							else {
								g.expr(cond.right)
								g.cg.cg_cmp_var_reg(cond.left as ast.Ident, .reg0)
							}
						}
						match cond.op {
							.gt {
								jump_addr = g.cg.cg_cjmp(.jle)
							}
							.ge {
								jump_addr = g.cg.cg_cjmp(.jl)
							}
							.lt {
								jump_addr = g.cg.cg_cjmp(.jge)
							}
							.le {
								jump_addr = g.cg.cg_cjmp(.jg)
							}
							else {
								g.n_error('${@LOCATION} unsupported conditional in for-c loop')
							}
						}
					}
					else {
						g.n_error('${@LOCATION} unhandled infix.left')
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
		id:  end_label
		pos: jump_addr
	}
	g.println('; jump to label ${end_label}')
	g.labels.branches << BranchLabel{
		name:  node.label
		start: start_label
		end:   end_label
	}
	g.stmts(node.stmts)
	g.labels.addrs[start_label] = g.pos()
	g.println('; label ${start_label}')
	if node.has_inc {
		g.stmts([node.inc])
	}
	g.labels.branches.pop()
	g.cg.cg_jmp_back(start)
	g.labels.addrs[end_label] = g.pos()
	g.println('; jump to label ${end_label}')

	// loop back
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	if node.is_inf {
		if node.stmts.len == 0 {
			g.cg.infloop()
			return
		}
		// infinite loop
		start := g.pos()
		start_label := g.labels.new_label()
		g.labels.addrs[start_label] = start
		g.println('; label ${start_label}')
		end_label := g.labels.new_label()
		g.labels.branches << BranchLabel{
			name:  node.label
			start: start_label
			end:   end_label
		}
		g.stmts(node.stmts)
		g.labels.branches.pop()
		g.cg.cg_jmp_back(start)
		g.println('jmp after infinite for')
		g.labels.addrs[end_label] = g.pos()
		g.println('; label ${end_label}')
		return
	}
	g.println('; for stmt {')

	start := g.pos()
	start_label := g.labels.new_label()
	g.labels.addrs[start_label] = start
	g.println('; label ${start_label} (start of for loop)')

	// Condition
	mut cjmp_addr := g.condition(node.cond, false) // jmp if false, location of `jne *00 00 00 00*` (to be patched by LabelPatch)
	end_label := g.labels.new_label()
	g.labels.patches << LabelPatch{
		id:  end_label
		pos: cjmp_addr
	}
	g.println('; cjmp to label ${end_label} (out of loop)')

	// Body of the loop
	g.labels.branches << BranchLabel{
		name:  node.label
		start: start_label
		end:   end_label
	}
	g.stmts(node.stmts)
	g.labels.branches.pop()
	// Go back to `cmp ...`
	g.cg.cg_jmp_back(start)

	g.println('; for stmt }')
	// Set the jump (out of the loop) addr to current pos
	g.labels.addrs[end_label] = g.pos()
	g.println('; label ${end_label} (out of for loop)')
}

fn (mut g Gen) for_in_stmt(node ast.ForInStmt) { // Work on that
	if node.is_range {
		g.println('; for ${node.val_var} in range {')
		// for a in node.cond .. node.high {

		i := g.cg.cg_allocate_var(node.val_var, 8, i64(0)) // iterator variable
		g.println('; evaluate node.cond for lower bound:')
		g.expr(node.cond) // outputs the lower loop bound (initial value) to the main reg
		g.println('; move the result to i')
		g.cg.cg_mov_reg_to_var(LocalVar{i, ast.i64_type_idx, node.val_var}, .reg0) // i = node.cond // initial value

		start := g.pos() // label-begin:

		g.println('; check iterator against upper loop bound')
		g.cg.cg_mov_var_to_reg(.reg0, LocalVar{i, ast.i64_type_idx, node.val_var})
		g.cg.cg_push(.reg0) // put the iterator on the stack
		g.expr(node.high) // final value (upper bound) to the main reg
		g.cg.cg_cmp_to_stack_top(.reg0)
		jump_addr := g.cg.cg_cjmp(.jge) // leave loop i >= upper bound
		end_label := g.labels.new_label()
		g.labels.patches << LabelPatch{
			id:  end_label
			pos: jump_addr
		}
		g.println('; jump to label ${end_label} (end_label)')

		start_label := g.labels.new_label() // used for continue
		g.labels.branches << BranchLabel{
			name:  node.label
			start: start_label
			end:   end_label
		}
		g.stmts(node.stmts) // writes the actual body of the loop

		g.labels.addrs[start_label] = g.pos() // used for continue (continue: jump before the inc)
		g.println('; label ${start_label} (continue_label)')

		g.cg.cg_inc_var(LocalVar{i, ast.i64_type_idx, node.val_var})
		g.labels.branches.pop()
		g.cg.cg_jmp_back(start) // loops

		g.labels.addrs[end_label] = g.pos()
		g.println('; label ${end_label} (end_label)')
		g.println('; for ${node.val_var} in range }')
	} else if node.kind == .string {
		g.println('; for ${node.val_var} in string {')
		// for c in my_string {

		key_var := if node.key_var == '' { 'i' } else { node.key_var }
		i := g.cg.cg_allocate_var(key_var, 8, i64(0)) // iterator variable
		c := g.cg.cg_allocate_var(node.val_var, 1, i64(0)) // char variable

		g.expr(node.cond) // get the address of the string variable
		g.cg.cg_mov_deref(.reg2, .reg0, ast.charptr_type)
		g.println('; push address of the string chars')
		g.cg.cg_push(.reg2) // address of the string
		g.cg.cg_add(.reg0, g.get_field_offset(ast.string_type, 'len'))
		g.println('; push address of the len:')
		g.cg.cg_push(.reg0) // address of the len

		start := g.pos() // label-begin:

		g.println('; check iterator against upper loop bound')
		g.cg.cg_mov_var_to_reg(.reg0, LocalVar{i, ast.i64_type_idx, key_var})
		g.println('; pop address of the len:')
		g.cg.cg_pop(.reg2)
		g.println('; push address of the len:')
		g.cg.cg_push(.reg2) // len
		g.cg.cg_mov_deref(.reg2, .reg2, ast.int_type)
		g.cg.cg_cmp_reg(.reg0, .reg2)
		jump_addr := g.cg.cg_cjmp(.jge) // leave loop i >= len

		g.println('; pop address of the len:')
		g.cg.cg_pop(.reg2) // len
		g.println('; pop address of the string chars')
		g.cg.cg_pop(.reg0) // address of the string
		g.println('; push address of the string chars')
		g.cg.cg_push(.reg0)
		g.println('; push address of the len:')
		g.cg.cg_push(.reg2) // len

		g.cg.cg_mov_var_to_reg(.reg2, LocalVar{i, ast.i64_type_idx, key_var})
		g.cg.cg_add_reg(.reg0, .reg2)
		g.cg.cg_mov_deref(.reg0, .reg0, ast.u8_type_idx)
		g.cg.cg_mov_reg_to_var(LocalVar{c, ast.u8_type_idx, node.val_var}, .reg0) // store the char

		end_label := g.labels.new_label()
		g.labels.patches << LabelPatch{
			id:  end_label
			pos: jump_addr
		}
		g.println('; jump to label ${end_label} (end_label)')

		start_label := g.labels.new_label() // used for continue
		g.labels.branches << BranchLabel{
			name:  node.label
			start: start_label
			end:   end_label
		}
		g.stmts(node.stmts) // writes the actual body of the loop

		g.labels.addrs[start_label] = g.pos() // used for continue (continue: jump before the inc)
		g.println('; label ${start_label} (continue_label)')

		g.cg.cg_inc_var(LocalVar{i, ast.i64_type_idx, key_var})
		g.labels.branches.pop()
		g.cg.cg_jmp_back(start) // loops

		g.labels.addrs[end_label] = g.pos()
		g.cg.cg_pop(.reg2) // len
		g.cg.cg_pop(.reg0) // address of the string
		g.println('; label ${end_label} (end_label)')
		g.println('; for ${node.val_var} in string }')
		/*
	} else if node.kind == .array {
	} else if node.kind == .array_fixed {
	} else if node.kind == .map {
	} else if node.kind == .struct {
	} else if it.kind in [.array, .string] || it.cond_type.has_flag(.variadic) {
	} else if it.kind == .map {
	*/
	} else {
		g.n_error('${@LOCATION} for-in ${node.kind} statement is not yet implemented')
	}
}

fn (mut g Gen) gen_assert(assert_node ast.AssertStmt) {
	g.println('; gen_assert {')
	mut cjmp_addr := i32(0)
	ane := assert_node.expr
	label := g.labels.new_label()
	cjmp_addr = g.condition(ane, true)
	g.labels.patches << LabelPatch{
		id:  label
		pos: cjmp_addr
	}
	g.println('; jump to label ${label}')
	g.cg.cg_trap()
	g.labels.addrs[label] = g.pos()
	g.println('; label ${label}')
	g.println('; gen_assert }')
}

fn (mut g Gen) gen_flag_hash_stmt(node ast.HashStmt) {
	if node.main.contains('-l') {
		g.linker_libs << node.main.all_after('-l').trim_space()
	} else if node.main.contains('-L') {
		g.linker_include_paths << node.main.all_after('-L').trim_space()
	} else if node.main.contains('-D') || node.main.contains('-I') {
		if g.pref.os == .linux && node.main.starts_with('darwin ') {
			// TODO: skip all mismatching flags. Extract the logic from cgen in a common method on HashStmt, and call it both here and in cgen.
			return
		}
		// g.v_error('`-D` and `-I` flags are not supported with the native backend', node.pos)
		println(util.formatted_error('warn', '`-D` and `-I` flags are not supported with the native backend',
			g.current_file.path, node.pos))
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
