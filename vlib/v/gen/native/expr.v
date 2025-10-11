// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast
import v.util
import v.errors

fn (mut g Gen) expr(node ast.Expr) {
	match node {
		ast.AtExpr {
			g.allocate_string(g.comptime_at(node), 3, .rel32)
		}
		ast.ParExpr {
			g.expr(node.expr)
		}
		ast.ArrayInit {
			pos := g.allocate_array('_anonarray', 8, i32(node.exprs.len))
			g.cg.init_array(LocalVar{ offset: pos, typ: node.typ }, node)
			g.cg.cg_lea_var_to_reg(.reg0, pos)
		}
		ast.BoolLiteral {
			g.cg.cg_mov64(.reg0, i64(node.val))
		}
		ast.CallExpr {
			match node.name {
				'C.syscall' {
					g.cg.cg_gen_syscall(node)
				}
				'println', 'print', 'eprintln', 'eprint' {
					expr := node.args[0].expr
					typ := node.args[0].typ
					g.gen_print_from_expr(expr, typ, node.name)
				}
				else {
					g.cg.cg_call_fn(node)
				}
			}
		}
		ast.FloatLiteral {
			val := g.eval.expr(node, ast.float_literal_type_idx).float_val()
			g.cg.load_fp(val)
		}
		ast.Ident {
			var := g.get_var_from_ident(node)
			match var {
				LocalVar {
					g.local_var_ident(node, var)
				}
				ExternVar {
					g.extern_var_ident(var)
				}
				PreprocVar {
					g.preproc_var_ident(var)
				}
				GlobalVar {
					g.global_var_ident(node, var)
				}
				Register {
					g.n_error('${@LOCATION} Unsupported variable kind ${var}')
				}
				ConstVar {
					g.const_var_ident(node, var)
				}
			}
		}
		ast.IfExpr {
			g.if_expr(node)
		}
		ast.InfixExpr {
			g.cg.cg_infix_expr(node)
		}
		ast.IntegerLiteral {
			// Integer literal stores both signed and unsigned integers, some unsigned integers are too big for i64 but not for u64
			// println(node.val)
			if node.val.len > 0 && node.val[0] == `-` { // if the number is negative
				g.cg.cg_mov64(.reg0, node.val.i64())
			} else {
				g.cg.cg_mov64(.reg0, node.val.u64())
			}
		}
		ast.Nil {
			g.cg.cg_mov64(.reg0, i64(0))
		}
		ast.PostfixExpr {
			g.postfix_expr(node)
		}
		ast.PrefixExpr {
			g.cg.cg_prefix_expr(node)
		}
		ast.StringLiteral {
			str := g.eval_str_lit_escape_codes(node)
			pos := g.cg.cg_create_string_struct(ast.string_type_idx, 'str_lit', str)
			g.cg.cg_lea_var_to_reg(.reg0, pos)
		}
		ast.CharLiteral {
			bytes := g.eval_escape_codes(node.val)
				.bytes()
			mut val := rune(0)
			for i, v in bytes {
				val |= v << (i * 8)
				if i >= sizeof(rune) {
					g.n_error('${@LOCATION} runes are only 4 bytes wide')
				}
			}
			g.cg.cg_movabs(.reg0, i64(val))
		}
		ast.StructInit {
			pos := g.allocate_by_type('_anonstruct', node.typ)
			g.cg.cg_init_struct(LocalVar{ offset: pos, typ: node.typ }, node)
			g.cg.cg_lea_var_to_reg(.reg0, pos)
		}
		ast.GoExpr {
			g.v_error('native backend doesnt support threads yet', node.pos)
		}
		ast.MatchExpr {
			g.cg.gen_match_expr(node)
		}
		ast.SelectorExpr {
			g.gen_selector_expr(node)
		}
		ast.CastExpr {
			g.cg.gen_cast_expr(node)
		}
		ast.EnumVal {
			type_name := g.table.get_type_name(node.typ)
			val := g.enum_vals[type_name].fields[node.val] or {
				g.n_error('${@LOCATION} enum field not found ${node.val}')
			}
			match val {
				Number {
					g.cg.cg_mov64(.reg0, val)
				}
				ast.Expr {
					g.expr(val)
				}
			}
		}
		ast.UnsafeExpr {
			g.expr(node.expr)
		}
		ast.ConcatExpr {
			g.gen_concat_expr(node)
		}
		ast.TypeOf {
			g.gen_typeof_expr(node, false)
		}
		ast.SizeOf {
			g.gen_sizeof_expr(node)
		}
		ast.IndexExpr {
			g.cg.cg_gen_index_expr(node)
			if node.left_type.is_string() {
				g.cg.cg_mov_deref(Amd64Register.rax, Amd64Register.rax, ast.u8_type_idx)
			} else {
				g.cg.cg_mov_deref(Amd64Register.rax, Amd64Register.rax, node.typ)
			}
		}
		else {
			util.show_compiler_message('error', errors.CompilerMessage{
				message:   'detail'
				file_path: g.current_file.path
				pos:       node.pos()
			})
			g.n_error('${@LOCATION} expr: unhandled node type: ${node.type_name()} ${node}')
		}
	}
}

fn (mut g Gen) local_var_ident(ident ast.Ident, var LocalVar) {
	if g.is_register_type(var.typ) {
		g.cg.cg_mov_var_to_reg(.reg0, ident)
	} else if g.is_fp_type(var.typ) {
		g.cg.load_fp_var(ident)
	} else {
		ts := g.table.sym(g.unwrap(var.typ))
		match ts.info {
			ast.Struct {
				g.cg.cg_lea_var_to_reg(.reg0, g.get_var_offset(ident.name))
			}
			ast.Enum {
				g.cg.cg_mov_var_to_reg(.reg0, ident)
			}
			ast.Array {
				g.cg.cg_lea_var_to_reg(.reg0, g.get_var_offset(ident.name))
			}
			else {
				g.n_error('${@LOCATION} Unsupported variable type ${ts.info}')
			}
		}
	}
}

fn (mut g Gen) global_var_ident(ident ast.Ident, var GlobalVar) {
	if g.is_register_type(var.typ) {
		g.cg.cg_mov_var_to_reg(.reg0, ident)
	} else {
		g.n_error('${@LOCATION} Unsupported variable type ${ident} ${var}')
	}
}

fn (mut g Gen) const_var_ident(ident ast.Ident, var ConstVar) {
	if g.is_register_type(var.typ) {
		g.cg.cg_mov_var_to_reg(.reg0, ident)
	} else if g.is_fp_type(var.typ) {
		g.n_error('${@LOCATION} Unsupported variable type ${ident} ${var}')
	} else {
		ts := g.table.sym(g.unwrap(var.typ))
		match ts.info {
			ast.Struct {
				g.cg.cg_mov_var_to_reg(.reg0, ident)
			}
			else {
				g.n_error('${@LOCATION} Unsupported variable type ${ident} ${var} ${ts.info}')
			}
		}
	}
}

fn (mut g Gen) extern_var_ident(var ExternVar) {
	if g.pref.os == .linux {
		g.extern_vars[g.pos() + 2] = var.name // + 2 for the mov64 instruction
		g.cg.cg_mov64(.reg0, Number(i64(0)))
		g.cg.cg_mov_deref(.reg0, .reg0, ast.u64_type_idx)
	} else if g.pref.os == .macos {
		eprintln('## TODO, macos, extern_var_ident, var: ${var}')
	} else {
		g.n_error('${@LOCATION} unsupported os for ${var}')
	}
}

fn (mut g Gen) preproc_var_ident(var PreprocVar) {
	g.cg.cg_mov64(.reg0, var.val)
}

fn (mut g Gen) condition(expr ast.Expr, neg bool) i32 {
	g.println('; condition cjmp if ${neg}:')
	g.expr(expr)
	g.cg.cg_cmp_zero(.reg0) // 0 is false
	return g.cg.cg_cjmp(if neg { .jne } else { .je })
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_comptime {
		if branch := g.comptime_conditional(node) {
			g.stmts(branch.stmts)
		}
		return
	}
	if node.branches.len == 0 {
		return
	}
	mut endif_label := i32(0)
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
				id:  label
				pos: cjmp_addr
			}
			g.println('; jump to label ${label}')
			g.stmts(branch.stmts)
			if has_endif {
				jump_addr := g.cg.cg_jmp(0)
				g.labels.patches << LabelPatch{
					id:  endif_label
					pos: jump_addr
				}
				g.println('; jump to label ${int(endif_label)}')
			}
			// println('after if g.pos=$g.pos() jneaddr=$cjmp_addr')
			g.labels.addrs[label] = g.pos()
			g.println('; label ${label}')
		}
	}
	if has_endif {
		g.labels.addrs[int(endif_label)] = g.pos()
		g.println('; label ${int(endif_label)}')
	}
}

fn (mut g Gen) postfix_expr(node ast.PostfixExpr) {
	if node.expr !is ast.Ident {
		return
	}
	ident := node.expr as ast.Ident
	match node.op {
		.inc {
			g.cg.cg_inc_var(ident)
		}
		.dec {
			g.cg.cg_dec_var(ident)
		}
		else {}
	}
}

fn (mut g Gen) fn_decl_str(info ast.FnType) string {
	mut fn_str := 'fn ('
	for i, arg in info.func.params {
		if arg.is_mut {
			fn_str += 'mut '
		}
		if i > 0 {
			fn_str += ', '
		}
		if arg.typ.has_flag(.option) {
			fn_str += '?'
		}
		fn_str += util.strip_main_name(g.table.get_type_name(arg.typ))
	}
	fn_str += ')'
	if info.func.return_type == ast.ovoid_type {
		fn_str += ' ?'
	} else if info.func.return_type == ast.rvoid_type {
		fn_str += ' !'
	} else if info.func.return_type != ast.void_type {
		x := util.strip_main_name(g.table.get_type_name(info.func.return_type))
		if info.func.return_type.has_flag(.option) {
			fn_str += ' ?${x}'
		} else if info.func.return_type.has_flag(.result) {
			fn_str += ' !${x}'
		} else {
			fn_str += ' ${x}'
		}
	}
	return fn_str
}

fn (mut g Gen) gen_typeof_expr(node ast.TypeOf, newline bool) {
	nl := if newline { '\n' } else { '' }
	ts := g.table.sym(node.typ)
	mut str := ''

	match ts.kind {
		.sum_type {
			g.n_error('${@LOCATION} `typeof()` is not implemented for sum types yet')
		}
		.array_fixed {
			fixed_info := ts.info as ast.ArrayFixed
			typ_name := g.table.get_type_name(fixed_info.elem_type)
			str = '[${fixed_info.size}]${util.strip_main_name(typ_name)}'
		}
		.function {
			func_info := ts.info as ast.FnType
			if node.typ.is_ptr() {
				str = '&'
			}
			str += g.fn_decl_str(func_info)
		}
		else {
			str = util.strip_main_name(if node.typ.has_flag(.variadic) {
				g.table.sym(g.table.value_type(node.typ)).name
			} else {
				g.table.type_to_str(node.typ)
			})
		}
	}

	g.cg.cg_learel(.reg0, g.allocate_string('${str}${nl}', 3, .rel32))
}

fn (mut g Gen) gen_sizeof_expr(node ast.SizeOf) {
	ts := g.table.sym(node.typ)
	if ts.language == .v && ts.kind in [.placeholder, .any] {
		g.v_error('unknown type `${ts.name}`', node.pos)
	}
	g.cg.cg_mov64(.reg0, i64(g.get_type_size(node.typ)))
}

fn (mut g Gen) gen_print_from_expr(expr ast.Expr, typ ast.Type, name string) {
	newline := name in ['println', 'eprintln']
	fd := if name in ['eprint', 'eprintln'] { i32(2) } else { i32(1) }
	match expr {
		ast.StringLiteral {
			str := g.eval_str_lit_escape_codes(expr)
			if newline {
				g.cg.cg_gen_print(str + '\n', fd)
			} else {
				g.cg.cg_gen_print(str, fd)
			}
		}
		ast.Nil {
			str := '0x0'
			if newline {
				g.cg.cg_gen_print(str + '\n', fd)
			} else {
				g.cg.cg_gen_print(str, fd)
			}
		}
		ast.CharLiteral {
			str := g.eval_escape_codes(expr.val)
			if newline {
				g.cg.cg_gen_print(str + '\n', fd)
			} else {
				g.cg.cg_gen_print(str, fd)
			}
		}
		ast.Ident {
			vo := g.try_var_offset(expr.name)

			if vo != -1 {
				g.gen_var_to_string(.reg0, expr, expr as ast.Ident)
				g.cg.cg_gen_print_reg(.reg0, -1, fd)
				if newline {
					g.cg.cg_gen_print('\n', fd)
				}
			} else {
				g.cg.cg_gen_print_reg(.reg0, -1, fd)
			}
		}
		ast.IntegerLiteral {
			if newline {
				g.cg.cg_gen_print('${expr.val}\n', fd)
			} else {
				g.cg.cg_gen_print('${expr.val}', fd)
			}
		}
		ast.BoolLiteral {
			// register 'true' and 'false' strings // g.expr(expr)
			// XXX mov64 shouldn't be used for addressing
			nl := if newline { '\n' } else { '' }

			if expr.val {
				g.cg.cg_gen_print('true' + nl, fd)
			} else {
				g.cg.cg_gen_print('false' + nl, fd)
			}
		}
		ast.SizeOf {
			size := g.get_type_size(expr.typ)
			if newline {
				g.cg.cg_gen_print('${size}\n', fd)
			} else {
				g.cg.cg_gen_print('${size}', fd)
			}
		}
		ast.OffsetOf {
			styp := g.styp(expr.struct_type)
			field_name := expr.field
			if styp.kind == .struct {
				off := g.get_field_offset(expr.struct_type, field_name)
				if newline {
					g.cg.cg_gen_print('${off}\n', fd)
				} else {
					g.cg.cg_gen_print('${off}', fd)
				}
			} else {
				g.v_error('_offsetof expects a struct Type as first argument', expr.pos)
			}
		}
		ast.None {
			if newline {
				g.cg.cg_gen_print('<none>\n', fd)
			} else {
				g.cg.cg_gen_print('<none>', fd)
			}
		}
		ast.AtExpr {
			if newline {
				g.cg.cg_gen_print(g.comptime_at(expr) + '\n', fd)
			} else {
				g.cg.cg_gen_print(g.comptime_at(expr), fd)
			}
		}
		ast.StringInterLiteral {
			printer := if fd == 1 { 'print' } else { 'eprint' }
			for i, val in expr.vals {
				g.cg.cg_gen_print(val, fd)
				if i < expr.exprs.len {
					g.cg_gen_print_from_expr(expr.exprs[i], expr.expr_types[i], printer)
				}
			}

			if newline {
				g.cg.cg_gen_print('\n', fd)
			}
		}
		ast.IfExpr {
			if expr.is_comptime {
				if branch := g.comptime_conditional(expr) {
					for i, stmt in branch.stmts {
						if i + 1 == branch.stmts.len && stmt is ast.ExprStmt {
							g.cg_gen_print_from_expr(stmt.expr, stmt.typ, name)
						} else {
							g.stmt(stmt)
						}
					}
				} else {
					g.n_error('${@LOCATION} nothing to print')
				}
			} else {
				g.n_error('${@LOCATION} non-comptime conditionals are not implemented yet.')
			}
		}
		else {
			g.expr(expr)
			g.gen_to_string(.reg0, typ)
			g.cg.cg_gen_print_reg(.reg0, -1, fd)
			if newline {
				g.cg.cg_gen_print('\n', fd)
			}
		}
	}
}

fn (mut g Gen) gen_selector_expr(expr ast.SelectorExpr) {
	g.println('; .${expr.field_name} {')
	g.expr(expr.expr)
	offset := g.get_field_offset(expr.expr_type, expr.field_name)
	if offset != 0 {
		g.cg.cg_add(.reg0, offset)
	}
	if expr.next_token != .dot { // the deref needs to be on the last selector (that has no . after it)
		ts := g.table.sym(expr.typ)
		if ts.info !is ast.Struct {
			g.cg.cg_mov_deref(.reg0, .reg0, expr.typ)
		}
	}
	g.println('; .${expr.field_name} }')
}

fn (mut g Gen) gen_left_value(node ast.Expr) {
	match node {
		ast.Ident {
			offset := g.get_var_offset(node.name)
			g.cg.cg_lea_var_to_reg(Amd64Register.rax, offset)
		}
		ast.SelectorExpr {
			g.expr(node.expr)
			offset := g.get_field_offset(node.expr_type, node.field_name)
			if offset != 0 {
				g.cg.cg_add(Amd64Register.rax, offset)
			}
		}
		ast.StructInit {
			g.expr(node)
		}
		ast.ArrayInit {
			g.expr(node) // TODO: add a test that uses this
		}
		ast.IndexExpr {
			g.cg.cg_gen_index_expr(node)
		}
		ast.PrefixExpr {
			if node.op != .mul {
				g.n_error('${@LOCATION} Unsupported left value')
			}
			g.expr(node.right)
		}
		else {
			g.n_error('${@LOCATION} Unsupported left value')
		}
	}
}
