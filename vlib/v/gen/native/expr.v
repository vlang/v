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
			g.code_gen.init_array(LocalVar{ offset: pos, typ: node.typ }, node)
			g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), pos)
		}
		ast.BoolLiteral {
			g.code_gen.mov64(g.code_gen.main_reg(), i64(node.val))
		}
		ast.CallExpr {
			match node.name {
				'C.syscall' {
					g.code_gen.gen_syscall(node)
				}
				'println', 'print', 'eprintln', 'eprint' {
					expr := node.args[0].expr
					typ := node.args[0].typ
					g.gen_print_from_expr(expr, typ, node.name)
				}
				else {
					g.code_gen.call_fn(node)
				}
			}
		}
		ast.FloatLiteral {
			val := g.eval.expr(node, ast.float_literal_type_idx).float_val()
			g.code_gen.load_fp(val)
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
				else {
					g.n_error('${@LOCATION} Unsupported variable kind')
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
			// Integer literal stores both signed and unsigned integers, some unsigned integers are too big for i64 but not for u64
			// println(node.val)
			if node.val.len > 0 && node.val[0] == `-` { // if the number is negative
				g.code_gen.mov64(g.code_gen.main_reg(), node.val.i64())
			} else {
				g.code_gen.mov64(g.code_gen.main_reg(), node.val.u64())
			}
		}
		ast.Nil {
			g.code_gen.mov64(g.code_gen.main_reg(), i64(0))
		}
		ast.PostfixExpr {
			g.postfix_expr(node)
		}
		ast.PrefixExpr {
			g.code_gen.prefix_expr(node)
		}
		ast.StringLiteral {
			str := g.eval_str_lit_escape_codes(node)
			pos := g.code_gen.create_string_struct(ast.string_type_idx, 'str_lit', str)
			g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), pos)
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
			g.gen_selector_expr(node)
		}
		ast.CastExpr {
			g.code_gen.gen_cast_expr(node)
		}
		ast.EnumVal {
			type_name := g.table.get_type_name(node.typ)
			val := g.enum_vals[type_name].fields[node.val] or {
				g.n_error('${@LOCATION} enum field not found ${node.val}')
			}
			match val {
				Number {
					g.code_gen.mov64(g.code_gen.main_reg(), val)
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
			if node.left_type.is_string() {
				g.expr(node.index)
				g.code_gen.push(Amd64Register.rax)

				g.expr(node.left) // load address of string struct
				g.code_gen.mov_deref(Amd64Register.rax, Amd64Register.rax, ast.u64_type_idx) // load value of the str pointer

				g.code_gen.pop2(Amd64Register.rdx) // index
				g.code_gen.add_reg2(Amd64Register.rax, Amd64Register.rdx) // add the offset to the address
				g.code_gen.mov_deref(Amd64Register.rax, Amd64Register.rax, ast.u8_type_idx)
			} else if node.left_type.is_pointer() {
				dump(node)
				g.n_error('${@LOCATION} expr: unhandled node type: Index expr is not applied on string')
			} else {
				g.n_error('${@LOCATION} expr: unhandled node type: Index expr is not applied on string')
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
		g.code_gen.mov_var_to_reg(g.code_gen.main_reg(), ident)
	} else if g.is_fp_type(var.typ) {
		g.code_gen.load_fp_var(ident)
	} else {
		ts := g.table.sym(g.unwrap(var.typ))
		match ts.info {
			ast.Struct {
				g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), g.get_var_offset(ident.name))
			}
			ast.Enum {
				g.code_gen.mov_var_to_reg(g.code_gen.main_reg(), ident)
			}
			else {
				g.n_error('${@LOCATION} Unsupported variable type')
			}
		}
	}
}

fn (mut g Gen) extern_var_ident(var ExternVar) {
	if g.pref.os == .linux {
		main_reg := g.code_gen.main_reg()
		g.extern_vars[g.pos()] = var.name
		g.code_gen.mov64(main_reg, Number(i64(0)))
		g.code_gen.mov_deref(main_reg, main_reg, ast.u64_type_idx)
	} else if g.pref.os == .macos {
		eprintln('## TODO, macos, extern_var_ident, var: ${var}')
	} else {
		g.n_error('${@LOCATION} unsupported os for ${var}')
	}
}

fn (mut g Gen) preproc_var_ident(var PreprocVar) {
	main_reg := g.code_gen.main_reg()
	g.code_gen.mov64(main_reg, var.val)
}

fn (mut g Gen) condition(expr ast.Expr, neg bool) i32 {
	g.println('; condition cjmp if ${neg}:')
	g.expr(expr)
	g.code_gen.cmp_zero(g.code_gen.main_reg()) // 0 is false
	return g.code_gen.cjmp(if neg { .jne } else { .je })
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
				jump_addr := g.code_gen.jmp(0)
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
			g.code_gen.inc_var(ident)
		}
		.dec {
			g.code_gen.dec_var(ident)
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

	g.code_gen.learel(g.code_gen.main_reg(), g.allocate_string('${str}${nl}', 3, .rel32))
}

fn (mut g Gen) gen_sizeof_expr(node ast.SizeOf) {
	ts := g.table.sym(node.typ)
	if ts.language == .v && ts.kind in [.placeholder, .any] {
		g.v_error('unknown type `${ts.name}`', node.pos)
	}
	g.code_gen.mov64(g.code_gen.main_reg(), i64(g.get_type_size(node.typ)))
}

fn (mut g Gen) gen_print_from_expr(expr ast.Expr, typ ast.Type, name string) {
	newline := name in ['println', 'eprintln']
	fd := if name in ['eprint', 'eprintln'] { i32(2) } else { i32(1) }
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
			// XXX mov64 shouldn't be used for addressing
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
			styp := g.styp(expr.struct_type)
			field_name := expr.field
			if styp.kind == .struct {
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
			printer := if fd == 1 { 'print' } else { 'eprint' }
			for i, val in expr.vals {
				g.code_gen.gen_print(val, fd)
				if i < expr.exprs.len {
					g.gen_print_from_expr(expr.exprs[i], expr.expr_types[i], printer)
				}
			}

			if newline {
				g.code_gen.gen_print('\n', fd)
			}
		}
		ast.IfExpr {
			if expr.is_comptime {
				if branch := g.comptime_conditional(expr) {
					for i, stmt in branch.stmts {
						if i + 1 == branch.stmts.len && stmt is ast.ExprStmt {
							g.gen_print_from_expr(stmt.expr, stmt.typ, name)
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
			g.gen_to_string(g.code_gen.main_reg(), typ)
			g.code_gen.gen_print_reg(g.code_gen.main_reg(), -1, fd)
			if newline {
				g.code_gen.gen_print('\n', fd)
			}
		}
	}
}

fn (mut g Gen) gen_selector_expr(expr ast.SelectorExpr) {
	g.println('; .${expr.field_name} {')
	main_reg := g.code_gen.main_reg()
	g.expr(expr.expr)
	offset := g.get_field_offset(expr.expr_type, expr.field_name)
	if offset != 0 {
		g.code_gen.add(main_reg, offset)
	}
	if expr.next_token != .dot { // the deref needs to be on the last selector (that has no . after it)
		ts := g.table.sym(expr.typ)
		if ts.info !is ast.Struct {
			g.code_gen.mov_deref(main_reg, main_reg, expr.typ)
		}
	}
	g.println('; .${expr.field_name} }')
}

fn (mut g Gen) gen_left_value(node ast.Expr) {
	match node {
		ast.Ident {
			offset := g.get_var_offset(node.name)
			g.code_gen.lea_var_to_reg(Amd64Register.rax, offset)
		}
		ast.SelectorExpr {
			g.expr(node.expr)
			offset := g.get_field_offset(node.expr_type, node.field_name)
			if offset != 0 {
				g.code_gen.add(Amd64Register.rax, offset)
			}
		}
		ast.StructInit {
			g.expr(node)
		}
		ast.ArrayInit {
			g.expr(node) // TODO: add a test that uses this
		}
		ast.IndexExpr { // TODO
			g.n_error('${@LOCATION} Unsupported IndexExpr left value')
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
