module eval

import v.pref
import v.ast
import v.util
import math
import strconv

pub fn (mut e Eval) expr(expr ast.Expr, expecting_ ast.Type) Object {
	expecting := e.table.unaliased_type(expecting_)
	match expr {
		ast.ChanInit {
			return Chan{
				val: chan Object{cap: if expr.has_cap {
					cap_value := e.expr(expr.cap_expr, ast.int_type)
					if cap_value is i64 {
						int(cap_value)
					} else {
						int((cap_value as Int).val)
					}
				} else {
					0
				}}
				typ: expr.typ
			}
		}
		ast.GoExpr {
			res := spawn e.call_expr(expr.call_expr)
			if expr.is_expr {
				return Thread{
					val: res
					typ: expr.typ
				}
			}
			return empty
		}
		ast.CallExpr {
			return e.call_expr(expr)
		}
		ast.StringLiteral {
			// escape the escapes
			mut res := ''
			mut is_escape := false
			for c in expr.val {
				if is_escape {
					res += e.get_escape(rune(c)).str()
				} else if c == `\\` {
					is_escape = true
				} else {
					res += rune(c).str()
				}
			}
			return Object(res)
		}
		ast.IfExpr {
			if expr.is_expr {
				e.error('`if` expressions not supported')
			}
			if expr.is_comptime {
				if expr.comptime_branch_idx > -1 {
					e.stmts(expr.branches[expr.comptime_branch_idx].stmts)
				}
				return empty
			}
			for i, b in expr.branches {
				mut result := e.expr(b.cond, ast.bool_type_idx)

				if expr.has_else && i + 1 == expr.branches.len { // else block
					e.stmts(b.stmts)
					break
				}
				if result is bool {
					if result as bool {
						e.stmts(b.stmts)
						break
					}
				} else {
					e.error('non-bool expression: ${b.cond}')
				}
			}
			return empty
		}
		ast.IntegerLiteral {
			// return u64(strconv.parse_uint(expr.val, 0, 64)
			return i64(strconv.parse_int(expr.val, 0, 64) or {
				e.error('invalid integer literal: ${expr.val}')
			}) // TODO: numbers larger than 2^63 (for u64)
		}
		ast.FloatLiteral {
			return f64(strconv.atof64(expr.val) or { e.error(err.str()) })
		}
		ast.BoolLiteral {
			return expr.val
		}
		ast.Ident {
			match expr.kind {
				.variable {
					return e.local_vars[expr.name].val
				}
				.constant {
					return if expr.name.contains('.') {
						e.mods[expr.name.all_before_last('.')]
					} else {
						e.mods[e.cur_mod]
					}[expr.name.all_after_last('.')] or { ast.EmptyStmt{} } as Object
				}
				else {
					e.error('unknown ident kind for `${expr.name}`: ${expr.kind}')
				}
			}
		}
		ast.CastExpr {
			x := e.expr(expr.expr, expr.expr_type)
			expr_unaliased_typ := e.table.unaliased_type(expr.typ)
			if expr_unaliased_typ in ast.signed_integer_type_idxs {
				match x {
					Uint {
						return Int{
							val: i64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					Int {
						return Int{
							val: i64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					Float {
						return Int{
							val: i64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					i64 {
						if expecting in ast.signed_integer_type_idxs {
							return Int{
								val: x
								size: i8(e.type_to_size(expecting))
							}
						} else {
							return Uint{
								val: u64(x)
								size: i8(e.type_to_size(expecting))
							}
						}
					}
					f64 {
						if expecting in ast.signed_integer_type_idxs {
							return Int{
								val: i64(x)
								size: i8(e.type_to_size(expecting))
							}
						} else {
							return Uint{
								val: u64(x)
								size: i8(e.type_to_size(expecting))
							}
						}
					}
					else {
						e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
					}
				}
			} else if expr_unaliased_typ in ast.unsigned_integer_type_idxs {
				match x {
					Uint {
						return Uint{
							val: u64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					Int {
						return Uint{
							val: u64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					Float {
						return Uint{
							val: u64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					i64 {
						return Uint{
							val: u64(x)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					f64 {
						if expecting in ast.signed_integer_type_idxs {
							return Int{
								val: i64(x)
								size: i8(e.type_to_size(expecting))
							}
						} else {
							return Uint{
								val: u64(x)
								size: i8(e.type_to_size(expecting))
							}
						}
					}
					else {
						e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
					}
				}
			} else if expr_unaliased_typ in ast.float_type_idxs {
				match x {
					Uint {
						return Float{
							val: f64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					Int {
						return Float{
							val: f64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					Float {
						return Float{
							val: f64(x.val)
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					f64 {
						return Float{
							val: x
							size: i8(e.type_to_size(expr_unaliased_typ))
						}
					}
					else {
						e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
					}
				}
			} else if expr_unaliased_typ in ast.pointer_type_idxs {
				y := *(x as Ptr).val
				if expr_unaliased_typ == ast.byteptr_type_idx {
					match y {
						char, voidptr {
							unsafe {
								return Ptr{
									val: (x as Ptr).val
								}
							}
						}
						else {
							e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
						}
					}
				} else if expr_unaliased_typ == ast.voidptr_type_idx
					|| expr_unaliased_typ == ast.nil_type_idx {
					match y {
						char, Int {
							unsafe {
								return Object(voidptr((x as Ptr).val))
							}
						}
						else {
							e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
						}
					}
				} else if expr_unaliased_typ == ast.charptr_type_idx {
					match y {
						voidptr, Int {
							unsafe {
								return Ptr{
									val: &char((x as Ptr).val)
								}
							}
						}
						else {
							e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
						}
					}
				}
			} else if e.table.sym(expr_unaliased_typ).kind in [.interface_, .sum_type] {
				if e.pref.is_verbose {
					util.show_compiler_message('warning:',
						pos: expr.pos
						file_path: e.cur_file
						message: 'sumtype or interface casts return void currently'
					)
				}
			} else {
				e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
			}
		}
		ast.SelectorExpr {
			exp := e.expr(expr.expr, expr.expr_type)
			match exp {
				string {
					match expr.field_name {
						'str' {
							return Ptr{
								val: exp.str
							}
						}
						'len' {
							return Int{exp.len, 32}
						}
						else {
							e.error('unknown selector to string: ${expr.field_name}')
						}
					}
				}
				Array {
					match expr.field_name {
						'len' {
							return Int{exp.val.len, 32}
						}
						else {
							e.error('unknown selector to array: ${expr.field_name}')
						}
					}
				}
				else {
					e.error('unknown selector expression: ${exp.type_name()}')
				}
			}
			e.error(exp.str())
		}
		ast.ArrayInit {
			if expr.has_len || expr.has_cap || expr.has_default {
				if expr.has_len && !expr.has_cap && expr.has_default {
					return Array{
						val: []Object{len: int((e.expr(expr.len_expr, ast.usize_type) as Int).val), init: e.expr(expr.default_expr,
							expr.elem_type)}
					}
				} else if !expr.has_len && expr.has_cap && !expr.has_default {
					return Array{
						val: []Object{cap: int((e.expr(expr.cap_expr, ast.usize_type) as Int).val)}
					}
				} else if !expr.has_len && !expr.has_cap && !expr.has_default {
					return Array{
						val: []Object{}
					}
				} else {
					e.error('unknown array init combination; len: ${expr.has_len}, cap: ${expr.has_cap}, init: ${expr.has_default}')
				}
			}
			if expr.is_fixed || expr.has_val {
				e.error('fixed arrays are not supported')
			}
			mut res := Array{
				val: []Object{cap: expr.exprs.len}
			}
			for i, exp in expr.exprs {
				res.val << e.expr(exp, expr.expr_types[i])
			}
			return res
		}
		ast.CharLiteral {
			if expr.val.len !in [1, 2] {
				e.error('invalid size of char literal: ${expr.val.len}')
			}
			if expr.val[0] == `\\` { // is an escape
				return e.get_escape(rune(expr.val[1]))
			}
			return rune(expr.val[0])
		}
		ast.Nil {
			return Ptr{
				val: unsafe { nil }
			}
		}
		ast.None {
			return none_
		}
		ast.StringInterLiteral {
			mut res := expr.vals[0]

			for i, exp in expr.exprs {
				res += e.expr(exp, expr.expr_types[i]).string()
				res += expr.vals[i + 1]
			}

			return res
		}
		ast.StructInit {
			// eprintln('unhandled struct init at line $expr.pos.line_nr')
		}
		ast.SizeOf {
			return Uint{e.type_to_size(expr.typ), 64}
		}
		ast.ParExpr {
			return e.expr(expr.expr, expecting)
		}
		ast.PrefixExpr {
			value := e.expr(expr.right, expr.right_type)
			match expr.op {
				.amp {
					return Ptr{
						val: &value
					}
				}
				.not {
					return !(value as bool)
				}
				.arrow {
					return <-(value as Chan).val
				}
				else {
					e.error('unhandled prefix expression ${expr.op}')
				}
			}
		}
		ast.InfixExpr {
			left := e.expr(expr.left, expr.left_type)
			if expecting == ast.bool_type_idx && expr.op in [.logical_or, .and] {
				left_b := left as bool
				if expr.op == .logical_or && left_b {
					return true
				}
				right_b := e.expr(expr.right, expr.right_type) as bool
				if expr.op == .logical_or && right_b {
					return true
				}
				return left_b && right_b
			}
			right := e.expr(expr.right, expr.right_type)
			if expr.op == .arrow {
				(left as Chan).val <- right
				return empty
			}
			return e.infix_expr(left, right, expr.op, expecting)
		}
		ast.PostfixExpr {
			match expr.op {
				.inc {
					e.add(expr.expr, Int{1, 64})
					return e.expr(expr.expr, ast.i64_type_idx)
				}
				.dec {
					e.add(expr.expr, Int{-1, 64})
					return e.expr(expr.expr, ast.i64_type_idx)
				}
				else {
					e.error('unhandled postfix expression ${expr.op}')
				}
			}
		}
		ast.UnsafeExpr {
			return e.expr(expr.expr, expecting)
		}
		ast.DumpExpr {
			value := e.expr(expr.expr, expr.expr_type).string()
			mut cur_file := e.cur_file
			if cur_file.starts_with(util.normalised_workdir) {
				cur_file = cur_file.replace_once(util.normalised_workdir, '')
			}
			eprintln('[${cur_file}:${expr.pos.line_nr}] ${expr.expr}: ${value}')
			return empty
		}
		ast.AtExpr {
			return expr.val
		}
		ast.OrExpr, ast.Comment, ast.CTempVar {
			// ignore
		}
		ast.AnonFn, ast.ArrayDecompose, ast.AsCast, ast.Assoc, ast.ComptimeCall,
		ast.ComptimeSelector, ast.ComptimeType, ast.ConcatExpr, ast.EmptyExpr, ast.EnumVal,
		ast.IfGuardExpr, ast.IndexExpr, ast.IsRefType, ast.Likely, ast.LockExpr, ast.MapInit,
		ast.MatchExpr, ast.NodeError, ast.OffsetOf, ast.RangeExpr, ast.SelectExpr, ast.SqlExpr,
		ast.TypeNode, ast.TypeOf {
			e.error('unhandled expression ${typeof(expr).name}')
		}
	}
	return empty
}

fn (mut e Eval) call_expr(expr ast.CallExpr) Object {
	mut args := expr.args.map(e.expr(it.expr, it.typ))
	if expr.is_method {
		args.prepend(e.expr(expr.left, expr.receiver_type))
	}
	match expr.language {
		.c {
			if expr.is_method {
				e.error('C does not have methods')
			}
			match expr.name.all_after('C.') {
				'read' {
					return Int{C.read(args[0].int_val(), args[1] as voidptr, args[2].int_val()), 64}
				}
				'write' {
					return Int{C.write(args[0].int_val(), args[1] as voidptr, args[2].int_val()), 64}
				}
				'malloc' {
					return Ptr{
						val: unsafe { C.malloc(args[0].int_val()) }
					}
				}
				'calloc' {
					return Ptr{
						val: unsafe { C.calloc(args[0].int_val(), args[1].int_val()) }
					}
				}
				'getcwd' {
					unsafe {
						return Ptr{
							val: C.getcwd((args[0] as Ptr).val as voidptr, args[1].int_val())
						}
					}
				}
				'memcpy' {
					unsafe {
						return Ptr{
							val: C.memcpy(args[0] as voidptr, args[1] as voidptr, args[2].int_val())
						}
					}
				}
				else {
					e.error('unknown c function: `${expr.name}`')
				}
			}
		}
		.v {
			// TODO: Anon functions
			name := expr.name.all_after_last('.')
			mod := expr.mod
			mut func := e.mods[mod][name] or { e.mods['builtin'][name] or { ast.EmptyStmt{} } }

			if expr.name == 'main.host_pop' {
				return e.stack_vals.pop()
			}

			if func is ast.FnDecl {
				e.run_func(func as ast.FnDecl, ...args)
				mut ret_val := if e.return_values.len == 1 {
					e.return_values[0]
				} else {
					e.return_values
				}
				if mut ret_val is None {
					ret_val = e.or_expr(expr.or_block, expr.return_type)
				}
				return ret_val
			}
			e.error('unknown function: ${mod}.${name} at line ${expr.pos.line_nr}')
		}
		// .js {
		// 	e.error('js is not supported')
		// }
		else {
			e.error('${expr.language} is not supported as a call expression language')
		}
	}
	return empty
}

fn (mut e Eval) or_expr(expr ast.OrExpr, expected_type ast.Type) Object {
	match expr.kind {
		.block {
			e.stmts(expr.stmts#[..-1])
			return e.expr((expr.stmts.last() as ast.ExprStmt).expr, expected_type)
		}
		.propagate_option {
			if e.inside_main {
				e.panic('optional not set')
			}
			return none_
		}
		.propagate_result {
			if e.inside_main {
				e.panic('result not set ()')
			}
		}
		else {
			return none_
		}
	}
	return none_
}

fn (e Eval) type_to_size(typ ast.Type) u64 {
	match typ {
		ast.voidptr_type_idx, ast.nil_type_idx, ast.byteptr_type_idx, ast.charptr_type_idx {
			return u64(if e.pref.m64 {
				64
			} else {
				32
			})
		}
		ast.i8_type_idx, ast.i16_type_idx, ast.int_type_idx, ast.i64_type_idx {
			return u64(math.exp2(f64(typ - 2))) // this formula converts the type number to the bitsize
		}
		ast.u8_type_idx, ast.u16_type_idx, ast.u32_type_idx, ast.u64_type_idx {
			return u64(math.exp2(f64(typ - 6))) // this formula converts the type number to the bitsize
		}
		ast.int_literal_type_idx, ast.float_literal_type_idx {
			return 64
		}
		ast.f32_type_idx, ast.f64_type_idx {
			return u64(math.exp2(f64(typ - 8))) // this formula converts the type number to the bitsize
		}
		else {
			e.error('type_to_size(): unknown type: ${e.table.sym(typ).str()}')
			return u64(-1)
		}
	}
}

fn (e Eval) get_escape(r rune) rune {
	return u8(r).str_escaped().runes()[0]
}
