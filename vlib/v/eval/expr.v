module eval

import v.pref
import v.ast
import v.util
import math
import strconv

fn (o Object) as_i64() !i64 {
	match o {
		i64 {
			return o
		}
		Int {
			return o.val
		}
		else {
			return error('can not cast object to i64')
		}
	}
}

pub fn (mut e Eval) expr(expr ast.Expr, expecting ast.Type) Object {
	match expr {
		ast.CallExpr {
			// println(expr.is_method)
			// is_method := expr.left.type_name() != 'unknown v.ast.Expr'
			// println(is_method)
			if expr.name == 'int' {
				e.error('methods not supported')
			}
			mut args := expr.args.map(e.expr(it.expr, it.typ))
			if expr.is_method {
				args.prepend(e.expr(expr.left, expr.receiver_type))
			}
			match expr.language {
				.c {
					if expr.is_method {
						e.error('c does not have methods')
					}
					match expr.name.all_after('C.') {
						'read' {
							return Int{C.read(args[0].int_val(), args[1] as voidptr, args[2].int_val()), 64}
						}
						'write' {
							return Int{C.write(args[0].int_val(), args[1] as voidptr,
								args[2].int_val()), 64}
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
									val: C.memcpy(args[0] as voidptr, args[1] as voidptr,
										args[2].int_val())
								}
							}
						}
						// 'printf' {
						// 	mut voidptr_args := []voidptr{}
						// 	for arg in args[1..] {
						// 		// if arg is Int {
						// 		// 	voidptr_args << voidptr(arg.val)
						// 		// } else {
						// 		voidptr_args << voidptr(&arg)
						// 		// }
						// 	}
						// 	// println((e.local_vars['s'].val as string).str == voidptr_args[1])
						// 	println('helo?$voidptr_args')
						// 	// println((byteptr(voidptr_args[1])[0]))
						// 	x := strconv.v_sprintf(args[0] as string, ...voidptr_args)
						// 	// println('helo!')
						// 	// println(x.len)
						// 	y := C.write(1, x.str, x.len)
						// 	println('aft')
						// 	return Int{y, 32}
						// }
						else {
							e.error('unknown c function: `${expr.name}`')
						}
					}
				}
				.v {
					// TODO: Anon functions
					name := expr.name.all_after_last('.')
					mod := expr.mod
					mut func := e.mods[mod][name] or {
						e.mods['builtin'][name] or { ast.EmptyStmt{} }
					}

					if func is ast.FnDecl {
						e.run_func(func as ast.FnDecl, ...args)
						if e.return_values.len == 1 {
							return e.return_values[0]
						} else {
							return e.return_values
						}
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
				for i, branch in expr.branches {
					mut do_if := false
					if expr.has_else && i + 1 == expr.branches.len { // else branch
						do_if = true
					} else {
						if branch.cond is ast.Ident {
							if known_os := pref.os_from_string(branch.cond.name) {
								do_if = e.pref.os == known_os
							} else {
								match branch.cond.name {
									'prealloc' {
										do_if = e.pref.prealloc
									}
									else {
										e.error('unknown compile time if: ${branch.cond.name}')
									}
								}
							}
						} else if branch.cond is ast.PostfixExpr {
							do_if = (branch.cond.expr as ast.Ident).name in e.pref.compile_defines
						}
					}
					if do_if {
						e.stmts(branch.stmts)
						break
					}
				}
				return empty
			} else {
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
		}
		ast.InfixExpr {
			left := e.expr(expr.left, expr.left_type)
			right := e.expr(expr.right, expr.right_type)
			return e.infix_expr(left, right, expr.op, expecting)
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
					// println(e.local_vars[expr.name].val.type_name())
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
			if expr.typ in ast.signed_integer_type_idxs {
				match x {
					Uint {
						return Int{
							val: i64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					Int {
						return Int{
							val: i64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					Float {
						return Int{
							val: i64(x.val)
							size: i8(e.type_to_size(expr.typ))
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
			} else if expr.typ in ast.unsigned_integer_type_idxs {
				match x {
					Uint {
						return Uint{
							val: u64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					Int {
						return Uint{
							val: u64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					Float {
						return Uint{
							val: u64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					i64 {
						return Uint{
							val: u64(x)
							size: i8(e.type_to_size(expr.typ))
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
			} else if expr.typ in ast.float_type_idxs {
				match x {
					Uint {
						return Float{
							val: f64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					Int {
						return Float{
							val: f64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					Float {
						return Float{
							val: f64(x.val)
							size: i8(e.type_to_size(expr.typ))
						}
					}
					f64 {
						return Float{
							val: x
							size: i8(e.type_to_size(expr.typ))
						}
					}
					else {
						e.error('unknown cast: ${e.table.sym(expr.expr_type).str()} to ${e.table.sym(expr.typ).str()}')
					}
				}
			} else if expr.typ in ast.pointer_type_idxs {
				y := *(x as Ptr).val
				if expr.typ == ast.byteptr_type_idx {
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
				} else if expr.typ == ast.voidptr_type_idx || expr.typ == ast.nil_type_idx {
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
				} else if expr.typ == ast.charptr_type_idx {
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
			} else if e.table.sym(expr.typ).kind in [.interface_, .sum_type] {
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
						val: []Object{len: int((e.expr(expr.len_expr, 7) as Int).val), init: e.expr(expr.default_expr,
							expr.elem_type)}
					}
				} else if !expr.has_len && expr.has_cap && !expr.has_default {
					return Array{
						val: []Object{cap: int((e.expr(expr.cap_expr, 7) as Int).val)}
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
			} else {
				return rune(expr.val[0])
			}
		}
		ast.StructInit {
			// eprintln('unhandled struct init at line $expr.pos.line_nr')
			return 'helo'
		}
		ast.SizeOf {
			return Uint{e.type_to_size(expr.typ), 64}
		}
		ast.ParExpr {
			return e.expr(expr.expr, expecting)
		}
		ast.PrefixExpr {
			match expr.op {
				.amp {
					x := e.expr(expr.right, expr.right_type)
					return Ptr{
						val: &x
					}
				}
				else {
					e.error('unhandled prefix expression ${expr.op}')
				}
			}
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
		ast.StringInterLiteral {
			mut res := expr.vals[0]

			for i, exp in expr.exprs {
				res += e.expr(exp, expr.expr_types[i]).string()
				res += expr.vals[i + 1]
			}

			return res
		}
		ast.UnsafeExpr {
			return e.expr(expr.expr, expecting)
		}
		ast.AnonFn, ast.ArrayDecompose, ast.AsCast, ast.Assoc, ast.AtExpr, ast.CTempVar,
		ast.ChanInit, ast.Comment, ast.ComptimeCall, ast.ComptimeSelector, ast.ComptimeType,
		ast.ConcatExpr, ast.DumpExpr, ast.EmptyExpr, ast.EnumVal, ast.GoExpr, ast.IfGuardExpr,
		ast.IndexExpr, ast.IsRefType, ast.Likely, ast.LockExpr, ast.MapInit, ast.MatchExpr,
		ast.Nil, ast.NodeError, ast.None, ast.OffsetOf, ast.OrExpr, ast.RangeExpr, ast.SelectExpr,
		ast.SqlExpr, ast.TypeNode, ast.TypeOf {
			e.error('unhandled expression ${typeof(expr).name}')
		}
	}
	return empty
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
	res := match r {
		`\\` {
			`\\`
		}
		`n` {
			`\n`
		}
		`0` {
			`\0`
		}
		else {
			`e`
		}
	}
	if res == `e` {
		e.error('unknown escape: `${r}`')
	}
	return res
}
