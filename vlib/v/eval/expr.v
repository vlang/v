module eval

import v.ast
import math
import strconv

pub fn (mut e Eval) expr(expr ast.Expr, expecting ast.Type) Object {
	match expr {
		ast.CallExpr {
			// println(expr.is_method)
			// is_method := expr.left.type_name() != 'unknown v.ast.Expr'
			// println(is_method)
			if expr.name == 'int' {
				panic('plz stop')
			}
			mut args := expr.args.map(e.expr(it.expr, it.typ))
			// if is_method {
			// args.prepend(e.expr(expr.left, expr.receiver_type))
			// }
			match expr.language {
				.c {
					match expr.name.all_after('C.') {
						'write' {
							return Int{C.write((args[0] as Int).val, args[1] as voidptr,
								(args[2] as Int).val), 32}
						}
						'calloc' {
							return Object(vcalloc(int((args[0] as Int).val * (args[1] as Int).val)))
						}
						'getcwd' {
							unsafe {
								return Charptr(&i8(&char(C.getcwd(charptr(&char(args[0] as Charptr)),
									(args[1] as Int).val))))
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
							e.error('unknown c function: `$expr.name`')
						}
					}
					return {}
				}
				.v {
					// TODO: Anon functions
					name := expr.name.all_after_last('.')
					mod := expr.mod
					mut func := e.mods[mod][name] or { e.mods['builtin'][name] }
					if func is ast.FnDecl {
						e.run_func(func as ast.FnDecl, ...args)
						return e.return_values
					}
					e.error('unknown function: ${mod}.$name at line $expr.pos.line_nr')
				}
				// .js {
				// 	e.error('js is not supported')
				// }
				else {
					e.error('$expr.language is not supported as a call expression language')
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
						match (branch.cond as ast.Ident).name {
							'windows' {
								do_if = e.pref.os == .windows
							}
							'macos' {
								do_if = e.pref.os == .macos
							}
							'linux' {
								do_if = e.pref.os == .linux
							}
							'android' {
								do_if = e.pref.os == .android
							}
							'freebsd' {
								do_if = e.pref.os == .freebsd
							}
							'prealloc' {
								do_if = e.pref.prealloc
							}
							else {
								e.error('unknown compile time if: ${(branch.cond as ast.Ident).name}')
							}
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
						e.error('non-bool expression: $b.cond')
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
			if expecting in ast.unsigned_integer_type_idxs {
				return Uint{strconv.parse_uint(expr.val, 0, 64), i8(e.type_to_size(expecting))}
			} else {
				return Int{strconv.parse_int(expr.val, 0, 64), i8(e.type_to_size(expecting))}
			}
		}
		ast.FloatLiteral {
			return Float{strconv.atof64(expr.val), i8(e.type_to_size(expecting))}
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
					}[expr.name.all_after_last('.')] as Object
				}
				else {
					e.error('unknown ident kind for `$expr.name`: $expr.kind')
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
					else {
						e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
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
					else {
						e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
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
					else {
						e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
					}
				}
			} else if expr.typ in ast.pointer_type_idxs {
				if expr.typ == ast.byteptr_type_idx {
					match x {
						Charptr, voidptr {
							return Object(byteptr(x))
						}
						else {
							e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
						}
					}
				} else if expr.typ == ast.voidptr_type_idx {
					match x {
						Charptr, byteptr {
							return Object(voidptr(x))
						}
						else {
							e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
						}
					}
				} else if expr.typ == ast.charptr_type_idx {
					match x {
						voidptr, byteptr {
							return Object(Charptr(&i8(x)))
						}
						else {
							println(e.local_vars['buf'])
							e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
						}
					}
				}
			} else {
				e.error('unknown cast: ${e.table.get_type_symbol(expr.expr_type).str()} to ${e.table.get_type_symbol(expr.typ).str()}')
			}
		}
		ast.SelectorExpr {
			exp := e.expr(expr.expr, expr.expr_type)
			match exp {
				string {
					match expr.field_name {
						'str' {
							return Object(exp.str)
						}
						'len' {
							return Int{exp.len, 32}
						}
						else {
							e.error('unknown selector to string: $expr.field_name')
						}
					}
				}
				Array {
					match expr.field_name {
						'len' {
							return Int{exp.val.len, 32}
						}
						else {
							e.error('unknown selector to array: $expr.field_name')
						}
					}
				}
				byteptr {
					match expr.field_name {
						'is_lit' {
							panic(e.cur_mod)
						}
						else {
							e.error('unknown selector to byteptr: $expr.field_name')
						}
					}
				}
				else {
					e.error('unknown selector expression: $exp.type_name()')
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
					e.error('unknown array init combination; len: $expr.has_len, cap: $expr.has_cap, init: $expr.has_default')
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
				e.error('invalid size of char literal: $expr.val.len')
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
		else {
			e.error('unhandled expression $expr.type_name()')
		}
	}
	return empty
}

fn (e Eval) type_to_size(typ ast.Type) u64 {
	match typ {
		ast.voidptr_type_idx, ast.byteptr_type_idx, ast.charptr_type_idx {
			return u64(if e.pref.m64 {
				64
			} else {
				32
			})
		}
		ast.i8_type_idx, ast.i16_type_idx, ast.int_type_idx, ast.i64_type_idx {
			return u64(math.exp2(f64(typ - 2))) // this formula converts the type number to the bitsize
		}
		ast.byte_type_idx, ast.u16_type_idx, ast.u32_type_idx, ast.u64_type_idx {
			return u64(math.exp2(f64(typ - 6))) // this formula converts the type number to the bitsize
		}
		ast.int_literal_type_idx, ast.float_literal_type_idx {
			return 64
		}
		ast.f32_type_idx, ast.f64_type_idx {
			return u64(math.exp2(f64(typ - 8))) // this formula converts the type number to the bitsize
		}
		else {
			e.error('type_to_size(): unknown type: ${e.table.get_type_symbol(typ).str()}')
			return -1
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
		else {
			`e`
		}
	}
	if res == `e` {
		e.error('unknown escape: `$r`')
	}
	return res
}
