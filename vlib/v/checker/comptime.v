// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.pref
import v.token
import v.util
import v.pkgconfig

fn (mut c Checker) comptime_call(mut node ast.ComptimeCall) ast.Type {
	sym := c.table.get_type_symbol(c.unwrap_generic(c.expr(node.left)))
	node.left_type = c.expr(node.left)
	if node.is_env {
		env_value := util.resolve_env_value("\$env('$node.args_var')", false) or {
			c.error(err.msg, node.env_pos)
			return ast.string_type
		}
		node.env_value = env_value
		return ast.string_type
	}
	if node.is_embed {
		// c.file.embedded_files << node.embed_file
		if node.embed_file.compression_type !in valid_comptime_compression_types {
			supported := valid_comptime_compression_types.map('.$it').join(', ')
			c.error('not supported compression type: .${node.embed_file.compression_type}. supported: $supported',
				node.pos)
		}
		return c.table.find_type_idx('v.embed_file.EmbedFileData')
	}
	if node.is_vweb {
		// TODO assoc parser bug
		pref_ := *c.pref
		pref2 := &pref.Preferences{
			...pref_
			is_vweb: true
		}
		mut c2 := new_checker(c.table, pref2)
		c2.check(node.vweb_tmpl)
		mut i := 0 // tmp counter var for skipping first three tmpl vars
		for k, _ in c2.file.scope.children[0].objects {
			if i < 2 {
				// Skip first three because they are tmpl vars see vlib/vweb/tmpl/tmpl.v
				i++
				continue
			}
			if k in c.fn_scope.objects && unsafe { c.fn_scope.objects[k] } is ast.Var {
				mut vsc := unsafe { c.fn_scope.objects[k] } as ast.Var
				vsc.is_used = true
				c.fn_scope.objects[k] = vsc
			}
		}
		c.warnings << c2.warnings
		c.errors << c2.errors
		c.notices << c2.notices
		c.nr_warnings += c2.nr_warnings
		c.nr_errors += c2.nr_errors
		c.nr_notices += c2.nr_notices
	}
	if node.method_name == 'html' {
		rtyp := c.table.find_type_idx('vweb.Result')
		node.result_type = rtyp
		return rtyp
	}
	if node.method_name == 'method' {
		for i, arg in node.args {
			// check each arg expression
			node.args[i].typ = c.expr(arg.expr)
		}
		// assume string for now
		return ast.string_type
	}
	if node.is_vweb {
		return ast.string_type
	}
	// s.$my_str()
	v := node.scope.find_var(node.method_name) or {
		c.error('unknown identifier `$node.method_name`', node.method_pos)
		return ast.void_type
	}
	if v.typ != ast.string_type {
		s := c.expected_msg(v.typ, ast.string_type)
		c.error('invalid string method call: $s', node.method_pos)
		return ast.void_type
	}
	// note: we should use a compile-time evaluation function rather than handle here
	// mut variables will not work after init
	mut method_name := ''
	if v.expr is ast.StringLiteral {
		method_name = v.expr.val
	} else {
		c.error('todo: not a string literal', node.method_pos)
	}
	f := sym.find_method(method_name) or {
		c.error('could not find method `$method_name`', node.method_pos)
		return ast.void_type
	}
	// println(f.name + ' ' + c.table.type_to_str(f.return_type))
	node.result_type = f.return_type
	return f.return_type
}

// comptime const eval
fn (mut c Checker) eval_comptime_const_expr(expr ast.Expr, nlevel int) ?ast.ComptTimeConstValue {
	if nlevel > 100 {
		// protect against a too deep comptime eval recursion
		return none
	}
	match expr {
		ast.ParExpr {
			return c.eval_comptime_const_expr(expr.expr, nlevel + 1)
		}
		// ast.EnumVal {
		//	c.note('>>>>>>>> expr: $expr', expr.pos)
		//	return expr.val.i64()
		// }
		ast.SizeOf {
			xtype := expr.typ
			if xtype.is_real_pointer() {
				if c.pref.m64 {
					return 8 // 64bit platform
				}
				return 4 // 32bit platform
			}
			if int(xtype) == xtype.idx() {
				match xtype {
					ast.char_type { return 1 }
					ast.i8_type { return 1 }
					ast.i16_type { return 2 }
					ast.int_type { return 4 }
					ast.i64_type { return 8 }
					//
					ast.byte_type { return 1 }
					ast.u8_type { return 1 }
					ast.u16_type { return 2 }
					ast.u32_type { return 4 }
					ast.u64_type { return 8 }
					else {}
				}
			}
			return none
		}
		ast.FloatLiteral {
			x := expr.val.f64()
			return x
		}
		ast.IntegerLiteral {
			x := expr.val.u64()
			if x > 9223372036854775807 {
				return x
			}
			return expr.val.i64()
		}
		ast.StringLiteral {
			return util.smart_quote(expr.val, expr.is_raw)
		}
		ast.CharLiteral {
			runes := expr.val.runes()
			if runes.len > 0 {
				return runes[0]
			}
			return none
		}
		ast.Ident {
			if expr.obj is ast.ConstField {
				// an existing constant?
				return c.eval_comptime_const_expr(expr.obj.expr, nlevel + 1)
			}
		}
		ast.CastExpr {
			cast_expr_value := c.eval_comptime_const_expr(expr.expr, nlevel + 1) or { return none }
			if expr.typ == ast.i8_type {
				return cast_expr_value.i8() or { return none }
			}
			if expr.typ == ast.i16_type {
				return cast_expr_value.i16() or { return none }
			}
			if expr.typ == ast.int_type {
				return cast_expr_value.int() or { return none }
			}
			if expr.typ == ast.i64_type {
				return cast_expr_value.i64() or { return none }
			}
			//
			if expr.typ == ast.byte_type {
				return cast_expr_value.byte() or { return none }
			}
			if expr.typ == ast.u16_type {
				return cast_expr_value.u16() or { return none }
			}
			if expr.typ == ast.u32_type {
				return cast_expr_value.u32() or { return none }
			}
			if expr.typ == ast.u64_type {
				return cast_expr_value.u64() or { return none }
			}
			//
			if expr.typ == ast.f32_type {
				return cast_expr_value.f32() or { return none }
			}
			if expr.typ == ast.f64_type {
				return cast_expr_value.f64() or { return none }
			}
		}
		ast.InfixExpr {
			left := c.eval_comptime_const_expr(expr.left, nlevel + 1) ?
			right := c.eval_comptime_const_expr(expr.right, nlevel + 1) ?
			if left is string && right is string {
				match expr.op {
					.plus {
						return left + right
					}
					else {
						return none
					}
				}
			} else if left is u64 && right is i64 {
				match expr.op {
					.plus { return i64(left) + i64(right) }
					.minus { return i64(left) - i64(right) }
					.mul { return i64(left) * i64(right) }
					.div { return i64(left) / i64(right) }
					.mod { return i64(left) % i64(right) }
					.xor { return i64(left) ^ i64(right) }
					.pipe { return i64(left) | i64(right) }
					.amp { return i64(left) & i64(right) }
					.left_shift { return i64(u64(left) << i64(right)) }
					.right_shift { return i64(u64(left) >> i64(right)) }
					.unsigned_right_shift { return i64(u64(left) >>> i64(right)) }
					else { return none }
				}
			} else if left is i64 && right is u64 {
				match expr.op {
					.plus { return i64(left) + i64(right) }
					.minus { return i64(left) - i64(right) }
					.mul { return i64(left) * i64(right) }
					.div { return i64(left) / i64(right) }
					.mod { return i64(left) % i64(right) }
					.xor { return i64(left) ^ i64(right) }
					.pipe { return i64(left) | i64(right) }
					.amp { return i64(left) & i64(right) }
					.left_shift { return i64(u64(left) << i64(right)) }
					.right_shift { return i64(u64(left) >> i64(right)) }
					.unsigned_right_shift { return i64(u64(left) >>> i64(right)) }
					else { return none }
				}
			} else if left is u64 && right is u64 {
				match expr.op {
					.plus { return left + right }
					.minus { return left - right }
					.mul { return left * right }
					.div { return left / right }
					.mod { return left % right }
					.xor { return left ^ right }
					.pipe { return left | right }
					.amp { return left & right }
					.left_shift { return left << right }
					.right_shift { return left >> right }
					.unsigned_right_shift { return left >>> right }
					else { return none }
				}
			} else if left is i64 && right is i64 {
				match expr.op {
					.plus { return left + right }
					.minus { return left - right }
					.mul { return left * right }
					.div { return left / right }
					.mod { return left % right }
					.xor { return left ^ right }
					.pipe { return left | right }
					.amp { return left & right }
					.left_shift { return i64(u64(left) << right) }
					.right_shift { return i64(u64(left) >> right) }
					.unsigned_right_shift { return i64(u64(left) >>> right) }
					else { return none }
				}
			} else if left is byte && right is byte {
				match expr.op {
					.plus { return left + right }
					.minus { return left - right }
					.mul { return left * right }
					.div { return left / right }
					.mod { return left % right }
					.xor { return left ^ right }
					.pipe { return left | right }
					.amp { return left & right }
					.left_shift { return left << right }
					.right_shift { return left >> right }
					.unsigned_right_shift { return left >>> right }
					else { return none }
				}
			}
		}
		// ast.ArrayInit {}
		// ast.PrefixExpr {
		//	c.note('prefixexpr: $expr', expr.pos)
		// }
		else {
			// eprintln('>>> nlevel: $nlevel | another $expr.type_name() | $expr ')
			return none
		}
	}
	return none
}

fn (mut c Checker) verify_vweb_params_for_method(node ast.Fn) (bool, int, int) {
	margs := node.params.len - 1 // first arg is the receiver/this
	if node.attrs.len == 0 {
		// allow non custom routed methods, with 1:1 mapping
		return true, -1, margs
	}
	if node.params.len > 1 {
		for param in node.params[1..] {
			param_sym := c.table.get_final_type_symbol(param.typ)
			if !(param_sym.is_string() || param_sym.is_number() || param_sym.is_float()
				|| param_sym.kind == .bool) {
				c.error('invalid type `$param_sym.name` for parameter `$param.name` in vweb app method `$node.name`',
					param.pos)
			}
		}
	}
	mut route_attributes := 0
	for a in node.attrs {
		if a.name.starts_with('/') {
			route_attributes += a.name.count(':')
		}
	}
	return route_attributes == margs, route_attributes, margs
}

fn (mut c Checker) verify_all_vweb_routes() {
	if c.vweb_gen_types.len == 0 {
		return
	}
	c.table.used_vweb_types = c.vweb_gen_types
	typ_vweb_result := c.table.find_type_idx('vweb.Result')
	old_file := c.file
	for vgt in c.vweb_gen_types {
		sym_app := c.table.get_type_symbol(vgt)
		for m in sym_app.methods {
			if m.return_type == typ_vweb_result {
				is_ok, nroute_attributes, nargs := c.verify_vweb_params_for_method(m)
				if !is_ok {
					f := &ast.FnDecl(m.source_fn)
					if isnil(f) {
						continue
					}
					if f.return_type == typ_vweb_result && f.receiver.typ == m.params[0].typ
						&& f.name == m.name && !f.attrs.contains('post') {
						c.change_current_file(f.source_file) // setup of file path for the warning
						c.warn('mismatched parameters count between vweb method `${sym_app.name}.$m.name` ($nargs) and route attribute $m.attrs ($nroute_attributes)',
							f.pos)
					}
				}
			}
		}
	}
	c.change_current_file(old_file)
}

fn (mut c Checker) evaluate_once_comptime_if_attribute(mut node ast.Attr) bool {
	if node.ct_evaled {
		return node.ct_skip
	}
	if node.ct_expr is ast.Ident {
		if node.ct_opt {
			if node.ct_expr.name in valid_comptime_not_user_defined {
				c.error('optional `[if expression ?]` tags, can be used only for user defined identifiers',
					node.pos)
				node.ct_skip = true
			} else {
				node.ct_skip = node.ct_expr.name !in c.pref.compile_defines
			}
			node.ct_evaled = true
			return node.ct_skip
		} else {
			if node.ct_expr.name !in valid_comptime_not_user_defined {
				c.note('`[if $node.ct_expr.name]` is deprecated. Use `[if $node.ct_expr.name ?]` instead',
					node.pos)
				node.ct_skip = node.ct_expr.name !in c.pref.compile_defines
				node.ct_evaled = true
				return node.ct_skip
			} else {
				if node.ct_expr.name in c.pref.compile_defines {
					// explicitly allow custom user overrides with `-d linux` for example, for easier testing:
					node.ct_skip = false
					node.ct_evaled = true
					return node.ct_skip
				}
			}
		}
	}
	c.inside_ct_attr = true
	node.ct_skip = c.comptime_if_branch(node.ct_expr, node.pos)
	c.inside_ct_attr = false
	node.ct_evaled = true
	return node.ct_skip
}

// comptime_if_branch checks the condition of a compile-time `if` branch. It returns `true`
// if that branch's contents should be skipped (targets a different os for example)
fn (mut c Checker) comptime_if_branch(cond ast.Expr, pos token.Position) bool {
	// TODO: better error messages here
	match cond {
		ast.BoolLiteral {
			return !cond.val
		}
		ast.ParExpr {
			return c.comptime_if_branch(cond.expr, pos)
		}
		ast.PrefixExpr {
			if cond.op != .not {
				c.error('invalid `\$if` condition', cond.pos)
			}
			return !c.comptime_if_branch(cond.right, cond.pos)
		}
		ast.PostfixExpr {
			if cond.op != .question {
				c.error('invalid \$if postfix operator', cond.pos)
			} else if cond.expr is ast.Ident {
				return cond.expr.name !in c.pref.compile_defines_all
			} else {
				c.error('invalid `\$if` condition', cond.pos)
			}
		}
		ast.InfixExpr {
			match cond.op {
				.and {
					l := c.comptime_if_branch(cond.left, cond.pos)
					r := c.comptime_if_branch(cond.right, cond.pos)
					return l || r // skip (return true) if at least one should be skipped
				}
				.logical_or {
					l := c.comptime_if_branch(cond.left, cond.pos)
					r := c.comptime_if_branch(cond.right, cond.pos)
					return l && r // skip (return true) only if both should be skipped
				}
				.key_is, .not_is {
					if cond.left is ast.TypeNode && cond.right is ast.TypeNode {
						// `$if Foo is Interface {`
						sym := c.table.get_type_symbol(cond.right.typ)
						if sym.kind != .interface_ {
							c.expr(cond.left)
							// c.error('`$sym.name` is not an interface', cond.right.position())
						}
						return false
					} else if cond.left in [ast.SelectorExpr, ast.TypeNode] {
						// `$if method.@type is string`
						c.expr(cond.left)
						return false
					} else {
						c.error('invalid `\$if` condition: expected a type or a selector expression or an interface check',
							cond.left.position())
					}
				}
				.eq, .ne {
					if cond.left is ast.SelectorExpr && cond.right is ast.IntegerLiteral {
						// $if method.args.len == 1
					} else if cond.left is ast.Ident {
						// $if version == 2
						left_type := c.expr(cond.left)
						right_type := c.expr(cond.right)
						expr := c.find_definition(cond.left) or {
							c.error(err.msg, cond.left.pos)
							return false
						}
						if !c.check_types(right_type, left_type) {
							left_name := c.table.type_to_str(left_type)
							right_name := c.table.type_to_str(right_type)
							c.error('mismatched types `$left_name` and `$right_name`',
								cond.pos)
						}
						// :)
						// until `v.eval` is stable, I can't think of a better way to do this
						different := expr.str() != cond.right.str()
						return if cond.op == .eq { different } else { !different }
					} else {
						c.error('invalid `\$if` condition: ${cond.left.type_name()}1',
							cond.pos)
					}
				}
				else {
					c.error('invalid `\$if` condition', cond.pos)
				}
			}
		}
		ast.Ident {
			cname := cond.name
			if cname in valid_comptime_if_os {
				mut is_os_target_different := false
				if !c.pref.output_cross_c {
					target_os := c.pref.os.str().to_lower()
					is_os_target_different = cname != target_os
				}
				return is_os_target_different
			} else if cname in valid_comptime_if_compilers {
				return pref.cc_from_string(cname) != c.pref.ccompiler_type
			} else if cname in valid_comptime_if_platforms {
				if cname == 'aarch64' {
					c.note('use `arm64` instead of `aarch64`', pos)
				}
				match cname {
					'amd64' { return c.pref.arch != .amd64 }
					'i386' { return c.pref.arch != .i386 }
					'aarch64' { return c.pref.arch != .arm64 }
					'arm64' { return c.pref.arch != .arm64 }
					'arm32' { return c.pref.arch != .arm32 }
					'rv64' { return c.pref.arch != .rv64 }
					'rv32' { return c.pref.arch != .rv32 }
					else { return false }
				}
			} else if cname in valid_comptime_if_cpu_features {
				return false
			} else if cname in valid_comptime_if_other {
				match cname {
					'js' { return !c.pref.backend.is_js() }
					'debug' { return !c.pref.is_debug }
					'prod' { return !c.pref.is_prod }
					'test' { return !c.pref.is_test }
					'glibc' { return false } // TODO
					'threads' { return c.table.gostmts == 0 }
					'prealloc' { return !c.pref.prealloc }
					'no_bounds_checking' { return cname !in c.pref.compile_defines_all }
					'freestanding' { return !c.pref.is_bare || c.pref.output_cross_c }
					'interpreter' { c.pref.backend != .interpret }
					else { return false }
				}
			} else if cname !in c.pref.compile_defines_all {
				if cname == 'linux_or_macos' {
					c.error('linux_or_macos is deprecated, use `\$if linux || macos {` instead',
						cond.pos)
					return false
				}
				// `$if some_var {}`, or `[if user_defined_tag] fn abc(){}`
				typ := c.expr(cond)
				if cond.obj !is ast.Var && cond.obj !is ast.ConstField
					&& cond.obj !is ast.GlobalField {
					if !c.inside_ct_attr {
						c.error('unknown var: `$cname`', pos)
					}
					return false
				}
				expr := c.find_obj_definition(cond.obj) or {
					c.error(err.msg, cond.pos)
					return false
				}
				if !c.check_types(typ, ast.bool_type) {
					type_name := c.table.type_to_str(typ)
					c.error('non-bool type `$type_name` used as \$if condition', cond.pos)
				}
				// :)
				// until `v.eval` is stable, I can't think of a better way to do this
				return !(expr as ast.BoolLiteral).val
			}
		}
		ast.ComptimeCall {
			if cond.is_pkgconfig {
				mut m := pkgconfig.main([cond.args_var]) or {
					c.error(err.msg, cond.pos)
					return true
				}
				m.run() or { return true }
			}
		}
		else {
			c.error('invalid `\$if` condition', pos)
		}
	}
	return false
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ ast.Type, node ast.CallExpr) {
	if node.args.len != 1 {
		c.error('expected 1 argument, but got $node.args.len', node.pos)
		// Finish early so that it doesn't fail later
		return
	}
	elem_sym := c.table.get_type_symbol(elem_typ)
	arg_expr := node.args[0].expr
	match arg_expr {
		ast.AnonFn {
			if arg_expr.decl.params.len > 1 {
				c.error('function needs exactly 1 argument', arg_expr.decl.pos)
			} else if is_map && (arg_expr.decl.return_type == ast.void_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`', arg_expr.decl.pos)
			} else if !is_map && (arg_expr.decl.return_type != ast.bool_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
					arg_expr.decl.pos)
			}
		}
		ast.Ident {
			if arg_expr.kind == .function {
				func := c.table.find_fn(arg_expr.name) or {
					c.error('$arg_expr.name does not exist', arg_expr.pos)
					return
				}
				if func.params.len > 1 {
					c.error('function needs exactly 1 argument', node.pos)
				} else if is_map
					&& (func.return_type == ast.void_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`',
						arg_expr.pos)
				} else if !is_map
					&& (func.return_type != ast.bool_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
						arg_expr.pos)
				}
			} else if arg_expr.kind == .variable {
				if arg_expr.obj is ast.Var {
					expr := arg_expr.obj.expr
					if expr is ast.AnonFn {
						// copied from above
						if expr.decl.params.len > 1 {
							c.error('function needs exactly 1 argument', expr.decl.pos)
						} else if is_map && (expr.decl.return_type == ast.void_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`',
								expr.decl.pos)
						} else if !is_map && (expr.decl.return_type != ast.bool_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
								expr.decl.pos)
						}
						return
					}
				}
				// NOTE: bug accessing typ field on sumtype variant (not cast properly).
				// leaving this here as the resulting issue is notoriously hard to debug.
				// if !is_map && arg_expr.info.typ != ast.bool_type {
				if !is_map && arg_expr.var_info().typ != ast.bool_type {
					c.error('type mismatch, should be bool', arg_expr.pos)
				}
			}
		}
		ast.CallExpr {
			if is_map && arg_expr.return_type in [ast.void_type, 0] {
				c.error('type mismatch, `$arg_expr.name` does not return anything', arg_expr.pos)
			} else if !is_map && arg_expr.return_type != ast.bool_type {
				c.error('type mismatch, `$arg_expr.name` must return a bool', arg_expr.pos)
			}
		}
		else {}
	}
}

fn (mut c Checker) map_builtin_method_call(mut node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) ast.Type {
	method_name := node.name
	mut ret_type := ast.void_type
	match method_name {
		'clone', 'move' {
			if method_name[0] == `m` {
				c.fail_if_immutable(node.left)
			}
			if node.left.is_auto_deref_var() {
				ret_type = left_type.deref()
			} else {
				ret_type = left_type
			}
		}
		'keys' {
			info := left_sym.info as ast.Map
			typ := c.table.find_or_register_array(info.key_type)
			ret_type = ast.Type(typ)
		}
		'delete' {
			c.fail_if_immutable(node.left)
			if node.args.len != 1 {
				c.error('expected 1 argument, but got $node.args.len', node.pos)
			}
			info := left_sym.info as ast.Map
			arg_type := c.expr(node.args[0].expr)
			c.check_expected_call_arg(arg_type, info.key_type, node.language, node.args[0]) or {
				c.error('$err.msg in argument 1 to `Map.delete`', node.args[0].pos)
			}
		}
		else {}
	}
	node.receiver_type = left_type.ref()
	node.return_type = ret_type
	return node.return_type
}

fn (mut c Checker) array_builtin_method_call(mut node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) ast.Type {
	method_name := node.name
	mut elem_typ := ast.void_type
	if method_name == 'slice' && !c.is_builtin_mod {
		c.error('.slice() is a private method, use `x[start..end]` instead', node.pos)
	}
	array_info := left_sym.info as ast.Array
	elem_typ = array_info.elem_type
	if method_name in ['filter', 'map', 'any', 'all'] {
		// position of `it` doesn't matter
		scope_register_it(mut node.scope, node.pos, elem_typ)
	} else if method_name == 'sort' {
		if node.left is ast.CallExpr {
			c.error('the `sort()` method can be called only on mutable receivers, but `$node.left` is a call expression',
				node.pos)
		}
		c.fail_if_immutable(node.left)
		// position of `a` and `b` doesn't matter, they're the same
		scope_register_a_b(mut node.scope, node.pos, elem_typ)

		if node.args.len > 1 {
			c.error('expected 0 or 1 argument, but got $node.args.len', node.pos)
		} else if node.args.len == 1 {
			if node.args[0].expr is ast.InfixExpr {
				if node.args[0].expr.op !in [.gt, .lt] {
					c.error('`.sort()` can only use `<` or `>` comparison', node.pos)
				}
				left_name := '${node.args[0].expr.left}'[0]
				right_name := '${node.args[0].expr.right}'[0]
				if left_name !in [`a`, `b`] || right_name !in [`a`, `b`] {
					c.error('`.sort()` can only use `a` or `b` as argument, e.g. `arr.sort(a < b)`',
						node.pos)
				} else if left_name == right_name {
					c.error('`.sort()` cannot use same argument', node.pos)
				}
				if (node.args[0].expr.left !is ast.Ident
					&& node.args[0].expr.left !is ast.SelectorExpr
					&& node.args[0].expr.left !is ast.IndexExpr)
					|| (node.args[0].expr.right !is ast.Ident
					&& node.args[0].expr.right !is ast.SelectorExpr
					&& node.args[0].expr.right !is ast.IndexExpr) {
					c.error('`.sort()` can only use ident, index or selector as argument, \ne.g. `arr.sort(a < b)`, `arr.sort(a.id < b.id)`, `arr.sort(a[0] < b[0])`',
						node.pos)
				}
			} else {
				c.error(
					'`.sort()` requires a `<` or `>` comparison as the first and only argument' +
					'\ne.g. `users.sort(a.id < b.id)`', node.pos)
			}
		} else if !(c.table.get_type_symbol(elem_typ).has_method('<')
			|| c.table.unalias_num_type(elem_typ) in [ast.int_type, ast.int_type.ref(), ast.string_type, ast.string_type.ref(), ast.i8_type, ast.i16_type, ast.i64_type, ast.byte_type, ast.rune_type, ast.u16_type, ast.u32_type, ast.u64_type, ast.f32_type, ast.f64_type, ast.char_type, ast.bool_type, ast.float_literal_type, ast.int_literal_type]) {
			c.error('custom sorting condition must be supplied for type `${c.table.type_to_str(elem_typ)}`',
				node.pos)
		}
	} else if method_name == 'wait' {
		elem_sym := c.table.get_type_symbol(elem_typ)
		if elem_sym.kind == .thread {
			if node.args.len != 0 {
				c.error('`.wait()` does not have any arguments', node.args[0].pos)
			}
			thread_ret_type := elem_sym.thread_info().return_type
			if thread_ret_type.has_flag(.optional) {
				c.error('`.wait()` cannot be called for an array when thread functions return optionals. Iterate over the arrays elements instead and handle each returned optional with `or`.',
					node.pos)
			}
			node.return_type = c.table.find_or_register_array(thread_ret_type)
		} else {
			c.error('`$left_sym.name` has no method `wait()` (only thread handles and arrays of them have)',
				node.left.position())
		}
	}
	// map/filter are supposed to have 1 arg only
	mut arg_type := left_type
	for arg in node.args {
		arg_type = c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
	}
	if method_name == 'map' {
		// check fn
		c.check_map_and_filter(true, elem_typ, node)
		arg_sym := c.table.get_type_symbol(arg_type)
		ret_type := match arg_sym.info {
			ast.FnType { arg_sym.info.func.return_type }
			else { arg_type }
		}
		node.return_type = c.table.find_or_register_array(c.unwrap_generic(ret_type))
	} else if method_name == 'filter' {
		// check fn
		c.check_map_and_filter(false, elem_typ, node)
	} else if method_name in ['any', 'all'] {
		c.check_map_and_filter(false, elem_typ, node)
		node.return_type = ast.bool_type
	} else if method_name == 'clone' {
		// need to return `array_xxx` instead of `array`
		// in ['clone', 'str'] {
		node.receiver_type = left_type.ref()
		if node.left.is_auto_deref_var() {
			node.return_type = left_type.deref()
		} else {
			node.return_type = node.receiver_type.set_nr_muls(0)
		}
	} else if method_name == 'sort' {
		node.return_type = ast.void_type
	} else if method_name == 'contains' {
		// c.warn('use `value in arr` instead of `arr.contains(value)`', node.pos)
		node.return_type = ast.bool_type
	} else if method_name == 'index' {
		node.return_type = ast.int_type
	} else if method_name in ['first', 'last', 'pop'] {
		node.return_type = array_info.elem_type
		if method_name == 'pop' {
			c.fail_if_immutable(node.left)
			node.receiver_type = left_type.ref()
		} else {
			node.receiver_type = left_type
		}
	}
	return node.return_type
}

fn scope_register_it(mut s ast.Scope, pos token.Position, typ ast.Type) {
	s.register(ast.Var{
		name: 'it'
		pos: pos
		typ: typ
		is_used: true
	})
}

fn scope_register_a_b(mut s ast.Scope, pos token.Position, typ ast.Type) {
	s.register(ast.Var{
		name: 'a'
		pos: pos
		typ: typ.ref()
		is_used: true
	})
	s.register(ast.Var{
		name: 'b'
		pos: pos
		typ: typ.ref()
		is_used: true
	})
}
