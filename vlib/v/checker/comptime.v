// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.pref
import v.token
import v.util
import v.pkgconfig

fn (mut c Checker) comptime_call(mut node ast.ComptimeCall) ast.Type {
	node.left_type = c.expr(node.left)
	if node.is_env {
		env_value := util.resolve_env_value("\$env('$node.args_var')", false) or {
			c.error(err.msg(), node.env_pos)
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
		save_cur_fn := c.table.cur_fn
		pref_ := *c.pref
		pref2 := &pref.Preferences{
			...pref_
			is_vweb: true
		}
		mut c2 := new_checker(c.table, pref2)
		c2.comptime_call_pos = node.pos.pos
		c2.check(node.vweb_tmpl)
		c.warnings << c2.warnings
		c.errors << c2.errors
		c.notices << c2.notices
		c.nr_warnings += c2.nr_warnings
		c.nr_errors += c2.nr_errors
		c.nr_notices += c2.nr_notices

		c.table.cur_fn = save_cur_fn
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
	left_sym := c.table.sym(c.unwrap_generic(node.left_type))
	f := left_sym.find_method(method_name) or {
		c.error('could not find method `$method_name`', node.method_pos)
		return ast.void_type
	}
	node.result_type = f.return_type
	return f.return_type
}

fn (mut c Checker) comptime_for(node ast.ComptimeFor) {
	typ := c.unwrap_generic(node.typ)
	sym := c.table.sym(typ)
	if sym.kind == .placeholder || typ.has_flag(.generic) {
		c.error('unknown type `$sym.name`', node.typ_pos)
	}
	if node.kind == .fields {
		if sym.kind == .struct_ {
			sym_info := sym.info as ast.Struct
			c.inside_comptime_for_field = true
			for field in sym_info.fields {
				c.comptime_fields_type[node.val_var] = node.typ
				c.comptime_fields_default_type = field.typ
				c.stmts(node.stmts)
			}
			c.inside_comptime_for_field = false
		}
	} else {
		c.stmts(node.stmts)
	}
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
			s, _ := c.table.type_size(expr.typ)
			return s
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
				return cast_expr_value.u8() or { return none }
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
			} else if left is u8 && right is u8 {
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
			param_sym := c.table.final_sym(param.typ)
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
		sym_app := c.table.sym(vgt)
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
fn (mut c Checker) comptime_if_branch(cond ast.Expr, pos token.Pos) bool {
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
						sym := c.table.sym(cond.right.typ)
						if sym.kind != .interface_ {
							c.expr(cond.left)
							// c.error('`$sym.name` is not an interface', cond.right.pos())
						}
						return false
					} else if cond.left is ast.TypeNode && cond.right is ast.ComptimeType {
						left := cond.left as ast.TypeNode
						checked_type := c.unwrap_generic(left.typ)
						return c.table.is_comptime_type(checked_type, cond.right)
					} else if cond.left in [ast.SelectorExpr, ast.TypeNode] {
						// `$if method.@type is string`
						c.expr(cond.left)
						return false
					} else {
						c.error('invalid `\$if` condition: expected a type or a selector expression or an interface check',
							cond.left.pos())
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
							c.error(err.msg(), cond.left.pos)
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
					'apk' { return !c.pref.is_apk }
					'js' { return !c.pref.backend.is_js() }
					'debug' { return !c.pref.is_debug }
					'prod' { return !c.pref.is_prod }
					'profile' { return !c.pref.is_prof }
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
				typ := c.unwrap_generic(c.expr(cond))
				if cond.obj !is ast.Var && cond.obj !is ast.ConstField
					&& cond.obj !is ast.GlobalField {
					if !c.inside_ct_attr {
						c.error('unknown var: `$cname`', pos)
					}
					return false
				}
				expr := c.find_obj_definition(cond.obj) or {
					c.error(err.msg(), cond.pos)
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
					c.error(err.msg(), cond.pos)
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
