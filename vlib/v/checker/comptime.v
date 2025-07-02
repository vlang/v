// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import os
import v.ast
import v.pref
import v.token
import v.util
import v.pkgconfig
import v.type_resolver

fn (mut c Checker) comptime_call(mut node ast.ComptimeCall) ast.Type {
	if node.left !is ast.EmptyExpr {
		node.left_type = c.expr(mut node.left)
	}
	if node.method_name == 'compile_error' {
		c.error(node.args_var, node.pos)
		return ast.void_type
	} else if node.method_name == 'compile_warn' {
		c.warn(node.args_var, node.pos)
		return ast.void_type
	}
	if node.is_env {
		env_value := util.resolve_env_value("\$env('${node.args_var}')", false) or {
			c.error(err.msg(), node.env_pos)
			return ast.string_type
		}
		node.env_value = env_value
		return ast.string_type
	}
	if node.is_compile_value {
		node.resolve_compile_value(c.pref.compile_values) or {
			c.error(err.msg(), node.pos)
			return ast.void_type
		}
		return node.result_type
	}
	if node.is_embed {
		if node.args.len == 1 {
			embed_arg := node.args[0]
			mut raw_path := ''
			if embed_arg.expr is ast.AtExpr {
				mut expr := embed_arg.expr
				c.at_expr(mut expr)
				raw_path = expr.val
			}
			if embed_arg.expr is ast.StringLiteral {
				raw_path = embed_arg.expr.val
			} else if embed_arg.expr is ast.Ident {
				if var := c.fn_scope.find_var(embed_arg.expr.name) {
					if var.expr is ast.StringLiteral {
						raw_path = var.expr.val
					}
				}
			}
			mut escaped_path := raw_path.replace('/', os.path_separator)
			// Validate that the epath exists, and that it is actually a file.
			if escaped_path == '' {
				c.error('supply a valid relative or absolute file path to the file to embed, that is known at compile time',
					node.pos)
				return ast.string_type
			}
			abs_path := os.real_path(escaped_path)
			// check absolute path first
			if !os.exists(abs_path) {
				// ... look relative to the source file:
				escaped_path = os.real_path(os.join_path_single(os.dir(c.file.path), escaped_path))
				if !os.exists(escaped_path) {
					c.error('"${escaped_path}" does not exist so it cannot be embedded',
						node.pos)
					return ast.string_type
				}
				if !os.is_file(escaped_path) {
					c.error('"${escaped_path}" is not a file so it cannot be embedded',
						node.pos)
					return ast.string_type
				}
			} else {
				escaped_path = abs_path
			}
			node.embed_file.rpath = raw_path
			node.embed_file.apath = escaped_path
		}
		// c.file.embedded_files << node.embed_file
		if node.embed_file.compression_type !in ast.valid_comptime_compression_types {
			supported := ast.valid_comptime_compression_types.map('.${it}').join(', ')
			c.error('not supported compression type: .${node.embed_file.compression_type}. supported: ${supported}',
				node.pos)
		}
		return c.table.find_type('v.embed_file.EmbedFileData')
	}
	if node.is_vweb {
		// TODO: assoc parser bug
		save_cur_fn := c.table.cur_fn
		pref_ := *c.pref
		pref2 := &pref.Preferences{
			...pref_
			is_vweb: true
		}
		mut c2 := new_checker(c.table, pref2)
		c2.comptime_call_pos = node.pos.pos
		c2.check(mut node.veb_tmpl)
		c.warnings << c2.warnings
		c.errors << c2.errors
		c.notices << c2.notices
		c.nr_warnings += c2.nr_warnings
		c.nr_errors += c2.nr_errors
		c.nr_notices += c2.nr_notices

		c.table.cur_fn = save_cur_fn
	}
	if node.method_name == 'html' {
		ret_sym := c.table.sym(c.table.cur_fn.return_type)
		if ret_sym.cname !in ['veb__Result', 'vweb__Result', 'x__vweb__Result'] {
			ct_call := if node.is_veb { 'veb' } else { 'vweb' }
			c.error('`\$${ct_call}.html()` must be called inside a web method, e.g. `fn (mut app App) foo(mut ctx Context) ${ct_call}.Result { return \$${ct_call}.html(\'index.html\') }`',
				node.pos)
		}
		rtyp := if node.is_veb {
			c.table.find_type('veb.Result')
		} else {
			c.table.find_type('vweb.Result')
		}
		node.result_type = rtyp
		return rtyp
	}
	if node.method_name == 'method' {
		if c.inside_anon_fn && 'method' !in c.cur_anon_fn.inherited_vars.map(it.name) {
			c.error('undefined ident `method` in the anonymous function', node.pos)
		}
		for i, mut arg in node.args {
			// check each arg expression
			node.args[i].typ = c.expr(mut arg.expr)
		}
		c.markused_comptimecall(mut node)
		c.stmts_ending_with_expression(mut node.or_block.stmts, c.expected_or_type)
		return c.type_resolver.get_type(node)
	}
	if node.method_name == 'res' {
		if !c.inside_defer {
			c.error('`res` can only be used in defer blocks', node.pos)
			return ast.void_type
		}

		if c.fn_return_type == ast.void_type {
			c.error('`res` can only be used in functions that returns something', node.pos)
			return ast.void_type
		}

		sym := c.table.sym(c.fn_return_type)

		if c.fn_return_type.has_flag(.result) {
			c.error('`res` cannot be used in functions that returns a Result', node.pos)
			return ast.void_type
		}

		if sym.info is ast.MultiReturn {
			if node.args_var == '' {
				c.error('`res` requires an index of the returned value', node.pos)
				return ast.void_type
			}
			idx := node.args_var.int()
			if idx < 0 || idx >= sym.info.types.len {
				c.error('index ${idx} out of range of ${sym.info.types.len} return types',
					node.pos)
				return ast.void_type
			}
			return sym.info.types[idx]
		}

		return c.fn_return_type
	}
	if node.is_vweb {
		return ast.string_type
	}
	// s.$my_str()
	v := node.scope.find_var(node.method_name) or {
		c.error('unknown identifier `${node.method_name}`', node.method_pos)
		return ast.void_type
	}
	if v.typ != ast.string_type {
		s := c.expected_msg(v.typ, ast.string_type)
		c.error('invalid string method call: ${s}', node.method_pos)
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
	left_type := c.unwrap_generic(node.left_type)
	left_sym := c.table.sym(left_type)
	f := left_sym.find_method(method_name) or {
		c.error('could not find method `${method_name}`', node.method_pos)
		return ast.void_type
	}
	c.markused_comptime_call(true, '${int(left_type)}.${method_name}')
	node.result_type = f.return_type
	return f.return_type
}

fn (mut c Checker) comptime_selector(mut node ast.ComptimeSelector) ast.Type {
	node.left_type = c.expr(mut node.left)
	mut expr_type := c.unwrap_generic(c.expr(mut node.field_expr))
	expr_sym := c.table.sym(expr_type)
	if expr_type != ast.string_type {
		c.error('expected `string` instead of `${expr_sym.name}` (e.g. `field.name`)',
			node.field_expr.pos())
	}
	if mut node.field_expr is ast.SelectorExpr {
		left_pos := node.field_expr.expr.pos()
		if c.type_resolver.type_map.len == 0 {
			c.error('compile time field access can only be used when iterating over `T.fields`',
				left_pos)
		}
		node.is_name = node.field_expr.field_name == 'name'
		if mut node.field_expr.expr is ast.Ident {
			node.typ_key = '${node.field_expr.expr.name}.typ'
		}
		expr_type = c.type_resolver.get_comptime_selector_type(node, ast.void_type)
		if expr_type != ast.void_type {
			if node.or_block.kind == .propagate_option {
				return expr_type.clear_flag(.option)
			}
			return expr_type
		}
		expr_name := node.field_expr.expr.str()
		if expr_name in c.type_resolver.type_map {
			return c.type_resolver.get_ct_type_or_default(expr_name, ast.void_type)
		}
		c.error('unknown `\$for` variable `${expr_name}`', left_pos)
	} else {
		c.error('expected selector expression e.g. `$(field.name)`', node.field_expr.pos())
	}
	return ast.void_type
}

fn (mut c Checker) comptime_for(mut node ast.ComptimeFor) {
	typ := if node.typ != ast.void_type {
		c.unwrap_generic(node.typ)
	} else {
		node.typ = c.expr(mut node.expr)
		c.unwrap_generic(node.typ)
	}
	sym := c.table.final_sym(typ)
	if sym.kind == .placeholder || typ.has_flag(.generic) {
		c.error('\$for expects a type name or variable name to be used here, but ${sym.name} is not a type or variable name',
			node.typ_pos)
		return
	} else if sym.kind == .void {
		c.error('only known compile-time variables can be used', node.typ_pos)
		return
	}
	if node.kind == .fields {
		if sym.kind in [.struct, .interface] {
			mut fields := []ast.StructField{}
			match sym.info {
				ast.Struct {
					fields = sym.info.fields.clone()
				}
				ast.Interface {
					fields = sym.info.fields.clone()
				}
				else {
					c.error('iterating over .fields is supported only for structs and interfaces, and ${sym.name} is neither',
						node.typ_pos)
					return
				}
			}
			has_different_types := fields.len > 1
				&& !fields.all(c.check_basic(it.typ, fields[0].typ))
			for field in fields {
				prev_inside_x_matches_type := c.inside_x_matches_type
				c.push_new_comptime_info()
				c.comptime.inside_comptime_for = true
				c.table.used_features.comptime_for = true
				if c.field_data_type == 0 {
					c.field_data_type = c.table.find_type('FieldData')
				}
				c.comptime.comptime_for_field_value = field
				c.comptime.comptime_for_field_var = node.val_var
				c.type_resolver.update_ct_type(node.val_var, c.field_data_type)
				c.type_resolver.update_ct_type('${node.val_var}.typ', node.typ)
				c.comptime.comptime_for_field_type = field.typ
				c.comptime.has_different_types = has_different_types
				c.stmts(mut node.stmts)

				unwrapped_expr_type := c.unwrap_generic(field.typ)
				tsym := c.table.sym(unwrapped_expr_type)
				c.table.dumps[int(unwrapped_expr_type.clear_flags(.option, .result, .atomic_f))] = tsym.cname
				c.markused_comptimefor(mut node, unwrapped_expr_type)
				if tsym.kind == .array_fixed {
					info := tsym.info as ast.ArrayFixed
					if !info.is_fn_ret {
						// for dumping fixed array we must register the fixed array struct to return from function
						c.table.find_or_register_array_fixed(info.elem_type, info.size,
							info.size_expr, true)
					}
				}
				c.pop_comptime_info()
				c.inside_x_matches_type = prev_inside_x_matches_type
			}
		} else if node.typ != ast.void_type && c.table.generic_type_names(node.typ).len == 0
			&& sym.kind != .placeholder {
			c.error('iterating over .fields is supported only for structs and interfaces, and ${sym.name} is neither',
				node.typ_pos)
			return
		}
	} else if node.kind == .values {
		if sym.kind == .enum {
			c.push_new_comptime_info()
			c.comptime.inside_comptime_for = true
			if c.enum_data_type == 0 {
				c.enum_data_type = c.table.find_type('EnumData')
			}
			c.comptime.comptime_for_enum_var = node.val_var
			c.type_resolver.update_ct_type(node.val_var, c.enum_data_type)
			c.type_resolver.update_ct_type('${node.val_var}.typ', node.typ)
			c.stmts(mut node.stmts)
			c.pop_comptime_info()
			c.table.used_features.comptime_for = true
		} else {
			c.error('iterating over .values is supported only for enums, and ${sym.name} is not an enum',
				node.typ_pos)
			return
		}
	} else if node.kind == .methods {
		methods := sym.get_methods()
		for method in methods {
			c.push_new_comptime_info()
			c.comptime.inside_comptime_for = true
			c.comptime.comptime_for_method = unsafe { &method }
			c.comptime.comptime_for_method_var = node.val_var
			c.comptime.comptime_for_method_ret_type = method.return_type
			c.type_resolver.update_ct_type('${node.val_var}.return_type', method.return_type)
			c.stmts(mut node.stmts)
			c.pop_comptime_info()
		}
		c.table.used_features.comptime_for = true
	} else if node.kind == .params {
		if !(sym.kind == .function || sym.name == 'FunctionData') {
			c.error('iterating over `.params` is supported only for functions, and `${sym.name}` is not a function',
				node.typ_pos)
			return
		}
		c.push_new_comptime_info()
		c.comptime.inside_comptime_for = true
		c.comptime.comptime_for_method_param_var = node.val_var
		c.stmts(mut node.stmts)
		c.pop_comptime_info()
	} else if node.kind == .variants {
		if c.variant_data_type == 0 {
			c.variant_data_type = c.table.find_type('VariantData')
		}
		mut variants := []ast.Type{}
		if c.comptime.comptime_for_field_var != '' && typ == c.field_data_type {
			sumtype_sym := c.table.sym(c.comptime.comptime_for_field_type)
			if sumtype_sym.kind == .sum_type {
				variants = (sumtype_sym.info as ast.SumType).variants.clone()
			}
		} else if sym.kind != .sum_type {
			c.error('${sym.name} is not Sum type to use with .variants', node.typ_pos)
		} else {
			variants = (sym.info as ast.SumType).variants.clone()
		}
		for variant in variants {
			c.push_new_comptime_info()
			c.comptime.inside_comptime_for = true
			c.comptime.comptime_for_variant_var = node.val_var
			c.type_resolver.update_ct_type(node.val_var, c.variant_data_type)
			c.type_resolver.update_ct_type('${node.val_var}.typ', variant)
			c.stmts(mut node.stmts)
			c.pop_comptime_info()
		}
	} else {
		c.stmts(mut node.stmts)
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
		ast.EnumVal {
			enum_name := if expr.enum_name == '' {
				c.table.type_to_str(c.expected_type)
			} else {
				expr.enum_name
			}
			if val := c.table.find_enum_field_val(enum_name, expr.val) {
				return val
			}
		}
		ast.SizeOf {
			s, _ := c.table.type_size(c.unwrap_generic(expr.typ))
			return i64(s)
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
			if expr.typ == ast.i32_type {
				return cast_expr_value.i32() or { return none }
			}
			if expr.typ == ast.i64_type {
				return cast_expr_value.i64() or { return none }
			}
			if expr.typ == ast.int_type {
				return cast_expr_value.i64() or { return none }
			}
			//
			if expr.typ == ast.u8_type {
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
			if expr.typ == ast.voidptr_type || expr.typ == ast.nil_type {
				ptrvalue := cast_expr_value.voidptr() or { return none }
				return ast.ComptTimeConstValue(ptrvalue)
			}
		}
		ast.InfixExpr {
			left := c.eval_comptime_const_expr(expr.left, nlevel + 1)?
			saved_expected_type := c.expected_type
			if expr.left is ast.EnumVal {
				c.expected_type = expr.left.typ
			} else if expr.left is ast.InfixExpr {
				mut infixexpr := expr
				for {
					if infixexpr.left is ast.InfixExpr {
						infixexpr = infixexpr.left as ast.InfixExpr
					} else {
						break
					}
				}
				if mut infixexpr.left is ast.EnumVal {
					c.expected_type = infixexpr.left.typ
				}
			}
			right := c.eval_comptime_const_expr(expr.right, nlevel + 1)?
			c.expected_type = saved_expected_type
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
		ast.IfExpr {
			if !expr.is_comptime {
				return none
			}
			for i in 0 .. expr.branches.len {
				mut branch := expr.branches[i]
				if !expr.has_else || i < expr.branches.len - 1 {
					if c.comptime_if_cond(mut branch.cond, branch.pos) == .eval {
						last_stmt := branch.stmts.last()
						if last_stmt is ast.ExprStmt {
							return c.eval_comptime_const_expr(last_stmt.expr, nlevel + 1)
						}
					}
				} else {
					last_stmt := branch.stmts.last()
					if last_stmt is ast.ExprStmt {
						return c.eval_comptime_const_expr(last_stmt.expr, nlevel + 1)
					}
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

fn (mut c Checker) verify_vweb_params_for_method(node &ast.Fn) (bool, int, int) {
	margs := node.params.len - 1 // first arg is the receiver/this
	// if node.attrs.len == 0 || (node.attrs.len == 1 && node.attrs[0].name == 'post') {
	if node.attrs.len == 0 {
		// allow non custom routed methods, with 1:1 mapping
		return true, -1, margs
	}
	if node.params.len > 1 {
		for param in node.params[1..] {
			param_sym := c.table.final_sym(param.typ)
			if !(param_sym.is_string() || param_sym.is_number() || param_sym.is_float()
				|| param_sym.kind == .bool) {
				c.error('invalid type `${param_sym.name}` for parameter `${param.name}` in vweb app method `${node.name}` (only strings, numbers, and bools are allowed)',
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
	c.table.used_features.used_veb_types = c.vweb_gen_types
	typ_vweb_result := c.table.find_type('vweb.Result')
	old_file := c.file
	for vgt in c.vweb_gen_types {
		sym_app := c.table.sym(vgt)
		for m in sym_app.methods {
			if m.return_type == typ_vweb_result {
				is_ok, nroute_attributes, nargs := c.verify_vweb_params_for_method(m)
				if !is_ok {
					f := unsafe { &ast.FnDecl(m.source_fn) }
					if f == unsafe { nil } {
						continue
					}
					if f.return_type == typ_vweb_result && f.receiver.typ == m.params[0].typ
						&& f.name == m.name && !f.attrs.contains('post') {
						c.change_current_file(f.source_file) // setup of file path for the warning
						c.warn('mismatched parameters count between vweb method `${sym_app.name}.${m.name}` (${nargs}) and route attribute ${m.attrs} (${nroute_attributes})',
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
	if mut node.ct_expr is ast.Ident {
		if node.ct_opt {
			if node.ct_expr.name in ast.valid_comptime_not_user_defined {
				c.error('option `@[if expression ?]` tags, can be used only for user defined identifiers',
					node.pos)
				node.ct_skip = true
			} else {
				node.ct_skip = node.ct_expr.name !in c.pref.compile_defines
			}
			node.ct_evaled = true
			return node.ct_skip
		} else {
			if node.ct_expr.name !in ast.valid_comptime_not_user_defined {
				c.note('`@[if ${node.ct_expr.name}]` is deprecated. Use `@[if ${node.ct_expr.name} ?]` instead',
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
	node.ct_skip = if c.comptime_if_cond(mut node.ct_expr, node.pos) == .skip {
		true
	} else {
		false
	}
	c.inside_ct_attr = false
	node.ct_evaled = true
	return node.ct_skip
}

enum ComptimeBranchSkipState {
	eval
	skip
	unknown
}

// comptime_if_cond checks the condition of a compile-time `if` branch. It returns `true`
// if that branch's contents should be skipped (targets a different os for example)
fn (mut c Checker) comptime_if_cond(mut cond ast.Expr, pos token.Pos) ComptimeBranchSkipState {
	mut should_record_ident := false
	mut is_user_ident := false
	mut ident_name := ''
	defer {
		if should_record_ident {
			if is_user_ident {
				c.ct_user_defines[ident_name] = $res()
			} else {
				c.ct_system_defines[ident_name] = $res()
			}
		}
	}
	// TODO: better error messages here
	match mut cond {
		ast.BoolLiteral {
			return if cond.val { .eval } else { .skip }
		}
		ast.ParExpr {
			return c.comptime_if_cond(mut cond.expr, pos)
		}
		ast.PrefixExpr {
			if cond.op != .not {
				c.error('invalid `\$if` condition', cond.pos)
			}
			reversed := c.comptime_if_cond(mut cond.right, cond.pos)
			return if reversed == .eval {
				.skip
			} else if reversed == .skip {
				.eval
			} else {
				reversed
			}
		}
		ast.PostfixExpr {
			if cond.op != .question {
				c.error('invalid \$if postfix operator', cond.pos)
			} else if mut cond.expr is ast.Ident {
				should_record_ident = true
				is_user_ident = true
				ident_name = cond.expr.name
				return if cond.expr.name in c.pref.compile_defines {
					.eval
				} else {
					if cond.expr.name in c.pref.compile_defines_all {
						ComptimeBranchSkipState.unknown
					} else {
						ComptimeBranchSkipState.skip
					}
				}
			} else {
				c.error('invalid `\$if` condition', cond.pos)
			}
		}
		ast.InfixExpr {
			match cond.op {
				.and {
					l := c.comptime_if_cond(mut cond.left, cond.pos)
					r := c.comptime_if_cond(mut cond.right, cond.pos)
					if l == .unknown || r == .unknown {
						return .unknown
					}
					return if l == .eval && r == .eval { .eval } else { .skip }
				}
				.logical_or {
					l := c.comptime_if_cond(mut cond.left, cond.pos)
					r := c.comptime_if_cond(mut cond.right, cond.pos)
					if l == .unknown || r == .unknown {
						return .unknown
					}
					return if l == .eval || r == .eval { .eval } else { .skip }
				}
				.key_is, .not_is {
					if cond.left is ast.TypeNode && mut cond.right is ast.TypeNode {
						// `$if Foo is Interface {`
						sym := c.table.sym(cond.right.typ)
						if sym.kind != .interface {
							c.expr(mut cond.left)
						} else {
							return c.check_compatible_types((cond.left as ast.TypeNode).typ,
								cond.right)
						}
						return .unknown
					} else if cond.left is ast.TypeNode && mut cond.right is ast.ComptimeType {
						left := cond.left as ast.TypeNode
						checked_type := c.unwrap_generic(left.typ)
						return if c.type_resolver.is_comptime_type(checked_type, cond.right) {
							.eval
						} else {
							.skip
						}
					} else if cond.left in [ast.Ident, ast.SelectorExpr, ast.TypeNode] {
						// `$if method.type is string`
						c.expr(mut cond.left)
						c.comptime.inside_comptime_if = true
						if mut cond.left is ast.SelectorExpr && cond.right is ast.ComptimeType {
							comptime_type := cond.right as ast.ComptimeType
							if c.comptime.is_comptime_selector_type(cond.left) {
								checked_type := c.type_resolver.get_type(cond.left)
								return if c.type_resolver.is_comptime_type(checked_type,
									comptime_type)
								{
									.eval
								} else {
									.skip
								}
							} else if cond.left.gkind_field == .unaliased_typ
								&& cond.left.name_type != 0 {
								// T.unaliased_typ
								checked_type := c.unwrap_generic(cond.left.name_type)
								return if c.type_resolver.is_comptime_type(c.table.unaliased_type(checked_type),
									comptime_type)
								{
									.eval
								} else {
									.skip
								}
							}
						}
						return .unknown
					} else {
						c.error('invalid `\$if` condition: expected a type or a selector expression or an interface check',
							cond.left.pos())
					}
				}
				.eq, .ne {
					if mut cond.left is ast.SelectorExpr
						&& cond.right in [ast.IntegerLiteral, ast.StringLiteral] {
						// $if field.indirections == 1
						// $if method.args.len == 1
						if cond.left.typ == 0 && cond.left.name_type == 0
							&& (cond.left.expr is ast.Ident && cond.left.expr.name.len == 1) {
							c.expr(mut cond.left)
						}
						return .unknown
					} else if cond.left is ast.SelectorExpr
						&& c.comptime.check_comptime_is_field_selector_bool(cond.left) {
						// field.is_public (from T.fields)
					} else if cond.right is ast.SelectorExpr
						&& c.comptime.check_comptime_is_field_selector_bool(cond.right) {
						// field.is_public (from T.fields)
					} else if cond.left is ast.Ident {
						// $if version == 2
						left_type := c.expr(mut cond.left)
						right_type := c.expr(mut cond.right)
						expr := c.find_definition(cond.left as ast.Ident) or {
							c.error(err.msg(), cond.left.pos())
							return .unknown
						}
						if !c.check_types(right_type, left_type) {
							left_name := c.table.type_to_str(left_type)
							right_name := c.table.type_to_str(right_type)
							c.error('mismatched types `${left_name}` and `${right_name}`',
								cond.pos)
						}
						// :)
						// until `v.eval` is stable, I can't think of a better way to do this
						different := expr.str() != cond.right.str()
						return if cond.op == .eq {
							if different {
								ComptimeBranchSkipState.skip
							} else {
								ComptimeBranchSkipState.eval
							}
						} else {
							if different {
								ComptimeBranchSkipState.eval
							} else {
								ComptimeBranchSkipState.skip
							}
						}
					} else if cond.left is ast.SizeOf {
					} else {
						c.error('invalid `\$if` condition: ${cond.left.type_name()}',
							cond.pos)
					}
				}
				.key_in, .not_in {
					if mut cond.right is ast.ArrayInit
						&& cond.left in [ast.TypeNode, ast.SelectorExpr, ast.Ident] {
						c.expr(mut cond.left)
						for expr in cond.right.exprs {
							if expr !in [ast.ComptimeType, ast.TypeNode] {
								c.error('invalid `\$if` condition, only types are allowed',
									expr.pos())
							}
						}
						return .unknown
					} else {
						c.error('invalid `\$if` condition', cond.pos)
					}
				}
				.gt, .lt, .ge, .le {
					if cond.left is ast.SelectorExpr && cond.left.typ == 0
						&& cond.left.name_type == 0
						&& (cond.left.expr is ast.Ident && cond.left.expr.name.len == 1) {
						c.expr(mut cond.left)
					}
					if cond.left is ast.SelectorExpr && cond.right is ast.IntegerLiteral
						&& c.comptime.is_comptime_selector_field_name(cond.left, 'indirections') {
						return .unknown
					} else if cond.left is ast.SizeOf {
						return .unknown
					}
					c.error('invalid `\$if` condition', cond.pos)
				}
				else {
					c.error('invalid `\$if` condition', cond.pos)
				}
			}
		}
		ast.Ident {
			cname := cond.name
			should_record_ident = true
			is_user_ident = false
			ident_name = cname
			if cname in ast.valid_comptime_if_os {
				mut ident_result := ComptimeBranchSkipState.skip
				if !c.pref.output_cross_c {
					if cname_enum_val := pref.os_from_string(cname) {
						if cname_enum_val == c.pref.os {
							ident_result = .eval
						}
					}
				}
				$if trace_comptime_os_checks ? {
					eprintln('>>> ident_name: ${ident_name} | c.pref.os: ${c.pref.os} | ident_result: ${ident_result}')
				}
				return ident_result
			} else if cname in ast.valid_comptime_if_compilers {
				return if pref.cc_from_string(cname) == c.pref.ccompiler_type {
					.eval
				} else {
					.skip
				}
			} else if cname in ast.valid_comptime_if_platforms {
				if cname == 'aarch64' {
					c.note('use `arm64` instead of `aarch64`', pos)
				}
				match cname {
					'amd64' { return if c.pref.arch == .amd64 { .eval } else { .skip } }
					'i386' { return if c.pref.arch == .i386 { .eval } else { .skip } }
					'aarch64' { return if c.pref.arch == .arm64 { .eval } else { .skip } }
					'arm64' { return if c.pref.arch == .arm64 { .eval } else { .skip } }
					'arm32' { return if c.pref.arch == .arm32 { .eval } else { .skip } }
					'rv64' { return if c.pref.arch == .rv64 { .eval } else { .skip } }
					'rv32' { return if c.pref.arch == .rv32 { .eval } else { .skip } }
					else { return .unknown }
				}
			} else if cname in ast.valid_comptime_if_cpu_features {
				match cname {
					'x64' { return if c.pref.m64 { .eval } else { .skip } }
					'x32' { return if !c.pref.m64 { .eval } else { .skip } }
					else { return .unknown }
				}
			} else if cname in ast.valid_comptime_if_other {
				match cname {
					'apk' {
						return if c.pref.is_apk { .eval } else { .skip }
					}
					'js' {
						return if c.pref.backend.is_js() { .eval } else { .skip }
					}
					'debug' {
						return if c.pref.is_debug { .eval } else { .skip }
					}
					'prod' {
						return if c.pref.is_prod { .eval } else { .skip }
					}
					'profile' {
						return if c.pref.is_prof { .eval } else { .skip }
					}
					'test' {
						return if c.pref.is_test { .eval } else { .skip }
					}
					'musl' {
						return .unknown
					}
					'glibc' {
						return .unknown
					}
					'threads' {
						return if c.table.gostmts > 0 { .eval } else { .skip }
					}
					'prealloc' {
						return if c.pref.prealloc { .eval } else { .skip }
					}
					'no_bounds_checking' {
						return if cname in c.pref.compile_defines_all { .eval } else { .skip }
					}
					'autofree' {
						return if c.pref.autofree { .eval } else { .skip }
					}
					'freestanding' {
						return if c.pref.is_bare && !c.pref.output_cross_c { .eval } else { .skip }
					}
					'interpreter' {
						return if c.pref.backend == .interpret { .eval } else { .skip }
					}
					else {
						return .unknown
					}
				}
			} else if cname !in c.pref.compile_defines_all {
				if cname == 'linux_or_macos' {
					c.error('linux_or_macos is deprecated, use `\$if linux || macos {` instead',
						cond.pos)
					return .unknown
				}
				// `$if some_var {}`, or `[if user_defined_tag] fn abc(){}`
				typ := c.unwrap_generic(c.expr(mut cond))
				if cond.obj !in [ast.Var, ast.ConstField, ast.GlobalField] {
					if !c.inside_ct_attr {
						c.error('unknown var: `${cname}`', pos)
					}
					return .unknown
				}
				expr := c.find_obj_definition(cond.obj) or {
					c.error(err.msg(), cond.pos)
					return .unknown
				}
				if !c.check_types(typ, ast.bool_type) {
					type_name := c.table.type_to_str(typ)
					c.error('non-bool type `${type_name}` used as \$if condition', cond.pos)
				}
				// :)
				// until `v.eval` is stable, I can't think of a better way to do this
				return if (expr as ast.BoolLiteral).val { .eval } else { .skip }
			}
		}
		ast.ComptimeCall {
			if cond.is_pkgconfig {
				mut m := pkgconfig.main([cond.args_var]) or {
					c.error(err.msg(), cond.pos)
					return .skip
				}
				m.run() or { return .skip }
				return .eval
			}
			if cond.is_compile_value {
				t := c.expr(mut cond)
				if t != ast.bool_type {
					c.error('inside \$if, only \$d() expressions that return bool are allowed',
						cond.pos)
					return .skip
				}
				return .unknown // always fully generate the code for that branch
			}
			return .eval
		}
		ast.SelectorExpr {
			if c.comptime.check_comptime_is_field_selector(cond) {
				if c.comptime.check_comptime_is_field_selector_bool(cond) {
					ret_bool := c.type_resolver.get_comptime_selector_bool_field(cond.field_name)
					return if ret_bool { .eval } else { .skip }
				}
				c.error('unknown field `${cond.field_name}` from ${c.comptime.comptime_for_field_var}',
					cond.pos)
			}
			return .unknown
		}
		else {
			c.error('invalid `\$if` condition', pos)
		}
	}
	return .unknown
}

// push_new_comptime_info saves the current comptime information
fn (mut c Checker) push_new_comptime_info() {
	c.type_resolver.info_stack << type_resolver.ResolverInfo{
		saved_type_map:               c.type_resolver.type_map.clone()
		inside_comptime_for:          c.comptime.inside_comptime_for
		inside_comptime_if:           c.comptime.inside_comptime_if
		has_different_types:          c.comptime.has_different_types
		comptime_for_variant_var:     c.comptime.comptime_for_variant_var
		comptime_for_field_var:       c.comptime.comptime_for_field_var
		comptime_for_field_type:      c.comptime.comptime_for_field_type
		comptime_for_field_value:     c.comptime.comptime_for_field_value
		comptime_for_enum_var:        c.comptime.comptime_for_enum_var
		comptime_for_method_var:      c.comptime.comptime_for_method_var
		comptime_for_method:          c.comptime.comptime_for_method
		comptime_for_method_ret_type: c.comptime.comptime_for_method_ret_type
	}
}

// pop_comptime_info pops the current comptime information frame
fn (mut c Checker) pop_comptime_info() {
	old := c.type_resolver.info_stack.pop()
	c.type_resolver.type_map = old.saved_type_map.clone()
	c.comptime.inside_comptime_for = old.inside_comptime_for
	c.comptime.inside_comptime_if = old.inside_comptime_if
	c.comptime.has_different_types = old.has_different_types
	c.comptime.comptime_for_variant_var = old.comptime_for_variant_var
	c.comptime.comptime_for_field_var = old.comptime_for_field_var
	c.comptime.comptime_for_field_type = old.comptime_for_field_type
	c.comptime.comptime_for_field_value = old.comptime_for_field_value
	c.comptime.comptime_for_enum_var = old.comptime_for_enum_var
	c.comptime.comptime_for_method_var = old.comptime_for_method_var
	c.comptime.comptime_for_method = old.comptime_for_method
	c.comptime.comptime_for_method_ret_type = old.comptime_for_method_ret_type
}

fn overflows_i8(val i64) bool {
	return val > max_i8 || val < min_i8
}

fn overflows_i16(val i64) bool {
	return val > max_i16 || val < min_i16
}

fn overflows_i32(val i64) bool {
	return val > max_i32 || val < min_i32
}

fn overflows_u8(val i64) bool {
	return val > max_u8 || val < min_u8
}

fn overflows_u16(val i64) bool {
	return val > max_u16 || val < min_u16
}

fn overflows_u32(val i64) bool {
	return val > max_u32 || val < min_u32
}
