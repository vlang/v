// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.table
import v.pref
import v.util

fn (mut c Checker) fn_decl(mut node ast.FnDecl) {
	c.returns = false
	if node.generic_params.len > 0 && c.cur_generic_types.len == 0 {
		// Just remember the generic function for now.
		// It will be processed later in c.post_process_generic_fns,
		// after all other normal functions are processed.
		// This is done so that all generic function calls can
		// have a chance to populate c.table.fn_gen_types with
		// the correct concrete types.
		c.file.generic_fns << node
		return
	}
	if node.language == .v && !c.is_builtin_mod {
		c.check_valid_snake_case(node.name, 'function name', node.pos)
	}
	if node.name == 'main.main' {
		c.main_fn_decl_node = node
	}
	if node.return_type != table.void_type {
		for attr in node.attrs {
			if attr.is_comptime_define {
				c.error('only functions that do NOT return values can have `[if $attr.name]` tags',
					node.pos)
				break
			}
		}
	}
	if node.is_method {
		mut sym := c.table.get_type_symbol(node.receiver.typ)
		if sym.kind == .array && !c.is_builtin_mod && node.name == 'map' {
			// TODO `node.map in array_builtin_methods`
			c.error('method overrides built-in array method', node.pos)
		} else if sym.kind == .sum_type && node.name == 'type_name' {
			c.error('method overrides built-in sum type method', node.pos)
		} else if sym.kind == .multi_return {
			c.error('cannot define method on multi-value', node.method_type_pos)
		}
		if sym.name.len == 1 {
			// One letter types are reserved for generics.
			c.error('unknown type `$sym.name`', node.receiver_pos)
			return
		}
		// make sure interface does not implement its own interface methods
		if sym.kind == .interface_ && sym.has_method(node.name) {
			if sym.info is table.Interface {
				info := sym.info as table.Interface
				// if the method is in info.methods then it is an interface method
				if info.has_method(node.name) {
					c.error('interface `$sym.name` cannot implement its own interface method `$node.name`',
						node.pos)
				}
			}
		}
		// needed for proper error reporting during vweb route checking
		sym.methods[node.method_idx].source_fn = voidptr(node)
	}
	if node.language == .v {
		// Make sure all types are valid
		for arg in node.params {
			c.ensure_type_exists(arg.typ, node.pos) or { return }
		}
	}
	if node.language == .v && node.name.after_char(`.`) == 'init' && !node.is_method
		&& node.params.len == 0 {
		if node.is_pub {
			c.error('fn `init` must not be public', node.pos)
		}
		if node.return_type != table.void_type {
			c.error('fn `init` cannot have a return type', node.pos)
		}
	}
	if node.return_type != table.Type(0) {
		c.ensure_type_exists(node.return_type, node.pos) or { return }
		if node.language == .v && node.is_method && node.name == 'str' {
			if node.return_type != table.string_type {
				c.error('.str() methods should return `string`', node.pos)
			}
			if node.params.len != 1 {
				c.error('.str() methods should have 0 arguments', node.pos)
			}
		}
		if node.language == .v && node.is_method
			&& node.name in ['+', '-', '*', '%', '/', '<', '=='] {
			if node.params.len != 2 {
				c.error('operator methods should have exactly 1 argument', node.pos)
			} else {
				receiver_sym := c.table.get_type_symbol(node.receiver.typ)
				param_sym := c.table.get_type_symbol(node.params[1].typ)
				if param_sym.kind !in [.struct_, .alias] || receiver_sym.kind !in [.struct_, .alias] {
					c.error('operator methods are only allowed for struct and type alias',
						node.pos)
				} else {
					parent_sym := c.table.get_final_type_symbol(node.receiver.typ)
					if node.rec_mut {
						c.error('receiver cannot be `mut` for operator overloading', node.receiver_pos)
					} else if node.params[1].is_mut {
						c.error('argument cannot be `mut` for operator overloading', node.pos)
					} else if node.receiver.typ != node.params[1].typ {
						c.error('expected `$receiver_sym.name` not `$param_sym.name` - both operands must be the same type for operator overloading',
							node.params[1].type_pos)
					} else if node.name in ['<', '=='] && node.return_type != table.bool_type {
						c.error('operator comparison methods should return `bool`', node.pos)
					} else if parent_sym.is_primitive() {
						c.error('cannot define operator methods on type alias for `$parent_sym.name`',
							node.pos)
					}
				}
			}
		}
	}
	// TODO c.pref.is_vet
	if node.language == .v && !node.is_method && node.params.len == 0 && node.is_test {
		if !c.pref.is_test {
			// simple heuristic
			for st in node.stmts {
				if st is ast.AssertStmt {
					c.warn('tests will not be run, because filename does not end with `_test.v`',
						node.pos)
					break
				}
			}
		}
		if node.return_type != table.void_type_idx
			&& node.return_type.clear_flag(.optional) != table.void_type_idx {
			c.error('test functions should either return nothing at all, or be marked to return `?`',
				node.pos)
		}
	}
	c.expected_type = table.void_type
	c.cur_fn = node
	// Add return if `fn(...) ? {...}` have no return at end
	if node.return_type != table.void_type && node.return_type.has_flag(.optional)
		&& (node.stmts.len == 0 || node.stmts[node.stmts.len - 1] !is ast.Return) {
		sym := c.table.get_type_symbol(node.return_type)
		if sym.kind == .void {
			node.stmts << ast.Return{
				pos: node.pos
			}
		}
	}
	c.fn_scope = node.scope
	c.stmts(node.stmts)
	node.has_return = c.returns || has_top_return(node.stmts)
	if node.language == .v && !node.no_body && node.return_type != table.void_type
		&& !node.has_return && node.name !in ['panic', 'exit'] {
		if c.inside_anon_fn {
			c.error('missing return at the end of an anonymous function', node.pos)
		} else {
			c.error('missing return at end of function `$node.name`', node.pos)
		}
	}
	c.returns = false
	node.source_file = c.file
}

pub fn (mut c Checker) call_expr(mut call_expr ast.CallExpr) table.Type {
	// First check everything that applies to both fns and methods
	// TODO merge logic from call_method and call_fn
	/*
	for i, call_arg in call_expr.args {
		if call_arg.is_mut {
			c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else if arg.typ.share() != call_arg.share {
				c.error('wrong shared type', call_arg.expr.position())
			}
		} else {
			if arg.is_mut && (!call_arg.is_mut || arg.typ.share() != call_arg.share) {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i+1}`',
					call_arg.expr.position())
			}
		}
	}
	*/
	// Now call `call_method` or `call_fn` for specific checks.
	typ := if call_expr.is_method { c.call_method(mut call_expr) } else { c.call_fn(mut call_expr) }
	// autofree: mark args that have to be freed (after saving them in tmp exprs)
	free_tmp_arg_vars := c.pref.autofree && !c.is_builtin_mod && call_expr.args.len > 0
		&& !call_expr.args[0].typ.has_flag(.optional)
	if free_tmp_arg_vars && !c.inside_const {
		for i, arg in call_expr.args {
			if arg.typ != table.string_type {
				continue
			}
			if arg.expr is ast.Ident || arg.expr is ast.StringLiteral
				|| arg.expr is ast.SelectorExpr {
				// Simple expressions like variables, string literals, selector expressions
				// (`x.field`) can't result in allocations and don't need to be assigned to
				// temporary vars.
				// Only expressions like `str + 'b'` need to be freed.
				continue
			}
			call_expr.args[i].is_tmp_autofree = true
		}
		// TODO copy pasta from above
		if call_expr.receiver_type == table.string_type && !(call_expr.left is ast.Ident
			|| call_expr.left is ast.StringLiteral
			|| call_expr.left is ast.SelectorExpr) {
			call_expr.free_receiver = true
		}
	}
	c.expected_or_type = call_expr.return_type.clear_flag(.optional)
	c.stmts(call_expr.or_block.stmts)
	c.expected_or_type = table.void_type
	if call_expr.or_block.kind == .propagate && !c.cur_fn.return_type.has_flag(.optional)
		&& !c.inside_const {
		if !c.cur_fn.is_main {
			c.error('to propagate the optional call, `$c.cur_fn.name` must return an optional',
				call_expr.or_block.pos)
		}
	}
	return typ
}

pub fn (mut c Checker) call_fn(mut call_expr ast.CallExpr) table.Type {
	fn_name := call_expr.name
	if fn_name == 'main' {
		c.error('the `main` function cannot be called in the program', call_expr.pos)
	}
	if fn_name == 'typeof' {
		// TODO: impl typeof properly (probably not going to be a fn call)
		return table.string_type
	}
	mut has_generic_generic := false // foo<T>() instead of foo<int>()
	mut generic_types := []table.Type{}
	for generic_type in call_expr.generic_types {
		if generic_type.has_flag(.generic) {
			has_generic_generic = true
			generic_types << c.unwrap_generic(generic_type)
		} else {
			generic_types << generic_type
		}
	}
	if has_generic_generic {
		if c.mod != '' {
			// Need to prepend the module when adding a generic type to a function
			c.table.register_fn_gen_type(c.mod + '.' + fn_name, generic_types)
		} else {
			c.table.register_fn_gen_type(fn_name, generic_types)
		}
	}
	if fn_name == 'json.encode' {
	} else if fn_name == 'json.decode' && call_expr.args.len > 0 {
		if call_expr.args.len != 2 {
			c.error("json.decode expects 2 arguments, a type and a string (e.g `json.decode(T, '')`)",
				call_expr.pos)
			return table.void_type
		}
		expr := call_expr.args[0].expr
		if expr !is ast.Type {
			typ := expr.type_name()
			c.error('json.decode: first argument needs to be a type, got `$typ`', call_expr.pos)
			return table.void_type
		}
		c.expected_type = table.string_type
		call_expr.args[1].typ = c.expr(call_expr.args[1].expr)
		if call_expr.args[1].typ != table.string_type {
			c.error('json.decode: second argument needs to be a string', call_expr.pos)
		}
		typ := expr as ast.Type
		ret_type := typ.typ.set_flag(.optional)
		call_expr.return_type = ret_type
		return ret_type
	}
	// look for function in format `mod.fn` or `fn` (builtin)
	mut f := table.Fn{}
	mut found := false
	mut found_in_args := false
	// anon fn direct call
	if mut call_expr.left is ast.AnonFn {
		// it was set to anon for checker errors, clear for gen
		call_expr.name = ''
		c.expr(call_expr.left)
		anon_fn_sym := c.table.get_type_symbol(call_expr.left.typ)
		f = (anon_fn_sym.info as table.FnType).func
		found = true
	}
	// try prefix with current module as it would have never gotten prefixed
	if !found && !fn_name.contains('.') && call_expr.mod != 'builtin' {
		name_prefixed := '${call_expr.mod}.$fn_name'
		if f1 := c.table.find_fn(name_prefixed) {
			call_expr.name = name_prefixed
			found = true
			f = f1
			c.table.fns[name_prefixed].usages++
		}
	}
	if !found && call_expr.left is ast.IndexExpr {
		c.expr(call_expr.left)
		expr := call_expr.left as ast.IndexExpr
		sym := c.table.get_type_symbol(expr.left_type)
		if sym.kind == .array {
			info := sym.info as table.Array
			elem_typ := c.table.get_type_symbol(info.elem_type)
			if elem_typ.info is table.FnType {
				return elem_typ.info.func.return_type
			}
		} else if sym.kind == .map {
			info := sym.info as table.Map
			value_typ := c.table.get_type_symbol(info.value_type)
			if value_typ.info is table.FnType {
				return value_typ.info.func.return_type
			}
		} else if sym.kind == .array_fixed {
			info := sym.info as table.ArrayFixed
			elem_typ := c.table.get_type_symbol(info.elem_type)
			if elem_typ.info is table.FnType {
				return elem_typ.info.func.return_type
			}
		}
		found = true
		return table.string_type
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f1 := c.table.find_fn(fn_name) {
			found = true
			f = f1
			c.table.fns[fn_name].usages++
		}
	}
	if c.pref.is_script && !found {
		os_name := 'os.$fn_name'
		if f1 := c.table.find_fn(os_name) {
			if f1.generic_names.len == call_expr.generic_types.len {
				c.table.fn_gen_types[os_name] = c.table.fn_gen_types['${call_expr.mod}.$call_expr.name']
			}
			call_expr.name = os_name
			found = true
			f = f1
			c.table.fns[os_name].usages++
		}
	}
	// check for arg (var) of fn type
	if !found {
		if v := call_expr.scope.find_var(fn_name) {
			if v.typ != 0 {
				vts := c.table.get_type_symbol(v.typ)
				if vts.kind == .function {
					info := vts.info as table.FnType
					f = info.func
					found = true
					found_in_args = true
				}
			}
		}
	}
	if !found {
		c.error('unknown function: $fn_name', call_expr.pos)
		return table.void_type
	}
	if !found_in_args {
		if _ := call_expr.scope.find_var(fn_name) {
			c.error('ambiguous call to: `$fn_name`, may refer to fn `$fn_name` or variable `$fn_name`',
				call_expr.pos)
		}
	}
	if !f.is_pub && f.language == .v && f.name.len > 0 && f.mod.len > 0 && f.mod != c.mod {
		c.error('function `$f.name` is private', call_expr.pos)
	}
	if !c.cur_fn.is_deprecated && f.is_deprecated {
		mut deprecation_message := 'function `$f.name` has been deprecated'
		for d in f.attrs {
			if d.name == 'deprecated' && d.arg != '' {
				deprecation_message += '; $d.arg'
			}
		}
		c.warn(deprecation_message, call_expr.pos)
	}
	if f.is_unsafe && !c.inside_unsafe
		&& (f.language != .c || (f.name[2] in [`m`, `s`] && f.mod == 'builtin')) {
		// builtin C.m*, C.s* only - temp
		c.warn('function `$f.name` must be called from an `unsafe` block', call_expr.pos)
	}
	if f.mod != 'builtin' && f.language == .v && f.no_body && !c.pref.translated && !f.is_unsafe {
		c.error('cannot call a function that does not have a body', call_expr.pos)
	}
	for generic_type in call_expr.generic_types {
		c.ensure_type_exists(generic_type, call_expr.generic_list_pos) or {}
	}
	if f.generic_names.len > 0 && f.return_type.has_flag(.generic) {
		rts := c.table.get_type_symbol(f.return_type)
		if rts.info is table.Struct {
			if rts.info.generic_types.len > 0 {
				gts := c.table.get_type_symbol(call_expr.generic_types[0])
				nrt := '$rts.name<$gts.name>'
				idx := c.table.type_idxs[nrt]
				c.ensure_type_exists(idx, call_expr.pos) or {}
				call_expr.return_type = table.new_type(idx).derive(f.return_type)
			}
		}
	} else {
		call_expr.return_type = f.return_type
	}
	if f.return_type == table.void_type && f.ctdefine.len > 0
		&& f.ctdefine !in c.pref.compile_defines {
		call_expr.should_be_skipped = true
	}
	// dont check number of args for JS functions since arguments are not required
	if call_expr.language != .js {
		min_required_args := if f.is_variadic { f.params.len - 1 } else { f.params.len }
		if call_expr.args.len < min_required_args {
			c.error('expected $min_required_args arguments, but got $call_expr.args.len',
				call_expr.pos)
		} else if !f.is_variadic && call_expr.args.len > f.params.len {
			unexpected_arguments := call_expr.args[min_required_args..]
			unexpected_arguments_pos := unexpected_arguments[0].pos.extend(unexpected_arguments.last().pos)
			c.error('expected $min_required_args arguments, but got $call_expr.args.len',
				unexpected_arguments_pos)
			return f.return_type
		}
	}
	// println / eprintln / panic can print anything
	if fn_name in ['println', 'print', 'eprintln', 'eprint', 'panic'] && call_expr.args.len > 0 {
		c.inside_println_arg = true
		c.expected_type = table.string_type
		call_expr.args[0].typ = c.expr(call_expr.args[0].expr)
		if call_expr.args[0].typ.is_void() {
			c.error('`$fn_name` can not print void expressions', call_expr.pos)
		}
		c.fail_if_unreadable(call_expr.args[0].expr, call_expr.args[0].typ, 'argument to print')
		c.inside_println_arg = false
		/*
		// TODO: optimize `struct T{} fn (t &T) str() string {return 'abc'} mut a := []&T{} a << &T{} println(a[0])`
		// It currently generates:
		// `println(T_str_no_ptr(*(*(T**)array_get(a, 0))));`
		// ... which works, but could be just:
		// `println(T_str(*(T**)array_get(a, 0)));`
		prexpr := call_expr.args[0].expr
		prtyp := call_expr.args[0].typ
		prtyp_sym := c.table.get_type_symbol(prtyp)
		prtyp_is_ptr := prtyp.is_ptr()
		prhas_str, prexpects_ptr, prnr_args := prtyp_sym.str_method_info()
		eprintln('>>> println hack typ: ${prtyp} | sym.name: ${prtyp_sym.name} | is_ptr: $prtyp_is_ptr | has_str: $prhas_str | expects_ptr: $prexpects_ptr | nr_args: $prnr_args | expr: ${prexpr.str()} ')
		*/
		return f.return_type
	}
	// `return error(err)` -> `return err`
	if fn_name == 'error' {
		arg := call_expr.args[0]
		call_expr.args[0].typ = c.expr(arg.expr)
		if call_expr.args[0].typ == table.error_type {
			c.warn('`error($arg)` can be shortened to just `$arg`', call_expr.pos)
		}
	}
	// TODO: typ optimize.. this node can get processed more than once
	if call_expr.expected_arg_types.len == 0 {
		for param in f.params {
			call_expr.expected_arg_types << param.typ
		}
	}
	for i, call_arg in call_expr.args {
		arg := if f.is_variadic && i >= f.params.len - 1 {
			f.params[f.params.len - 1]
		} else {
			f.params[i]
		}
		if f.is_variadic && call_arg.expr is ast.ArrayDecompose {
			if i > f.params.len - 1 {
				c.error('too many arguments in call to `$f.name`', call_expr.pos)
			}
		}
		c.expected_type = arg.typ
		typ := c.check_expr_opt_call(call_arg.expr, c.expr(call_arg.expr))
		call_expr.args[i].typ = typ
		typ_sym := c.table.get_type_symbol(typ)
		arg_typ_sym := c.table.get_type_symbol(arg.typ)
		if f.is_variadic && typ.has_flag(.variadic) && call_expr.args.len - 1 > i {
			c.error('when forwarding a variadic variable, it must be the final argument',
				call_arg.pos)
		}
		arg_share := arg.typ.share()
		if arg_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('function with `shared` arguments cannot be called inside `lock`/`rlock` block',
				call_arg.pos)
		}
		if call_arg.is_mut && f.language == .v {
			to_lock, pos := c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else {
				if arg.typ.share() != call_arg.share {
					c.error('wrong shared type', call_arg.expr.position())
				}
				if to_lock != '' && !arg.typ.has_flag(.shared_f) {
					c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
						pos)
				}
			}
		} else {
			if arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$call_expr.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
					i + 1}`', call_arg.expr.position())
			} else {
				c.fail_if_unreadable(call_arg.expr, typ, 'argument')
			}
		}
		// Handle expected interface
		if arg_typ_sym.kind == .interface_ {
			c.type_implements(typ, arg.typ, call_arg.expr.position())
			continue
		}
		c.check_expected_call_arg(typ, arg.typ, call_expr.language) or {
			// str method, allow type with str method if fn arg is string
			// Passing an int or a string array produces a c error here
			// Deleting this condition results in propper V error messages
			// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
			// continue
			// }
			if typ_sym.kind == .void && arg_typ_sym.kind == .string {
				continue
			}
			if f.generic_names.len > 0 {
				continue
			}
			c.error('$err.msg in argument ${i + 1} to `$fn_name`', call_arg.pos)
		}
	}
	if f.generic_names.len != call_expr.generic_types.len {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_types(f, mut call_expr)
	}
	if call_expr.generic_types.len > 0 && f.return_type != 0 {
		if typ := c.resolve_generic_type(f.return_type, f.generic_names, call_expr.generic_types) {
			call_expr.return_type = typ
			return typ
		}
	}
	if call_expr.generic_types.len > 0 && f.generic_names.len == 0 {
		c.error('a non generic function called like a generic one', call_expr.generic_list_pos)
	}
	if f.generic_names.len > 0 {
		return call_expr.return_type
	}
	return f.return_type
}

pub fn (mut c Checker) call_method(mut call_expr ast.CallExpr) table.Type {
	left_type := c.expr(call_expr.left)
	c.expected_type = left_type
	is_generic := left_type.has_flag(.generic)
	call_expr.left_type = left_type
	// Set default values for .return_type & .receiver_type too,
	// or there will be hard to diagnose 0 type panics in cgen.
	call_expr.return_type = left_type
	call_expr.receiver_type = left_type
	left_type_sym := c.table.get_type_symbol(c.unwrap_generic(left_type))
	method_name := call_expr.name
	mut unknown_method_msg := 'unknown method: `${left_type_sym.name}.$method_name`'
	if left_type.has_flag(.optional) {
		c.error('optional type cannot be called directly', call_expr.left.position())
		return table.void_type
	}
	if left_type_sym.kind in [.sum_type, .interface_] && method_name == 'type_name' {
		return table.string_type
	}
	mut has_generic_generic := false // x.foo<T>() instead of x.foo<int>()
	mut generic_types := []table.Type{}
	for generic_type in call_expr.generic_types {
		if generic_type.has_flag(.generic) {
			has_generic_generic = true
			generic_types << c.unwrap_generic(generic_type)
		} else {
			generic_types << generic_type
		}
	}
	if has_generic_generic {
		if c.mod != '' {
			// Need to prepend the module when adding a generic type to a function
			c.table.register_fn_gen_type(c.mod + '.' + call_expr.name, generic_types)
		} else {
			c.table.register_fn_gen_type(call_expr.name, generic_types)
		}
	}
	// TODO: remove this for actual methods, use only for compiler magic
	// FIXME: Argument count != 1 will break these
	if left_type_sym.kind == .array && method_name in array_builtin_methods {
		return c.call_array_builtin_method(mut call_expr, left_type, left_type_sym)
	} else if left_type_sym.kind == .map && method_name in ['clone', 'keys', 'move'] {
		return c.call_map_builtin_method(mut call_expr, left_type, left_type_sym)
	} else if left_type_sym.kind == .array && method_name in ['insert', 'prepend'] {
		info := left_type_sym.info as table.Array
		arg_expr := if method_name == 'insert' {
			call_expr.args[1].expr
		} else {
			call_expr.args[0].expr
		}
		arg_type := c.expr(arg_expr)
		arg_sym := c.table.get_type_symbol(arg_type)
		if !c.check_types(arg_type, info.elem_type) && !c.check_types(left_type, arg_type) {
			c.error('cannot $method_name `$arg_sym.name` to `$left_type_sym.name`', arg_expr.position())
		}
	} else if left_type_sym.kind == .thread && method_name == 'wait' {
		info := left_type_sym.info as table.Thread
		if call_expr.args.len > 0 {
			c.error('wait() does not have any arguments', call_expr.args[0].pos)
		}
		call_expr.return_type = info.return_type
		return info.return_type
	}
	mut method := table.Fn{}
	mut has_method := false
	mut is_method_from_embed := false
	if m := c.table.type_find_method(left_type_sym, method_name) {
		method = m
		has_method = true
	} else {
		// can this logic be moved to table.type_find_method() so it can be used from anywhere
		if left_type_sym.info is table.Struct {
			mut found_methods := []table.Fn{}
			mut embed_of_found_methods := []table.Type{}
			for embed in left_type_sym.info.embeds {
				embed_sym := c.table.get_type_symbol(embed)
				if m := c.table.type_find_method(embed_sym, method_name) {
					found_methods << m
					embed_of_found_methods << embed
				}
			}
			if found_methods.len == 1 {
				method = found_methods[0]
				has_method = true
				is_method_from_embed = true
				call_expr.from_embed_type = embed_of_found_methods[0]
			} else if found_methods.len > 1 {
				c.error('ambiguous method `$method_name`', call_expr.pos)
			}
		}
		if left_type_sym.kind == .aggregate {
			// the error message contains the problematic type
			unknown_method_msg = err.msg
		}
	}
	if has_method {
		if !method.is_pub && !c.pref.is_test && method.mod != c.mod {
			// If a private method is called outside of the module
			// its receiver type is defined in, show an error.
			// println('warn $method_name lef.mod=$left_type_sym.mod c.mod=$c.mod')
			c.error('method `${left_type_sym.name}.$method_name` is private', call_expr.pos)
		}
		rec_share := method.params[0].typ.share()
		if rec_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('method with `shared` receiver cannot be called inside `lock`/`rlock` block',
				call_expr.pos)
		}
		if method.params[0].is_mut {
			to_lock, pos := c.fail_if_immutable(call_expr.left)
			// call_expr.is_mut = true
			if to_lock != '' && rec_share != .shared_t {
				c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
					pos)
			}
		} else {
			c.fail_if_unreadable(call_expr.left, left_type, 'receiver')
		}
		if (!left_type_sym.is_builtin() && method.mod != 'builtin') && method.language == .v
			&& method.no_body {
			c.error('cannot call a method that does not have a body', call_expr.pos)
		}
		if method.return_type == table.void_type && method.ctdefine.len > 0
			&& method.ctdefine !in c.pref.compile_defines {
			call_expr.should_be_skipped = true
		}
		nr_args := if method.params.len == 0 { 0 } else { method.params.len - 1 }
		min_required_args := method.params.len - if method.is_variadic && method.params.len > 1 {
			2
		} else {
			1
		}
		if call_expr.args.len < min_required_args {
			c.error('expected $min_required_args arguments, but got $call_expr.args.len',
				call_expr.pos)
		} else if !method.is_variadic && call_expr.args.len > nr_args {
			unexpected_arguments := call_expr.args[min_required_args..]
			unexpected_arguments_pos := unexpected_arguments[0].pos.extend(unexpected_arguments.last().pos)
			c.error('expected $nr_args arguments, but got $call_expr.args.len', unexpected_arguments_pos)
			return method.return_type
		}
		// if method_name == 'clone' {
		// println('CLONE nr args=$method.args.len')
		// }
		// call_expr.args << method.args[0].typ
		// call_expr.exp_arg_types << method.args[0].typ
		for i, arg in call_expr.args {
			exp_arg_typ := if method.is_variadic && i >= method.params.len - 1 {
				method.params[method.params.len - 1].typ
			} else {
				method.params[i + 1].typ
			}
			exp_arg_sym := c.table.get_type_symbol(exp_arg_typ)
			c.expected_type = exp_arg_typ
			got_arg_typ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
			call_expr.args[i].typ = got_arg_typ
			if method.is_variadic && got_arg_typ.has_flag(.variadic) && call_expr.args.len - 1 > i {
				c.error('when forwarding a variadic variable, it must be the final argument',
					arg.pos)
			}
			if exp_arg_sym.kind == .interface_ {
				c.type_implements(got_arg_typ, exp_arg_typ, arg.expr.position())
				continue
			}
			if method.generic_names.len > 0 {
				continue
			}
			c.check_expected_call_arg(got_arg_typ, exp_arg_typ, call_expr.language) or {
				// str method, allow type with str method if fn arg is string
				// Passing an int or a string array produces a c error here
				// Deleting this condition results in propper V error messages
				// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
				// continue
				// }
				if got_arg_typ != table.void_type {
					c.error('$err.msg in argument ${i + 1} to `${left_type_sym.name}.$method_name`',
						arg.pos)
				}
			}
			param := if method.is_variadic && i >= method.params.len - 1 {
				method.params[method.params.len - 1]
			} else {
				method.params[i + 1]
			}
			param_share := param.typ.share()
			if param_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
				c.error('method with `shared` arguments cannot be called inside `lock`/`rlock` block',
					arg.pos)
			}
			if arg.is_mut {
				to_lock, pos := c.fail_if_immutable(arg.expr)
				if !param.is_mut {
					tok := arg.share.str()
					c.error('`$call_expr.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
						arg.expr.position())
				} else {
					if param.typ.share() != arg.share {
						c.error('wrong shared type', arg.expr.position())
					}
					if to_lock != '' && param_share != .shared_t {
						c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
							pos)
					}
				}
			} else {
				if param.is_mut {
					tok := arg.share.str()
					c.error('`$call_expr.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
						i + 1}`', arg.expr.position())
				} else {
					c.fail_if_unreadable(arg.expr, got_arg_typ, 'argument')
				}
			}
		}
		if method.is_unsafe && !c.inside_unsafe {
			c.warn('method `${left_type_sym.name}.$method_name` must be called from an `unsafe` block',
				call_expr.pos)
		}
		if !c.cur_fn.is_deprecated && method.is_deprecated {
			mut deprecation_message := 'method `${left_type_sym.name}.$method.name` has been deprecated'
			for attr in method.attrs {
				if attr.name == 'deprecated' && attr.arg != '' {
					deprecation_message += '; $attr.arg'
				}
			}
			c.warn(deprecation_message, call_expr.pos)
		}
		// TODO: typ optimize.. this node can get processed more than once
		if call_expr.expected_arg_types.len == 0 {
			for i in 1 .. method.params.len {
				call_expr.expected_arg_types << method.params[i].typ
			}
		}
		if is_method_from_embed {
			call_expr.receiver_type = call_expr.from_embed_type.derive(method.params[0].typ)
		} else if is_generic {
			// We need the receiver to be T in cgen.
			// TODO: cant we just set all these to the concrete type in checker? then no need in gen
			call_expr.receiver_type = left_type.derive(method.params[0].typ).set_flag(.generic)
		} else {
			call_expr.receiver_type = method.params[0].typ
		}
		call_expr.return_type = method.return_type
		if method.generic_names.len != call_expr.generic_types.len {
			// no type arguments given in call, attempt implicit instantiation
			c.infer_fn_types(method, mut call_expr)
		}
		if call_expr.generic_types.len > 0 && method.return_type != 0 {
			if typ := c.resolve_generic_type(method.return_type, method.generic_names,
				call_expr.generic_types)
			{
				call_expr.return_type = typ
				return typ
			}
		}
		if call_expr.generic_types.len > 0 && method.generic_names.len == 0 {
			c.error('a non generic function called like a generic one', call_expr.generic_list_pos)
		}
		if method.generic_names.len > 0 {
			return call_expr.return_type
		}
		return method.return_type
	}
	// TODO: str methods
	if method_name == 'str' {
		if left_type_sym.kind == .interface_ {
			iname := left_type_sym.name
			c.error('interface `$iname` does not have a .str() method. Use typeof() instead',
				call_expr.pos)
		}
		call_expr.receiver_type = left_type
		call_expr.return_type = table.string_type
		if call_expr.args.len > 0 {
			c.error('.str() method calls should have no arguments', call_expr.pos)
		}
		c.fail_if_unreadable(call_expr.left, left_type, 'receiver')
		return table.string_type
	}
	// call struct field fn type
	// TODO: can we use SelectorExpr for all? this dosent really belong here
	if field := c.table.find_field(left_type_sym, method_name) {
		field_type_sym := c.table.get_type_symbol(field.typ)
		if field_type_sym.kind == .function {
			// call_expr.is_method = false
			call_expr.is_field = true
			info := field_type_sym.info as table.FnType
			call_expr.return_type = info.func.return_type
			mut earg_types := []table.Type{}
			for mut arg in call_expr.args {
				targ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
				arg.typ = targ
				earg_types << targ
			}
			call_expr.expected_arg_types = earg_types
			return info.func.return_type
		}
	}
	if left_type != table.void_type {
		suggestion := util.new_suggestion(method_name, left_type_sym.methods.map(it.name))
		c.error(suggestion.say(unknown_method_msg), call_expr.pos)
	}
	return table.void_type
}

fn (mut c Checker) call_map_builtin_method(mut call_expr ast.CallExpr, left_type table.Type, left_type_sym table.TypeSymbol) table.Type {
	method_name := call_expr.name
	mut ret_type := table.void_type
	match method_name {
		'clone', 'move' {
			if method_name[0] == `m` {
				c.fail_if_immutable(call_expr.left)
			}
			if call_expr.left.is_auto_deref_var() {
				ret_type = left_type.deref()
			} else {
				ret_type = left_type
			}
		}
		'keys' {
			info := left_type_sym.info as table.Map
			typ := c.table.find_or_register_array(info.key_type)
			ret_type = table.Type(typ)
		}
		else {}
	}
	call_expr.receiver_type = left_type.to_ptr()
	call_expr.return_type = ret_type
	return call_expr.return_type
}

fn (mut c Checker) call_array_builtin_method(mut call_expr ast.CallExpr, left_type table.Type, left_type_sym table.TypeSymbol) table.Type {
	method_name := call_expr.name
	mut elem_typ := table.void_type
	if method_name == 'slice' && !c.is_builtin_mod {
		c.error('.slice() is a private method, use `x[start..end]` instead', call_expr.pos)
	}
	array_info := left_type_sym.info as table.Array
	elem_typ = array_info.elem_type
	if method_name in ['filter', 'map', 'any', 'all'] {
		// position of `it` doesn't matter
		scope_register_it(mut call_expr.scope, call_expr.pos, elem_typ)
	} else if method_name == 'sort' {
		c.fail_if_immutable(call_expr.left)
		// position of `a` and `b` doesn't matter, they're the same
		scope_register_a_b(mut call_expr.scope, call_expr.pos, elem_typ)

		if call_expr.args.len > 1 {
			c.error('expected 0 or 1 argument, but got $call_expr.args.len', call_expr.pos)
		} else if call_expr.args.len == 1 {
			if call_expr.args[0].expr is ast.InfixExpr {
				if call_expr.args[0].expr.op !in [.gt, .lt] {
					c.error('`.sort()` can only use `<` or `>` comparison', call_expr.pos)
				}
				left_name := '${call_expr.args[0].expr.left}'[0]
				right_name := '${call_expr.args[0].expr.right}'[0]
				if left_name !in [`a`, `b`] || right_name !in [`a`, `b`] {
					c.error('`.sort()` can only use `a` or `b` as argument, e.g. `arr.sort(a < b)`',
						call_expr.pos)
				} else if left_name == right_name {
					c.error('`.sort()` cannot use same argument', call_expr.pos)
				}
			} else {
				c.error(
					'`.sort()` requires a `<` or `>` comparison as the first and only argument' +
					'\ne.g. `users.sort(a.id < b.id)`', call_expr.pos)
			}
		}
	} else if method_name == 'wait' {
		elem_sym := c.table.get_type_symbol(elem_typ)
		if elem_sym.kind == .thread {
			if call_expr.args.len != 0 {
				c.error('`.wait()` does not have any arguments', call_expr.args[0].pos)
			}
			thread_ret_type := elem_sym.thread_info().return_type
			if thread_ret_type.has_flag(.optional) {
				c.error('`.wait()` cannot be called for an array when thread functions return optionals. Iterate over the arrays elements instead and handle each returned optional with `or`.',
					call_expr.pos)
			}
			call_expr.return_type = c.table.find_or_register_array(thread_ret_type)
		} else {
			c.error('`$left_type_sym.name` has no method `wait()` (only thread handles and arrays of them have)',
				call_expr.left.position())
		}
	}
	// map/filter are supposed to have 1 arg only
	mut arg_type := left_type
	for arg in call_expr.args {
		arg_type = c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
	}
	if method_name == 'map' {
		// check fn
		c.check_map_and_filter(true, elem_typ, call_expr)
		arg_sym := c.table.get_type_symbol(arg_type)
		ret_type := match arg_sym.info {
			table.FnType { arg_sym.info.func.return_type }
			else { arg_type }
		}
		call_expr.return_type = c.table.find_or_register_array(ret_type)
	} else if method_name == 'filter' {
		// check fn
		c.check_map_and_filter(false, elem_typ, call_expr)
	} else if method_name in ['any', 'all'] {
		c.check_map_and_filter(false, elem_typ, call_expr)
		call_expr.return_type = table.bool_type
	} else if method_name == 'clone' {
		// need to return `array_xxx` instead of `array`
		// in ['clone', 'str'] {
		call_expr.receiver_type = left_type.to_ptr()
		if call_expr.left.is_auto_deref_var() {
			call_expr.return_type = left_type.deref()
		} else {
			call_expr.return_type = call_expr.receiver_type.set_nr_muls(0)
		}
	} else if method_name == 'sort' {
		call_expr.return_type = table.void_type
	} else if method_name == 'contains' {
		call_expr.return_type = table.bool_type
	} else if method_name == 'index' {
		call_expr.return_type = table.int_type
	} else if method_name in ['first', 'last', 'pop'] {
		call_expr.return_type = array_info.elem_type
		if method_name == 'pop' {
			call_expr.receiver_type = left_type.to_ptr()
		} else {
			call_expr.receiver_type = left_type
		}
	}
	return call_expr.return_type
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ table.Type, call_expr ast.CallExpr) {
	if call_expr.args.len != 1 {
		c.error('expected 1 argument, but got $call_expr.args.len', call_expr.pos)
		// Finish early so that it doesn't fail later
		return
	}
	elem_sym := c.table.get_type_symbol(elem_typ)
	arg_expr := call_expr.args[0].expr
	match arg_expr {
		ast.AnonFn {
			if arg_expr.decl.params.len > 1 {
				c.error('function needs exactly 1 argument', arg_expr.decl.pos)
			} else if is_map && (arg_expr.decl.return_type == table.void_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`', arg_expr.decl.pos)
			} else if !is_map && (arg_expr.decl.return_type != table.bool_type
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
					c.error('function needs exactly 1 argument', call_expr.pos)
				} else if is_map
					&& (func.return_type == table.void_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`',
						arg_expr.pos)
				} else if !is_map
					&& (func.return_type != table.bool_type || func.params[0].typ != elem_typ) {
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
						} else if is_map && (expr.decl.return_type == table.void_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a $elem_sym.name) T {...}`',
								expr.decl.pos)
						} else if !is_map && (expr.decl.return_type != table.bool_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a $elem_sym.name) bool {...}`',
								expr.decl.pos)
						}
						return
					}
				}
				if !is_map && arg_expr.info.typ != table.bool_type {
					c.error('type mismatch, should be bool', arg_expr.pos)
				}
			}
		}
		else {}
	}
}
