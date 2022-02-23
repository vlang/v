module checker

import v.ast
import v.pref
import time
import v.util
import v.token

fn (mut c Checker) fn_decl(mut node ast.FnDecl) {
	$if trace_post_process_generic_fns_types ? {
		if node.generic_names.len > 0 {
			eprintln('>>> post processing node.name: ${node.name:-30} | $node.generic_names <=> $c.table.cur_concrete_types')
		}
	}
	if node.generic_names.len > 0 && c.table.cur_concrete_types.len == 0 {
		// Just remember the generic function for now.
		// It will be processed later in c.post_process_generic_fns,
		// after all other normal functions are processed.
		// This is done so that all generic function calls can
		// have a chance to populate c.table.fn_generic_types with
		// the correct concrete types.
		c.file.generic_fns << node
		c.need_recheck_generic_fns = true
		return
	}
	node.ninstances++
	// save all the state that fn_decl or inner  statements/expressions
	// could potentially modify, since functions can be nested, due to
	// anonymous function support, and ensure that it is restored, when
	// fn_decl returns:
	prev_fn_scope := c.fn_scope
	prev_in_for_count := c.in_for_count
	prev_inside_defer := c.inside_defer
	prev_inside_unsafe := c.inside_unsafe
	prev_inside_anon_fn := c.inside_anon_fn
	prev_returns := c.returns
	prev_stmt_level := c.stmt_level
	c.fn_level++
	c.in_for_count = 0
	c.inside_defer = false
	c.inside_unsafe = false
	c.returns = false
	defer {
		c.stmt_level = prev_stmt_level
		c.fn_level--
		c.returns = prev_returns
		c.inside_anon_fn = prev_inside_anon_fn
		c.inside_unsafe = prev_inside_unsafe
		c.inside_defer = prev_inside_defer
		c.in_for_count = prev_in_for_count
		c.fn_scope = prev_fn_scope
	}
	// Check generics fn/method without generic type parameters
	mut need_generic_names := false
	if node.generic_names.len == 0 {
		if node.return_type.has_flag(.generic) {
			need_generic_names = true
		} else {
			for param in node.params {
				if param.typ.has_flag(.generic) {
					need_generic_names = true
					break
				}
			}
		}
		if need_generic_names {
			c.error('generic function declaration must specify generic type names, e.g. foo<T>',
				node.pos)
		}
	}
	if node.language == .v && !c.is_builtin_mod && !node.is_anon {
		c.check_valid_snake_case(node.name, 'function name', node.pos)
		if !node.is_method && node.mod == 'main' && node.short_name in c.table.builtin_pub_fns {
			c.error('cannot redefine builtin public function `$node.short_name`', node.pos)
		}
	}
	if node.name == 'main.main' {
		c.main_fn_decl_node = *node
	}
	if node.return_type != ast.void_type {
		if ct_attr_idx := node.attrs.find_comptime_define() {
			sexpr := node.attrs[ct_attr_idx].ct_expr.str()
			c.error('only functions that do NOT return values can have `[if $sexpr]` tags',
				node.pos)
		}
		if node.generic_names.len > 0 {
			gs := c.table.sym(node.return_type)
			if gs.info is ast.Struct {
				if gs.info.is_generic && !node.return_type.has_flag(.generic) {
					c.error('return generic struct in fn declaration must specify the generic type names, e.g. Foo<T>',
						node.return_type_pos)
				}
			}
		}
		return_sym := c.table.sym(node.return_type)
		if return_sym.info is ast.MultiReturn {
			for multi_type in return_sym.info.types {
				multi_sym := c.table.sym(multi_type)
				if multi_type == ast.error_type {
					c.error('type `IError` cannot be used in multi-return, return an option instead',
						node.return_type_pos)
				} else if multi_type.has_flag(.optional) {
					c.error('option cannot be used in multi-return, return an option instead',
						node.return_type_pos)
				} else if multi_sym.kind == .array_fixed {
					c.error('fixed array cannot be used in multi-return', node.return_type_pos)
				}
			}
		} else if return_sym.kind == .array_fixed {
			c.error('fixed array cannot be returned by function', node.return_type_pos)
		}
	} else {
		for mut a in node.attrs {
			if a.kind == .comptime_define {
				node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut a)
			}
		}
	}
	if node.is_method {
		mut sym := c.table.sym(node.receiver.typ)
		if sym.kind == .array && !c.is_builtin_mod && node.name == 'map' {
			// TODO `node.map in array_builtin_methods`
			c.error('method overrides built-in array method', node.pos)
		} else if sym.kind == .sum_type && node.name == 'type_name' {
			c.error('method overrides built-in sum type method', node.pos)
		} else if sym.kind == .sum_type && node.name == 'type_idx' {
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
			if mut sym.info is ast.Interface {
				// if the method is in info.methods then it is an interface method
				if sym.info.has_method(node.name) {
					c.error('interface `$sym.name` cannot implement its own interface method `$node.name`',
						node.pos)
				}
			}
		}
		if mut sym.info is ast.Struct {
			if field := c.table.find_field(sym, node.name) {
				field_sym := c.table.sym(field.typ)
				if field_sym.kind == .function {
					c.error('type `$sym.name` has both field and method named `$node.name`',
						node.pos)
				}
			}
			if node.name == 'free' {
				if node.return_type != ast.void_type {
					c.error('`.free()` methods should not have a return type', node.return_type_pos)
				}
				if !node.receiver.typ.is_ptr() {
					tname := sym.name.after_char(`.`)
					c.error('`.free()` methods should be defined on either a `(mut x &$tname)`, or a `(x &$tname)` receiver',
						node.receiver_pos)
				}
				if node.params.len != 1 {
					c.error('`.free()` methods should have 0 arguments', node.pos)
				}
			}
		}
		// needed for proper error reporting during vweb route checking
		if node.method_idx < sym.methods.len {
			sym.methods[node.method_idx].source_fn = voidptr(node)
		} else {
			c.error('method index: $node.method_idx >= sym.methods.len: $sym.methods.len',
				node.pos)
		}
	}
	if node.language == .v {
		// Make sure all types are valid
		for mut param in node.params {
			c.ensure_type_exists(param.typ, param.type_pos) or { return }
			if param.name in reserved_type_names {
				c.error('invalid use of reserved type `$param.name` as a parameter name',
					param.pos)
			}
			if !param.typ.is_ptr() { // value parameter, i.e. on stack - check for `[heap]`
				arg_typ_sym := c.table.sym(param.typ)
				if arg_typ_sym.kind == .struct_ {
					info := arg_typ_sym.info as ast.Struct
					if info.is_heap { // set auto_heap to promote value parameter
						mut v := node.scope.find_var(param.name) or { continue }
						v.is_auto_heap = true
					}
					if info.generic_types.len > 0 && !param.typ.has_flag(.generic)
						&& info.concrete_types.len == 0 {
						c.error('generic struct in fn declaration must specify the generic type names, e.g. Foo<T>',
							param.type_pos)
					}
				} else if arg_typ_sym.kind == .interface_ {
					info := arg_typ_sym.info as ast.Interface
					if info.generic_types.len > 0 && !param.typ.has_flag(.generic)
						&& info.concrete_types.len == 0 {
						c.error('generic interface in fn declaration must specify the generic type names, e.g. Foo<T>',
							param.type_pos)
					}
				} else if arg_typ_sym.kind == .sum_type {
					info := arg_typ_sym.info as ast.SumType
					if info.generic_types.len > 0 && !param.typ.has_flag(.generic)
						&& info.concrete_types.len == 0 {
						c.error('generic sumtype in fn declaration must specify the generic type names, e.g. Foo<T>',
							param.type_pos)
					}
				}
			}
			if (c.pref.translated || c.file.is_translated) && node.is_variadic
				&& node.params.len == 1 && param.typ.is_ptr() {
				// TODO c2v hack to fix `(const char *s, ...)`
				param.typ = ast.int_type.ref()
			}
		}
	}
	if node.language == .v && node.name.after_char(`.`) == 'init' && !node.is_method
		&& node.params.len == 0 {
		if node.is_pub {
			c.error('fn `init` must not be public', node.pos)
		}
		if node.return_type != ast.void_type {
			c.error('fn `init` cannot have a return type', node.pos)
		}
	}
	if node.return_type != ast.Type(0) {
		c.ensure_type_exists(node.return_type, node.return_type_pos) or { return }
		if node.language == .v && node.is_method && node.name == 'str' {
			if node.return_type != ast.string_type {
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
				receiver_sym := c.table.sym(node.receiver.typ)
				param_sym := c.table.sym(node.params[1].typ)
				if param_sym.kind == .string && receiver_sym.kind == .string {
					// bypass check for strings
					// TODO there must be a better way to handle that
				} else if param_sym.kind !in [.struct_, .alias]
					|| receiver_sym.kind !in [.struct_, .alias] {
					c.error('operator methods are only allowed for struct and type alias',
						node.pos)
				} else {
					parent_sym := c.table.final_sym(node.receiver.typ)
					if node.rec_mut {
						c.error('receiver cannot be `mut` for operator overloading', node.receiver_pos)
					} else if node.params[1].is_mut {
						c.error('argument cannot be `mut` for operator overloading', node.pos)
					} else if node.receiver.typ != node.params[1].typ {
						c.error('expected `$receiver_sym.name` not `$param_sym.name` - both operands must be the same type for operator overloading',
							node.params[1].type_pos)
					} else if node.name in ['<', '=='] && node.return_type != ast.bool_type {
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
	if c.file.is_test && (!node.is_method && (node.short_name.starts_with('test_')
		|| node.short_name.starts_with('testsuite_'))) {
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

		if node.params.len != 0 {
			c.error('test functions should take 0 parameters', node.pos)
		}

		if node.return_type != ast.void_type_idx
			&& node.return_type.clear_flag(.optional) != ast.void_type_idx {
			c.error('test functions should either return nothing at all, or be marked to return `?`',
				node.pos)
		}
	}
	c.expected_type = ast.void_type
	c.table.cur_fn = unsafe { node }
	// c.table.cur_fn = node
	// Add return if `fn(...) ? {...}` have no return at end
	if node.return_type != ast.void_type && node.return_type.has_flag(.optional)
		&& (node.stmts.len == 0 || node.stmts[node.stmts.len - 1] !is ast.Return) {
		sym := c.table.sym(node.return_type)
		if sym.kind == .void {
			node.stmts << ast.Return{
				pos: node.pos
			}
		}
	}
	c.fn_scope = node.scope
	c.stmts(node.stmts)
	node_has_top_return := has_top_return(node.stmts)
	node.has_return = c.returns || node_has_top_return
	c.check_noreturn_fn_decl(mut node)
	if node.language == .v && !node.no_body && node.return_type != ast.void_type && !node.has_return
		&& !node.is_noreturn {
		if c.inside_anon_fn {
			c.error('missing return at the end of an anonymous function', node.pos)
		} else if !node.attrs.contains('_naked') {
			c.error('missing return at end of function `$node.name`', node.pos)
		}
	}
	node.source_file = c.file
}

fn (mut c Checker) anon_fn(mut node ast.AnonFn) ast.Type {
	keep_fn := c.table.cur_fn
	keep_inside_anon := c.inside_anon_fn
	defer {
		c.table.cur_fn = keep_fn
		c.inside_anon_fn = keep_inside_anon
	}
	for param in node.decl.params {
		if param.name.len == 0 {
			c.error('use `_` to name an unused parameter', param.pos)
		}
	}
	c.table.cur_fn = unsafe { &node.decl }
	c.inside_anon_fn = true
	for mut var in node.inherited_vars {
		parent_var := node.decl.scope.parent.find_var(var.name) or {
			panic('unexpected checker error: cannot find parent of inherited variable `$var.name`')
		}
		if var.is_mut && !parent_var.is_mut {
			c.error('original `$parent_var.name` is immutable, declare it with `mut` to make it mutable',
				var.pos)
		}
		var.typ = parent_var.typ
	}
	c.stmts(node.decl.stmts)
	c.fn_decl(mut node.decl)
	return node.typ
}

pub fn (mut c Checker) call_expr(mut node ast.CallExpr) ast.Type {
	// First check everything that applies to both fns and methods
	// TODO merge logic from method_call and fn_call
	/*
	for i, call_arg in node.args {
		if call_arg.is_mut {
			c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.pos())
			} else if arg.typ.share() != call_arg.share {
				c.error('wrong shared type', call_arg.expr.pos())
			}
		} else {
			if arg.is_mut && (!call_arg.is_mut || arg.typ.share() != call_arg.share) {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i+1}`',
					call_arg.expr.pos())
			}
		}
	}
	*/
	// Now call `method_call` or `fn_call` for specific checks.
	old_inside_fn_arg := c.inside_fn_arg
	c.inside_fn_arg = true
	mut continue_check := true
	typ := if node.is_method {
		c.method_call(mut node)
	} else {
		c.fn_call(mut node, mut continue_check)
	}
	if !continue_check {
		return ast.void_type
	}
	c.inside_fn_arg = old_inside_fn_arg
	// autofree: mark args that have to be freed (after saving them in tmp exprs)
	free_tmp_arg_vars := c.pref.autofree && !c.is_builtin_mod && node.args.len > 0
		&& !node.args[0].typ.has_flag(.optional)
	if free_tmp_arg_vars && !c.inside_const {
		for i, arg in node.args {
			if arg.typ != ast.string_type {
				continue
			}
			if arg.expr in [ast.Ident, ast.StringLiteral, ast.SelectorExpr] {
				// Simple expressions like variables, string literals, selector expressions
				// (`x.field`) can't result in allocations and don't need to be assigned to
				// temporary vars.
				// Only expressions like `str + 'b'` need to be freed.
				continue
			}
			node.args[i].is_tmp_autofree = true
		}
		// TODO copy pasta from above
		if node.receiver_type == ast.string_type
			&& node.left !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr] {
			node.free_receiver = true
		}
	}
	c.expected_or_type = node.return_type.clear_flag(.optional)
	c.stmts_ending_with_expression(node.or_block.stmts)
	c.expected_or_type = ast.void_type
	if node.or_block.kind == .propagate && !c.table.cur_fn.return_type.has_flag(.optional)
		&& !c.inside_const {
		if !c.table.cur_fn.is_main {
			c.error('to propagate the optional call, `$c.table.cur_fn.name` must return an optional',
				node.or_block.pos)
		}
	}
	return typ
}

pub fn (mut c Checker) fn_call(mut node ast.CallExpr, mut continue_check &bool) ast.Type {
	fn_name := node.name
	if fn_name == 'main' {
		c.error('the `main` function cannot be called in the program', node.pos)
	}
	mut has_generic := false // foo<T>() instead of foo<int>()
	mut concrete_types := []ast.Type{}
	node.concrete_types = node.raw_concrete_types
	for concrete_type in node.concrete_types {
		if concrete_type.has_flag(.generic) {
			has_generic = true
			concrete_types << c.unwrap_generic(concrete_type)
		} else {
			concrete_types << concrete_type
		}
	}
	if !isnil(c.table.cur_fn) && c.table.cur_concrete_types.len == 0 && has_generic {
		c.error('generic fn using generic types cannot be called outside of generic fn',
			node.pos)
	}
	if concrete_types.len > 0 {
		mut no_exists := true
		if fn_name.contains('.') {
			no_exists = c.table.register_fn_concrete_types(node.fkey(), concrete_types)
		} else {
			no_exists = c.table.register_fn_concrete_types(c.mod + '.' + node.fkey(),
				concrete_types)
			// if the generic fn does not exist in the current fn calling module, continue
			// to look in builtin module
			if !no_exists {
				no_exists = c.table.register_fn_concrete_types(node.fkey(), concrete_types)
			}
		}
		if no_exists {
			c.need_recheck_generic_fns = true
		}
	}
	if fn_name == 'JS.await' {
		if node.args.len > 1 {
			c.error('JS.await expects 1 argument, a promise value (e.g `JS.await(fs.read())`',
				node.pos)
			return ast.void_type
		}

		typ := c.expr(node.args[0].expr)
		tsym := c.table.sym(typ)

		if !tsym.name.starts_with('Promise<') {
			c.error('JS.await: first argument must be a promise, got `$tsym.name`', node.pos)
			return ast.void_type
		}
		c.table.cur_fn.has_await = true
		match tsym.info {
			ast.Struct {
				mut ret_type := tsym.info.concrete_types[0]
				ret_type = ret_type.set_flag(.optional)
				node.return_type = ret_type
				return ret_type
			}
			else {
				c.error('JS.await: Promise must be a struct type', node.pos)
				return ast.void_type
			}
		}
		panic('unreachable')
	}
	if fn_name == 'json.encode' {
	} else if fn_name == 'json.decode' && node.args.len > 0 {
		if node.args.len != 2 {
			c.error("json.decode expects 2 arguments, a type and a string (e.g `json.decode(T, '')`)",
				node.pos)
			return ast.void_type
		}
		expr := node.args[0].expr
		if expr is ast.TypeNode {
			sym := c.table.sym(expr.typ)
			if !c.table.known_type(sym.name) {
				c.error('json.decode: unknown type `$sym.name`', node.pos)
			}
		} else {
			// if expr !is ast.TypeNode {
			typ := expr.type_name()
			c.error('json.decode: first argument needs to be a type, got `$typ`', node.pos)
			return ast.void_type
		}
		c.expected_type = ast.string_type
		node.args[1].typ = c.expr(node.args[1].expr)
		if node.args[1].typ != ast.string_type {
			c.error('json.decode: second argument needs to be a string', node.pos)
		}
		typ := expr as ast.TypeNode
		ret_type := typ.typ.set_flag(.optional)
		node.return_type = ret_type
		return ret_type
	}
	// look for function in format `mod.fn` or `fn` (builtin)
	mut func := ast.Fn{}
	mut found := false
	mut found_in_args := false
	// anon fn direct call
	if mut node.left is ast.AnonFn {
		// it was set to anon for checker errors, clear for gen
		node.name = ''
		c.expr(node.left)
		if node.left.typ != ast.Type(0) {
			anon_fn_sym := c.table.sym(node.left.typ)
			func = (anon_fn_sym.info as ast.FnType).func
			found = true
		}
	}
	// try prefix with current module as it would have never gotten prefixed
	if !found && !fn_name.contains('.') && node.mod != 'builtin' {
		name_prefixed := '${node.mod}.$fn_name'
		if f := c.table.find_fn(name_prefixed) {
			node.name = name_prefixed
			found = true
			func = f
			c.table.fns[name_prefixed].usages++
		}
	}
	if !found && node.left is ast.IndexExpr {
		c.expr(node.left)
		expr := node.left as ast.IndexExpr
		sym := c.table.sym(expr.left_type)
		if sym.kind == .array {
			info := sym.info as ast.Array
			elem_typ := c.table.sym(info.elem_type)
			if elem_typ.info is ast.FnType {
				return elem_typ.info.func.return_type
			} else {
				c.error('cannot call the element of the array, it is not a function',
					node.pos)
			}
		} else if sym.kind == .map {
			info := sym.info as ast.Map
			value_typ := c.table.sym(info.value_type)
			if value_typ.info is ast.FnType {
				return value_typ.info.func.return_type
			} else {
				c.error('cannot call the value of the map, it is not a function', node.pos)
			}
		} else if sym.kind == .array_fixed {
			info := sym.info as ast.ArrayFixed
			elem_typ := c.table.sym(info.elem_type)
			if elem_typ.info is ast.FnType {
				return elem_typ.info.func.return_type
			} else {
				c.error('cannot call the element of the array, it is not a function',
					node.pos)
			}
		} else {
			// TODO: assert? is this possible?
		}
		found = true
		return ast.string_type
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f := c.table.find_fn(fn_name) {
			found = true
			func = f
			c.table.fns[fn_name].usages++
		}
	}
	mut is_native_builtin := false
	if !found && c.pref.backend == .native {
		if fn_name in ast.native_builtins {
			c.table.fns[fn_name].usages++
			found = true
			func = c.table.fns[fn_name]
			is_native_builtin = true
		}
	}
	if !found && c.pref.is_vsh {
		// TOOD: test this hack more extensively
		os_name := 'os.$fn_name'
		if f := c.table.find_fn(os_name) {
			if f.generic_names.len == node.concrete_types.len {
				node_alias_name := node.fkey()
				c.table.fn_generic_types[os_name] = c.table.fn_generic_types[node_alias_name]
			}
			node.name = os_name
			found = true
			func = f
			c.table.fns[os_name].usages++
		}
	}
	if is_native_builtin {
		return ast.void_type
	}
	// check for arg (var) of fn type
	if !found {
		mut typ := 0
		if obj := node.scope.find(node.name) {
			match obj {
				ast.GlobalField {
					typ = obj.typ
				}
				ast.Var {
					typ = if obj.smartcasts.len != 0 { obj.smartcasts.last() } else { obj.typ }
				}
				else {}
			}
		}

		if typ != 0 {
			generic_vts := c.table.final_sym(typ)
			if generic_vts.kind == .function {
				info := generic_vts.info as ast.FnType
				func = info.func
				found = true
				found_in_args = true
			} else {
				vts := c.table.sym(c.unwrap_generic(typ))
				if vts.kind == .function {
					info := vts.info as ast.FnType
					func = info.func
					found = true
					found_in_args = true
				}
			}
		}
	}
	// global fn?
	if !found {
		if obj := c.file.global_scope.find(fn_name) {
			if obj.typ != 0 {
				sym := c.table.sym(obj.typ)
				if sym.kind == .function {
					found = true
					func = (sym.info as ast.FnType).func
				}
			}
		}
	}
	if !found {
		continue_check = false
		c.error('unknown function: $fn_name', node.pos)
		return ast.void_type
	}

	node.is_noreturn = func.is_noreturn
	node.is_ctor_new = func.is_ctor_new
	if !found_in_args {
		if node.scope.known_var(fn_name) {
			c.error('ambiguous call to: `$fn_name`, may refer to fn `$fn_name` or variable `$fn_name`',
				node.pos)
		}
	}
	if !func.is_pub && func.language == .v && func.name.len > 0 && func.mod.len > 0
		&& func.mod != c.mod {
		c.error('function `$func.name` is private', node.pos)
	}
	if !isnil(c.table.cur_fn) && !c.table.cur_fn.is_deprecated && func.is_deprecated {
		c.deprecate_fnmethod('function', func.name, func, node)
	}
	if func.is_unsafe && !c.inside_unsafe
		&& (func.language != .c || (func.name[2] in [`m`, `s`] && func.mod == 'builtin')) {
		// builtin C.m*, C.s* only - temp
		c.warn('function `$func.name` must be called from an `unsafe` block', node.pos)
	}
	node.is_keep_alive = func.is_keep_alive
	if func.mod != 'builtin' && func.language == .v && func.no_body && !c.pref.translated
		&& !c.file.is_translated && !func.is_unsafe {
		c.error('cannot call a function that does not have a body', node.pos)
	}
	for concrete_type in node.concrete_types {
		c.ensure_type_exists(concrete_type, node.concrete_list_pos) or {}
	}
	if func.generic_names.len > 0 && node.args.len == 0 && node.concrete_types.len == 0 {
		c.error('no argument generic function must add concrete types, e.g. foo<int>()',
			node.pos)
		return func.return_type
	}
	if func.return_type == ast.void_type && func.is_conditional
		&& func.ctdefine_idx != ast.invalid_type_idx {
		node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut func.attrs[func.ctdefine_idx])
	}
	// dont check number of args for JS functions since arguments are not required
	if node.language != .js {
		c.check_expected_arg_count(mut node, func) or { return func.return_type }
	}
	// println / eprintln / panic can print anything
	if fn_name in ['println', 'print', 'eprintln', 'eprint', 'panic'] && node.args.len > 0 {
		c.inside_println_arg = true
		c.expected_type = ast.string_type
		node.args[0].typ = c.expr(node.args[0].expr)
		arg := node.args[0]
		c.check_expr_opt_call(arg.expr, arg.typ)
		if arg.typ.is_void() {
			c.error('`$fn_name` can not print void expressions', node.pos)
		} else if arg.typ == ast.char_type && arg.typ.nr_muls() == 0 {
			c.error('`$fn_name` cannot print type `char` directly, print its address or cast it to an integer instead',
				node.pos)
		}
		c.fail_if_unreadable(arg.expr, arg.typ, 'argument to print')
		c.inside_println_arg = false
		node.return_type = ast.void_type
		/*
		// TODO: optimize `struct T{} fn (t &T) str() string {return 'abc'} mut a := []&T{} a << &T{} println(a[0])`
		// It currently generates:
		// `println(T_str_no_ptr(*(*(T**)array_get(a, 0))));`
		// ... which works, but could be just:
		// `println(T_str(*(T**)array_get(a, 0)));`
		prexpr := node.args[0].expr
		prtyp := node.args[0].typ
		prtyp_sym := c.table.sym(prtyp)
		prtyp_is_ptr := prtyp.is_ptr()
		prhas_str, prexpects_ptr, prnr_args := prtyp_sym.str_method_info()
		eprintln('>>> println hack typ: ${prtyp} | sym.name: ${prtyp_sym.name} | is_ptr: $prtyp_is_ptr | has_str: $prhas_str | expects_ptr: $prexpects_ptr | nr_args: $prnr_args | expr: ${prexpr.str()} ')
		*/
		return func.return_type
	}
	// `return error(err)` -> `return err`
	if fn_name == 'error' && node.args.len == 1 {
		arg := node.args[0]
		node.args[0].typ = c.expr(arg.expr)
		if node.args[0].typ == ast.error_type {
			c.warn('`error($arg)` can be shortened to just `$arg`', node.pos)
		}
	}
	// TODO: typ optimize.. this node can get processed more than once
	if node.expected_arg_types.len == 0 {
		for param in func.params {
			node.expected_arg_types << param.typ
		}
	}
	if !c.pref.backend.is_js() && node.args.len > 0 && func.params.len == 0 {
		c.error('too many arguments in call to `$func.name` (non-js backend: $c.pref.backend)',
			node.pos)
	}
	for i, mut call_arg in node.args {
		if func.params.len == 0 {
			continue
		}
		param := if func.is_variadic && i >= func.params.len - 1 {
			func.params[func.params.len - 1]
		} else {
			func.params[i]
		}
		if func.is_variadic && call_arg.expr is ast.ArrayDecompose {
			if i > func.params.len - 1 {
				c.error('too many arguments in call to `$func.name`', node.pos)
			}
		}
		c.expected_type = param.typ

		e_sym := c.table.sym(c.expected_type)
		if call_arg.expr is ast.MapInit && e_sym.kind == .struct_ {
			c.error('cannot initialize a struct with a map', call_arg.pos)
			continue
		} else if call_arg.expr is ast.StructInit && e_sym.kind == .map {
			c.error('cannot initialize a map with a struct', call_arg.pos)
			continue
		}

		typ := c.check_expr_opt_call(call_arg.expr, c.expr(call_arg.expr))
		node.args[i].typ = typ
		if c.inside_comptime_for_field {
			if mut call_arg.expr is ast.Ident {
				if mut call_arg.expr.obj is ast.Var {
					node.args[i].typ = call_arg.expr.obj.typ
				}
			}
		}
		typ_sym := c.table.sym(typ)
		param_typ_sym := c.table.sym(param.typ)
		if func.is_variadic && typ.has_flag(.variadic) && node.args.len - 1 > i {
			c.error('when forwarding a variadic variable, it must be the final argument',
				call_arg.pos)
		}
		arg_share := param.typ.share()
		if arg_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('function with `shared` arguments cannot be called inside `lock`/`rlock` block',
				call_arg.pos)
		}
		if call_arg.is_mut {
			to_lock, pos := c.fail_if_immutable(call_arg.expr)
			if !call_arg.expr.is_lvalue() {
				c.error('cannot pass expression as `mut`', call_arg.expr.pos())
			}
			if !param.is_mut {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.pos())
			} else {
				if param.typ.share() != call_arg.share {
					c.error('wrong shared type', call_arg.expr.pos())
				}
				if to_lock != '' && !param.typ.has_flag(.shared_f) {
					c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
						pos)
				}
			}
		} else {
			if param.is_mut {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
					i + 1}`', call_arg.expr.pos())
			} else {
				c.fail_if_unreadable(call_arg.expr, typ, 'argument')
			}
		}
		mut final_param_sym := param_typ_sym
		mut final_param_typ := param.typ
		if func.is_variadic && param_typ_sym.info is ast.Array {
			final_param_typ = param_typ_sym.array_info().elem_type
			final_param_sym = c.table.sym(final_param_typ)
		}
		// NB: Casting to voidptr is used as an escape mechanism, so:
		// 1. allow passing *explicit* voidptr (native or through cast) to functions
		// expecting voidptr or ...voidptr
		// ... but 2. disallow passing non-pointers - that is very rarely what the user wanted,
		// it can lead to codegen errors (except for 'magic' functions like `json.encode` that,
		// the compiler has special codegen support for), so it should be opt in, that is it
		// shoould require an explicit voidptr(x) cast (and probably unsafe{} ?) .
		if call_arg.typ != param.typ
			&& (param.typ == ast.voidptr_type || final_param_sym.idx == ast.voidptr_type_idx)
			&& !call_arg.typ.is_any_kind_of_pointer() && func.language == .v
			&& !call_arg.expr.is_lvalue() && func.name != 'json.encode' && !c.pref.translated
			&& !c.file.is_translated {
			c.error('expression cannot be passed as `voidptr`', call_arg.expr.pos())
		}
		// Handle expected interface
		if final_param_sym.kind == .interface_ {
			if c.type_implements(typ, final_param_typ, call_arg.expr.pos()) {
				if !typ.is_ptr() && !typ.is_pointer() && !c.inside_unsafe
					&& typ_sym.kind != .interface_ {
					c.mark_as_referenced(mut &call_arg.expr, true)
				}
			}
			continue
		}
		c.check_expected_call_arg(typ, c.unwrap_generic(param.typ), node.language, call_arg) or {
			// str method, allow type with str method if fn arg is string
			// Passing an int or a string array produces a c error here
			// Deleting this condition results in propper V error messages
			// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
			// continue
			// }
			if typ_sym.kind == .void && param_typ_sym.kind == .string {
				continue
			}
			if param.typ.has_flag(.generic) {
				continue
			}
			if c.pref.translated || c.file.is_translated {
				// TODO duplicated logic in check_types() (check_types.v)
				// Allow enums to be used as ints and vice versa in translated code
				if param.typ == ast.int_type && typ_sym.kind == .enum_ {
					continue
				}
				if typ == ast.int_type && param_typ_sym.kind == .enum_ {
					continue
				}
				// In C unsafe number casts are used all the time (e.g. `char*` where
				// `int*` is expected etc), so just allow them all.
				mut param_is_number := param.typ.is_number()
				if param.typ.is_ptr() {
					param_is_number = param.typ.deref().is_number()
				}
				mut typ_is_number := typ.is_number()
				if typ.is_ptr() {
					typ_is_number = typ.deref().is_number()
				}
				if param_is_number && typ_is_number {
					continue
				}
				// Allow voidptrs for everything
				if param.typ == ast.voidptr_type_idx || typ == ast.voidptr_type_idx {
					continue
				}
				// Allow `[32]i8` as `&i8` etc
				if (typ_sym.kind == .array_fixed && param_is_number)
					|| (param_typ_sym.kind == .array_fixed && typ_is_number) {
					continue
				}
				// Allow `int` as `&i8`
				if param.typ.is_any_kind_of_pointer() && typ_is_number {
					continue
				}
			}
			c.error('$err.msg() in argument ${i + 1} to `$fn_name`', call_arg.pos)
		}
		// Warn about automatic (de)referencing, which will be removed soon.
		if func.language != .c && !c.inside_unsafe && typ.nr_muls() != param.typ.nr_muls()
			&& !(call_arg.is_mut && param.is_mut) && !(!call_arg.is_mut && !param.is_mut)
			&& param.typ !in [ast.byteptr_type, ast.charptr_type, ast.voidptr_type] {
			// sym := c.table.sym(typ)
			c.warn('automatic referencing/dereferencing is deprecated and will be removed soon (got: $typ.nr_muls() references, expected: $param.typ.nr_muls() references)',
				call_arg.pos)
		}
	}
	if func.generic_names.len != node.concrete_types.len {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_generic_types(func, mut node)
		concrete_types = node.concrete_types
	}
	if func.generic_names.len > 0 {
		for i, mut call_arg in node.args {
			param := if func.is_variadic && i >= func.params.len - 1 {
				func.params[func.params.len - 1]
			} else {
				func.params[i]
			}
			c.expected_type = param.typ
			typ := c.check_expr_opt_call(call_arg.expr, c.expr(call_arg.expr))

			if param.typ.has_flag(.generic) && func.generic_names.len == node.concrete_types.len {
				if unwrap_typ := c.table.resolve_generic_to_concrete(param.typ, func.generic_names,
					concrete_types)
				{
					utyp := c.unwrap_generic(typ)
					unwrap_sym := c.table.sym(unwrap_typ)
					if unwrap_sym.kind == .interface_ {
						if c.type_implements(utyp, unwrap_typ, call_arg.expr.pos()) {
							if !utyp.is_ptr() && !utyp.is_pointer() && !c.inside_unsafe
								&& c.table.sym(utyp).kind != .interface_ {
								c.mark_as_referenced(mut &call_arg.expr, true)
							}
						}
						continue
					}
					c.check_expected_call_arg(utyp, unwrap_typ, node.language, call_arg) or {
						if c.comptime_fields_type.len > 0 {
							continue
						}
						c.error('$err.msg() in argument ${i + 1} to `$fn_name`', call_arg.pos)
					}
				}
			}
		}
	}
	// resolve return generics struct to concrete type
	if func.generic_names.len > 0 && func.return_type.has_flag(.generic)
		&& c.table.cur_fn.generic_names.len == 0 {
		node.return_type = c.table.unwrap_generic_type(func.return_type, func.generic_names,
			concrete_types)
	} else {
		node.return_type = func.return_type
	}
	if node.concrete_types.len > 0 && func.return_type != 0 && c.table.cur_fn.generic_names.len == 0 {
		if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
			concrete_types)
		{
			node.return_type = typ
			return typ
		}
	}
	if node.concrete_types.len > 0 && func.generic_names.len == 0 {
		c.error('a non generic function called like a generic one', node.concrete_list_pos)
	}

	if node.concrete_types.len > func.generic_names.len {
		c.error('too many generic parameters got $node.concrete_types.len, expected $func.generic_names.len',
			node.concrete_list_pos)
	}
	if func.generic_names.len > 0 {
		if has_generic {
			if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
				node.concrete_types)
			{
				if typ.has_flag(.generic) {
					node.return_type = typ
				}
			}
			return node.return_type
		} else if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
			concrete_types)
		{
			node.return_type = typ
			return typ
		}
	}
	return func.return_type
}

pub fn (mut c Checker) method_call(mut node ast.CallExpr) ast.Type {
	left_type := c.expr(node.left)
	c.expected_type = left_type
	mut is_generic := left_type.has_flag(.generic)
	// x is Bar<T>, x.foo() -> x.foo<T>()
	if is_generic && node.concrete_types.len == 0 {
		rec_sym := c.table.sym(left_type)
		if rec_sym.info is ast.Struct {
			node.concrete_types = rec_sym.info.generic_types
		}
	}
	node.left_type = left_type
	// Set default values for .return_type & .receiver_type too,
	// or there will be hard tRo diagnose 0 type panics in cgen.
	node.return_type = left_type
	node.receiver_type = left_type

	if c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(left_type, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	unwrapped_left_type := c.unwrap_generic(left_type)
	left_sym := c.table.sym(unwrapped_left_type)
	final_left_sym := c.table.final_sym(unwrapped_left_type)

	method_name := node.name
	mut unknown_method_msg := if field := c.table.find_field(left_sym, method_name) {
		'unknown method `$field.name` did you mean to access the field with the same name instead?'
	} else {
		'unknown method or field: `${left_sym.name}.$method_name`'
	}
	if left_type.has_flag(.optional) {
		c.error('optional type cannot be called directly', node.left.pos())
		return ast.void_type
	}
	if left_sym.kind in [.sum_type, .interface_] {
		if method_name == 'type_name' {
			return ast.string_type
		}
		if method_name == 'type_idx' {
			return ast.int_type
		}
	}
	if left_type == ast.void_type {
		// No need to print this error, since this means that the variable is unknown,
		// and there already was an error before.
		// c.error('`void` type has no methods', node.left.pos())
		return ast.void_type
	}
	mut concrete_types := []ast.Type{}
	for concrete_type in node.concrete_types {
		if concrete_type.has_flag(.generic) {
			concrete_types << c.unwrap_generic(concrete_type)
		} else {
			concrete_types << concrete_type
		}
	}
	if concrete_types.len > 0 {
		if c.table.register_fn_concrete_types(node.fkey(), concrete_types) {
			c.need_recheck_generic_fns = true
		}
	}
	// TODO: remove this for actual methods, use only for compiler magic
	// FIXME: Argument count != 1 will break these
	if left_sym.kind == .array && method_name in array_builtin_methods {
		return c.array_builtin_method_call(mut node, left_type, c.table.sym(left_type))
	} else if (left_sym.kind == .map || final_left_sym.kind == .map)
		&& method_name in ['clone', 'keys', 'move', 'delete'] {
		if left_sym.kind == .map {
			return c.map_builtin_method_call(mut node, left_type, left_sym)
		} else {
			parent_type := (left_sym.info as ast.Alias).parent_type
			return c.map_builtin_method_call(mut node, parent_type, final_left_sym)
		}
	} else if left_sym.kind == .array && method_name in ['insert', 'prepend'] {
		if method_name == 'insert' {
			if node.args.len != 2 {
				c.error('`array.insert()` should have 2 arguments, e.g. `insert(1, val)`',
					node.pos)
				return ast.void_type
			} else {
				arg_type := c.expr(node.args[0].expr)
				if arg_type !in [ast.int_type, ast.int_literal_type] {
					c.error('the first argument of `array.insert()` should be integer',
						node.args[0].expr.pos())
					return ast.void_type
				}
			}
		} else {
			if node.args.len != 1 {
				c.error('`array.prepend()` should have 1 argument, e.g. `prepend(val)`',
					node.pos)
				return ast.void_type
			}
		}
		info := left_sym.info as ast.Array
		arg_expr := if method_name == 'insert' { node.args[1].expr } else { node.args[0].expr }
		arg_type := c.expr(arg_expr)
		arg_sym := c.table.sym(arg_type)
		if !c.check_types(arg_type, info.elem_type) && !c.check_types(left_type, arg_type) {
			c.error('cannot $method_name `$arg_sym.name` to `$left_sym.name`', arg_expr.pos())
		}
	} else if final_left_sym.kind == .array && method_name in ['first', 'last', 'pop'] {
		if final_left_sym.info is ast.Array {
			node.return_type = final_left_sym.info.elem_type
			return node.return_type
		}
	} else if c.pref.backend.is_js() && left_sym.name.starts_with('Promise<')
		&& method_name == 'wait' {
		info := left_sym.info as ast.Struct
		if node.args.len > 0 {
			c.error('wait() does not have any arguments', node.args[0].pos)
		}
		c.table.cur_fn.has_await = true
		node.return_type = info.concrete_types[0]
		node.return_type.set_flag(.optional)
		return node.return_type
	} else if left_sym.kind == .thread && method_name == 'wait' {
		info := left_sym.info as ast.Thread
		if node.args.len > 0 {
			c.error('wait() does not have any arguments', node.args[0].pos)
		}
		node.return_type = info.return_type
		return info.return_type
	} else if left_sym.kind == .char && left_type.nr_muls() == 0 && method_name == 'str' {
		c.error('calling `.str()` on type `char` is not allowed, use its address or cast it to an integer instead',
			node.left.pos().extend(node.pos))
		return ast.void_type
	}
	mut method := ast.Fn{}
	mut has_method := false
	mut is_method_from_embed := false
	if m := c.table.find_method(left_sym, method_name) {
		method = m
		has_method = true
	} else {
		if left_sym.kind in [.struct_, .sum_type, .interface_] {
			mut parent_type := ast.void_type
			if left_sym.info is ast.Struct {
				parent_type = left_sym.info.parent_type
			} else if left_sym.info is ast.SumType {
				parent_type = left_sym.info.parent_type
			} else if left_sym.info is ast.Interface {
				parent_type = left_sym.info.parent_type
			}
			if parent_type != 0 {
				type_sym := c.table.sym(parent_type)
				if m := c.table.find_method(type_sym, method_name) {
					method = m
					has_method = true
					is_generic = true
				}
			}
		}
		if !has_method {
			has_method = true
			mut embed_types := []ast.Type{}
			method, embed_types = c.table.find_method_from_embeds(left_sym, method_name) or {
				if err.msg() != '' {
					c.error(err.msg(), node.pos)
				}
				has_method = false
				ast.Fn{}, []ast.Type{}
			}
			if embed_types.len != 0 {
				is_method_from_embed = true
				node.from_embed_types = embed_types
			}
		}
		if left_sym.kind == .aggregate {
			// the error message contains the problematic type
			unknown_method_msg = err.msg()
		}
	}
	if has_method {
		node.is_noreturn = method.is_noreturn
		node.is_ctor_new = method.is_ctor_new
		if !method.is_pub && !c.pref.is_test && method.mod != c.mod {
			// If a private method is called outside of the module
			// its receiver type is defined in, show an error.
			// println('warn $method_name lef.mod=$left_type_sym.mod c.mod=$c.mod')
			c.error('method `${left_sym.name}.$method_name` is private', node.pos)
		}
		rec_share := method.params[0].typ.share()
		if rec_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('method with `shared` receiver cannot be called inside `lock`/`rlock` block',
				node.pos)
		}
		if method.params[0].is_mut {
			to_lock, pos := c.fail_if_immutable(node.left)
			if !node.left.is_lvalue() {
				c.error('cannot pass expression as `mut`', node.left.pos())
			}
			// node.is_mut = true
			if to_lock != '' && rec_share != .shared_t {
				c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
					pos)
			}
		} else {
			c.fail_if_unreadable(node.left, left_type, 'receiver')
		}
		if left_sym.language != .js && (!left_sym.is_builtin() && method.mod != 'builtin')
			&& method.language == .v && method.no_body {
			c.error('cannot call a method that does not have a body', node.pos)
		}
		if method.return_type == ast.void_type && method.is_conditional
			&& method.ctdefine_idx != ast.invalid_type_idx {
			node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut method.attrs[method.ctdefine_idx])
		}
		c.check_expected_arg_count(mut node, method) or { return method.return_type }
		mut exp_arg_typ := ast.Type(0) // type of 1st arg for special builtin methods
		mut param_is_mut := false
		mut no_type_promotion := false
		if left_sym.kind == .chan {
			elem_typ := (left_sym.info as ast.Chan).elem_type
			if method_name == 'try_push' {
				exp_arg_typ = elem_typ.ref()
			} else if method_name == 'try_pop' {
				exp_arg_typ = elem_typ
				param_is_mut = true
				no_type_promotion = true
			}
		}
		// if method_name == 'clone' {
		// println('CLONE nr args=$method.args.len')
		// }
		// node.args << method.args[0].typ
		// node.exp_arg_types << method.args[0].typ
		for i, mut arg in node.args {
			if i > 0 || exp_arg_typ == ast.Type(0) {
				exp_arg_typ = if method.is_variadic && i >= method.params.len - 1 {
					method.params[method.params.len - 1].typ
				} else {
					method.params[i + 1].typ
				}
				param_is_mut = false
				no_type_promotion = false
			}
			exp_arg_sym := c.table.sym(exp_arg_typ)
			c.expected_type = exp_arg_typ
			mut got_arg_typ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
			node.args[i].typ = got_arg_typ
			if no_type_promotion {
				if got_arg_typ != exp_arg_typ {
					c.error('cannot use `${c.table.sym(got_arg_typ).name}` as argument for `$method.name` (`$exp_arg_sym.name` expected)',
						arg.pos)
				}
			}
			if method.is_variadic && got_arg_typ.has_flag(.variadic) && node.args.len - 1 > i {
				c.error('when forwarding a variadic variable, it must be the final argument',
					arg.pos)
			}
			mut final_arg_sym := exp_arg_sym
			mut final_arg_typ := exp_arg_typ
			if method.is_variadic && exp_arg_sym.info is ast.Array {
				final_arg_typ = exp_arg_sym.array_info().elem_type
				final_arg_sym = c.table.sym(final_arg_typ)
			}
			if exp_arg_typ.has_flag(.generic) {
				if concrete_types.len == 0 {
					continue
				}

				if exp_utyp := c.table.resolve_generic_to_concrete(exp_arg_typ, method.generic_names,
					concrete_types)
				{
					exp_arg_typ = exp_utyp
				} else {
					continue
				}

				if got_arg_typ.has_flag(.generic) {
					if got_utyp := c.table.resolve_generic_to_concrete(got_arg_typ, method.generic_names,
						concrete_types)
					{
						got_arg_typ = got_utyp
					} else {
						continue
					}
				}
			}
			param := if method.is_variadic && i >= method.params.len - 1 {
				method.params[method.params.len - 1]
			} else {
				method.params[i + 1]
			}
			param_is_mut = param_is_mut || param.is_mut
			param_share := param.typ.share()
			if param_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
				c.error('method with `shared` arguments cannot be called inside `lock`/`rlock` block',
					arg.pos)
			}
			if arg.is_mut {
				to_lock, pos := c.fail_if_immutable(arg.expr)
				if !param_is_mut {
					tok := arg.share.str()
					c.error('`$node.name` parameter `$param.name` is not `$tok`, `$tok` is not needed`',
						arg.expr.pos())
				} else {
					if param_share != arg.share {
						c.error('wrong shared type', arg.expr.pos())
					}
					if to_lock != '' && param_share != .shared_t {
						c.error('$to_lock is `shared` and must be `lock`ed to be passed as `mut`',
							pos)
					}
				}
			} else {
				if param_is_mut {
					tok := arg.share.str()
					c.error('`$node.name` parameter `$param.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${
						i + 1}`', arg.expr.pos())
				} else {
					c.fail_if_unreadable(arg.expr, got_arg_typ, 'argument')
				}
			}
			if left_sym.kind == .array && method_name == 'sort_with_compare' {
				array_info := left_sym.info as ast.Array
				elem_typ := array_info.elem_type
				arg_sym := c.table.sym(arg.typ)
				if arg_sym.kind == .function {
					func_info := arg_sym.info as ast.FnType
					if func_info.func.params.len == 2 {
						if func_info.func.params[0].typ.nr_muls() != elem_typ.nr_muls() + 1 {
							arg_typ_str := c.table.type_to_str(func_info.func.params[0].typ)
							expected_typ_str := c.table.type_to_str(elem_typ.ref())
							c.error('sort_with_compare callback function parameter `${func_info.func.params[0].name}` with type `$arg_typ_str` should be `$expected_typ_str`',
								func_info.func.params[0].type_pos)
						}
						if func_info.func.params[1].typ.nr_muls() != elem_typ.nr_muls() + 1 {
							arg_typ_str := c.table.type_to_str(func_info.func.params[1].typ)
							expected_typ_str := c.table.type_to_str(elem_typ.ref())
							c.error('sort_with_compare callback function parameter `${func_info.func.params[1].name}` with type `$arg_typ_str` should be `$expected_typ_str`',
								func_info.func.params[1].type_pos)
						}
					}
				}
			}
			// Handle expected interface
			if final_arg_sym.kind == .interface_ {
				if c.type_implements(got_arg_typ, final_arg_typ, arg.expr.pos()) {
					if !got_arg_typ.is_ptr() && !got_arg_typ.is_pointer() && !c.inside_unsafe {
						got_arg_typ_sym := c.table.sym(got_arg_typ)
						if got_arg_typ_sym.kind != .interface_ {
							c.mark_as_referenced(mut &arg.expr, true)
						}
					}
				}
				continue
			}
			c.check_expected_call_arg(got_arg_typ, exp_arg_typ, node.language, arg) or {
				// str method, allow type with str method if fn arg is string
				// Passing an int or a string array produces a c error here
				// Deleting this condition results in propper V error messages
				// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
				// continue
				// }
				if got_arg_typ != ast.void_type {
					c.error('$err.msg() in argument ${i + 1} to `${left_sym.name}.$method_name`',
						arg.pos)
				}
			}
		}
		if method.is_unsafe && !c.inside_unsafe {
			c.warn('method `${left_sym.name}.$method_name` must be called from an `unsafe` block',
				node.pos)
		}
		if !c.table.cur_fn.is_deprecated && method.is_deprecated {
			c.deprecate_fnmethod('method', '${left_sym.name}.$method.name', method, node)
		}
		// TODO: typ optimize.. this node can get processed more than once
		if node.expected_arg_types.len == 0 {
			for i in 1 .. method.params.len {
				node.expected_arg_types << method.params[i].typ
			}
		}
		if is_method_from_embed {
			node.receiver_type = node.from_embed_types.last().derive(method.params[0].typ)
		} else if is_generic {
			// We need the receiver to be T in cgen.
			// TODO: cant we just set all these to the concrete type in checker? then no need in gen
			node.receiver_type = left_type.derive(method.params[0].typ).set_flag(.generic)
		} else {
			node.receiver_type = method.params[0].typ
		}
		if method.generic_names.len != node.concrete_types.len {
			// no type arguments given in call, attempt implicit instantiation
			c.infer_fn_generic_types(method, mut node)
			concrete_types = node.concrete_types
		} else {
			if node.concrete_types.len > 0 && !node.concrete_types[0].has_flag(.generic) {
				c.table.register_fn_concrete_types(method.fkey(), node.concrete_types)
			}
		}
		// resolve return generics struct to concrete type
		if method.generic_names.len > 0 && method.return_type.has_flag(.generic)
			&& c.table.cur_fn.generic_names.len == 0 {
			node.return_type = c.table.unwrap_generic_type(method.return_type, method.generic_names,
				concrete_types)
		} else {
			node.return_type = method.return_type
		}
		if node.concrete_types.len > 0 && method.return_type != 0
			&& c.table.cur_fn.generic_names.len == 0 {
			if typ := c.table.resolve_generic_to_concrete(method.return_type, method.generic_names,
				concrete_types)
			{
				node.return_type = typ
				return typ
			}
		}
		if node.concrete_types.len > 0 && method.generic_names.len == 0 {
			c.error('a non generic function called like a generic one', node.concrete_list_pos)
		}
		if node.concrete_types.len > method.generic_names.len {
			c.error('too many generic parameters got $node.concrete_types.len, expected $method.generic_names.len',
				node.concrete_list_pos)
		}
		if method.generic_names.len > 0 {
			if !left_type.has_flag(.generic) {
				if left_sym.info is ast.Struct {
					if method.generic_names.len == left_sym.info.concrete_types.len {
						node.concrete_types = left_sym.info.concrete_types
					}
				}
			}
			return node.return_type
		}
		return method.return_type
	}
	// TODO: str methods
	if method_name == 'str' {
		if left_sym.kind == .interface_ {
			iname := left_sym.name
			c.error('interface `$iname` does not have a .str() method. Use typeof() instead',
				node.pos)
		}
		node.receiver_type = left_type
		node.return_type = ast.string_type
		if node.args.len > 0 {
			c.error('.str() method calls should have no arguments', node.pos)
		}
		c.fail_if_unreadable(node.left, left_type, 'receiver')
		return ast.string_type
	} else if method_name == 'free' {
		return ast.void_type
	}
	// call struct field fn type
	// TODO: can we use SelectorExpr for all? this dosent really belong here
	if field := c.table.find_field_with_embeds(left_sym, method_name) {
		field_sym := c.table.sym(c.unwrap_generic(field.typ))
		if field_sym.kind == .function {
			node.is_method = false
			node.is_field = true
			info := field_sym.info as ast.FnType

			c.check_expected_arg_count(mut node, info.func) or { return info.func.return_type }
			node.return_type = info.func.return_type
			mut earg_types := []ast.Type{}

			for i, mut arg in node.args {
				targ := c.check_expr_opt_call(arg.expr, c.expr(arg.expr))
				arg.typ = targ
				earg_types << targ

				if i < info.func.params.len {
					exp_arg_typ := info.func.params[i].typ
					c.check_expected_call_arg(targ, c.unwrap_generic(exp_arg_typ), node.language,
						arg) or {
						if targ != ast.void_type {
							c.error('$err.msg() in argument ${i + 1} to `${left_sym.name}.$method_name`',
								arg.pos)
						}
					}
				}
			}
			node.expected_arg_types = earg_types
			node.is_method = true
			_, node.from_embed_types = c.table.find_field_from_embeds(left_sym, method_name) or {
				return info.func.return_type
			}
			return info.func.return_type
		}
	}
	if left_type != ast.void_type {
		suggestion := util.new_suggestion(method_name, left_sym.methods.map(it.name))
		c.error(suggestion.say(unknown_method_msg), node.pos)
	}
	return ast.void_type
}

fn (mut c Checker) deprecate_fnmethod(kind string, name string, the_fn ast.Fn, node ast.CallExpr) {
	mut deprecation_message := ''
	now := time.now()
	mut after_time := now
	for attr in the_fn.attrs {
		if attr.name == 'deprecated' && attr.arg != '' {
			deprecation_message = attr.arg
		}
		if attr.name == 'deprecated_after' && attr.arg != '' {
			after_time = time.parse_iso8601(attr.arg) or {
				c.error('invalid time format', attr.pos)
				time.now()
			}
		}
	}
	c.deprecate(kind, name, deprecation_message, now, after_time, node.pos)
}

fn (mut c Checker) deprecate(kind string, name string, deprecation_message string, now time.Time, after_time time.Time, pos token.Pos) {
	start_message := '$kind `$name`'
	error_time := after_time.add_days(180)
	if error_time < now {
		c.error(semicolonize('$start_message has been deprecated since $after_time.ymmdd()',
			deprecation_message), pos)
	} else if after_time < now {
		c.warn(semicolonize('$start_message has been deprecated since $after_time.ymmdd(), it will be an error after $error_time.ymmdd()',
			deprecation_message), pos)
	} else if after_time == now {
		c.warn(semicolonize('$start_message has been deprecated', deprecation_message),
			pos)
	} else {
		c.note(semicolonize('$start_message will be deprecated after $after_time.ymmdd(), and will become an error after $error_time.ymmdd()',
			deprecation_message), pos)
	}
}

fn semicolonize(main string, details string) string {
	if details == '' {
		return main
	}
	return '$main; $details'
}

fn (mut c Checker) post_process_generic_fns() {
	// Loop thru each generic function concrete type.
	// Check each specific fn instantiation.
	for i in 0 .. c.file.generic_fns.len {
		mut node := c.file.generic_fns[i]
		c.mod = node.mod
		fkey := node.fkey()
		gtypes := c.table.fn_generic_types[fkey]
		$if trace_post_process_generic_fns ? {
			eprintln('> post_process_generic_fns $node.mod | $node.name | fkey: $fkey | gtypes: $gtypes')
		}
		for concrete_types in gtypes {
			c.table.cur_concrete_types = concrete_types
			c.fn_decl(mut node)
			if node.name == 'vweb.run' {
				for ct in concrete_types {
					if ct !in c.vweb_gen_types {
						c.vweb_gen_types << ct
					}
				}
			}
		}
		c.table.cur_concrete_types = []
		$if trace_post_process_generic_fns ? {
			if node.generic_names.len > 0 {
				eprintln('       > fn_decl node.name: $node.name | generic_names: $node.generic_names | ninstances: $node.ninstances')
			}
		}
	}
}

pub fn (mut c Checker) check_expected_arg_count(mut node ast.CallExpr, f &ast.Fn) ? {
	nr_args := node.args.len
	nr_params := if node.is_method && f.params.len > 0 { f.params.len - 1 } else { f.params.len }
	mut min_required_params := f.params.len
	if node.is_method {
		min_required_params--
	}
	if f.is_variadic {
		min_required_params--
	}
	if min_required_params < 0 {
		min_required_params = 0
	}
	if nr_args < min_required_params {
		if min_required_params == nr_args + 1 {
			last_typ := f.params.last().typ
			last_sym := c.table.sym(last_typ)
			if last_sym.info is ast.Struct {
				is_params := last_sym.info.attrs.filter(it.name == 'params' && !it.has_arg).len > 0
				if is_params {
					// allow empty trailing struct syntax arg (`f()` where `f` is `fn(ConfigStruct)`)
					node.args << ast.CallArg{
						expr: ast.StructInit{
							typ: last_typ
						}
					}
					return
				}
			}
		}
		c.error('expected $min_required_params arguments, but got $nr_args', node.pos)
		return error('')
	} else if !f.is_variadic && nr_args > nr_params {
		unexpected_args_pos := node.args[min_required_params].pos.extend(node.args.last().pos)
		c.error('expected $min_required_params arguments, but got $nr_args', unexpected_args_pos)
		return error('')
	}
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ ast.Type, node ast.CallExpr) {
	if node.args.len != 1 {
		c.error('expected 1 argument, but got $node.args.len', node.pos)
		// Finish early so that it doesn't fail later
		return
	}
	elem_sym := c.table.sym(elem_typ)
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
				c.error('$err.msg() in argument 1 to `Map.delete`', node.args[0].pos)
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
		} else if !(c.table.sym(elem_typ).has_method('<')
			|| c.table.unalias_num_type(elem_typ) in [ast.int_type, ast.int_type.ref(), ast.string_type, ast.string_type.ref(), ast.i8_type, ast.i16_type, ast.i64_type, ast.byte_type, ast.rune_type, ast.u16_type, ast.u32_type, ast.u64_type, ast.f32_type, ast.f64_type, ast.char_type, ast.bool_type, ast.float_literal_type, ast.int_literal_type]) {
			c.error('custom sorting condition must be supplied for type `${c.table.type_to_str(elem_typ)}`',
				node.pos)
		}
	} else if method_name == 'wait' {
		elem_sym := c.table.sym(elem_typ)
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
				node.left.pos())
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
		arg_sym := c.table.sym(arg_type)
		ret_type := match arg_sym.info {
			ast.FnType { arg_sym.info.func.return_type }
			else { arg_type }
		}
		node.return_type = c.table.find_or_register_array(c.unwrap_generic(ret_type))
		ret_sym := c.table.sym(ret_type)
		if ret_sym.kind == .multi_return {
			c.error('returning multiple values is not supported in .map() calls', node.pos)
		}
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

fn scope_register_it(mut s ast.Scope, pos token.Pos, typ ast.Type) {
	s.register(ast.Var{
		name: 'it'
		pos: pos
		typ: typ
		is_used: true
	})
}

fn scope_register_a_b(mut s ast.Scope, pos token.Pos, typ ast.Type) {
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
