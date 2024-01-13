module checker

import v.ast
import v.pref
import v.util
import v.token

fn (mut c Checker) fn_decl(mut node ast.FnDecl) {
	$if trace_post_process_generic_fns_types ? {
		if node.generic_names.len > 0 {
			eprintln('>>> post processing node.name: ${node.name:-30} | ${node.generic_names} <=> ${c.table.cur_concrete_types}')
		}
	}
	// record the vweb route methods (public non-generic methods):
	if node.generic_names.len > 0 && node.is_pub {
		typ_vweb_result := c.table.find_type_idx('vweb.Result')
		if node.return_type == typ_vweb_result {
			rec_sym := c.table.sym(node.receiver.typ)
			if rec_sym.kind == .struct_ {
				if _ := c.table.find_field_with_embeds(rec_sym, 'Context') {
					// there is no point in the message here, for methods that are not public; since they will not be available as routes anyway
					c.note('generic method routes of vweb will be skipped', node.pos)
				}
			}
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
			if node.is_method {
				c.add_error_detail('use `fn (r SomeType[T]) foo[T]() {`, not just `fn (r SomeType[T]) foo() {`')
				c.error('generic method declaration must specify generic type names',
					node.pos)
			} else {
				c.add_error_detail('use `fn foo[T](x T) {`, not just `fn foo(x T) {`')
				c.error('generic function declaration must specify generic type names',
					node.pos)
			}
		}
	}
	if node.language == .v && !c.is_builtin_mod && !node.is_anon {
		c.check_valid_snake_case(node.get_name(), 'function name', node.pos)
		if !node.is_method && node.mod == 'main' && node.short_name in c.table.builtin_pub_fns {
			c.error('cannot redefine builtin public function `${node.short_name}`', node.pos)
		}
	}
	if node.name == 'main.main' {
		c.main_fn_decl_node = *node
	}
	if node.language == .v && node.attrs.len > 0 {
		if attr_export := node.attrs.find_first('export') {
			if attr_export.arg == '' {
				c.error('missing argument for @[export] attribute', attr_export.pos)
			}
		}
	}
	c.fn_return_type = node.return_type
	if node.return_type != ast.void_type {
		if ct_attr_idx := node.attrs.find_comptime_define() {
			sexpr := node.attrs[ct_attr_idx].ct_expr.str()
			c.error('only functions that do NOT return values can have `[if ${sexpr}]` tags',
				node.pos)
		}
		if node.generic_names.len > 0 {
			gs := c.table.sym(node.return_type)
			if gs.info is ast.Struct {
				if gs.info.is_generic && !node.return_type.has_flag(.generic) {
					c.error('return generic struct `${gs.name}` in fn declaration must specify the generic type names, e.g. ${gs.name}[T]',
						node.return_type_pos)
				}
			}
		}
		return_sym := c.table.sym(node.return_type)
		if return_sym.info is ast.Alias {
			parent_sym := c.table.sym(return_sym.info.parent_type)
			if parent_sym.info is ast.ArrayFixed {
				c.table.find_or_register_array_fixed(parent_sym.info.elem_type, parent_sym.info.size,
					parent_sym.info.size_expr, true)
			}
			if return_sym.name == 'byte' {
				c.warn('byte is deprecated, use u8 instead', node.return_type_pos)
			}
		}
		final_return_sym := c.table.final_sym(node.return_type)
		if final_return_sym.info is ast.MultiReturn {
			for multi_type in final_return_sym.info.types {
				if multi_type == ast.error_type {
					c.error('type `IError` cannot be used in multi-return, return an Option instead',
						node.return_type_pos)
				} else if multi_type.has_flag(.result) {
					c.error('result cannot be used in multi-return, return a Result instead',
						node.return_type_pos)
				}
			}
		}
		// Ensure each generic type of the parameter was declared in the function's definition
		if node.return_type.has_flag(.generic) {
			generic_names := c.table.generic_type_names(node.return_type)
			for name in generic_names {
				if name !in node.generic_names {
					fn_generic_names := node.generic_names.join(', ')
					c.error('generic type name `${name}` is not mentioned in fn `${node.name}[${fn_generic_names}]`',
						node.return_type_pos)
				}
			}
		}
	} else {
		for mut a in node.attrs {
			if a.kind == .comptime_define {
				node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut a)
			}
		}
	}
	if node.is_method {
		if node.receiver.typ.has_flag(.option) {
			c.error('option types cannot have methods', node.receiver_pos)
		}
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
			c.error('unknown type `${sym.name}`', node.receiver_pos)
			return
		}
		// make sure interface does not implement its own interface methods
		if mut sym.info is ast.Interface && sym.has_method(node.name) {
			// if the method is in info.methods then it is an interface method
			if sym.info.has_method(node.name) {
				c.error('interface `${sym.name}` cannot implement its own interface method `${node.name}`',
					node.pos)
			}
		}
		if mut sym.info is ast.Struct {
			if field := c.table.find_field(sym, node.name) {
				field_sym := c.table.sym(field.typ)
				if field_sym.kind == .function {
					c.error('type `${sym.name}` has both field and method named `${node.name}`',
						node.pos)
				}
			}
			if node.name == 'free' {
				if node.return_type != ast.void_type {
					c.error('`.free()` methods should not have a return type', node.return_type_pos)
				}
				if !node.receiver.typ.is_ptr() {
					tname := sym.name.after_char(`.`)
					c.error('`.free()` methods should be defined on either a `(mut x &${tname})`, or a `(x &${tname})` receiver',
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
			c.error('method index: ${node.method_idx} >= sym.methods.len: ${sym.methods.len}',
				node.pos)
		}
	}
	if node.language == .v {
		// Make sure all types are valid
		for mut param in node.params {
			if !c.ensure_type_exists(param.typ, param.type_pos) {
				return
			}
			if reserved_type_names_chk.matches(param.name) {
				c.error('invalid use of reserved type `${param.name}` as a parameter name',
					param.pos)
			}
			if param.typ.has_flag(.result) {
				c.error('Result type argument is not supported currently', param.type_pos)
			}
			arg_typ_sym := c.table.sym(param.typ)
			pure_sym_name := arg_typ_sym.embed_name()
			if arg_typ_sym.info is ast.Struct {
				if !param.typ.is_ptr() && arg_typ_sym.info.is_heap { // set auto_heap to promote value parameter
					mut v := node.scope.find_var(param.name) or { continue }
					v.is_auto_heap = true
				}
				if arg_typ_sym.info.generic_types.len > 0 && !param.typ.has_flag(.generic)
					&& arg_typ_sym.info.concrete_types.len == 0 {
					c.error('generic struct `${pure_sym_name}` in fn declaration must specify the generic type names, e.g. ${pure_sym_name}[T]',
						param.type_pos)
				}
			} else if arg_typ_sym.info is ast.Interface {
				if arg_typ_sym.info.generic_types.len > 0 && !param.typ.has_flag(.generic)
					&& arg_typ_sym.info.concrete_types.len == 0 {
					c.error('generic interface `${pure_sym_name}` in fn declaration must specify the generic type names, e.g. ${pure_sym_name}[T]',
						param.type_pos)
				}
			} else if arg_typ_sym.info is ast.SumType {
				if arg_typ_sym.info.generic_types.len > 0 && !param.typ.has_flag(.generic)
					&& arg_typ_sym.info.concrete_types.len == 0 {
					c.error('generic sumtype `${pure_sym_name}` in fn declaration must specify the generic type names, e.g. ${pure_sym_name}[T]',
						param.type_pos)
				}
			}
			// Ensure each generic type of the parameter was declared in the function's definition
			if param.typ.has_flag(.generic) {
				generic_names := c.table.generic_type_names(param.typ)
				for name in generic_names {
					if name !in node.generic_names {
						fn_generic_names := node.generic_names.join(', ')
						c.error('generic type name `${name}` is not mentioned in fn `${node.name}[${fn_generic_names}]`',
							param.type_pos)
					}
				}
			}
			if param.name == node.mod && param.name != 'main' {
				c.error('duplicate of a module name `${param.name}`', param.pos)
			}
			// Check if parameter name is already registered as imported module symbol
			if c.check_import_sym_conflict(param.name) {
				c.error('duplicate of an import symbol `${param.name}`', param.pos)
			}
			if arg_typ_sym.kind == .alias && arg_typ_sym.name == 'byte' {
				c.warn('byte is deprecated, use u8 instead', param.type_pos)
			}
		}
		if !node.is_method {
			// Check if function name is already registered as imported module symbol
			if c.check_import_sym_conflict(node.short_name) {
				c.error('duplicate of an import symbol `${node.short_name}`', node.pos)
			}
			if node.params.len == 0 && node.name.after_char(`.`) == 'init' {
				if node.is_pub {
					c.error('fn `init` must not be public', node.pos)
				}
				if node.return_type != ast.void_type {
					c.error('fn `init` cannot have a return type', node.pos)
				}
			}
			if !c.is_builtin_mod && node.mod == 'main'
				&& node.name.after_char(`.`) in reserved_type_names {
				c.error('top level declaration cannot shadow builtin type', node.pos)
			}
		}
	}
	if node.return_type != ast.Type(0) {
		if !c.ensure_type_exists(node.return_type, node.return_type_pos) {
			return
		}
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
				receiver_type := node.receiver.typ
				receiver_sym := c.table.sym(receiver_type)

				param_type := node.params[1].typ
				param_sym := c.table.sym(param_type)

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
					} else if !c.check_same_type_ignoring_pointers(node.receiver.typ,
						node.params[1].typ) {
						c.error('expected `${receiver_sym.name}` not `${param_sym.name}` - both operands must be the same type for operator overloading',
							node.params[1].type_pos)
					} else if node.return_type != ast.bool_type && node.name in ['<', '=='] {
						c.error('operator comparison methods should return `bool`', node.pos)
					} else if parent_sym.is_primitive() {
						// aliases of primitive types are explicitly allowed
					} else if receiver_type != param_type {
						srtype := c.table.type_to_str(receiver_type)
						sptype := c.table.type_to_str(param_type)
						c.error('the receiver type `${srtype}` should be the same type as the operand `${sptype}`',
							node.pos)
					} else if node.return_type.has_option_or_result() {
						c.error('return type cannot be Option or Result', node.return_type_pos)
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
			&& node.return_type.clear_flag(.option) != ast.void_type_idx
			&& node.return_type.clear_flag(.result) != ast.void_type_idx {
			c.error('test functions should either return nothing at all, or be marked to return `?` or `!`',
				node.pos)
		}
	}
	c.expected_type = ast.void_type
	c.table.cur_fn = unsafe { node }
	// c.table.cur_fn = node
	// Add return if `fn(...) ? {...}` have no return at end
	if node.return_type != ast.void_type && node.return_type.has_flag(.option)
		&& (node.stmts.len == 0 || node.stmts.last() !is ast.Return) {
		sym := c.table.sym(node.return_type)
		if sym.kind == .void {
			node.stmts << ast.Return{
				pos: node.pos
			}
		}
	}
	// same for result `fn (...) ! { ... }`
	if node.return_type != ast.void_type && node.return_type.has_flag(.result)
		&& (node.stmts.len == 0 || node.stmts.last() !is ast.Return) {
		sym := c.table.sym(node.return_type)
		if sym.kind == .void {
			node.stmts << ast.Return{
				pos: node.pos
			}
		}
	}
	c.fn_scope = node.scope
	c.stmts(mut node.stmts)
	node_has_top_return := has_top_return(node.stmts)
	node.has_return = c.returns || node_has_top_return
	c.check_noreturn_fn_decl(mut node)
	if node.language == .v && !node.no_body && node.return_type != ast.void_type && !node.has_return
		&& !node.is_noreturn {
		if c.inside_anon_fn {
			c.error('missing return at the end of an anonymous function', node.pos)
		} else if !node.attrs.contains('_naked') {
			c.error('missing return at end of function `${node.name}`', node.pos)
		}
	}
	node.source_file = c.file

	if node.name in c.table.fns {
		if node.name != 'main.main' {
			mut dep_names := []string{}
			for stmt in node.stmts {
				dnames := c.table.dependent_names_in_stmt(stmt)
				for dname in dnames {
					if dname in dep_names {
						continue
					}
					dep_names << dname
				}
			}
			if dep_names.len > 0 {
				unsafe {
					c.table.fns[node.name].dep_names = dep_names
				}
			}
		}
		unsafe {
			c.table.fns[node.name].source_fn = voidptr(node)
		}
	}

	// vweb checks
	if node.attrs.len > 0 && c.file.imports.any(it.mod == 'vweb') {
		// If it's a vweb action (has the ['/url'] attribute), make sure it returns a vweb.Result
		for attr in node.attrs {
			if attr.name.starts_with('/') {
				if c.table.sym(node.return_type).name != 'vweb.Result' {
					c.error('vweb actions must return `vweb.Result`', node.pos)
				}
				break
			}
		}
	}
}

// check_same_type_ignoring_pointers util function to check if the Types are the same, including all
// corner cases.
// FIXME: if the optimization is done after the checker, we can safely remove this util function
fn (c Checker) check_same_type_ignoring_pointers(type_a ast.Type, type_b ast.Type) bool {
	// FIXME: if possible pass the ast.Node and check the property `is_auto_rec`
	if type_a != type_b {
		// before failing we must be sure that the parser didn't optimize the function
		clean_type_a := type_a.set_nr_muls(0)
		clean_type_b := type_b.set_nr_muls(0)
		return clean_type_a == clean_type_b
	}
	return true
}

fn (mut c Checker) anon_fn(mut node ast.AnonFn) ast.Type {
	keep_fn := c.table.cur_fn
	keep_inside_anon := c.inside_anon_fn
	keep_anon_fn := c.cur_anon_fn
	defer {
		c.table.cur_fn = keep_fn
		c.inside_anon_fn = keep_inside_anon
		c.cur_anon_fn = keep_anon_fn
	}
	if node.decl.no_body {
		c.error('anonymous function must declare a body', node.decl.pos)
	}
	for param in node.decl.params {
		if param.name.len == 0 {
			c.error('use `_` to name an unused parameter', param.pos)
		}
	}
	c.table.cur_fn = unsafe { &node.decl }
	c.inside_anon_fn = true
	c.cur_anon_fn = unsafe { &node }
	mut has_generic := false
	for mut var in node.inherited_vars {
		parent_var := node.decl.scope.parent.find_var(var.name) or {
			panic('unexpected checker error: cannot find parent of inherited variable `${var.name}`')
		}
		if var.is_mut && !parent_var.is_mut {
			c.error('original `${parent_var.name}` is immutable, declare it with `mut` to make it mutable',
				var.pos)
		}
		if parent_var.expr is ast.IfGuardExpr {
			sym := c.table.sym(parent_var.expr.expr_type)
			if sym.info is ast.MultiReturn {
				for i, v in parent_var.expr.vars {
					if v.name == var.name {
						var.typ = sym.info.types[i]
						break
					}
				}
			} else {
				var.typ = parent_var.expr.expr_type.clear_option_and_result()
			}
		} else {
			var.typ = parent_var.typ
		}
		if var.typ.has_flag(.generic) {
			has_generic = true
		}
		node.decl.scope.update_var_type(var.name, var.typ)
	}
	if has_generic && node.decl.generic_names.len == 0 {
		c.error('generic closure fn must specify type parameter, e.g. fn [foo] [T]()',
			node.decl.pos)
	}
	c.stmts(mut node.decl.stmts)
	c.fn_decl(mut node.decl)
	return node.typ
}

fn (mut c Checker) call_expr(mut node ast.CallExpr) ast.Type {
	// Check whether the inner function definition is before the call
	if var := node.scope.find_var(node.name) {
		if var.expr is ast.AnonFn && var.pos.pos > node.pos.pos {
			c.error('unknown function: ${node.name}', node.pos)
		}
	}
	// If the left expr has an or_block, it needs to be checked for legal or_block statement.
	left_type := c.expr(mut node.left)
	c.check_expr_option_or_result_call(node.left, left_type)
	// TODO merge logic from method_call and fn_call
	// First check everything that applies to both fns and methods
	old_inside_fn_arg := c.inside_fn_arg
	c.inside_fn_arg = true
	mut continue_check := true
	// Now call `method_call` or `fn_call` for specific checks.
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
		&& !c.inside_const && !node.args[0].typ.has_flag(.option)
		&& !node.args[0].typ.has_flag(.result) && !(node.args[0].expr is ast.CallExpr
		&& (node.args[0].expr.return_type.has_flag(.option)
		|| node.args[0].expr.return_type.has_flag(.result)))
	if free_tmp_arg_vars {
		for i, arg in node.args {
			if arg.typ != ast.string_type {
				continue
			}
			if arg.expr in [ast.Ident, ast.StringLiteral, ast.SelectorExpr]
				|| (arg.expr is ast.CallExpr && arg.expr.or_block.kind != .absent) {
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
	c.expected_or_type = node.return_type.clear_flag(.result)
	c.stmts_ending_with_expression(mut node.or_block.stmts)
	c.expected_or_type = ast.void_type

	if !c.inside_const && c.table.cur_fn != unsafe { nil } && !c.table.cur_fn.is_main
		&& !c.table.cur_fn.is_test {
		// TODO: use just `if node.or_block.kind == .propagate_result && !c.table.cur_fn.return_type.has_flag(.result) {` after the deprecation for ?!Type
		if node.or_block.kind == .propagate_result && !c.table.cur_fn.return_type.has_flag(.result)
			&& !c.table.cur_fn.return_type.has_flag(.option) {
			c.add_instruction_for_result_type()
			c.error('to propagate the Result call, `${c.table.cur_fn.name}` must return a Result',
				node.or_block.pos)
		}
		if node.or_block.kind == .propagate_option && !c.table.cur_fn.return_type.has_flag(.option) {
			c.add_instruction_for_option_type()
			c.error('to propagate the Option call, `${c.table.cur_fn.name}` must return an Option',
				node.or_block.pos)
		}
	}

	return typ
}

fn (mut c Checker) builtin_args(mut node ast.CallExpr, fn_name string, func ast.Fn) {
	c.inside_interface_deref = true
	c.expected_type = ast.string_type
	node.args[0].typ = c.expr(mut node.args[0].expr)
	arg := node.args[0]
	c.check_expr_option_or_result_call(arg.expr, arg.typ)
	if arg.typ.is_void() {
		c.error('`${fn_name}` can not print void expressions', node.pos)
	} else if arg.typ == ast.char_type && arg.typ.nr_muls() == 0 {
		c.error('`${fn_name}` cannot print type `char` directly, print its address or cast it to an integer instead',
			node.pos)
	} else if arg.expr is ast.ArrayDecompose {
		c.error('`${fn_name}` cannot print variadic values', node.pos)
	}
	c.fail_if_unreadable(arg.expr, arg.typ, 'argument to print')
	c.inside_interface_deref = false
	node.return_type = ast.void_type
	c.set_node_expected_arg_types(mut node, func)

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
}

fn (mut c Checker) needs_unwrap_generic_type(typ ast.Type) bool {
	if typ == 0 || !typ.has_flag(.generic) {
		return false
	}
	sym := c.table.sym(typ)
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			return true
		}
		ast.Array {
			return c.needs_unwrap_generic_type(sym.info.elem_type)
		}
		ast.ArrayFixed {
			return c.needs_unwrap_generic_type(sym.info.elem_type)
		}
		ast.Map {
			if c.needs_unwrap_generic_type(sym.info.key_type) {
				return true
			}
			if c.needs_unwrap_generic_type(sym.info.value_type) {
				return true
			}
		}
		ast.Chan {
			return c.needs_unwrap_generic_type(sym.info.elem_type)
		}
		ast.Thread {
			return c.needs_unwrap_generic_type(sym.info.return_type)
		}
		else {
			return false
		}
	}
	return false
}

fn (mut c Checker) fn_call(mut node ast.CallExpr, mut continue_check &bool) ast.Type {
	fn_name := node.name
	if fn_name == 'main' {
		c.error('the `main` function cannot be called in the program', node.pos)
	}
	mut has_generic := false // foo[T]() instead of foo[int]()
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
	if c.table.cur_fn != unsafe { nil } && c.table.cur_concrete_types.len == 0 && has_generic {
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

		typ := c.expr(mut node.args[0].expr)
		tsym := c.table.sym(typ)

		if !tsym.name.starts_with('Promise[') {
			c.error('JS.await: first argument must be a promise, got `${tsym.name}`',
				node.pos)
			return ast.void_type
		}
		if c.table.cur_fn != unsafe { nil } {
			c.table.cur_fn.has_await = true
		}
		match tsym.info {
			ast.Struct {
				mut ret_type := tsym.info.concrete_types[0]
				ret_type = ret_type.set_flag(.option)
				node.return_type = ret_type
				return ret_type
			}
			else {
				c.error('JS.await: Promise must be a struct type', node.pos)
				return ast.void_type
			}
		}
		panic('unreachable')
	} else if node.args.len > 0 && node.args[0].typ.has_flag(.shared_f) && fn_name == 'json.encode' {
		c.error('json.encode cannot handle shared data', node.pos)
		return ast.void_type
	} else if node.args.len > 0 && fn_name == 'json.decode' {
		if node.args.len != 2 {
			c.error("json.decode expects 2 arguments, a type and a string (e.g `json.decode(T, '')`)",
				node.pos)
			return ast.void_type
		}
		expr := node.args[0].expr
		if expr is ast.TypeNode {
			mut unwrapped_typ := c.unwrap_generic(expr.typ)
			if c.needs_unwrap_generic_type(expr.typ) {
				unwrapped_typ = c.table.unwrap_generic_type(expr.typ, c.table.cur_fn.generic_names,
					c.table.cur_concrete_types)
			}
			sym := c.table.sym(unwrapped_typ)
			if c.table.known_type(sym.name) && sym.kind != .placeholder {
				mut kind := sym.kind
				if sym.info is ast.Alias {
					kind = c.table.sym(sym.info.parent_type).kind
				}
				if kind !in [.struct_, .sum_type, .map, .array] {
					c.error('json.decode: expected sum type, struct, map or array, found ${kind}',
						expr.pos)
				}
			} else {
				c.error('json.decode: unknown type `${sym.name}`', node.pos)
			}
		} else {
			typ := expr.type_name()
			c.error('json.decode: first argument needs to be a type, got `${typ}`', node.pos)
			return ast.void_type
		}
		c.expected_type = ast.string_type
		node.args[1].typ = c.expr(mut node.args[1].expr)
		if node.args[1].typ != ast.string_type {
			c.error('json.decode: second argument needs to be a string', node.pos)
		}
		typ := expr as ast.TypeNode
		ret_type := typ.typ.set_flag(.result)
		node.return_type = ret_type
		return ret_type
	} else if fn_name == '__addr' {
		if !c.inside_unsafe {
			c.error('`__addr` must be called from an unsafe block', node.pos)
		}
		if node.args.len != 1 {
			c.error('`__addr` requires 1 argument', node.pos)
			return ast.void_type
		}
		typ := c.expr(mut node.args[0].expr)
		node.args[0].typ = typ
		node.return_type = typ.ref()
		return node.return_type
	}
	// look for function in format `mod.fn` or `fn` (builtin)
	mut func := ast.Fn{}
	mut found := false
	mut found_in_args := false
	// anon fn direct call
	if node.left is ast.AnonFn {
		// it was set to anon for checker errors, clear for gen
		node.name = ''
		c.expr(mut node.left)
		left := node.left as ast.AnonFn
		if left.typ != ast.Type(0) {
			anon_fn_sym := c.table.sym(left.typ)
			func = (anon_fn_sym.info as ast.FnType).func
			found = true
		}
	}
	// try prefix with current module as it would have never gotten prefixed
	if !found && node.mod != 'builtin' && !fn_name.contains('.') {
		name_prefixed := '${node.mod}.${fn_name}'
		if f := c.table.find_fn(name_prefixed) {
			node.name = name_prefixed
			found = true
			func = f
			unsafe { c.table.fns[name_prefixed].usages++ }
		}
	}
	if !found && node.left is ast.IndexExpr {
		c.expr(mut node.left)
		left := node.left as ast.IndexExpr
		sym := c.table.final_sym(left.left_type)
		if sym.info is ast.Array {
			elem_sym := c.table.sym(sym.info.elem_type)
			if elem_sym.info is ast.FnType {
				func = elem_sym.info.func
				found = true
				node.is_fn_var = true
				node.fn_var_type = sym.info.elem_type
			} else {
				c.error('cannot call the element of the array, it is not a function',
					node.pos)
			}
		} else if sym.info is ast.Map {
			value_sym := c.table.sym(sym.info.value_type)
			if value_sym.info is ast.FnType {
				func = value_sym.info.func
				found = true
				node.is_fn_var = true
				node.fn_var_type = sym.info.value_type
			} else {
				c.error('cannot call the value of the map, it is not a function', node.pos)
			}
		} else if sym.info is ast.ArrayFixed {
			elem_sym := c.table.sym(sym.info.elem_type)
			if elem_sym.info is ast.FnType {
				func = elem_sym.info.func
				found = true
				node.is_fn_var = true
				node.fn_var_type = sym.info.elem_type
			} else {
				c.error('cannot call the element of the array, it is not a function',
					node.pos)
			}
		}
	}
	if !found && node.left is ast.CallExpr {
		c.expr(mut node.left)
		left := node.left as ast.CallExpr
		if left.return_type != 0 {
			sym := c.table.sym(left.return_type)
			if sym.info is ast.FnType {
				node.return_type = sym.info.func.return_type
				found = true
				func = sym.info.func
			}
		}
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f := c.table.find_fn(fn_name) {
			found = true
			func = f
			unsafe { c.table.fns[fn_name].usages++ }
		}
	}
	// already imported symbol (static Foo.new() in another module)
	if !found && fn_name.len > 0 && fn_name[0].is_capital() {
		if index := fn_name.index('__static__') {
			owner_name := fn_name#[..index]
			for import_sym in c.file.imports.filter(it.syms.any(it.name == owner_name)) {
				qualified_name := '${import_sym.mod}.${fn_name}'
				if f := c.table.find_fn(qualified_name) {
					found = true
					func = f
					node.name = qualified_name
					unsafe { c.table.fns[qualified_name].usages++ }
					break
				}
			}
		}
	}
	// Enum.from_string, `mod.Enum.from_string('item')`, `Enum.from_string('item')`
	if !found && fn_name.ends_with('__static__from_string') {
		enum_name := fn_name.all_before('__static__')
		mut full_enum_name := if !enum_name.contains('.') {
			c.mod + '.' + enum_name
		} else {
			enum_name
		}
		mut idx := c.table.type_idxs[full_enum_name]
		if idx > 0 {
			// is from another mod.
			if enum_name.contains('.') {
				if !c.check_type_and_visibility(full_enum_name, idx, .enum_, node.pos) {
					return ast.void_type
				}
			}
		} else if !enum_name.contains('.') {
			// find from another mods.
			for import_sym in c.file.imports {
				full_enum_name = '${import_sym.mod}.${enum_name}'
				idx = c.table.type_idxs[full_enum_name]
				if idx < 1 {
					continue
				}
				if !c.check_type_and_visibility(full_enum_name, idx, .enum_, node.pos) {
					return ast.void_type
				}
				break
			}
		}
		if idx == 0 {
			c.error('unknown enum `${enum_name}`', node.pos)
			return ast.void_type
		}

		ret_typ := ast.Type(idx).set_flag(.option)
		if node.args.len != 1 {
			c.error('expected 1 argument, but got ${node.args.len}', node.pos)
		} else {
			node.args[0].typ = c.expr(mut node.args[0].expr)
			if node.args[0].typ != ast.string_type {
				styp := c.table.type_to_str(node.args[0].typ)
				c.error('expected `string` argument, but got `${styp}`', node.pos)
			}
		}
		node.return_type = ret_typ
		return ret_typ
	}
	mut is_native_builtin := false
	if !found && c.pref.backend == .native {
		if fn_name in ast.native_builtins {
			unsafe { c.table.fns[fn_name].usages++ }
			found = true
			func = unsafe { c.table.fns[fn_name] }
			is_native_builtin = true
		}
	}
	if !found && c.pref.is_vsh {
		// TODO: test this hack more extensively
		os_name := 'os.${fn_name}'
		if f := c.table.find_fn(os_name) {
			if f.generic_names.len == node.concrete_types.len {
				node_alias_name := node.fkey()
				mut existing := c.table.fn_generic_types[os_name] or { [] }
				existing << c.table.fn_generic_types[node_alias_name]
				existing << node.concrete_types
				c.table.fn_generic_types[os_name] = existing
			}
			node.name = os_name
			found = true
			func = f
			unsafe { c.table.fns[os_name].usages++ }
		}
	}
	if is_native_builtin {
		if node.args.len > 0 && fn_name in ['println', 'print', 'eprintln', 'eprint', 'panic'] {
			c.builtin_args(mut node, fn_name, func)
			return func.return_type
		}
		return ast.void_type
	}
	// check for arg (var) of fn type
	if !found {
		mut typ := 0
		if mut obj := node.scope.find(node.name) {
			match mut obj {
				ast.GlobalField {
					typ = obj.typ
					node.is_fn_var = true
					node.fn_var_type = typ
				}
				ast.Var {
					if obj.smartcasts.len != 0 {
						typ = obj.smartcasts.last()
					} else {
						if obj.typ == 0 {
							if mut obj.expr is ast.IfGuardExpr {
								typ = c.expr(mut obj.expr.expr)
							} else {
								typ = c.expr(mut obj.expr)
							}
						} else {
							typ = obj.typ
						}
					}
					node.is_fn_var = true
					node.fn_var_type = typ
				}
				else {}
			}
		}

		if typ != 0 {
			generic_vts := c.table.final_sym(typ)
			if generic_vts.info is ast.FnType {
				func = generic_vts.info.func
				found = true
				found_in_args = true
			} else {
				vts := c.table.sym(c.unwrap_generic(typ))
				if vts.info is ast.FnType {
					func = vts.info.func
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
				if sym.info is ast.FnType {
					func = sym.info.func
					found = true
				}
			}
		}
	}
	// a same module constant?
	if !found {
		// allow for `const abc = myfunc`, then calling `abc()`
		qualified_const_name := if fn_name.contains('.') { fn_name } else { '${c.mod}.${fn_name}' }
		if mut obj := c.table.global_scope.find_const(qualified_const_name) {
			if obj.typ == 0 {
				obj.typ = c.expr(mut obj.expr)
			}
			if obj.typ != 0 {
				sym := c.table.sym(obj.typ)
				if sym.info is ast.FnType {
					// at this point, the const metadata should be already known,
					// and we are sure that it is just a function
					unsafe { c.table.fns[qualified_const_name].usages++ }
					unsafe { c.table.fns[func.name].usages++ }
					found = true
					func = sym.info.func
					node.is_fn_a_const = true
					node.fn_var_type = obj.typ
					node.const_name = qualified_const_name
				}
			}
		}
	}

	if !found {
		continue_check = false
		if dot_index := fn_name.index('.') {
			if !fn_name[0].is_capital() {
				mod_name := fn_name#[..dot_index]
				mut mod_func_names := []string{}
				for ctfnk, ctfnv in c.table.fns {
					if ctfnv.is_pub && ctfnk.starts_with(mod_name) {
						mod_func_names << ctfnk
					}
				}
				suggestion := util.new_suggestion(fn_name, mod_func_names)
				c.error(suggestion.say('unknown function: ${fn_name} '), node.pos)
				return ast.void_type
			}
		}
		c.error('unknown function: ${node.get_name()}', node.pos)
		return ast.void_type
	}
	node.is_file_translated = func.is_file_translated
	node.is_noreturn = func.is_noreturn
	node.is_ctor_new = func.is_ctor_new
	if !found_in_args {
		if node.scope.known_var(fn_name) {
			c.error('ambiguous call to: `${fn_name}`, may refer to fn `${fn_name}` or variable `${fn_name}`',
				node.pos)
		}
	}
	if !func.is_pub && func.language == .v && func.name.len > 0 && func.mod.len > 0
		&& func.mod != c.mod && !c.pref.is_test {
		c.error('function `${func.name}` is private', node.pos)
	}
	if c.table.cur_fn != unsafe { nil } && !c.table.cur_fn.is_deprecated && func.is_deprecated {
		c.deprecate('function', func.name, func.attrs, node.pos)
	}
	if func.is_unsafe && !c.inside_unsafe
		&& (func.language != .c || (func.name[2] in [`m`, `s`] && func.mod == 'builtin')) {
		// builtin C.m*, C.s* only - temp
		c.warn('function `${func.name}` must be called from an `unsafe` block', node.pos)
	}
	node.is_keep_alive = func.is_keep_alive
	if func.language == .v && func.no_body && !c.pref.translated && !c.file.is_translated
		&& !func.is_unsafe && !func.is_file_translated && func.mod != 'builtin' {
		c.error('cannot call a function that does not have a body', node.pos)
	}
	if node.concrete_types.len > 0 && func.generic_names.len > 0
		&& node.concrete_types.len != func.generic_names.len {
		plural := if func.generic_names.len == 1 { '' } else { 's' }
		c.error('expected ${func.generic_names.len} generic parameter${plural}, got ${node.concrete_types.len}',
			node.concrete_list_pos)
	}
	for concrete_type in node.concrete_types {
		c.ensure_type_exists(concrete_type, node.concrete_list_pos)
	}
	if func.generic_names.len > 0 && node.args.len == 0 && node.concrete_types.len == 0 {
		c.error('no argument generic function must add concrete types, e.g. foo[int]()',
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
	if node.args.len > 0 && fn_name in ['println', 'print', 'eprintln', 'eprint', 'panic'] {
		c.builtin_args(mut node, fn_name, func)
		return func.return_type
	}
	// `return error(err)` -> `return err`
	if node.args.len == 1 && fn_name == 'error' {
		mut arg := node.args[0]
		node.args[0].typ = c.expr(mut arg.expr)
		if node.args[0].typ == ast.error_type {
			c.warn('`error(${arg})` can be shortened to just `${arg}`', node.pos)
		}
	}
	c.set_node_expected_arg_types(mut node, func)
	if !c.pref.backend.is_js() && node.args.len > 0 && func.params.len == 0 {
		c.error('too many arguments in call to `${func.name}` (non-js backend: ${c.pref.backend})',
			node.pos)
	}
	mut has_decompose := false
	for i, mut call_arg in node.args {
		if func.params.len == 0 {
			continue
		}
		if !func.is_variadic && has_decompose {
			c.error('cannot have parameter after array decompose', node.pos)
		}
		param := if func.is_variadic && i >= func.params.len - 1 {
			func.params.last()
		} else {
			func.params[i]
		}
		if func.is_variadic && call_arg.expr is ast.ArrayDecompose {
			if i > func.params.len - 1 {
				c.error('too many arguments in call to `${func.name}`', node.pos)
			}
		}
		has_decompose = call_arg.expr is ast.ArrayDecompose
		if func.is_variadic && i >= func.params.len - 1 {
			param_sym := c.table.sym(param.typ)
			mut expected_type := param.typ
			if param_sym.info is ast.Array {
				expected_type = param_sym.info.elem_type
				c.expected_type = expected_type
			}
			typ := c.expr(mut call_arg.expr)
			if i == node.args.len - 1 {
				if c.table.sym(typ).kind == .array && call_arg.expr !is ast.ArrayDecompose
					&& c.table.sym(expected_type).kind !in [.sum_type, .interface_]
					&& !param.typ.has_flag(.generic) && expected_type != typ {
					styp := c.table.type_to_str(typ)
					elem_styp := c.table.type_to_str(expected_type)
					c.error('to pass `${call_arg.expr}` (${styp}) to `${func.name}` (which accepts type `...${elem_styp}`), use `...${call_arg.expr}`',
						node.pos)
				} else if call_arg.expr is ast.ArrayDecompose
					&& c.table.sym(expected_type).kind == .sum_type
					&& expected_type.idx() != typ.idx() {
					expected_type_str := c.table.type_to_str(expected_type)
					got_type_str := c.table.type_to_str(typ)
					c.error('cannot use `...${got_type_str}` as `...${expected_type_str}` in argument ${
						i + 1} to `${fn_name}`', call_arg.pos)
				}
			}
		} else {
			c.expected_type = param.typ
		}

		e_sym := c.table.sym(c.expected_type)
		if call_arg.expr is ast.MapInit && e_sym.kind == .struct_ {
			c.error('cannot initialize a struct with a map', call_arg.pos)
			continue
		} else if call_arg.expr is ast.StructInit && e_sym.kind == .map {
			c.error('cannot initialize a map with a struct', call_arg.pos)
			continue
		}

		mut arg_typ := c.check_expr_option_or_result_call(call_arg.expr, c.expr(mut call_arg.expr))
		if call_arg.expr is ast.StructInit {
			arg_typ = c.expr(mut call_arg.expr)
		}
		node.args[i].typ = arg_typ
		if c.comptime.comptime_for_field_var != '' {
			if mut call_arg.expr is ast.Ident {
				if mut call_arg.expr.obj is ast.Var {
					node.args[i].typ = call_arg.expr.obj.typ
				}
			}
		}
		arg_typ_sym := c.table.sym(arg_typ)
		if arg_typ_sym.kind == .none_ && param.typ.has_flag(.generic) {
			c.error('cannot use `none` as generic argument', call_arg.pos)
		}
		param_typ_sym := c.table.sym(param.typ)
		if func.is_variadic && arg_typ.has_flag(.variadic) && node.args.len - 1 > i {
			c.error('when forwarding a variadic variable, it must be the final argument',
				call_arg.pos)
		}
		arg_share := param.typ.share()
		if arg_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('function with `shared` arguments cannot be called inside `lock`/`rlock` block',
				call_arg.pos)
		}
		if call_arg.is_mut {
			to_lock, pos := c.fail_if_immutable(mut call_arg.expr)
			if !call_arg.expr.is_lvalue() {
				if call_arg.expr is ast.StructInit {
					c.error('cannot pass a struct initialization as `mut`, you may want to use a variable `mut var := ${call_arg.expr}`',
						call_arg.expr.pos())
				} else {
					c.error('cannot pass expression as `mut`', call_arg.expr.pos())
				}
			}
			if !param.is_mut {
				tok := call_arg.share.str()
				c.error('`${node.name}` parameter `${param.name}` is not `${tok}`, `${tok}` is not needed`',
					call_arg.expr.pos())
			} else {
				if param.typ.share() != call_arg.share {
					c.error('wrong shared type `${call_arg.share.str()}`, expected: `${param.typ.share().str()}`',
						call_arg.expr.pos())
				}
				if to_lock != '' && !param.typ.has_flag(.shared_f) {
					c.error('${to_lock} is `shared` and must be `lock`ed to be passed as `mut`',
						pos)
				}
			}
		} else {
			if param.is_mut {
				tok := param.specifier()
				c.error('function `${node.name}` parameter `${param.name}` is `${tok}`, so use `${tok} ${call_arg.expr}` instead',
					call_arg.expr.pos())
			} else {
				c.fail_if_unreadable(call_arg.expr, arg_typ, 'argument')
			}
		}
		mut final_param_sym := unsafe { param_typ_sym }
		mut final_param_typ := param.typ
		if func.is_variadic && param_typ_sym.info is ast.Array {
			final_param_typ = param_typ_sym.info.elem_type
			final_param_sym = c.table.sym(final_param_typ)
		}
		// Note: Casting to voidptr is used as an escape mechanism, so:
		// 1. allow passing *explicit* voidptr (native or through cast) to functions
		// expecting voidptr or ...voidptr
		// ... but 2. disallow passing non-pointers - that is very rarely what the user wanted,
		// it can lead to codegen errors (except for 'magic' functions like `json.encode` that,
		// the compiler has special codegen support for), so it should be opt in, that is it
		// should require an explicit voidptr(x) cast (and probably unsafe{} ?) .
		if call_arg.typ != param.typ && (param.typ == ast.voidptr_type
			|| final_param_sym.idx == ast.voidptr_type_idx
			|| param.typ == ast.nil_type || final_param_sym.idx == ast.nil_type_idx)
			&& !call_arg.typ.is_any_kind_of_pointer() && func.language == .v
			&& !call_arg.expr.is_lvalue() && func.name != 'json.encode' && !c.pref.translated
			&& !c.file.is_translated {
			c.error('expression cannot be passed as `voidptr`', call_arg.expr.pos())
		}
		// Handle expected interface
		if final_param_sym.kind == .interface_ {
			if c.type_implements(arg_typ, final_param_typ, call_arg.expr.pos()) {
				if !arg_typ.is_any_kind_of_pointer() && !c.inside_unsafe
					&& arg_typ_sym.kind != .interface_ {
					c.mark_as_referenced(mut &call_arg.expr, true)
				}
			}

			if arg_typ !in [ast.voidptr_type, ast.nil_type]
				&& !c.check_multiple_ptr_match(arg_typ, param.typ, param, call_arg) {
				got_typ_str, expected_typ_str := c.get_string_names_of(arg_typ, param.typ)
				c.error('cannot use `${got_typ_str}` as `${expected_typ_str}` in argument ${i + 1} to `${fn_name}`',
					call_arg.pos)
			}
			continue
		}
		if param.typ.is_ptr() && !param.is_mut && !call_arg.typ.is_any_kind_of_pointer()
			&& call_arg.expr.is_literal() && func.language == .v && !c.pref.translated {
			c.error('literal argument cannot be passed as reference parameter `${c.table.type_to_str(param.typ)}`',
				call_arg.pos)
		}
		c.check_expected_call_arg(arg_typ, c.unwrap_generic(param.typ), node.language,
			call_arg) or {
			if param.typ.has_flag(.generic) {
				continue
			}
			if param_typ_sym.info is ast.Array && arg_typ_sym.info is ast.Array {
				param_elem_type := c.table.unaliased_type(param_typ_sym.info.elem_type)
				arg_elem_type := c.table.unaliased_type(arg_typ_sym.info.elem_type)
				param_nr_muls := param.typ.nr_muls()
				arg_nr_muls := if call_arg.is_mut {
					arg_typ.nr_muls() + 1
				} else {
					arg_typ.nr_muls()
				}
				if param_nr_muls == arg_nr_muls
					&& param_typ_sym.info.nr_dims == arg_typ_sym.info.nr_dims
					&& param_elem_type == arg_elem_type {
					continue
				}
			}
			if c.pref.translated || c.file.is_translated {
				// in case of variadic make sure to use array elem type for checks
				// check_expected_call_arg already does this before checks also.
				param_type := if param.typ.has_flag(.variadic) {
					param_typ_sym.array_info().elem_type
				} else {
					param.typ
				}
				// TODO duplicated logic in check_types() (check_types.v)
				// Allow enums to be used as ints and vice versa in translated code
				if param_type.idx() in ast.integer_type_idxs && arg_typ_sym.kind == .enum_ {
					continue
				}
				if arg_typ.idx() in ast.integer_type_idxs && param_typ_sym.kind == .enum_ {
					continue
				}

				if (arg_typ == ast.bool_type && param_type.is_int())
					|| (arg_typ.is_int() && param_type == ast.bool_type) {
					continue
				}

				// In C unsafe number casts are used all the time (e.g. `char*` where
				// `int*` is expected etc), so just allow them all.
				mut param_is_number := c.table.unaliased_type(param_type).is_number()
				if param_type.is_ptr() {
					param_is_number = param_type.deref().is_number()
				}
				mut typ_is_number := c.table.unaliased_type(arg_typ).is_number()
				if arg_typ.is_ptr() {
					typ_is_number = arg_typ.deref().is_number()
				}
				if param_is_number && typ_is_number {
					continue
				}
				// Allow voidptrs for everything
				if param_type == ast.voidptr_type_idx || param_type == ast.nil_type_idx
					|| arg_typ == ast.voidptr_type_idx || arg_typ == ast.nil_type_idx {
					continue
				}
				if param_type.is_any_kind_of_pointer() && arg_typ.is_any_kind_of_pointer() {
					continue
				}
				unaliased_param_sym := c.table.sym(c.table.unaliased_type(param_type))
				unaliased_arg_sym := c.table.sym(c.table.unaliased_type(arg_typ))
				// Allow `[32]i8` as `&i8` etc
				if ((unaliased_arg_sym.kind == .array_fixed || unaliased_arg_sym.kind == .array)
					&& (param_is_number
					|| c.table.unaliased_type(param_type).is_any_kind_of_pointer()))
					|| ((unaliased_param_sym.kind == .array_fixed
					|| unaliased_param_sym.kind == .array)
					&& (typ_is_number || c.table.unaliased_type(arg_typ).is_any_kind_of_pointer())) {
					continue
				}
				// Allow `[N]anyptr` as `[N]anyptr`
				if unaliased_arg_sym.info is ast.Array && unaliased_param_sym.info is ast.Array {
					if unaliased_arg_sym.info.elem_type.is_any_kind_of_pointer()
						&& unaliased_param_sym.info.elem_type.is_any_kind_of_pointer() {
						continue
					}
				} else if unaliased_arg_sym.info is ast.ArrayFixed
					&& unaliased_param_sym.info is ast.ArrayFixed {
					if unaliased_arg_sym.info.elem_type.is_any_kind_of_pointer()
						&& unaliased_param_sym.info.elem_type.is_any_kind_of_pointer() {
						continue
					}
				}
				// Allow `int` as `&i8`
				if param_type.is_any_kind_of_pointer() && typ_is_number {
					continue
				}
				// Allow `&i8` as `int`
				if arg_typ.is_any_kind_of_pointer() && param_is_number {
					continue
				}
			}
			c.error('${err.msg()} in argument ${i + 1} to `${fn_name}`', call_arg.pos)
		}
		if final_param_sym.kind == .struct_ && arg_typ !in [ast.voidptr_type, ast.nil_type]
			&& !c.check_multiple_ptr_match(arg_typ, param.typ, param, call_arg) {
			got_typ_str, expected_typ_str := c.get_string_names_of(arg_typ, param.typ)
			c.error('cannot use `${got_typ_str}` as `${expected_typ_str}` in argument ${i + 1} to `${fn_name}`',
				call_arg.pos)
		}
		// Warn about automatic (de)referencing, which will be removed soon.
		if func.language != .c && !c.inside_unsafe && arg_typ.nr_muls() != param.typ.nr_muls()
			&& !(call_arg.is_mut && param.is_mut) && !(!call_arg.is_mut && !param.is_mut)
			&& param.typ !in [ast.byteptr_type, ast.charptr_type, ast.voidptr_type, ast.nil_type] {
			c.warn('automatic referencing/dereferencing is deprecated and will be removed soon (got: ${arg_typ.nr_muls()} references, expected: ${param.typ.nr_muls()} references)',
				call_arg.pos)
		}
	}
	if func.generic_names.len != node.concrete_types.len {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_generic_types(func, mut node)
		concrete_types = node.concrete_types.map(c.unwrap_generic(it))
		c.resolve_fn_generic_args(func, mut node)
	}
	if func.generic_names.len > 0 {
		for i, mut call_arg in node.args {
			param := if func.is_variadic && i >= func.params.len - 1 {
				func.params.last()
			} else {
				func.params[i]
			}
			c.expected_type = param.typ
			typ := c.check_expr_option_or_result_call(call_arg.expr, c.expr(mut call_arg.expr))

			if param.typ.has_flag(.generic) && func.generic_names.len == node.concrete_types.len {
				if unwrap_typ := c.table.resolve_generic_to_concrete(param.typ, func.generic_names,
					concrete_types)
				{
					utyp := c.unwrap_generic(typ)
					unwrap_sym := c.table.sym(unwrap_typ)
					if unwrap_sym.kind == .interface_ {
						if c.type_implements(utyp, unwrap_typ, call_arg.expr.pos()) {
							if !utyp.is_any_kind_of_pointer() && !c.inside_unsafe
								&& c.table.sym(utyp).kind != .interface_ {
								c.mark_as_referenced(mut &call_arg.expr, true)
							}
						}
						continue
					}
					c.check_expected_call_arg(utyp, unwrap_typ, node.language, call_arg) or {
						if c.comptime.type_map.len > 0 {
							continue
						}
						c.error('${err.msg()} in argument ${i + 1} to `${fn_name}`', call_arg.pos)
					}
				}
			}
		}
	}
	// resolve return generics struct to concrete type
	if func.generic_names.len > 0 && func.return_type.has_flag(.generic)
		&& c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len == 0 {
		node.return_type = c.table.unwrap_generic_type(func.return_type, func.generic_names,
			concrete_types)
	} else {
		node.return_type = func.return_type
	}
	if node.concrete_types.len > 0 && func.return_type != 0 && c.table.cur_fn != unsafe { nil }
		&& c.table.cur_fn.generic_names.len == 0 {
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

	if func.generic_names.len > 0 {
		if has_generic || node.concrete_types.any(it.has_flag(.generic)) {
			if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
				node.concrete_types)
			{
				if typ.has_flag(.generic) {
					node.return_type = typ
				}
			}
			return node.return_type
		} else {
			if node.concrete_types.len > 0 && !node.concrete_types.any(it.has_flag(.generic)) {
				if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
					node.concrete_types)
				{
					node.return_type = typ
					return typ
				}
			}
			if typ := c.table.resolve_generic_to_concrete(func.return_type, func.generic_names,
				concrete_types)
			{
				if typ.has_flag(.generic) {
					node.return_type = typ
				}
				return typ
			}
		}
	}
	return func.return_type
}

fn (mut c Checker) resolve_comptime_args(func ast.Fn, node_ ast.CallExpr, concrete_types []ast.Type) map[int]ast.Type {
	mut comptime_args := map[int]ast.Type{}
	has_dynamic_vars := (c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0)
		|| c.comptime.comptime_for_field_var != ''
	if has_dynamic_vars {
		offset := if func.is_method { 1 } else { 0 }
		mut k := -1
		for i, call_arg in node_.args {
			param := if func.is_variadic && i >= func.params.len - (offset + 1) {
				func.params.last()
			} else {
				func.params[offset + i]
			}
			k++
			if !param.typ.has_flag(.generic) {
				continue
			}
			param_typ := param.typ
			if call_arg.expr is ast.Ident {
				if call_arg.expr.obj is ast.Var {
					if call_arg.expr.obj.ct_type_var !in [.generic_param, .no_comptime] {
						mut ctyp := c.comptime.get_comptime_var_type(call_arg.expr)
						if ctyp != ast.void_type {
							arg_sym := c.table.sym(ctyp)
							if arg_sym.info is ast.Array && param_typ.has_flag(.generic)
								&& c.table.final_sym(param_typ).kind == .array {
								ctyp = arg_sym.info.elem_type
							}
							comptime_args[k] = ctyp
						}
					} else if call_arg.expr.obj.ct_type_var == .generic_param {
						mut ctyp := c.comptime.get_comptime_var_type(call_arg.expr)
						if ctyp != ast.void_type {
							arg_sym := c.table.final_sym(call_arg.typ)
							param_typ_sym := c.table.sym(param_typ)

							if param_typ.has_flag(.variadic) {
								ctyp = ast.mktyp(ctyp)
								comptime_args[k] = ctyp
							} else if arg_sym.info is ast.Array && param_typ.has_flag(.generic)
								&& param_typ_sym.kind == .array {
								ctyp = c.get_generic_array_element_type(arg_sym.info)
								comptime_args[k] = ctyp
							} else if arg_sym.kind in [.struct_, .interface_, .sum_type] {
								mut generic_types := []ast.Type{}
								match arg_sym.info {
									ast.Struct, ast.Interface, ast.SumType {
										if param_typ_sym.generic_types.len > 0 {
											generic_types = param_typ_sym.generic_types.clone()
										} else {
											generic_types = arg_sym.info.generic_types.clone()
										}
									}
									else {}
								}
								generic_names := generic_types.map(c.table.sym(it).name)
								for _, gt_name in c.table.cur_fn.generic_names {
									if gt_name in generic_names
										&& generic_types.len == concrete_types.len {
										idx := generic_names.index(gt_name)
										comptime_args[k] = concrete_types[idx]
										break
									}
								}
							} else if arg_sym.kind == .any {
								cparam_type_sym := c.table.sym(c.unwrap_generic(ctyp))
								if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
									comptime_args[k] = cparam_type_sym.info.elem_type
								} else if param_typ_sym.kind == .map
									&& cparam_type_sym.info is ast.Map {
									comptime_args[k] = cparam_type_sym.info.key_type
									comptime_args[k + 1] = cparam_type_sym.info.value_type
									k++
								} else {
									if node_.args[i].expr.is_auto_deref_var() {
										ctyp = ctyp.deref()
									}
									if ctyp.nr_muls() > 0 && param_typ.nr_muls() > 0 {
										ctyp = ctyp.set_nr_muls(0)
									}
									comptime_args[k] = ctyp
								}
							} else {
								comptime_args[k] = ctyp
							}
						}
					}
				}
			} else if call_arg.expr is ast.PrefixExpr {
				if call_arg.expr.right is ast.ComptimeSelector {
					comptime_args[k] = c.comptime.get_comptime_var_type(call_arg.expr.right)
					comptime_args[k] = comptime_args[k].deref()
					if comptime_args[k].nr_muls() > 0 && param_typ.nr_muls() > 0 {
						comptime_args[k] = comptime_args[k].set_nr_muls(0)
					}
				}
			} else if call_arg.expr is ast.ComptimeSelector {
				ct_value := c.comptime.get_comptime_var_type(call_arg.expr)
				param_typ_sym := c.table.sym(param_typ)
				if ct_value != ast.void_type {
					cparam_type_sym := c.table.sym(c.unwrap_generic(ct_value))
					if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
						comptime_args[k] = cparam_type_sym.info.elem_type
					} else {
						comptime_args[k] = ct_value
					}
				}
			} else if call_arg.expr is ast.ComptimeCall {
				comptime_args[k] = c.comptime.get_comptime_var_type(call_arg.expr)
			}
		}
	}
	return comptime_args
}

fn (mut c Checker) resolve_fn_generic_args(func ast.Fn, mut node ast.CallExpr) []ast.Type {
	mut concrete_types := node.concrete_types.map(c.unwrap_generic(it))

	// dynamic values from comptime and generic parameters
	// overwrite concrete_types[ receiver_concrete_type + arg number ]
	if concrete_types.len > 0 {
		mut rec_len := 0
		// discover receiver concrete_type len
		if func.is_method && node.left_type.has_flag(.generic) {
			rec_sym := c.table.final_sym(c.unwrap_generic(node.left_type))
			match rec_sym.info {
				ast.Struct, ast.Interface, ast.SumType {
					rec_len += rec_sym.info.generic_types.len
				}
				else {}
			}
		}
		mut comptime_args := c.resolve_comptime_args(func, node, concrete_types)
		if comptime_args.len > 0 {
			for k, v in comptime_args {
				if (rec_len + k) < concrete_types.len {
					concrete_types[rec_len + k] = c.unwrap_generic(v)
				}
			}
			if c.table.register_fn_concrete_types(func.fkey(), concrete_types) {
				c.need_recheck_generic_fns = true
			}
		}
	}

	return concrete_types
}

// cast_fixed_array_ret casts a ArrayFixed type created to return to a non returning one
fn (mut c Checker) cast_fixed_array_ret(typ ast.Type, sym ast.TypeSymbol) ast.Type {
	if sym.info is ast.ArrayFixed && sym.info.is_fn_ret {
		return c.table.find_or_register_array_fixed(sym.info.elem_type, sym.info.size,
			sym.info.size_expr, false)
	}
	return typ
}

// cast_to_fixed_array_ret casts a ArrayFixed type created to do not return to a returning one
fn (mut c Checker) cast_to_fixed_array_ret(typ ast.Type, sym ast.TypeSymbol) ast.Type {
	if sym.info is ast.ArrayFixed && !sym.info.is_fn_ret {
		return c.table.find_or_register_array_fixed(sym.info.elem_type, sym.info.size,
			sym.info.size_expr, true)
	}
	return typ
}

// checks if a type from another module is as expected and visible(`is_pub`)
fn (mut c Checker) check_type_and_visibility(name string, type_idx int, expected_kind &ast.Kind, pos &token.Pos) bool {
	mut sym := c.table.sym_by_idx(type_idx)
	if sym.kind == .alias {
		parent_type := (sym.info as ast.Alias).parent_type
		sym = c.table.sym(parent_type)
	}
	if sym.kind != expected_kind {
		c.error('expected ${expected_kind}, but `${name}` is ${sym.kind}', pos)
		return false
	}
	if !sym.is_pub {
		c.error('module `${sym.mod}` type `${sym.name}` is private', pos)
		return false
	}
	return true
}

fn (mut c Checker) method_call(mut node ast.CallExpr) ast.Type {
	left_type := c.expr(mut node.left)
	if left_type == ast.void_type {
		c.error('cannot call a method using an invalid expression', node.pos)
		return ast.void_type
	}
	c.expected_type = left_type
	mut is_generic := left_type.has_flag(.generic)
	node.left_type = left_type
	// Set default values for .return_type & .receiver_type too,
	// or there will be hard tRo diagnose 0 type panics in cgen.
	node.return_type = left_type
	node.receiver_type = left_type

	if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(left_type, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	unwrapped_left_type := c.unwrap_generic(left_type)
	left_sym := c.table.sym(unwrapped_left_type)
	final_left_sym := c.table.final_sym(unwrapped_left_type)

	method_name := node.name
	mut unknown_method_msg := if field := c.table.find_field(left_sym, method_name) {
		'unknown method `${field.name}` did you mean to access the field with the same name instead?'
	} else {
		name := left_sym.symbol_name_except_generic().replace_each(['<', '[', '>', ']'])
		'unknown method or field: `${name}.${method_name}`'
	}
	if left_type.has_flag(.option) && method_name != 'str' {
		c.error('Option type cannot be called directly', node.left.pos())
		return ast.void_type
	} else if left_type.has_flag(.result) {
		c.error('Result type cannot be called directly', node.left.pos())
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
	if final_left_sym.kind == .array {
		if array_builtin_methods_chk.matches(method_name) && (left_sym.kind == .array
			|| (left_sym.kind == .alias && !left_sym.has_method(method_name))) {
			return c.array_builtin_method_call(mut node, left_type)
		} else if method_name in ['insert', 'prepend'] {
			if method_name == 'insert' {
				if node.args.len != 2 {
					c.error('`array.insert()` should have 2 arguments, e.g. `insert(1, val)`',
						node.pos)
					return ast.void_type
				} else {
					arg_type := c.expr(mut node.args[0].expr)
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
			info := final_left_sym.info as ast.Array
			mut arg_expr := if method_name == 'insert' {
				node.args[1].expr
			} else {
				node.args[0].expr
			}
			arg_type := c.expr(mut arg_expr)
			arg_sym := c.table.sym(arg_type)
			if !c.check_types(arg_type, info.elem_type) && !c.check_types(left_type, arg_type) {
				c.error('cannot ${method_name} `${arg_sym.name}` to `${left_sym.name}`',
					arg_expr.pos())
			}
		}
	} else if final_left_sym.kind == .map
		&& method_name in ['clone', 'keys', 'values', 'move', 'delete'] && !(left_sym.kind == .alias
		&& left_sym.has_method(method_name)) {
		unaliased_left_type := c.table.unaliased_type(left_type)
		return c.map_builtin_method_call(mut node, unaliased_left_type)
	} else if c.pref.backend.is_js() && left_sym.name.starts_with('Promise[')
		&& method_name == 'wait' {
		info := left_sym.info as ast.Struct
		if node.args.len > 0 {
			c.error('wait() does not have any arguments', node.args[0].pos)
		}
		if c.table.cur_fn != unsafe { nil } {
			c.table.cur_fn.has_await = true
		}
		node.return_type = info.concrete_types[0]
		node.return_type.set_flag(.option)
		return node.return_type
	} else if left_sym.info is ast.Thread && method_name == 'wait' {
		if node.args.len > 0 {
			c.error('wait() does not have any arguments', node.args[0].pos)
		}
		node.return_type = left_sym.info.return_type
		return left_sym.info.return_type
	} else if final_left_sym.info is ast.ArrayFixed && method_name == 'wait' {
		elem_sym := c.table.sym(final_left_sym.info.elem_type)
		if elem_sym.kind == .thread {
			if node.args.len != 0 {
				c.error('`.wait()` does not have any arguments', node.args[0].pos)
			}
			thread_ret_type := c.unwrap_generic(elem_sym.thread_info().return_type)
			if thread_ret_type.has_flag(.option) {
				c.error('`.wait()` cannot be called for an array when thread functions return options. Iterate over the arrays elements instead and handle each returned option with `or`.',
					node.pos)
			} else if thread_ret_type.has_flag(.result) {
				c.error('`.wait()` cannot be called for an array when thread functions return results. Iterate over the arrays elements instead and handle each returned result with `or`.',
					node.pos)
			}
			node.return_type = c.table.find_or_register_array(thread_ret_type)
			return node.return_type
		} else {
			c.error('`${left_sym.name}` has no method `wait()` (only thread handles and arrays of them have)',
				node.left.pos())
		}
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
		if left_sym.kind == .interface_ && m.from_embeded_type != 0 {
			is_method_from_embed = true
			node.from_embed_types = [m.from_embeded_type]
		}
	} else {
		if final_left_sym.kind in [.struct_, .sum_type, .interface_, .alias, .array] {
			mut parent_type := ast.void_type
			match final_left_sym.info {
				ast.Struct, ast.SumType, ast.Interface, ast.Alias {
					parent_type = final_left_sym.info.parent_type
				}
				ast.Array {
					typ := c.table.unaliased_type(final_left_sym.info.elem_type)
					parent_type = ast.Type(c.table.find_or_register_array(typ))
				}
				else {}
			}
			if parent_type != 0 {
				type_sym := c.table.sym(parent_type)
				if m := c.table.find_method(type_sym, method_name) {
					method = m
					has_method = true
					is_generic = true
					if left_sym.kind == .interface_ && m.from_embeded_type != 0 {
						is_method_from_embed = true
						node.from_embed_types = [m.from_embeded_type]
					}
				}
			}
		}
		if !has_method {
			has_method = true
			mut embed_types := []ast.Type{}
			method, embed_types = c.table.find_method_from_embeds(final_left_sym, method_name) or {
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
		if final_left_sym.kind == .aggregate {
			// the error message contains the problematic type
			unknown_method_msg = err.msg()
		}
	}

	if !has_method {
		// TODO: str methods
		if method_name == 'str' {
			if left_sym.kind == .interface_ {
				iname := left_sym.name
				c.error('interface `${iname}` does not have a .str() method. Use typeof() instead',
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
			if !c.is_builtin_mod && !c.inside_unsafe && !method.is_unsafe {
				c.warn('manual memory management with `free()` is only allowed in unsafe code',
					node.pos)
			}
			if left_sym.kind == .array_fixed {
				name := left_sym.symbol_name_except_generic().replace_each(['<', '[', '>', ']'])
				c.error('unknown method or field: ${name}.free()', node.pos)
			}
			return ast.void_type
		}
		// call struct field fn type
		// TODO: can we use SelectorExpr for all? this dosent really belong here
		if field := c.table.find_field_with_embeds(left_sym, method_name) {
			if field.typ.has_flag(.option) {
				c.error('Option function field must be unwrapped first', node.pos)
			}
			field_sym := c.table.sym(c.unwrap_generic(field.typ))
			if field_sym.kind == .function {
				node.is_method = false
				node.is_field = true
				info := field_sym.info as ast.FnType

				c.check_expected_arg_count(mut node, info.func) or { return info.func.return_type }
				node.return_type = info.func.return_type
				mut earg_types := []ast.Type{}

				for i, mut arg in node.args {
					targ := c.check_expr_option_or_result_call(arg.expr, c.expr(mut arg.expr))
					arg.typ = targ

					param := if info.func.is_variadic && i >= info.func.params.len - 1 {
						info.func.params.last()
					} else {
						info.func.params[i]
					}
					if c.table.sym(param.typ).kind == .interface_ {
						// cannot hide interface expected type to make possible to pass its interface type automatically
						earg_types << if targ.idx() != param.typ.idx() { param.typ } else { targ }
					} else {
						earg_types << param.typ
					}
					param_share := param.typ.share()
					if param_share == .shared_t
						&& (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
						c.error('method with `shared` arguments cannot be called inside `lock`/`rlock` block',
							arg.pos)
					}
					if arg.is_mut {
						to_lock, pos := c.fail_if_immutable(mut arg.expr)
						if !param.is_mut {
							tok := arg.share.str()
							c.error('`${node.name}` parameter ${i + 1} is not `${tok}`, `${tok}` is not needed`',
								arg.expr.pos())
						} else {
							if param_share != arg.share {
								c.error('wrong shared type `${arg.share.str()}`, expected: `${param_share.str()}`',
									arg.expr.pos())
							}
							if to_lock != '' && param_share != .shared_t {
								c.error('${to_lock} is `shared` and must be `lock`ed to be passed as `mut`',
									pos)
							}
						}
					} else {
						if param.is_mut {
							tok := param.specifier()
							c.error('method `${node.name}` parameter ${i + 1} is `${tok}`, so use `${tok} ${arg.expr}` instead',
								arg.expr.pos())
						} else {
							c.fail_if_unreadable(arg.expr, targ, 'argument')
						}
					}

					if i < info.func.params.len {
						exp_arg_typ := info.func.params[i].typ
						c.check_expected_call_arg(targ, c.unwrap_generic(exp_arg_typ),
							node.language, arg) or {
							if targ != ast.void_type {
								c.error('${err.msg()} in argument ${i + 1} to `${left_sym.name}.${method_name}`',
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
		if left_sym.kind in [.struct_, .aggregate, .interface_, .sum_type] {
			if c.smartcast_mut_pos != token.Pos{} {
				c.note('smartcasting requires either an immutable value, or an explicit mut keyword before the value',
					c.smartcast_mut_pos)
			}
			if c.smartcast_cond_pos != token.Pos{} {
				c.note('smartcast can only be used on the ident or selector, e.g. match foo, match foo.bar',
					c.smartcast_cond_pos)
			}
		}
		if left_type != ast.void_type {
			suggestion := util.new_suggestion(method_name, left_sym.methods.map(it.name))
			c.error(suggestion.say(unknown_method_msg), node.pos)
		}
		return ast.void_type
	}

	// x is Bar[T], x.foo() -> x.foo[T]()
	rec_sym := if is_method_from_embed {
		c.table.final_sym(node.from_embed_types.last())
	} else {
		c.table.final_sym(node.left_type)
	}
	rec_is_generic := if is_method_from_embed {
		node.from_embed_types.last().has_flag(.generic)
	} else {
		left_type.has_flag(.generic)
	}
	mut rec_concrete_types := []ast.Type{}
	match rec_sym.info {
		ast.Struct, ast.SumType, ast.Interface {
			if rec_sym.info.concrete_types.len > 0 {
				rec_concrete_types = rec_sym.info.concrete_types.clone()
			}
			if rec_is_generic && node.concrete_types.len == 0
				&& method.generic_names.len == rec_sym.info.generic_types.len {
				node.concrete_types = rec_sym.info.generic_types
			} else if rec_is_generic && node.concrete_types.len > 0
				&& method.generic_names.len > node.concrete_types.len
				&& rec_sym.info.generic_types.len + node.concrete_types.len == method.generic_names.len {
				t_concrete_types := node.concrete_types.clone()
				node.concrete_types = rec_sym.info.generic_types
				node.concrete_types << t_concrete_types
			} else if !rec_is_generic && rec_sym.info.concrete_types.len > 0
				&& node.concrete_types.len > 0
				&& rec_sym.info.concrete_types.len + node.concrete_types.len == method.generic_names.len {
				t_concrete_types := node.concrete_types.clone()
				node.concrete_types = rec_sym.info.concrete_types
				node.concrete_types << t_concrete_types
			} else if !rec_is_generic && rec_concrete_types.len > 0
				&& method.generic_names.len == rec_concrete_types.len {
				node.concrete_types = rec_concrete_types
			}
		}
		else {}
	}
	mut concrete_types := node.concrete_types.map(c.unwrap_generic(it))
	if concrete_types.len > 0 && c.table.register_fn_concrete_types(method.fkey(), concrete_types) {
		c.need_recheck_generic_fns = true
	}
	node.is_noreturn = method.is_noreturn
	node.is_ctor_new = method.is_ctor_new
	node.return_type = method.return_type
	if !method.is_pub && method.mod != c.mod {
		// If a private method is called outside of the module
		// its receiver type is defined in, show an error.
		// println('warn $method_name lef.mod=$left_type_sym.mod c.mod=$c.mod')
		c.error('method `${left_sym.name}.${method_name}` is private', node.pos)
	}
	rec_share := method.params[0].typ.share()
	if rec_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
		c.error('method with `shared` receiver cannot be called inside `lock`/`rlock` block',
			node.pos)
	}
	if method.params[0].is_mut {
		to_lock, pos := c.check_for_mut_receiver(mut node.left)
		// node.is_mut = true
		if to_lock != '' && rec_share != .shared_t {
			c.error('${to_lock} is `shared` and must be `lock`ed to be passed as `mut`',
				pos)
		}
	} else {
		c.fail_if_unreadable(node.left, left_type, 'receiver')
	}
	if left_sym.language != .js && (!left_sym.is_builtin() && method.mod != 'builtin')
		&& method.language == .v && method.no_body {
		c.error('cannot call a method that does not have a body', node.pos)
	}
	if node.concrete_types.len > 0 && method.generic_names.len > 0
		&& node.concrete_types.len != method.generic_names.len {
		plural := if method.generic_names.len == 1 { '' } else { 's' }
		c.error('expected ${method.generic_names.len} generic parameter${plural}, got ${node.concrete_types.len}',
			node.concrete_list_pos)
	}
	for concrete_type in node.concrete_types {
		c.ensure_type_exists(concrete_type, node.concrete_list_pos)
	}
	if method.return_type == ast.void_type && method.is_conditional
		&& method.ctdefine_idx != ast.invalid_type_idx {
		node.should_be_skipped = c.evaluate_once_comptime_if_attribute(mut method.attrs[method.ctdefine_idx])
	}
	c.check_expected_arg_count(mut node, method) or { return method.return_type }
	mut exp_arg_typ := ast.Type(0) // type of 1st arg for special builtin methods
	mut param_is_mut := false
	mut no_type_promotion := false
	if left_sym.info is ast.Chan {
		if method_name == 'try_push' {
			exp_arg_typ = left_sym.info.elem_type.ref()
		} else if method_name == 'try_pop' {
			exp_arg_typ = left_sym.info.elem_type
			param_is_mut = true
			no_type_promotion = true
		}
	}

	for i, mut arg in node.args {
		if i > 0 || exp_arg_typ == ast.Type(0) {
			exp_arg_typ = if method.is_variadic && i >= method.params.len - 1 {
				method.params.last().typ
			} else {
				method.params[i + 1].typ
			}
			// If initialize a generic struct with short syntax,
			// need to get the parameter information from the original generic method
			if is_method_from_embed && arg.expr is ast.StructInit {
				expr := arg.expr as ast.StructInit
				is_short_syntax := expr.is_short_syntax && expr.typ == ast.void_type
				if is_short_syntax {
					embed_type := node.from_embed_types.last()
					embed_sym := c.table.sym(embed_type)
					if embed_sym.info is ast.Struct {
						info := embed_sym.info as ast.Struct
						if info.concrete_types.len > 0 {
							parent_type := info.parent_type
							parent_sym := c.table.sym(parent_type)
							if parent_sym.info is ast.Struct && parent_sym.info.is_generic {
								if f := parent_sym.find_method(method_name) {
									exp_arg_typ = if f.is_variadic && i >= f.params.len - 1 {
										f.params.last().typ
									} else {
										f.params[i + 1].typ
									}
								}
							}
						}
					}
				}
			}
			param_is_mut = false
			no_type_promotion = false
		}
		exp_arg_sym := c.table.sym(exp_arg_typ)
		c.expected_type = exp_arg_typ

		mut got_arg_typ := c.check_expr_option_or_result_call(arg.expr, c.expr(mut arg.expr))
		node.args[i].typ = got_arg_typ
		if no_type_promotion {
			if got_arg_typ != exp_arg_typ {
				c.error('cannot use `${c.table.sym(got_arg_typ).name}` as argument for `${method.name}` (`${exp_arg_sym.name}` expected)',
					arg.pos)
			}
		}
		if method.is_variadic && got_arg_typ.has_flag(.variadic) && node.args.len - 1 > i {
			c.error('when forwarding a variadic variable, it must be the final argument',
				arg.pos)
		}
		mut final_arg_sym := unsafe { exp_arg_sym }
		mut final_arg_typ := exp_arg_typ
		if method.is_variadic && exp_arg_sym.info is ast.Array {
			final_arg_typ = exp_arg_sym.info.elem_type
			final_arg_sym = c.table.sym(final_arg_typ)
		}
		param := if method.is_variadic && i >= method.params.len - 1 {
			method.params.last()
		} else {
			method.params[i + 1]
		}

		if method.is_variadic && arg.expr is ast.ArrayDecompose {
			if i > method.params.len - 2 {
				c.error('too many arguments in call to `${method.name}`', node.pos)
			}
		}
		if method.is_variadic && i >= method.params.len - 2 {
			param_sym := c.table.sym(param.typ)
			mut expected_type := param.typ
			if param_sym.kind == .array {
				info := param_sym.array_info()
				expected_type = info.elem_type
				c.expected_type = expected_type
			}
			typ := c.expr(mut arg.expr)
			if i == node.args.len - 1 {
				if c.table.sym(typ).kind == .array && arg.expr !is ast.ArrayDecompose
					&& c.table.sym(expected_type).kind !in [.sum_type, .interface_]
					&& !param.typ.has_flag(.generic) && expected_type != typ {
					styp := c.table.type_to_str(typ)
					elem_styp := c.table.type_to_str(expected_type)
					c.error('to pass `${arg.expr}` (${styp}) to `${method.name}` (which accepts type `...${elem_styp}`), use `...${arg.expr}`',
						node.pos)
				} else if arg.expr is ast.ArrayDecompose
					&& c.table.sym(expected_type).kind == .sum_type
					&& expected_type.idx() != typ.idx() {
					expected_type_str := c.table.type_to_str(expected_type)
					got_type_str := c.table.type_to_str(typ)
					c.error('cannot use `...${got_type_str}` as `...${expected_type_str}` in argument ${
						i + 1} to `${method_name}`', arg.pos)
				}
			}
		} else {
			c.expected_type = param.typ
		}

		param_is_mut = param_is_mut || param.is_mut
		param_share := param.typ.share()
		if param_share == .shared_t && (c.locked_names.len > 0 || c.rlocked_names.len > 0) {
			c.error('method with `shared` arguments cannot be called inside `lock`/`rlock` block',
				arg.pos)
		}
		if arg.is_mut {
			to_lock, pos := c.fail_if_immutable(mut arg.expr)
			if !param_is_mut {
				tok := arg.share.str()
				c.error('`${node.name}` parameter `${param.name}` is not `${tok}`, `${tok}` is not needed`',
					arg.expr.pos())
			} else {
				if param_share != arg.share {
					c.error('wrong shared type `${arg.share.str()}`, expected: `${param_share.str()}`',
						arg.expr.pos())
				}
				if to_lock != '' && param_share != .shared_t {
					c.error('${to_lock} is `shared` and must be `lock`ed to be passed as `mut`',
						pos)
				}
			}
		} else {
			if param_is_mut {
				tok := if param.typ.has_flag(.shared_f) { 'shared' } else { arg.share.str() }
				c.error('method `${node.name}` parameter `${param.name}` is `${tok}`, so use `${tok} ${arg.expr}` instead',
					arg.expr.pos())
			} else {
				c.fail_if_unreadable(arg.expr, got_arg_typ, 'argument')
			}
		}
		if concrete_types.len > 0 && method.generic_names.len != rec_concrete_types.len {
			concrete_types = c.resolve_fn_generic_args(method, mut node)
			if !concrete_types[0].has_flag(.generic) {
				c.table.register_fn_concrete_types(method.fkey(), concrete_types)
			}
		}
		if exp_arg_typ.has_flag(.generic) {
			method_concrete_types := if method.generic_names.len == rec_concrete_types.len {
				rec_concrete_types
			} else {
				concrete_types
			}
			if exp_utyp := c.table.resolve_generic_to_concrete(exp_arg_typ, method.generic_names,
				method_concrete_types)
			{
				exp_arg_typ = exp_utyp
			} else {
				continue
			}
			if got_arg_typ.has_flag(.generic) {
				if c.table.cur_fn != unsafe { nil } && c.table.cur_concrete_types.len > 0 {
					got_arg_typ = c.unwrap_generic(got_arg_typ)
				} else {
					if got_utyp := c.table.resolve_generic_to_concrete(got_arg_typ, method.generic_names,
						method_concrete_types)
					{
						got_arg_typ = got_utyp
					} else {
						continue
					}
				}
			}
		}
		if left_sym.info is ast.Array && method_name in ['sort_with_compare', 'sorted_with_compare'] {
			elem_typ := left_sym.info.elem_type
			arg_sym := c.table.sym(arg.typ)
			if arg_sym.kind == .function {
				func_info := arg_sym.info as ast.FnType
				if func_info.func.params.len == 2 {
					if func_info.func.params[0].typ.nr_muls() != elem_typ.nr_muls() + 1 {
						arg_typ_str := c.table.type_to_str(func_info.func.params[0].typ)
						expected_typ_str := c.table.type_to_str(elem_typ.ref())
						c.error('${method_name} callback function parameter `${func_info.func.params[0].name}` with type `${arg_typ_str}` should be `${expected_typ_str}`',
							func_info.func.params[0].type_pos)
					}
					if func_info.func.params[1].typ.nr_muls() != elem_typ.nr_muls() + 1 {
						arg_typ_str := c.table.type_to_str(func_info.func.params[1].typ)
						expected_typ_str := c.table.type_to_str(elem_typ.ref())
						c.error('${method_name} callback function parameter `${func_info.func.params[1].name}` with type `${arg_typ_str}` should be `${expected_typ_str}`',
							func_info.func.params[1].type_pos)
					}
				}
			}
		}
		// Handle expected interface
		if final_arg_sym.kind == .interface_ {
			if c.type_implements(got_arg_typ, final_arg_typ, arg.expr.pos()) {
				if !got_arg_typ.is_any_kind_of_pointer() && !c.inside_unsafe {
					got_arg_typ_sym := c.table.sym(got_arg_typ)
					if got_arg_typ_sym.kind != .interface_ {
						c.mark_as_referenced(mut &arg.expr, true)
					}
				}
			}

			if got_arg_typ !in [ast.voidptr_type, ast.nil_type]
				&& !c.check_multiple_ptr_match(got_arg_typ, param.typ, param, arg) {
				got_typ_str, expected_typ_str := c.get_string_names_of(got_arg_typ, param.typ)
				c.error('cannot use `${got_typ_str}` as `${expected_typ_str}` in argument ${i + 1} to `${method_name}`',
					arg.pos)
			}
			continue
		}
		if final_arg_sym.kind == .none_ && param.typ.has_flag(.generic) {
			c.error('cannot use `none` as generic argument', arg.pos)
		}
		if param.typ.is_ptr() && !arg.typ.is_any_kind_of_pointer() && arg.expr.is_literal()
			&& !c.pref.translated {
			c.error('literal argument cannot be passed as reference parameter `${c.table.type_to_str(param.typ)}`',
				arg.pos)
		}
		c.check_expected_call_arg(c.unwrap_generic(got_arg_typ), exp_arg_typ, node.language,
			arg) or {
			// str method, allow type with str method if fn arg is string
			// Passing an int or a string array produces a c error here
			// Deleting this condition results in proper V error messages
			// if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
			// continue
			// }
			param_typ_sym := c.table.sym(exp_arg_typ)
			arg_typ_sym := c.table.sym(got_arg_typ)
			if param_typ_sym.info is ast.Array && arg_typ_sym.info is ast.Array {
				param_elem_type := c.table.unaliased_type(param_typ_sym.info.elem_type)
				arg_elem_type := c.table.unaliased_type(arg_typ_sym.info.elem_type)
				if exp_arg_typ.nr_muls() == got_arg_typ.nr_muls()
					&& param_typ_sym.info.nr_dims == arg_typ_sym.info.nr_dims
					&& param_elem_type == arg_elem_type {
					continue
				}
			}
			c.error('${err.msg()} in argument ${i + 1} to `${left_sym.name}.${method_name}`',
				arg.pos)
		}
		param_typ_sym := c.table.sym(exp_arg_typ)
		if param_typ_sym.kind == .struct_ && got_arg_typ !in [ast.voidptr_type, ast.nil_type]
			&& !c.check_multiple_ptr_match(got_arg_typ, param.typ, param, arg) {
			got_typ_str, expected_typ_str := c.get_string_names_of(got_arg_typ, param.typ)
			c.error('cannot use `${got_typ_str}` as `${expected_typ_str}` in argument ${i + 1} to `${method_name}`',
				arg.pos)
		}
	}
	if method.is_unsafe && !c.inside_unsafe {
		c.warn('method `${left_sym.name}.${method_name}` must be called from an `unsafe` block',
			node.pos)
	}
	if c.table.cur_fn != unsafe { nil } && !c.table.cur_fn.is_deprecated && method.is_deprecated {
		c.deprecate('method', '${left_sym.name}.${method.name}', method.attrs, node.pos)
	}
	c.set_node_expected_arg_types(mut node, method)
	if is_method_from_embed {
		node.receiver_type = node.from_embed_types.last().derive(method.params[0].typ)
	} else if is_generic {
		// We need the receiver to be T in cgen.
		// TODO: cant we just set all these to the concrete type in checker? then no need in gen
		node.receiver_type = left_type.derive(method.params[0].typ).set_flag(.generic)
	} else {
		node.receiver_type = method.params[0].typ
	}
	if left_sym.kind == .interface_ && is_method_from_embed && method.return_type.has_flag(.generic)
		&& method.generic_names.len == 0 {
		method.generic_names = c.table.get_generic_names((rec_sym.info as ast.Interface).generic_types)
	}
	if method.generic_names.len != node.concrete_types.len {
		// no type arguments given in call, attempt implicit instantiation
		c.infer_fn_generic_types(method, mut node)
		concrete_types = node.concrete_types.map(c.unwrap_generic(it))
	}
	if concrete_types.len > 0 && !concrete_types[0].has_flag(.generic) {
		c.table.register_fn_concrete_types(method.fkey(), concrete_types)
		c.resolve_fn_generic_args(method, mut node)
	}

	// resolve return generics struct to concrete type
	if method.generic_names.len > 0 && method.return_type.has_flag(.generic)
		&& c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len == 0 {
		node.return_type = c.table.unwrap_generic_type(method.return_type, method.generic_names,
			concrete_types)
	} else {
		node.return_type = method.return_type
	}

	if node.concrete_types.len > 0 && node.concrete_types.all(!it.has_flag(.generic))
		&& method.return_type.has_flag(.generic) && method.generic_names.len > 0
		&& method.generic_names.len == node.concrete_types.len {
		if typ := c.table.resolve_generic_to_concrete(method.return_type, method.generic_names,
			concrete_types)
		{
			node.return_type = typ
		} else {
			node.return_type = c.table.unwrap_generic_type(method.return_type, method.generic_names,
				concrete_types)
		}
	}
	if node.concrete_types.len > 0 && method.generic_names.len == 0 {
		c.error('a non generic function called like a generic one', node.concrete_list_pos)
	}
	if method.generic_names.len > 0 {
		if !left_type.has_flag(.generic) {
			if left_sym.info is ast.Struct {
				if method.generic_names.len == left_sym.info.concrete_types.len {
					node.concrete_types = left_sym.info.concrete_types
				}
			}
		}
	}

	return node.return_type
}

fn (mut c Checker) spawn_expr(mut node ast.SpawnExpr) ast.Type {
	ret_type := c.call_expr(mut node.call_expr)
	if node.call_expr.or_block.kind != .absent {
		c.error('option handling cannot be done in `spawn` call. Do it when calling `.wait()`',
			node.call_expr.or_block.pos)
	}
	// Make sure there are no mutable arguments
	for arg in node.call_expr.args {
		if arg.is_mut && !arg.typ.is_ptr() {
			c.error('function in `spawn` statement cannot contain mutable non-reference arguments',
				arg.expr.pos())
		}
	}
	if node.call_expr.is_method && node.call_expr.receiver_type.is_ptr()
		&& !node.call_expr.left_type.is_ptr() {
		c.error('method in `spawn` statement cannot have non-reference mutable receiver',
			node.call_expr.left.pos())
	}

	if c.pref.backend.is_js() {
		return c.table.find_or_register_promise(c.unwrap_generic(ret_type))
	} else {
		return c.table.find_or_register_thread(c.unwrap_generic(ret_type))
	}
}

fn (mut c Checker) go_expr(mut node ast.GoExpr) ast.Type {
	// TODO copypasta from spawn_expr
	ret_type := c.call_expr(mut node.call_expr)
	if node.call_expr.or_block.kind != .absent {
		c.error('option handling cannot be done in `go` call. Do it when calling `.wait()`',
			node.call_expr.or_block.pos)
	}
	// Make sure there are no mutable arguments
	for arg in node.call_expr.args {
		if arg.is_mut && !arg.typ.is_ptr() {
			c.error('function in `go` statement cannot contain mutable non-reference arguments',
				arg.expr.pos())
		}
	}
	if node.call_expr.is_method && node.call_expr.receiver_type.is_ptr()
		&& !node.call_expr.left_type.is_ptr() {
		c.error('method in `go` statement cannot have non-reference mutable receiver',
			node.call_expr.left.pos())
	}

	if c.pref.backend.is_js() {
		return c.table.find_or_register_promise(c.unwrap_generic(ret_type))
	} else {
		return c.table.find_or_register_thread(c.unwrap_generic(ret_type))
	}
}

fn (mut c Checker) set_node_expected_arg_types(mut node ast.CallExpr, func &ast.Fn) {
	if node.expected_arg_types.len == 0 {
		start_idx := if func.is_method { 1 } else { 0 }
		for i in start_idx .. func.params.len {
			node.expected_arg_types << func.params[i].typ
		}
	}
}

fn (mut c Checker) post_process_generic_fns() ! {
	mut all_generic_fns := map[string]int{}
	// Loop thru each generic function concrete type.
	// Check each specific fn instantiation.
	for i in 0 .. c.file.generic_fns.len {
		mut node := c.file.generic_fns[i]
		c.mod = node.mod
		fkey := node.fkey()
		all_generic_fns[fkey]++
		if all_generic_fns[fkey] > generic_fn_cutoff_limit_per_fn {
			c.error('generic function visited more than ${generic_fn_cutoff_limit_per_fn} times',
				node.pos)
			return error('fkey: ${fkey}')
		}
		gtypes := c.table.fn_generic_types[fkey]
		$if trace_post_process_generic_fns ? {
			eprintln('> post_process_generic_fns ${node.mod} | ${node.name} | fkey: ${fkey} | gtypes: ${gtypes} | c.file.generic_fns.len: ${c.file.generic_fns.len}')
		}
		for concrete_types in gtypes {
			c.table.cur_concrete_types = concrete_types
			c.fn_decl(mut node)
			if node.name in ['x.vweb.run', 'x.vweb.run_at', 'vweb.run', 'vweb.run_at'] {
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
				eprintln('       > fn_decl node.name: ${node.name} | generic_names: ${node.generic_names} | ninstances: ${node.ninstances}')
			}
		}
	}
}

fn (mut c Checker) check_expected_arg_count(mut node ast.CallExpr, f &ast.Fn) ! {
	nr_args := node.args.len
	nr_params := if node.is_method && f.params.len > 0 { f.params.len - 1 } else { f.params.len }
	mut min_required_params := f.params.len
	if node.is_method {
		min_required_params--
	}
	if f.is_variadic {
		min_required_params--
	} else {
		has_decompose := node.args.any(it.expr is ast.ArrayDecompose)
		if has_decompose {
			// if call(...args) is present
			min_required_params = nr_args - 1
		}
	}
	if min_required_params < 0 {
		min_required_params = 0
	}
	if nr_args < min_required_params {
		if min_required_params == nr_args + 1 {
			last_typ := f.params.last().typ
			last_sym := c.table.sym(last_typ)
			if last_sym.info is ast.Struct {
				is_params := last_sym.info.attrs.any(it.name == 'params' && !it.has_arg)
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
		c.error('expected ${min_required_params} arguments, but got ${nr_args}', node.pos)
		return error('')
	} else if !f.is_variadic && nr_args > nr_params {
		unexpected_args_pos := node.args[min_required_params].pos.extend(node.args.last().pos)
		c.error('expected ${min_required_params} arguments, but got ${nr_args}', unexpected_args_pos)
		return error('')
	}
}

fn (mut c Checker) check_map_and_filter(is_map bool, elem_typ ast.Type, node ast.CallExpr) {
	if node.args.len != 1 {
		c.error('expected 1 argument, but got ${node.args.len}', node.pos)
		// Finish early so that it doesn't fail later
		return
	}
	elem_sym := c.table.sym(elem_typ)
	arg_expr := node.args[0].expr
	match arg_expr {
		ast.AnonFn {
			if arg_expr.decl.return_type.has_flag(.option) {
				c.error('option needs to be unwrapped before using it in map/filter',
					node.args[0].pos)
			}
			if arg_expr.decl.params.len > 1 {
				c.error('function needs exactly 1 argument', arg_expr.decl.pos)
			} else if is_map && (arg_expr.decl.return_type == ast.void_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a ${elem_sym.name}) T {...}`',
					arg_expr.decl.pos)
			} else if !is_map && (arg_expr.decl.return_type != ast.bool_type
				|| arg_expr.decl.params[0].typ != elem_typ) {
				c.error('type mismatch, should use `fn(a ${elem_sym.name}) bool {...}`',
					arg_expr.decl.pos)
			}
		}
		ast.Ident {
			if arg_expr.kind == .function {
				func := c.table.find_fn(arg_expr.name) or {
					c.error('${arg_expr.name} does not exist', arg_expr.pos)
					return
				}
				if func.return_type.has_flag(.option) {
					c.error('option needs to be unwrapped before using it in map/filter',
						node.pos)
				}
				if func.params.len > 1 {
					c.error('function needs exactly 1 argument', node.pos)
				} else if is_map
					&& (func.return_type == ast.void_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a ${elem_sym.name}) T {...}`',
						arg_expr.pos)
				} else if !is_map
					&& (func.return_type != ast.bool_type || func.params[0].typ != elem_typ) {
					c.error('type mismatch, should use `fn(a ${elem_sym.name}) bool {...}`',
						arg_expr.pos)
				}
			} else if arg_expr.kind == .variable {
				if arg_expr.obj is ast.Var {
					expr := arg_expr.obj.expr
					if expr is ast.AnonFn {
						// copied from above
						if expr.decl.return_type.has_flag(.option) {
							c.error('option needs to be unwrapped before using it in map/filter',
								arg_expr.pos)
						}
						if expr.decl.params.len > 1 {
							c.error('function needs exactly 1 argument', expr.decl.pos)
						} else if is_map && (expr.decl.return_type == ast.void_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a ${elem_sym.name}) T {...}`',
								expr.decl.pos)
						} else if !is_map && (expr.decl.return_type != ast.bool_type
							|| expr.decl.params[0].typ != elem_typ) {
							c.error('type mismatch, should use `fn(a ${elem_sym.name}) bool {...}`',
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
				c.error('type mismatch, `${arg_expr.name}` does not return anything',
					arg_expr.pos)
			} else if !is_map && arg_expr.return_type != ast.bool_type {
				if arg_expr.or_block.kind != .absent && (arg_expr.return_type.has_flag(.option)
					|| arg_expr.return_type.has_flag(.result))
					&& arg_expr.return_type.clear_option_and_result() == ast.bool_type {
					return
				}
				c.error('type mismatch, `${arg_expr.name}` must return a bool', arg_expr.pos)
			}
		}
		ast.StringLiteral, ast.StringInterLiteral {
			if !is_map {
				c.error('type mismatch, should use e.g. `${node.name}(it > 2)`', arg_expr.pos)
			}
		}
		ast.InfixExpr {
			if arg_expr.op == .left_shift && arg_expr.is_stmt
				&& c.table.final_sym(arg_expr.left_type).kind == .array {
				c.error('array append cannot be used in an expression', arg_expr.pos)
			}
		}
		else {}
	}
}

fn (mut c Checker) map_builtin_method_call(mut node ast.CallExpr, left_type_ ast.Type) ast.Type {
	method_name := node.name
	mut ret_type := ast.void_type
	// resolve T
	left_type := if c.table.final_sym(left_type_).kind == .any {
		c.unwrap_generic(left_type_)
	} else {
		left_type_
	}
	left_sym := c.table.final_sym(left_type)
	match method_name {
		'clone', 'move' {
			if node.args.len != 0 {
				c.error('`.${method_name}()` does not have any arguments', node.args[0].pos)
			}
			if method_name[0] == `m` {
				c.check_for_mut_receiver(mut node.left)
			}
			if node.left.is_auto_deref_var() || left_type.has_flag(.shared_f) {
				ret_type = node.left_type.deref()
			} else {
				ret_type = node.left_type
			}
			ret_type = ret_type.clear_flag(.shared_f)
		}
		'keys', 'values' {
			if node.args.len != 0 {
				c.error('`.${method_name}()` does not have any arguments', node.args[0].pos)
			}
			info := left_sym.info as ast.Map
			typ := if method_name == 'keys' {
				c.table.find_or_register_array(info.key_type)
			} else {
				c.table.find_or_register_array(info.value_type)
			}
			ret_type = ast.Type(typ)
			if info.key_type.has_flag(.generic) && method_name == 'keys' {
				ret_type = ret_type.set_flag(.generic)
			}
			if info.value_type.has_flag(.generic) && method_name == 'values' {
				ret_type = ret_type.set_flag(.generic)
			}
		}
		'delete' {
			c.check_for_mut_receiver(mut node.left)
			if node.args.len != 1 {
				c.error('expected 1 argument, but got ${node.args.len}', node.pos)
			}
			info := left_sym.info as ast.Map
			arg_type := c.expr(mut node.args[0].expr)
			c.check_expected_call_arg(arg_type, info.key_type, node.language, node.args[0]) or {
				c.error('${err.msg()} in argument 1 to `Map.delete`', node.args[0].pos)
			}
		}
		else {}
	}
	node.receiver_type = node.left_type.ref()
	node.return_type = ret_type
	return node.return_type
}

// ensure_same_array_return_type makes sure, that the return type of .clone(), .sorted(), .sorted_with_compare() etc,
// is `array_xxx`, instead of the plain `array` .
fn (mut c Checker) ensure_same_array_return_type(mut node ast.CallExpr, left_type ast.Type) {
	node.receiver_type = left_type.ref()
	if node.left.is_auto_deref_var() {
		node.return_type = left_type.deref()
	} else {
		node.return_type = node.receiver_type.set_nr_muls(0)
	}
	if node.return_type.has_flag(.shared_f) {
		node.return_type = node.return_type.clear_flag(.shared_f)
	}
}

fn (mut c Checker) array_builtin_method_call(mut node ast.CallExpr, left_type ast.Type) ast.Type {
	left_sym := c.table.final_sym(left_type)
	method_name := node.name
	mut elem_typ := ast.void_type
	if !c.is_builtin_mod && method_name == 'slice' {
		c.error('.slice() is a private method, use `x[start..end]` instead', node.pos)
		return ast.void_type
	}
	unwrapped_left_type := c.unwrap_generic(left_type)
	unaliased_left_type := c.table.unaliased_type(unwrapped_left_type)
	array_info := if left_sym.info is ast.Array {
		left_sym.info as ast.Array
	} else {
		c.table.sym(unaliased_left_type).info as ast.Array
	}
	elem_typ = array_info.elem_type
	if method_name in ['filter', 'map', 'any', 'all'] {
		if node.args.len > 0 && mut node.args[0].expr is ast.LambdaExpr {
			if node.args[0].expr.params.len != 1 {
				c.error('lambda expressions used in the builtin array methods require exactly 1 parameter',
					node.args[0].expr.pos)
				return ast.void_type
			}
			if method_name == 'map' {
				c.lambda_expr_fix_type_of_param(mut node.args[0].expr, mut node.args[0].expr.params[0],
					elem_typ)
				le_type := c.expr(mut node.args[0].expr.expr)
				// eprintln('>>>>> node.args[0].expr: ${ast.Expr(node.args[0].expr)} | elem_typ: ${elem_typ} | etype: ${le_type}')
				c.support_lambda_expr_one_param(elem_typ, le_type, mut node.args[0].expr)
			} else {
				c.support_lambda_expr_one_param(elem_typ, ast.bool_type, mut node.args[0].expr)
			}
		} else {
			// position of `it` doesn't matter
			scope_register_it(mut node.scope, node.pos, elem_typ)
		}
	} else if node.args.len == 1 && method_name == 'sorted_with_compare' {
		if node.args.len > 0 && mut node.args[0].expr is ast.LambdaExpr {
			c.support_lambda_expr_in_sort(elem_typ.ref(), ast.int_type, mut node.args[0].expr)
		}
	} else if method_name == 'sort' || method_name == 'sorted' {
		if method_name == 'sort' {
			if node.left is ast.CallExpr {
				c.error('the `sort()` method can be called only on mutable receivers, but `${node.left}` is a call expression',
					node.pos)
			}
			c.check_for_mut_receiver(mut node.left)
		}
		// position of `a` and `b` doesn't matter, they're the same
		scope_register_a_b(mut node.scope, node.pos, elem_typ)

		if node.args.len > 1 {
			c.error('expected 0 or 1 argument, but got ${node.args.len}', node.pos)
		} else if node.args.len == 1 {
			if mut node.args[0].expr is ast.LambdaExpr {
				c.support_lambda_expr_in_sort(elem_typ.ref(), ast.bool_type, mut node.args[0].expr)
			} else if node.args[0].expr is ast.InfixExpr {
				if node.args[0].expr.op !in [.gt, .lt] {
					c.error('`.${method_name}()` can only use `<` or `>` comparison',
						node.pos)
				}
				left_name := '${node.args[0].expr.left}'[0]
				right_name := '${node.args[0].expr.right}'[0]
				if left_name !in [`a`, `b`] || right_name !in [`a`, `b`] {
					c.error('`.${method_name}()` can only use `a` or `b` as argument, e.g. `arr.${method_name}(a < b)`',
						node.pos)
				} else if left_name == right_name {
					c.error('`.${method_name}()` cannot use same argument', node.pos)
				}
				if node.args[0].expr.left !in [ast.Ident, ast.SelectorExpr, ast.IndexExpr]
					|| node.args[0].expr.right !in [ast.Ident, ast.SelectorExpr, ast.IndexExpr] {
					c.error('`.${method_name}()` can only use ident, index or selector as argument, \ne.g. `arr.${method_name}(a < b)`, `arr.${method_name}(a.id < b.id)`, `arr.${method_name}(a[0] < b[0])`',
						node.pos)
				}
			} else {
				c.error(
					'`.${method_name}()` requires a `<` or `>` comparison as the first and only argument' +
					'\ne.g. `users.${method_name}(a.id < b.id)`', node.pos)
			}
		} else if !(c.table.sym(elem_typ).has_method('<')
			|| c.table.unalias_num_type(elem_typ) in [ast.int_type, ast.int_type.ref(), ast.string_type, ast.string_type.ref(), ast.i8_type, ast.i16_type, ast.i64_type, ast.u8_type, ast.rune_type, ast.u16_type, ast.u32_type, ast.u64_type, ast.f32_type, ast.f64_type, ast.char_type, ast.bool_type, ast.float_literal_type, ast.int_literal_type]) {
			c.error('custom sorting condition must be supplied for type `${c.table.type_to_str(elem_typ)}`',
				node.pos)
		}
	} else if method_name == 'wait' {
		elem_sym := c.table.sym(elem_typ)
		if elem_sym.kind == .thread {
			if node.args.len != 0 {
				c.error('`.wait()` does not have any arguments', node.args[0].pos)
			}
			thread_ret_type := c.unwrap_generic(elem_sym.thread_info().return_type)
			if thread_ret_type.has_flag(.option) {
				c.error('`.wait()` cannot be called for an array when thread functions return options. Iterate over the arrays elements instead and handle each returned option with `or`.',
					node.pos)
			} else if thread_ret_type.has_flag(.result) {
				c.error('`.wait()` cannot be called for an array when thread functions return results. Iterate over the arrays elements instead and handle each returned result with `or`.',
					node.pos)
			}
			node.return_type = c.table.find_or_register_array(thread_ret_type)
		} else {
			c.error('`${left_sym.name}` has no method `wait()` (only thread handles and arrays of them have)',
				node.left.pos())
		}
	}
	// map/filter are supposed to have 1 arg only
	mut arg_type := unaliased_left_type
	for mut arg in node.args {
		arg_type = c.check_expr_option_or_result_call(arg.expr, c.expr(mut arg.expr))
	}
	if method_name == 'map' {
		// eprintln('>>>>>>> map node.args[0].expr: ${node.args[0].expr}, left_type: ${left_type} | elem_typ: ${elem_typ} | arg_type: ${arg_type}')
		// check fn
		c.check_map_and_filter(true, elem_typ, node)
		arg_sym := c.table.sym(arg_type)
		ret_type := match arg_sym.info {
			ast.FnType {
				if node.args[0].expr is ast.SelectorExpr {
					arg_type
				} else {
					arg_sym.info.func.return_type
				}
			}
			else {
				arg_type
			}
		}
		node.return_type = c.table.find_or_register_array(c.unwrap_generic(ret_type))
		if node.return_type.has_flag(.shared_f) {
			node.return_type = node.return_type.clear_flag(.shared_f).deref()
		}
		ret_sym := c.table.sym(ret_type)
		if ret_sym.kind == .multi_return {
			c.error('returning multiple values is not supported in .map() calls', node.pos)
		}
	} else if method_name == 'filter' {
		// check fn
		if node.return_type.has_flag(.shared_f) {
			node.return_type = node.return_type.clear_flag(.shared_f).deref()
		} else if node.left.is_auto_deref_var() {
			node.return_type = node.return_type.deref()
		}
		c.check_map_and_filter(false, elem_typ, node)
	} else if method_name in ['any', 'all'] {
		c.check_map_and_filter(false, elem_typ, node)
		node.return_type = ast.bool_type
	} else if method_name == 'clone' {
		if node.args.len != 0 {
			c.error('`.clone()` does not have any arguments', node.args[0].pos)
		}
		c.ensure_same_array_return_type(mut node, left_type)
	} else if method_name == 'sorted' {
		c.ensure_same_array_return_type(mut node, left_type)
	} else if method_name == 'sorted_with_compare' {
		c.ensure_same_array_return_type(mut node, left_type)
		// Inject a (voidptr) cast for the callback argument, to pass -cstrict, otherwise:
		// error: incompatible function pointer types passing
		//                            'int (string *, string *)'  (aka 'int (struct string *, struct string *)')
		//       to parameter of type 'int (*)(voidptr, voidptr)' (aka 'int (*)(void *, void *)')
		node.args[0].expr = ast.CastExpr{
			expr: node.args[0].expr
			typ: ast.voidptr_type
			typname: 'voidptr'
			expr_type: c.expr(mut node.args[0].expr)
			pos: node.pos
		}
	} else if method_name == 'sort' {
		node.return_type = ast.void_type
	} else if method_name == 'contains' {
		// c.warn('use `value in arr` instead of `arr.contains(value)`', node.pos)
		if node.args.len != 1 {
			c.error('`.contains()` expected 1 argument, but got ${node.args.len}', node.pos)
		} else if !left_sym.has_method('contains') {
			arg_typ := c.expr(mut node.args[0].expr)
			c.check_expected_call_arg(arg_typ, elem_typ, node.language, node.args[0]) or {
				c.error('${err.msg()} in argument 1 to `.contains()`', node.args[0].pos)
			}
		}
		for i, mut arg in node.args {
			node.args[i].typ = c.expr(mut arg.expr)
		}
		node.return_type = ast.bool_type
	} else if method_name == 'index' {
		if node.args.len != 1 {
			c.error('`.index()` expected 1 argument, but got ${node.args.len}', node.pos)
		} else if !left_sym.has_method('index') {
			arg_typ := c.expr(mut node.args[0].expr)
			c.check_expected_call_arg(arg_typ, elem_typ, node.language, node.args[0]) or {
				c.error('${err.msg()} in argument 1 to `.index()`', node.args[0].pos)
			}
		}
		for i, mut arg in node.args {
			node.args[i].typ = c.expr(mut arg.expr)
		}
		node.return_type = ast.int_type
	} else if method_name in ['first', 'last', 'pop'] {
		if node.args.len != 0 {
			c.error('`.${method_name}()` does not have any arguments', node.args[0].pos)
		}
		node.return_type = array_info.elem_type
		if method_name == 'pop' {
			c.check_for_mut_receiver(mut node.left)
			node.receiver_type = node.left_type.ref()
		} else {
			node.receiver_type = node.left_type
		}
	} else if method_name == 'delete' {
		c.check_for_mut_receiver(mut node.left)
		unwrapped_left_sym := c.table.sym(unwrapped_left_type)
		if method := c.table.find_method(unwrapped_left_sym, method_name) {
			node.receiver_type = method.receiver_type
		}
		node.return_type = ast.void_type
	}
	return node.return_type
}

fn (mut c Checker) check_for_mut_receiver(mut expr ast.Expr) (string, token.Pos) {
	to_lock, pos := c.fail_if_immutable(mut expr)
	if !expr.is_lvalue() {
		c.error('cannot pass expression as `mut`', expr.pos())
	}
	return to_lock, pos
}

fn scope_register_it(mut s ast.Scope, pos token.Pos, typ ast.Type) {
	scope_register_var_name(mut s, pos, typ, 'it')
}

fn scope_register_a_b(mut s ast.Scope, pos token.Pos, typ ast.Type) {
	scope_register_var_name(mut s, pos, typ.ref(), 'a')
	scope_register_var_name(mut s, pos, typ.ref(), 'b')
}

fn scope_register_var_name(mut s ast.Scope, pos token.Pos, typ ast.Type, name string) {
	s.register(ast.Var{
		name: name
		pos: pos
		typ: typ
		is_used: true
	})
}
