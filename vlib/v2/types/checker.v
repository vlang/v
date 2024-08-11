// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import time
import v2.ast
import v2.errors
import v2.pref
import v2.token

struct Environment {
mut:
	// errors with no default value
	scopes shared map[string]&Scope = map[string]&Scope{}
	// types map[int]Type
	// TODO:
	// methods map...
	// methods map[int][]&Fn
	methods           map[string][]&Fn
	generic_types     map[string][]map[string]Type
	cur_generic_types []map[string]Type
}

pub fn Environment.new() &Environment {
	return &Environment{}
}

enum DeferredKind {
	fn_decl
	fn_decl_generic
	struct_decl
	const_decl
}

struct Deferred {
	kind  DeferredKind
	func  fn () = unsafe { nil }
	scope &Scope
}

struct Checker {
	pref &pref.Preferences
	// info Info
	// TODO: mod
	mod &Module = new_module('main', '')
mut:
	env           &Environment = &Environment{}
	file_set      &token.FileSet
	scope         &Scope = new_scope(unsafe { nil })
	c_scope       &Scope = new_scope(unsafe { nil })
	deferred      []Deferred
	expected_type ?Type

	generic_params []string
	// TODO: remove once fields/methods with same name
	// are no longer allowed & removed.
	expecting_method bool
}

pub fn Checker.new(prefs &pref.Preferences, file_set &token.FileSet, env &Environment) &Checker {
	return &Checker{
		pref:     unsafe { prefs }
		file_set: unsafe { file_set }
		env:      unsafe { env }
	}
}

pub fn (mut c Checker) get_module_scope(module_name string, parent &Scope) &Scope {
	// return c.env.scopes[module_name] or {
	// 	s := new_scope(parent)
	// 	c.env.scopes[module_name] = s
	// 	s
	// }
	return lock c.env.scopes {
		c.env.scopes[module_name] or {
			s := new_scope(parent)
			c.env.scopes[module_name] = s
			s
		}
	}
}

pub fn (mut c Checker) check_files(files []ast.File) {
	// c.file_set = unsafe { file_set }
	c.preregister_all_scopes(files)
	c.preregister_all_types(files)

	for file in files {
		c.check_file(file)
	}
	// c.log('DEFERRED - START')
	// TODO: a better way to do this, please.
	// ideally just resolve what needs to be
	// const decl
	for d in c.deferred {
		if d.kind != .const_decl {
			continue
		}
		c.scope = d.scope
		d.func()
	}
	// fn decls
	for d in c.deferred {
		if d.kind != .fn_decl {
			continue
		}
		c.scope = d.scope
		d.func()
	}
	// fn decls - generic
	for d in c.deferred {
		if d.kind != .fn_decl_generic {
			continue
		}
		c.scope = d.scope
		d.func()
	}
	// c.log('DEFERRED - END')
}

pub fn (mut c Checker) check_file(file ast.File) {
	if !c.pref.verbose {
		unsafe {
			goto start_no_time
		}
	}
	mut sw := time.new_stopwatch()
	start_no_time:
	// file_scope := new_scope(c.mod.scope)
	// mut mod_scope := new_scope(c.mod.scope)
	// c.env.scopes[file.mod] = mod_scope
	mut mod_scope := lock c.env.scopes {
		c.env.scopes[file.mod] or {
			panic('not found for mod: ${file.mod}')
			// c.env.scopes[file.mod] = c.mod.scope
			// c.mod.scope
		}
	}
	c.scope = mod_scope
	// mut mod_scope := c.env.scopes[file.mod] or {
	// 	panic('scope should exist')
	// }
	// c.scope = mod_scope
	for stmt in file.stmts {
		// if stmt is ast.Decl {
		match stmt {
			ast.EnumDecl, ast.InterfaceDecl, ast.StructDecl, ast.TypeDecl {
				continue
			}
			else {
				c.decl(stmt)
			}
		}
		// }
	}
	for stmt in file.stmts {
		c.stmt(stmt)
	}
	if c.pref.verbose {
		check_time := sw.elapsed()
		println('type check ${file.name}: ${check_time.milliseconds()}ms (${check_time.microseconds()}µs)')
	}
}

fn (mut c Checker) preregister_scopes(file ast.File) {
	builtin_scope := c.get_module_scope('builtin', universe)

	mod_scope := c.get_module_scope(file.mod, builtin_scope)
	c.scope = mod_scope
	// add self (own module) for constants. can use own module prefix inside module
	c.scope.insert(file.mod, Module{ scope: c.get_module_scope(file.mod, builtin_scope) })
	// add imports
	for imp in file.imports {
		mod := if imp.is_aliased { imp.name.all_after_last('.') } else { imp.alias }
		c.scope.insert(imp.alias, Module{ scope: c.get_module_scope(mod, builtin_scope) })
	}
	// add C
	c.scope.insert('C', Module{ scope: c.c_scope })
}

fn (mut c Checker) preregister_all_scopes(files []ast.File) {
	// builtin_scope := c.get_module_scope('builtin', universe)
	// preregister scopes & imports
	for file in files {
		c.preregister_scopes(file)
		// mod_scope := c.get_module_scope(file.mod, builtin_scope)
		// c.scope = mod_scope
		// // add self (own module) for constants
		// c.scope.insert(file.mod, Module{scope: c.get_module_scope(file.mod, builtin_scope)})
		// // add imports
		// for imp in file.imports {
		// 	mod :=  if imp.is_aliased { imp.name.all_after_last('.') } else { imp.alias }
		// 	c.scope.insert(imp.alias, Module{scope: c.get_module_scope(mod, builtin_scope)})
		// }
		// // add C
		// c.scope.insert('C', Module{scope: c.c_scope})
	}
}

fn (mut c Checker) preregister_types(file ast.File) {
	mut mod_scope := c.env.scopes[file.mod] or { panic('scope should exist') }
	c.scope = mod_scope
	for stmt in file.stmts {
		// if stmt is ast.Decl {
		match stmt {
			ast.EnumDecl, ast.InterfaceDecl, ast.StructDecl, ast.TypeDecl {}
			else {
				continue
			}
		}
		c.decl(stmt)
		// }
	}
}

fn (mut c Checker) preregister_all_types(files []ast.File) {
	for file in files {
		c.preregister_types(file)
	}
	// c.log('DEFERRED - START')
	for d in c.deferred {
		if d.kind != .struct_decl {
			continue
		}
		c.scope = d.scope
		d.func()
	}
	// c.log('DEFERRED - END')
}

fn (mut c Checker) decl(decl ast.Stmt) {
	match decl {
		ast.ConstDecl {
			for field in decl.fields {
				// c.log('const decl: $field.name')
				obj := Const{
					mod:  c.mod
					name: field.name
					// typ: c.expr(field.value)
				}
				c.scope.insert(obj.name, obj)
				// TODO: check if contains references to other consts and only
				// delay those. or keep deferring until type is known otherwise error
				// work out best way to do this, and use same approach for everything
				c.later(fn [mut c, field] () {
					// c.log('updating const $field.name type')
					const_type := c.expr(field.value)
					if mut cd := c.scope.lookup(field.name) {
						if mut cd is Const {
							cd.typ = const_type.typed_default()
						}
					}
				}, .const_decl)
			}
		}
		ast.EnumDecl {
			// TODO: if non builtin types can be used as part of
			// the expr then these will need to get delayed also.
			mut fields := []Field{}
			for field in decl.fields {
				fields << Field{
					name: field.name
				}
			}
			// as_type := decl.as_type !is ast.EmptyExpr { c.expr(decl.as_type) } else { Type(int_) }
			obj := Enum{
				is_flag: decl.attributes.has('flag')
				name:    decl.name
				fields:  fields
			}
			c.scope.insert(obj.name, Type(obj))
		}
		ast.FnDecl {
			// if decl.typ.generic_params.len > 0 {
			// 	fn_decl := decl
			// 	c.later(fn[mut c, fn_decl]() {
			// 		c.fn_decl(fn_decl)
			// 	}, .fn_decl_generic)
			// } else {
			// 	c.fn_decl(decl)
			// }
			c.fn_decl(decl)
		}
		ast.GlobalDecl {
			// for field in decl.fields {
			// 	// field_type := c.expr(field.typ)
			// 	// if field.name == 'g_timers' {
			// 	// 	// dump(field.typ)
			// 	// 	panic('# g_timers: $field_type.name()')
			// 	// }
			// 	obj := Global{
			// 		name: field.name
			// 		// typ: c.expr(field.typ)
			// 		typ: c.expr(field.value)
			// 	}
			// 	// c.log('GlobalDecl: $field.name - $obj.typ.type_name()')
			// 	c.scope.insert(field.name, obj)
			// }
		}
		ast.InterfaceDecl {
			// TODO:
			obj := Interface{
				name: decl.name
			}
			c.scope.insert(decl.name, Type(obj))
			interface_decl := decl
			mut scope := c.scope
			c.later(fn [mut c, mut scope, interface_decl] () {
				mut fields := []Field{}
				for field in interface_decl.fields {
					fields << Field{
						name: field.name
						typ:  c.expr(field.typ)
					}
				}
				if mut id := scope.lookup(interface_decl.name) {
					if mut id is Type {
						if mut id is Interface {
							id.fields = fields
						}
					}
				}
			}, .struct_decl)
		}
		ast.StructDecl {
			// c.log(' # StructDecl: $decl.name')
			// TODO: clean this up
			struct_decl := decl
			c.later(fn [mut c, struct_decl] () {
				// c.log('add fields: $struct_decl.name')
				mut fields := []Field{}
				for field in struct_decl.fields {
					fields << Field{
						name: field.name
						typ:  c.expr(field.typ)
					}
				}
				mut embedded := []Struct{}
				for embedded_expr in struct_decl.embedded {
					embedded_type := c.expr(embedded_expr)
					if embedded_type is Struct {
						embedded << embedded_type
					} else {
						c.error_with_pos('can only structs, `${embedded_type.name()}` is not a struct.',
							struct_decl.pos)
					}
				}
				mut update_scope := if struct_decl.language == .c { c.c_scope } else { c.scope }
				// TODO: work best way to do this?
				// modify the original type since, that is
				// the one every Type will be pointing to
				if mut sd := update_scope.lookup(struct_decl.name) {
					if mut sd is Type {
						if mut sd is Struct {
							sd.fields = fields
							sd.embedded = embedded
						}
					}
				}
				// obj := types.Struct{
				// 	name: struct_decl.name
				// 	fields: fields
				// }
				// typ := Type(obj)
				// c.scope.insert(struct_decl.name, typ)
			}, .struct_decl)
			// c.log('struct decl: $decl.name')
			obj := Struct{
				name: decl.name
				// fields: [Field{name: 'len', typ: ast.Ident{name: 'int'}}]
				// fields: [Field{name: 'len'}]
			}
			mut typ := Type(obj)
			// TODO: proper
			if decl.language == .c {
				c.c_scope.insert(decl.name, typ)
			} else {
				c.scope.insert(decl.name, typ)
			}
		}
		ast.TypeDecl {
			type_decl := decl
			// alias
			if decl.variants.len == 0 {
				alias_type := Alias{
					name: decl.name
					// TODO: defer
					// parent: c.expr(decl.base_type)
				}
				mut typ := Type(alias_type)
				c.scope.insert(decl.name, typ)
				c.later(fn [mut c, type_decl] () {
					// mut obj := c.scope.lookup(type_decl.name) or { panic(err.msg()) }
					// mut typ := obj.typ()
					// if mut typ is Alias {
					// 	typ.base_type = c.expr(type_decl.base_type)
					// }
					if mut obj := c.scope.lookup(type_decl.name) {
						if mut obj is Type {
							if mut obj is Alias {
								obj.base_type = c.expr(type_decl.base_type)
							}
						}
					}
				}, .struct_decl)
			}
			// sum type
			else {
				sum_type := SumType{
					name: decl.name
					// variants: decl.variants
				}
				mut typ := Type(sum_type)
				c.scope.insert(decl.name, typ)
				c.later(fn [mut c, type_decl] () {
					mut obj := c.scope.lookup(type_decl.name) or { panic(err.msg()) }
					mut typ := obj.typ()
					// mut typ := c.expr(ast.Ident{name: type_decl.name})
					if mut typ is SumType {
						for variant in type_decl.variants {
							typ.variants << c.expr(variant)
						}
					}
				}, .struct_decl)
			}
		}
		else {}
	}
}

fn (mut c Checker) check_types(exp_type Type, got_type Type) bool {
	// TODO: will this work with Primitive? might need to add comparison methods
	if got_type == exp_type {
		return true
	}
	// number literals
	if exp_type.is_number() && got_type.is_number_literal() {
		return true
	}
	// primitives
	if exp_type is Primitive && got_type is Primitive {
		// mut got_props := got_type.props
		// got_props.clear(.untyped)
		// // checks if both match flot or int
		// if got_props == exp_type.props {
		// 	return true
		// }
	}

	return false
}

fn (mut c Checker) expr(expr ast.Expr) Type {
	c.log('expr: ${expr.type_name()}')
	match expr {
		ast.ArrayInitExpr {
			// c.log('ArrayInit:')
			// `[1,2,3,4]`
			if expr.exprs.len > 0 {
				is_fixed := expr.len !is ast.EmptyExpr
				// TODO: check all exprs
				first_elem_type := c.expr(expr.exprs.first())
				// NOTE: why did I have this shortcut here?
				// if expr.exprs.len == 1 {
				// 	if first_elem_type.is_number_literal() {
				// 		return Array{
				// 			elem_type: int_
				// 		}
				// 	}
				// }
				// TODO: promote [0] - proper
				expected_type_prev := c.expected_type
				expected_type := c.expected_type or {
					// `[Type.value_a, .value_b]`
					// set expected type for checking `.value_b`
					if first_elem_type is Enum {
						c.expected_type = first_elem_type
					}
					first_elem_type
				}

				for i, elem_expr in expr.exprs {
					if i == 0 {
						continue
					}
					mut elem_type := c.expr(elem_expr)
					// TODO: best way to handle this?
					if elem_type.is_number_literal() && first_elem_type.is_number() {
						elem_type = first_elem_type
					}
					// sum type, check variants
					if (expected_type is SumType && elem_type !in expected_type.variants)
						&& elem_type != first_elem_type // everything else
					  {
						// TODO: add general method for promotion/coercion
						c.error_with_pos('expecting element of type: ${first_elem_type.name()}, got ${elem_type.name()}',
							expr.pos)
					}
				}
				c.expected_type = expected_type_prev
				return if is_fixed {
					ArrayFixed{
						len:       expr.exprs.len
						elem_type: first_elem_type
					}
				} else {
					Array{
						elem_type: first_elem_type
					}
				}
			}
			// `[]int{}`
			return c.expr(expr.typ)
		}
		ast.AsCastExpr {
			// TODO:
			c.expr(expr.expr)
			return c.expr(expr.typ)
		}
		ast.BasicLiteral {
			// c.log('ast.BasicLiteral: $expr.kind.str(): $expr.value')
			match expr.kind {
				.char {
					return char_
				}
				.key_false, .key_true {
					return bool_
				}
				// TODO:
				.number {
					// TODO: had to be a better way to do this
					// should this be handled earlier? scanner?
					if expr.value.contains('.') {
						return float_literal_
					}
					return int_literal_
				}
				else {
					panic('invalid ast.BasicLiteral kind: ${expr.kind}')
				}
			}
		}
		ast.CallOrCastExpr {
			// c.log('CallOrCastExpr: $lhs_type.name()')
			// lhs_type := c.expr(expr.lhs)
			// // call
			// if lhs_type is FnType {
			// 	return c.call_expr(ast.CallExpr{lhs: expr.lhs, args: [expr.expr]})
			// }
			// // cast
			// // expr_type := c.expr(expr.expr)
			// // TODO: check if expr_type can be cast to lhs_type
			// return lhs_type
			return c.expr(c.resolve_call_or_cast_expr(expr))
		}
		ast.CallExpr {
			// TODO/FIXME:
			// we need a way to handle C.stat|sigaction() / C.stat|sigaction{}
			// multiple items with same name inside scope lookup.
			if expr.lhs is ast.SelectorExpr {
				if expr.lhs.rhs.name == 'stat' {
					return int_
				}
			}
			return c.call_expr(expr)
		}
		ast.CastExpr {
			typ := c.expr(expr.typ)
			c.log('CastExpr: ${typ.name()}')
			return typ
		}
		ast.ComptimeExpr {
			cexpr := c.resolve_expr(expr.expr)
			// TODO: move to checker, where `ast.*Or*` nodes will be resolved.
			if cexpr !in [ast.CallExpr, ast.IfExpr] {
				c.error_with_pos('unsupported comptime: ${cexpr.type_name()}', expr.pos)
			}
			// TODO: $if dynamic_bohem ...
			if cexpr is ast.IfExpr {
				c.log('TODO: comptime IfExpr')
				return void_
			}
			c.log('ComptimeExpr: ' + cexpr.type_name())
			// return c.expr(cexpr)
		}
		ast.EmptyExpr {
			// TODO:
			return void_
		}
		ast.FnLiteral {
			return c.fn_type(expr.typ, FnTypeAttribute.empty)
		}
		ast.GenericArgs {
			// NOTE: first generic args handled in CallExpr
			// this is generic struct nested in generic args
			mut name := ''
			match expr.lhs {
				ast.Ident {
					name = expr.lhs.name
				}
				else {}
			}
			mut args := []string{}
			for arg in expr.args {
				if arg is ast.Ident {
					args << arg.name
				}
			}
			lhs_type := c.expr(expr.lhs)
			if lhs_type is FnType {
				return FnType{
					...lhs_type
					generic_params: args
				}
			}

			return Struct{
				name:           name
				generic_params: args
			}
		}
		ast.GenericArgOrIndexExpr {
			return c.expr(c.resolve_generic_arg_or_index_expr(expr))
		}
		ast.Ident {
			// c.log('ident: $expr.name')
			obj := c.ident(expr)
			typ := obj.typ()
			// TODO:
			if expr.name == 'string' {
				if typ is Struct {
					return string_
				}
			}
			return typ
		}
		ast.IfExpr {
			// if guard
			if expr.cond is ast.IfGuardExpr {
				c.open_scope()
				c.assign_stmt(expr.cond.stmt, true)
				c.stmt_list(expr.stmts)
				c.close_scope()
				if expr.else_expr is ast.IfExpr {
					c.open_scope()
					c.scope.insert('err', Type(Struct{ name: 'Error' }))
					c.stmt_list(expr.else_expr.stmts)
					c.close_scope()
				}
				// TODO: never used, add a special type?
				return void_
			}

			// normal if
			return c.if_expr(expr)
		}
		ast.IfGuardExpr {
			c.assign_stmt(expr.stmt, true)
			// TODO:
			return bool_
		}
		ast.IndexExpr {
			lhs_type := c.expr(expr.lhs)
			// TODO: make sure lhs_type is indexable
			// if !lhs_type.is_indexable() { c.error('cannot index ${lhs_type.name()}') }
			value_type := if expr.expr is ast.RangeExpr {
				lhs_type
			} else {
				lhs_type.value_type()
			}
			// c.log('IndexExpr: ${value_type.name()} / ${lhs_type.name()}')
			return value_type
		}
		ast.InfixExpr {
			lhs_type := c.expr(expr.lhs)
			// TODO: why was I setting expected type for enum here?
			// expected_type := c.expected_type
			// if lhs_type is Enum {
			// 	c.expected_type = lhs_type
			// }
			c.expr(expr.rhs)
			// c.expected_type = expected_type
			if expr.op.is_comparison() {
				return bool_
			}
			return lhs_type
		}
		ast.InitExpr {
			// TODO: try handle this from expr
			// mut typ_expr := expr.typ
			// if expr.typ is ast.GenericArgs {
			// 	typ_expr = expr.typ.lhs
			// }
			typ := c.expr(expr.typ)
			// TODO:
			// if typ is ast.ChannelType {
			// 	for field in expr.fields {
			// 		match field.name {
			// 			'cap' {}
			// 			else { c.error_with_pos('unknown channel attribute `${key}`') }
			// 		}
			// 	}
			// }
			// TODO:
			// for field in expr.fields {
			// 	if field.value !is ast.EmptyExpr {
			// 		field_expr_type := c.expr(field.value)
			// 	}
			// }
			return typ
		}
		ast.KeywordOperator {
			// TODO:
			typ := c.expr(expr.exprs[0])
			match expr.op {
				.key_go, .key_spawn {
					return Thread{}
				}
				else {
					return typ
				}
			}
		}
		ast.MapInitExpr {
			// TODO: type check keys/vals
			// `map[type]type{}`
			if expr.typ !is ast.EmptyExpr {
				typ := c.expr(expr.typ)
				return typ
			}
			// `{}`
			if expr.keys.len == 0 {
				return c.expected_type or {
					c.error_with_pos('empty map {} used in unsupported context', expr.pos)
				}
			}
			// `{key: value}`
			key0_type := c.expr(expr.keys[0])
			value0_type := c.expr(expr.vals[0])
			return Map{
				key_type:   key0_type
				value_type: value0_type
			}
		}
		ast.MatchExpr {
			return c.match_expr(expr, true)
		}
		ast.ModifierExpr {
			// if expr.expr !is ast.Ident && expr.expr !is ast.Type {
			// 	panic('not ident: $expr.expr.type_name()')
			// }
			return c.expr(expr.expr)
		}
		ast.OrExpr {
			cond := c.resolve_expr(expr.expr)
			cond_type := c.expr(cond).unwrap()
			// c.log('OrExpr: ${cond_type.name()}')
			if expr.stmts.len > 0 {
				last_stmt := expr.stmts.last()
				if last_stmt is ast.ExprStmt {
					expr_stmt_type := c.expr(last_stmt.expr).unwrap()
					// c.log('OrExpr: last_stmt_type: ${cond_type.name()}')
					// TODO: non returning call (currently just checking void)
					// should probably lookup function/method and check for noreturn attribute?
					// if cond is ast.CallExpr {}
					// do we need to do promotion here

					// last stmt expr does does not return a type
					if expr_stmt_type !is Void && !c.check_types(cond_type, expr_stmt_type) {
						c.error_with_pos('or expr expecting ${cond_type.name()}, got ${expr_stmt_type.name()}',
							expr.pos)
					}
					return cond_type
				}
			}
			return cond_type
		}
		ast.ParenExpr {
			return c.expr(expr.expr)
		}
		ast.PostfixExpr {
			// TODO:
			// typ := c.expr(expr.expr)
			// if typ is FnType {
			// 	if rt := typ.return_type {
			// 		if rt in [OptionType, ResultType] { return typ }
			// 	}
			// 	if expr.op == .not {
			// 		return_type := OptionType{base_type: typ.return_type or { void_ }}
			// 		return FnType{...typ, return_type: return_type}
			// 	}
			// 	else if expr.op == .question {
			// 		return_type := ResultType{base_type: typ.return_type or { void_ }}
			// 		return FnType{...typ, return_type: return_type}
			// 	}
			// }
			// return typ

			return c.expr(expr.expr)
		}
		ast.PrefixExpr {
			expr_type := c.expr(expr.expr)
			if expr.op == .amp {
				return Pointer{
					base_type: expr_type
				}
			} else if expr.op == .mul {
				if expr_type is Pointer {
					// c.log('DEREF')
					return expr_type.base_type
				} else {
					c.error_with_pos('deref on non pointer type `${expr_type.name()}`',
						expr.pos)
				}
			}
			return c.expr(expr.expr)
		}
		ast.RangeExpr {
			c.expr(expr.start)
			c.expr(expr.end)
			return Type(Array{
				elem_type: int_
			})
		}
		ast.SelectExpr {
			c.stmt_list(expr.stmts)
		}
		ast.SelectorExpr {
			// enum value: `.green`
			if expr.lhs is ast.EmptyExpr {
				// c.log('got enum value')
				// // dump(expr)
				// return c.expected_type
				return c.expected_type or {
					c.error_with_pos('c.expected_type is not set', expr.pos)
				}

				// return int_
			}
			// normal selector
			return c.selector_expr(expr)
		}
		ast.StringInterLiteral {
			// TODO:
			return string_
		}
		ast.StringLiteral {
			return string_
		}
		ast.Tuple {
			mut types := []Type{}
			for x in expr.exprs {
				types << c.expr(x)
			}
			return Tuple{
				types: types
			}
		}
		ast.Type {
			match expr {
				ast.AnonStructType {}
				ast.ArrayType {
					return Array{
						elem_type: c.expr(expr.elem_type)
					}
				}
				ast.ArrayFixedType {
					// TODO:
					return Array{
						elem_type: c.expr(expr.elem_type)
					}
				}
				ast.ChannelType {
					return Channel{
						elem_type: if expr.elem_type !is ast.EmptyExpr {
							c.expr(expr.elem_type)
						} else {
							none
						}
					}
				}
				ast.FnType {
					return c.fn_type(expr, FnTypeAttribute.empty)
				}
				ast.GenericType {
					// TODO:
					return c.expr(expr.name)
					// return c
				}
				ast.MapType {
					return Map{
						key_type:   c.expr(expr.key_type)
						value_type: c.expr(expr.value_type)
					}
				}
				ast.NilType {
					return nil_
				}
				ast.NoneType {
					return none_
				}
				ast.OptionType {
					return OptionType{
						base_type: c.expr(expr.base_type)
					}
				}
				ast.ResultType {
					return ResultType{
						base_type: c.expr(expr.base_type)
					}
				}
				ast.ThreadType {
					return Thread{
						elem_type: if expr.elem_type !is ast.EmptyExpr {
							c.expr(expr.elem_type)
						} else {
							none
						}
					}
				}
				ast.TupleType {
					mut types := []Type{}
					for tx in expr.types {
						types << c.expr(tx)
					}
					return Tuple{
						types: types
					}
				}
				// else{
				// 	c.log('expr.Type: not implemented ${expr.type_name()} - ${typeof(expr).name}')
				// }
			}
		}
		ast.UnsafeExpr {
			// TODO: proper
			c.stmt_list(expr.stmts)
			last_stmt := expr.stmts.last()
			if last_stmt is ast.ExprStmt {
				return c.expr(last_stmt.expr)
			}
			// TODO: impl: avoid returning types everywhere / using void
			// perhaps use a struct and set the type and other info in it when needed
			return void_
		}
		else {}
	}
	// TODO: remove (add all variants)
	c.log('expr: unhandled ${expr.type_name()}')
	return int_
}

fn (mut c Checker) stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssertStmt {
			c.expr(stmt.expr)
		}
		ast.AssignStmt {
			c.assign_stmt(stmt, false)
		}
		ast.BlockStmt {
			c.stmt_list(stmt.stmts)
		}
		// ast.Decl {
		// 	// Handled earlier
		// 	// match stmt {
		// 	// 	ast.FnDecl{}
		// 	// 	ast.TypeDecl {}
		// 	// 	else {}
		// 	// }
		// }
		ast.GlobalDecl {
			for field in stmt.fields {
				// c.log('GlobalDecl: $field.name - $obj.typ.type_name()')
				field_type := if field.typ !is ast.EmptyExpr {
					c.expr(field.typ)
				} else {
					c.expr(field.value)
				}
				obj := Global{
					name: field.name
					typ:  field_type
				}
				c.scope.insert(field.name, obj)
			}
		}
		ast.DeferStmt {
			c.stmt_list(stmt.stmts)
		}
		ast.ExprStmt {
			if stmt.expr is ast.MatchExpr {
				c.match_expr(stmt.expr, false)
			} else {
				c.expr(stmt.expr)
			}
		}
		ast.ForStmt {
			c.open_scope()
			// TODO: vars for other for loops
			if stmt.init is ast.ForInStmt {
				expr_type := c.expr(stmt.init.expr)
				if stmt.init.key is ast.Ident {
					// TODO: remove
					if expr_type is Void {
						c.scope.print(false)
						// dump(stmt)
						panic('## expr_type is Void!')
					}
					key_type := expr_type.key_type()
					c.scope.insert(stmt.init.key.name, key_type)
				}
				mut value_type := expr_type.value_type()
				if stmt.init.value is ast.ModifierExpr {
					if stmt.init.value.kind == .key_mut {
						value_type = value_type.ref()
					}
					if stmt.init.value.expr is ast.Ident {
						c.scope.insert(stmt.init.value.expr.name, value_type)
					}
				} else if stmt.init.value is ast.Ident {
					c.scope.insert(stmt.init.value.name, value_type)
				}
			} else {
				if stmt.cond !is ast.EmptyExpr {
					sc_names, sc_types := c.extract_smartcasts(stmt.cond)
					c.apply_smartcasts(sc_names, sc_types)
				}
				// c.stmt(stmt.init)
			}
			// sc_names, sc_types := c.extract_smartcasts(stmt.cond)
			// c.apply_smartcasts(sc_names, sc_types)
			c.stmt(stmt.init)
			c.expr(stmt.cond)
			c.stmt(stmt.post)
			c.stmt_list(stmt.stmts)
			c.close_scope()
		}
		ast.ImportStmt {
			// c.log('import: $stmt.name as $stmt.alias')
		}
		ast.ReturnStmt {
			c.log('ReturnStmt:')
			for expr in stmt.exprs {
				c.expr(expr)
			}
		}
		else {}
	}
}

fn (mut c Checker) stmt_list(stmts []ast.Stmt) {
	for stmt in stmts {
		c.stmt(stmt)
	}
}

fn (mut c Checker) later(func fn (), kind DeferredKind) {
	c.deferred << Deferred{
		kind:  kind
		func:  func
		scope: c.scope
	}
}

fn (mut c Checker) assign_stmt(stmt ast.AssignStmt, unwrap_optional bool) {
	for i, lx in stmt.lhs {
		// TODO: proper / tuple (handle multi return)
		rx := stmt.rhs[i] or { stmt.rhs[0] }
		// lhs_type := c.scope.lookup_parent
		// TODO: ident field for blank ident?
		mut is_blank_ident := false
		if lx is ast.Ident {
			is_blank_ident = lx.name == '_'
		}
		mut lhs_type := Type(void_)
		if stmt.op != .decl_assign && !is_blank_ident {
			lhs_type = c.expr(lx)
		}
		expected_type := c.expected_type
		if lhs_type is Enum {
			c.expected_type = lhs_type
		}
		rhs_type := c.expr(rx)
		// if t := expected_type {
		// 	c.log('AssignStmt: setting expected_type to: $t.name()')
		// } else {
		// 	c.log('AssignStmt: setting expected_type to: none')
		// }
		c.expected_type = expected_type
		// c.expected_type = none
		mut expr_type := rhs_type
		if unwrap_optional {
			expr_type = expr_type.unwrap()
		}
		if rhs_type is Tuple {
			expr_type = rhs_type.types[i]
		}
		// promote untyped literals
		expr_type = expr_type.typed_default()
		// TODO: assignment check
		// if stmt.op != .decl_assign {
		// 	c.assignment(expr_type, lhs_type) or {
		// 		c.log('error!!')
		// 		c.error_with_pos(err.msg(), stmt.pos)
		// 	}
		// }
		// TODO: proper
		// TODO: modifiers, lx_unwrapped := c.unwrap...
		// or some method to use modifiers
		lx_unwrapped := c.unwrap_expr(lx)
		if lx_unwrapped is ast.Ident {
			c.scope.insert(lx_unwrapped.name, expr_type)
		}
	}
}

// TODO:
// fn (mut c Checker) assignment(lx ast.Expr, typ Type) {
fn (mut c Checker) assignment(from_type Type, to_type Type) ! {
	// same type
	if from_type == to_type {
		return
	}
	// numbers literals
	if from_type.is_number_literal() && to_type.is_number() {
		return
	}
	// aliases
	if from_type is Alias {
		return c.assignment(from_type.base_type, to_type)
	}
	if to_type is Alias {
		return c.assignment(from_type, to_type.base_type)
	}
	// TODO: provide context about if inside unsafe
	if to_type is FnType {
		// allow nil to fn pointer
		if from_type is Nil {
			return
		}
	}
	// pointers
	if to_type is Pointer {
		if from_type is Nil {
			return
		}
		if from_type.is_number() {
			return
		}
		// same
		if from_type is Pointer {
			// allow assigning &void to any pointer
			if from_type.base_type is Void {
				return
			}
			//
			if from_type.base_type.is_number() {
				return
			}
			// return c.assignment(from_type.base_type, to_type.base_type)!
			if from_type.base_type == to_type.base_type {
				return
			}
		}
	}
	if to_type.is_number() {
		// for now all all numvers to be compatible
		if from_type.is_number() {
			return
		}
		// TODO: since char is currently its own type, should this be changed?
		if from_type is Char {
			return
		}
	}
	// // dump(from_type)
	// // dump(to_type)
	return error('cannot assign `${from_type.name()}` to `${to_type.name()}`')
}

// fn (mut c Checker) implicit_type(from_type Type, to_type Type) !Type {
// 	if from_type.is_number_literal() && to_type.is_numver() {
// 		return to_type
// 	}
// }

fn (mut c Checker) block(stmts []ast.Stmt) {
}

fn (mut c Checker) apply_smartcast(sc_name_ ast.Expr, sc_type Type) {
	sc_name := c.unwrap_ident(sc_name_)
	if sc_name is ast.Ident {
		// println('added smartcast for ${sc_name.name} to ${sc_type.name()}')
		c.scope.insert(sc_name.name, sc_type)
	} else if sc_name is ast.SelectorExpr {
		// field := c.selector_expr(sc_name)
		// if sc_name.lhs is ast.Ident {
		// 	field := c.find_field_or_method()
		// 	c.scope.insert(sc_name.lhs.name, SmartCastSelector{origin: field, field: sc_name.rhs.name, cast_type: sc_type})
		// }
		// c.log('@@ selector smartcast: ${sc_name.name()} - ${field.type_name()}')
		// println('added smartcast for ${sc_name.name()} to ${sc_type.name()}')
		c.scope.field_smartcasts[sc_name.name()] = sc_type
	}
}

fn (mut c Checker) apply_smartcasts(sc_names []ast.Expr, sc_types []Type) {
	for i, sc_name in sc_names {
		c.apply_smartcast(sc_name, sc_types[i])
	}
}

fn (mut c Checker) extract_smartcasts(expr ast.Expr) ([]ast.Expr, []Type) {
	mut names := []ast.Expr{}
	mut types := []Type{}
	expr_u := c.unwrap_expr(expr)
	if expr_u is ast.InfixExpr {
		if expr_u.op == .key_is {
			// eprintln('adding smartcast')
			names << expr_u.lhs
			types << c.expr(c.unwrap_expr(expr_u.rhs))
			// typ := c.expr(expr_u.rhs)
			// types << typ
		}
		// TODO: is this just needed for && ?
		else {
			lhs_names, lhs_types := c.extract_smartcasts(c.unwrap_expr(expr_u.lhs))
			rhs_names, rhs_types := c.extract_smartcasts(c.unwrap_expr(expr_u.rhs))
			names << lhs_names
			types << lhs_types
			names << rhs_names
			types << rhs_types
		}
	}
	// else if cond is ast.ParenExpr {
	// 	names2, types2 := c.extract_smartcasts(cond.expr)
	// 	names << names2
	// 	types << types2
	// }
	return names, types
}

fn (mut c Checker) fn_decl(decl ast.FnDecl) {
	// c.log('ast.FnDecl: $decl.name: $c.file.name')
	// c.log('return type:')
	// c.expr(decl.typ.return_type)
	mut prev_scope := c.scope
	c.open_scope()
	// mut fn_scope := c.scope
	// if decl.typ.generic_params.len > 0 {
	// 	eprintln('## GENERIC FN DECL: $decl.name')
	// }
	mut typ := Type(c.fn_type(decl.typ, FnTypeAttribute.from_ast_attributes(decl.attributes)))
	obj := Fn{
		name: decl.name
		typ:  typ
	}
	// TODO:
	if decl.is_method {
		// mut is_mut := false
		// receiver_type := if decl.receiver.typ is ast.ModifierExpr {
		// 	c.log('MUT RECEIVER')
		// 	is_mut = decl.receiver.typ.kind == .key_mut
		// 	if is_mut { Type(Pointer{base_type: c.expr(decl.receiver.typ.expr)}) }
		// 	else { c.expr(decl.receiver.typ) }
		// } else {
		// 	c.expr(decl.receiver.typ)
		// }
		mut receiver_type := c.expr(decl.receiver.typ)
		if decl.receiver.is_mut {
			if mut receiver_type is Pointer {
				c.error_with_pos('use `mut Type` not `mut &Type`. TODO: proper error message',
					decl.receiver.pos)
			}
			receiver_type = Pointer{
				base_type: receiver_type
			}
		}

		c.scope.insert(decl.receiver.name, receiver_type)
		// TODO: interface methods
		receiver_base_type := receiver_type.base_type()
		method_owner_type := if receiver_type is Pointer {
			receiver_base_type
		} else {
			receiver_type
		}
		// type_name := if base_type is Interface { receiver_type.name() } else { base_type.name() }
		// c.env.methods[type_name] << &obj
		// c.env.methods[receiver_type.base_type().name()] << &obj
		unsafe { c.env.methods[method_owner_type.name()] << &obj }
		c.log('registering method: ${decl.name} for ${receiver_type.name()} - ${method_owner_type.name()} - ${receiver_base_type.name()}')
		c.scope.insert(decl.receiver.name, c.expr(decl.receiver.typ))
	} else {
		if decl.language == .c {
			c.c_scope.insert(decl.name, obj)
		} else {
			prev_scope.insert(decl.name, obj)
		}
	}
	fn_decl := decl
	deferred_kind := if decl.typ.generic_params.len > 0 {
		DeferredKind.fn_decl_generic
	} else {
		DeferredKind.fn_decl
	}
	// if decl.typ.generic_params.len == 0 {
	c.later(fn [mut c, fn_decl, typ] () {
		// if fn_decl.typ.generic_params.len > 0 {
		// 	panic('GENERIC FN')
		// }
		// eprintln('@@ FnDecl: $fn_decl.name - $fn_decl.typ.generic_params.len')
		if typ is FnType {
			if fn_decl.typ.generic_params.len > 0 {
				// eprintln('## DEFERRED GENERIC FN: $fn_decl.name - $typ.generic_params.len')
				// eprintln('FnType:')
				// eprintln(typ)
				generic_types := c.env.generic_types[fn_decl.name] or {
					c.error_with_pos('missing generic type information for ${fn_decl.name}',
						fn_decl.pos)
				}
				c.env.cur_generic_types << generic_types
				// c.env.cur_generic_types << typ.generic_types
				// // dump(generic_types)
			}
			// if typ.generic_params.len == 0 || (typ.generic_params.len > 0 && typ.generic_types.len > 0) {
			if typ.generic_params.len == 0
				|| (typ.generic_params.len > 0 && c.env.cur_generic_types.len > 0) {
				expected_type := c.expected_type
				c.expected_type = typ.return_type
				c.stmt_list(fn_decl.stmts)
				c.expected_type = expected_type
			}
			c.env.cur_generic_types = []
		}
	}, deferred_kind)
	// }
	c.close_scope()
}

fn (mut c Checker) if_expr(expr ast.IfExpr) Type {
	c.open_scope()
	// BIG MESS peanut head! Fix this :)
	// TODO: this probably is not the best way to do this
	// need to think about this some more. also should this only
	// work for the top level? how complicated can this get? eiip
	// NOTE: in the current compiler there are smartcasts and then there
	// are explicit auto casts. I have inplemeted this just using smartcasts
	// for now, however I will come back to this, and work out what is most appropriate
	mut cond_type := Type(void_)
	mut is_inline_smartcast := false
	cond := c.unwrap_expr(expr.cond)
	if cond is ast.InfixExpr {
		cond_type = Type(bool_)
		mut left_node := c.unwrap_expr(cond.lhs)
		for mut left_node is ast.InfixExpr {
			left_node_rhs := c.unwrap_expr(left_node.rhs)
			if left_node.op == .and && left_node_rhs is ast.InfixExpr {
				if left_node_rhs.op == .key_is {
					is_inline_smartcast = true
					// c.expr(left_node.rhs)
					sc_names, sc_types := c.extract_smartcasts(left_node.rhs)
					c.apply_smartcasts(sc_names, sc_types)
					// c.expr(left_node.lhs)
				}
			}
			if left_node.op == .key_is {
				is_inline_smartcast = true
				// c.expr(left_node.lhs)
				sc_names, sc_types := c.extract_smartcasts(left_node)
				c.apply_smartcasts(sc_names, sc_types)
				// c.expr(left_node.rhs)
				break
			} else if left_node.op == .and {
				left_node = c.unwrap_expr(left_node.lhs)
			} else {
				break
			}
		}
		right_node := c.unwrap_expr(cond.rhs)
		if right_node is ast.InfixExpr && right_node.op == .key_is {
			is_inline_smartcast = true
			// c.expr(right_node.lhs)
			sc_names, sc_types := c.extract_smartcasts(right_node)
			c.apply_smartcasts(sc_names, sc_types)
			// c.expr(right_node.rhs)
		}
	}
	if !is_inline_smartcast {
		// println('## not is_inline_smartcast')
		cond_type = c.expr(expr.cond)
		sc_names, sc_types := c.extract_smartcasts(expr.cond)
		c.apply_smartcasts(sc_names, sc_types)
	}
	_ = cond_type
	c.stmt_list(expr.stmts)
	mut typ := Type(void_)
	if expr.stmts.len > 0 {
		last_stmt := expr.stmts.last()
		if last_stmt is ast.ExprStmt {
			typ = c.expr(last_stmt.expr)
		}
	}
	c.close_scope()
	// return typ
	else_type := if expr.else_expr !is ast.EmptyExpr { c.expr(expr.else_expr) } else { typ }
	return else_type
	// TODO: check all branches types match
}

fn (mut c Checker) match_expr(expr ast.MatchExpr, used_as_expr bool) Type {
	expr_type := c.expr(expr.expr)
	expected_type := c.expected_type
	if expr_type is Enum {
		c.expected_type = expr_type
	}
	mut last_stmt_type := Type(void_)
	for i, branch in expr.branches {
		c.open_scope()
		for cond in branch.cond {
			expr_unwrapped := c.unwrap_ident(expr.expr)
			cond_type := c.expr(cond)
			if cond in [ast.Ident, ast.SelectorExpr] {
				// `.value` enum value (without type)
				if cond is ast.SelectorExpr && cond.lhs is ast.EmptyExpr {
					continue
				}
				c.apply_smartcast(expr_unwrapped, cond_type)
			}
		}
		c.stmt_list(branch.stmts)
		// mut is_noreturn := false
		if used_as_expr && branch.stmts.len > 0 {
			last_stmt := branch.stmts.last()
			if last_stmt is ast.ExprStmt {
				t := c.expr(last_stmt.expr)
				// TODO: non else branch can only return void if its a noreturn
				// if last_stmt is ast.ExprStmt {
				// 	if last_stmt.expr is ast.CallExpr {
				// 		lhs := c.expr(last_stmt.expr.lhs)
				// 		if lhs is FnType {
				// 			is_noreturn = lhs.attributes.has(.noreturn)
				// 		}
				// 	}
				// }
				// TODO: fix last branch / void expr
				// actually make sure its an else branch
				if _ := expected_type {
					if i > 0 && (i < expr.branches.len - 1 && t !is Void) {
						// if t != last_stmt_type {
						if !t.is_compatible_with(last_stmt_type) {
							// TODO: better error message
							c.error_with_pos('${i} all branches must return same type (exp ${last_stmt_type.name()} got ${t.name()})',
								last_stmt.expr.pos())
						}
					}
					last_stmt_type = t
				}
			}
		}
		c.close_scope()
	}
	c.expected_type = expected_type
	return last_stmt_type
}

// TODO: unwrap ModifierExpr etc
// fn (mut c Checker) argument() ast.Expr {}

fn (mut c Checker) resolve_generic_arg_or_index_expr(expr ast.GenericArgOrIndexExpr) ast.Expr {
	lhs_type := c.expr(expr.lhs)
	// expr_type := c.expr(expr.expr)
	if lhs_type is FnType {
		return ast.GenericArgs{
			lhs:  expr.lhs
			args: [expr.expr]
		}
	} else {
		// c.unwrap_lhs_expr(ast.IndexExpr{lhs: expr.lhs, expr: expr.expr})
		return ast.IndexExpr{
			lhs:  expr.lhs
			expr: expr.expr
		}
	}
}

fn (mut c Checker) resolve_call_or_cast_expr(expr ast.CallOrCastExpr) ast.Expr {
	lhs_type := c.expr(expr.lhs)
	// expr_type := c.expr(expr.expr)
	if lhs_type is FnType {
		return ast.CallExpr{
			lhs:  expr.lhs
			args: [expr.expr]
			pos:  expr.pos
		}
	} else {
		// c.log(expr)
		return ast.CastExpr{
			typ:  expr.lhs
			expr: expr.expr
			pos:  expr.pos
		}
	}
}

// TODO:
fn (mut c Checker) resolve_expr(expr ast.Expr) ast.Expr {
	match expr {
		ast.CallOrCastExpr {
			return c.resolve_call_or_cast_expr(expr)
		}
		ast.GenericArgOrIndexExpr {
			return c.resolve_generic_arg_or_index_expr(expr)
		}
		else {
			return expr
		}
	}
}

// TODO:
fn (mut c Checker) unwrap_ident(expr ast.Expr) ast.Expr {
	match expr {
		ast.ModifierExpr {
			return expr.expr
		}
		else {
			return expr
		}
	}
}

// TODO:
fn (mut c Checker) unwrap_expr(expr ast.Expr) ast.Expr {
	match expr {
		// ast.Ident, ast.IndexExpr, ast.SelectorExpr {
		// 	return expr
		// }
		ast.CallOrCastExpr {
			// return c.unwrap_expr(c.resolve_expr(expr.expr))
			return c.unwrap_expr(c.resolve_call_or_cast_expr(expr))
		}
		ast.ModifierExpr {
			// TODO: actually do something with modifiers :)
			return c.unwrap_expr(expr.expr)
		}
		ast.ParenExpr {
			return c.unwrap_expr(expr.expr)
		}
		ast.PrefixExpr {
			return c.unwrap_expr(expr.expr)
		}
		else {
			// TODO: expr.pos()
			// c.error_with_pos('unwrap_expr: missing impl for expr ${expr.type_name()}', 2)
			// no unwrap needed
			return expr
		}
	}
}

// TODO:
fn (mut c Checker) unwrap_lhs_expr(expr ast.Expr) ast.Expr {
	match expr {
		ast.ModifierExpr {
			// return expr.expr
			return c.unwrap_lhs_expr(expr.expr)
		}
		ast.GenericArgs {
			// lhs_type := c.expr(expr.lhs)
			mut generic_arg_types := []Type{}
			for arg in expr.args {
				generic_arg_types << c.expr(arg)
			}
			return expr.lhs
		}
		ast.GenericArgOrIndexExpr {
			return c.unwrap_lhs_expr(c.resolve_generic_arg_or_index_expr(expr))
		}
		else {
			return expr
		}
	}
}

// TODO: clean up & add any missing cases & missing recursion
fn (mut c Checker) infer_generic_type(param_type Type, arg_type Type, mut type_map map[string]Type) ! {
	map_type := fn [mut type_map] (name string, typ Type) ! {
		if existing := type_map[name] {
			// TODO: might need custom eq methods
			if existing != typ && typ !is NamedType {
				return error('${name} was previously used as ${existing.name()}, got ${typ.name()}')
			}
		}
		type_map[name] = typ
	}
	match param_type {
		Array {
			if param_type.elem_type is NamedType && arg_type is Array {
				map_type(param_type.elem_type, arg_type.elem_type)!
			}
		}
		Channel {
			if pt_et := param_type.elem_type {
				if pt_et is NamedType && arg_type is Channel {
					if at_et := arg_type.elem_type {
						map_type(pt_et, at_et)!
					}
				}
			}
		}
		FnType {
			if arg_type is FnType {
				for i, param_param in param_type.params {
					arg_param := arg_type.params[i]
					c.infer_generic_type(param_param.typ, arg_param.typ, mut type_map)!
				}
				if param_rt := param_type.return_type {
					if arg_rt := arg_type.return_type {
						c.infer_generic_type(param_rt, arg_rt, mut type_map)!
					}
				}
			}
		}
		Map {
			if arg_type is Map {
				if param_type.key_type is NamedType {
					map_type(param_type.key_type, arg_type.key_type)!
				}
				if param_type.value_type is NamedType {
					map_type(param_type.value_type, arg_type.value_type)!
				}
			}
		}
		NamedType {
			map_type(param_type, arg_type)!
		}
		Struct {}
		OptionType {
			if param_type.base_type is NamedType && arg_type is OptionType {
				map_type(param_type.base_type, arg_type.base_type)!
			}
		}
		ResultType {
			if param_type.base_type is NamedType && arg_type is ResultType {
				map_type(param_type.base_type, arg_type.base_type)!
			}
		}
		Thread {
			if pt_et := param_type.elem_type {
				if pt_et is NamedType && arg_type is Thread {
					if at_et := arg_type.elem_type {
						map_type(pt_et, at_et)!
					}
				}
			}
		}
		else {
			println('missing ${param_type.type_name()}')
		}
	}
}

fn (mut c Checker) call_expr(expr ast.CallExpr) Type {
	lhs_expr := c.resolve_expr(expr.lhs)
	// TODO: remove, see comment at field decl
	expecting_method := c.expecting_method
	if lhs_expr is ast.SelectorExpr {
		c.expecting_method = true
	}
	mut fn_ := c.expr(lhs_expr)
	// fn_ := c.expr(expr.lhs)
	c.expecting_method = expecting_method
	// c.log('call expr: $fn_.type_name() - $fn_.name() - ${lhs_expr.type_name()}')

	// if expr.lhs is PrefixExpr {
	// 	xpr.op
	// }
	// if expr.lhs is Postfix
	// TODO: time.StopWatch has a fields and methods with the same name
	// and field was being returned instead of the methods. we could set a precedence
	// however what if the field is a fn type? (i guess one or the other could still habve priority)
	// TODO: talk to alex and spy about this
	if expr.lhs is ast.SelectorExpr {
		// POO POO
		// if expr.lhs.lhs is ast.Ident {
		if fn_ is Primitive && expr.lhs.rhs.name == 'start' {
			// c.log('#### ${expr.lhs.rhs.name}')
			// what is going on here?
			fn_ = FnType{}
		} else if fn_ is Primitive && expr.lhs.rhs.name == 'elapsed' {
			// c.log('#### ${expr.lhs.rhs.name}')
			fn_ = FnType{
				return_type: Type(Alias{
					name:      'Duration'
					base_type: i64_
				})
			}
		}
		// }
	}
	// 	else if lhs_expr is ast.GenericArgs {
	// 	c.log('GENERIC CALL: ')

	// }
	if mut fn_ is Alias {
		fn_ = fn_.base_type
	}
	if mut fn_ is FnType {
		if fn_.generic_params.len > 0 {
			// file := c.file_set.file(expr.pos)
			// pos := file.position(expr.pos)
			// eprintln('GENERIC CALL: $expr.lhs.name() - $expr.pos - $file.name:$pos.line')
			mut generic_type_map := map[string]Type{}
			// generic types provided `[int, string]`
			if lhs_expr is ast.GenericArgs {
				// panic('GOT GENERIC CALL')
				mut generic_types := []Type{}
				for i, ga in lhs_expr.args {
					generic_param := fn_.generic_params[i]
					generic_type := c.expr(ga)
					generic_types << generic_type
					generic_type_map[generic_param] = generic_type
				}
				// eprintln('GENERIC TYPES: $expr.lhs.name()')
				// dump(generic_types)
			}
			// infer generic types from args
			else {
				// TODO: move above (to be done globally)
				// once error is fixed
				mut arg_types := []Type{}
				// eprintln('INFERRED GENERIC TYPES: $expr.lhs.name()')
				for i, arg in expr.args {
					param := fn_.params[i]
					arg_type := c.expr(arg).typed_default()
					arg_types << arg_type
					// eprintln('call arg: ${param.typ.type_name()} - ${arg_type.type_name()}')

					// println('infcerring call: ')
					c.infer_generic_type(param.typ, arg_type, mut generic_type_map) or {
						c.error_with_pos(err.msg(), expr.pos)
					}
					// if param.typ !is NamedType {
					// 	eprintln('Generic argument: $param.typ.name() - $arg_type.name()')
					// 	eprintln('should be NamedType but got $param.typ.name()')
					// }
				}
			}
			// eprintln('## GENERIC TYPE MAP: $expr.lhs.name()')
			// eprintln('========================')
			// for k,v in generic_type_map {
			// 	eprintln(' * $k -> $v')
			// }
			// eprintln('========================')
			// if expr.lhs is ast.Ident {
			// 	if expr.lhs.name == 'generic_fn_d' {
			// 		panic('.')
			// 	}
			// }
			// dump(generic_type_map)
			if generic_type_map.len > 0 {
				fn_.generic_types << generic_type_map
				c.env.generic_types[lhs_expr.name()] << generic_type_map
			}
		} else if lhs_expr is ast.GenericArgs {
			c.error_with_pos('cannot call non generic function with generic argument list',
				expr.pos)
		}

		// TODO: is this best place for this?
		// why is it not being used any more? check if we are somehow skipping its uses
		// need to find a better way to do this, this only happens because there are
		// compiler magic, call_expr & selector expr both end up needing information which
		// the other one has
		if lhs_expr is ast.SelectorExpr {
			if lhs_expr.rhs.name in ['filter', 'map'] {
				if rt := fn_.return_type {
					// c.log('#### it variable inserted')
					// fn_.scope.insert('it', rt)
					c.scope.insert('it', rt.value_type())
				}
				if lhs_expr.rhs.name == 'map' {
					rt := c.expr(expr.args[0])
					c.log('map: ${rt.name()}')
					fn_ = FnType{
						...fn_
						return_type: Array{
							elem_type: rt
						}
					}
				}
			}
		}
		// TODO/FIXME: is FnType.return_type goin to stay optional
		// or not and just use void in case of returning nothing
		if return_type := fn_.return_type {
			// if expr.lhs is ast.SelectorExpr {
			// 	if expr.lhs.lhs is ast.Ident {
			// 		if expr.lhs.lhs.name == 'g_timers' && expr.lhs.rhs.name == 'show' {
			// 			c.log('## 2 ${expr.lhs.lhs.name}: fn_.type()')
			// 		}
			// 	}
			// }
			c.log('returning: ${return_type.name()}')
			return return_type
		}
		return void_
	}

	for arg in expr.args {
		c.expr(arg)
	}

	c.error_with_pos('call on non fn: ${fn_.type_name()}', expr.pos)
}

// TODO:
// fn (mut c Checker) fn_arguments() {}

// fn (mut c Checker) declare(scope &Scope, name string) {

// }

fn (mut c Checker) fn_type(fn_type ast.FnType, attributes FnTypeAttribute) FnType {
	// c.open_scope()
	// array of T types
	mut generic_params := []string{}
	for generic_param in fn_type.generic_params {
		if generic_param is ast.Ident {
			// generic_params << NamedType(generic_param.name)
			generic_params << generic_param.name
		} else {
			// TODO: correct position
			c.error_with_pos('expecting identifier', 0)
		}
	}
	mut params := []Parameter{}
	mut is_variadic := false
	// TODO: proper
	if generic_params.len > 0 {
		c.generic_params = generic_params
	}
	for i, param in fn_type.params {
		// of param type is generic, replace with NamedType
		// mut param_type := if param.typ is ast.Ident {
		// 	if param.typ.name in generic_params {
		// 		Type(NamedType(param.typ.name))
		// 	} else {
		// 		c.expr(param.typ)
		// 	}
		// } else { c.expr(param.typ) }
		mut param_type := c.expr(param.typ)
		if param.typ is ast.PrefixExpr {
			if param.typ.op == .ellipsis {
				if i < fn_type.params.len - 1 {
					c.error_with_pos('variadic must be last parameter.', param.pos)
				}
				param_type = Array{
					elem_type: param_type
				}
				is_variadic = true
			}
		}
		// if param.is_mut && param_type !is Pointer {
		if param.is_mut {
			param_type = Pointer{
				base_type: param_type
			}
		}
		params << Parameter{
			name: param.name
			// typ: c.expr(param.typ)
			typ:    param_type
			is_mut: param.is_mut
		}
		c.scope.insert(param.name, param_type)
	}
	// if return type is generic replace with NamedType
	// mut return_type := if fn_type.return_type is ast.Ident {
	// 	if fn_type.return_type.name in generic_params {
	// 		Type(NamedType(fn_type.return_type.name))
	// 	} else {
	// 		c.expr(fn_type.return_type)
	// 	}
	// } else { c.expr(fn_type.return_type) }

	mut typ := FnType{
		generic_params: generic_params
		params:         params
		// return_type: return_type
		return_type: c.expr(fn_type.return_type)
		is_variadic: is_variadic
		attributes:  attributes
		// scope: c.scope
	}
	if generic_params.len > 0 {
		c.generic_params = []
	}
	// c.close_scope()
	return typ
}

fn (mut c Checker) ident(ident ast.Ident) Object {
	obj := c.scope.lookup_parent(ident.name, ident.pos) or {
		if ident.name == '_' {
			c.error_with_pos('cannot use _ as value or type', ident.pos)
			// TODO: compiler error
			return Type(void_)
		} else {
			// TODO/FIXME: generic param - hack
			// if ident.name.len == 1 && ident.name[0].is_capital() {
			// 	return Type(string_)
			// }
		}

		if ident.name in c.generic_params {
			return Type(NamedType(ident.name))
		}

		for generic_types in c.env.cur_generic_types {
			if generic_type := generic_types[ident.name] {
				// eprintln('## replaced generic type ${ident.name} with ${generic_type.name()}')
				return generic_type
			}
		}

		// TODO: proper
		if ident.name in [
			'@VROOT',
			'@VMODROOT',
			'@VEXEROOT',
			'@VCURRENTHASH',
			'@FN',
			'@METHOD',
			'@MOD',
			'@STRUCT',
			'@VEXE',
			'@FILE',
			'@LINE',
			'@COLUMN',
			'@VHASH',
			'@VMOD_FILE',
			'@FILE_LINE',
		] {
			return Type(string_)
		}

		// TODO: proper
		if ident.name in ['compile_error', 'compile_warn'] {
			return Type(FnType{
				return_type: void_
			})
		}

		// c.scope.print(false)
		// c.scope.print(true)
		c.error_with_pos('unknown ident `${ident.name} - ${ident.pos}`', ident.pos)
	}
	c.log('ident: ${ident.name} - ${obj.typ().name()}')
	return obj
}

fn (mut c Checker) selector_expr(expr ast.SelectorExpr) Type {
	c.log('## selector_expr')
	// file := c.file_set.file(expr.pos)
	// pos := file.position(expr.pos)
	// c.log('${expr.name()} - $expr.rhs.name')
	// println('selector_expr: ${expr.rhs.name} - ${file.name}:${pos.line} - ${pos.column}')

	// TODO: check todo in scope.v
	if expr.lhs in [ast.Ident, ast.SelectorExpr] {
		// println('looking for smartcast $expr.name()')
		if cast_type := c.scope.lookup_field_smartcast(expr.name()) {
			// println('## 1 found smartcast for ${expr.name()} - ${cast_type.type_name()} - ${cast_type.name()} - ${expr.rhs.name}')
			return cast_type
		}
		if cast_type := c.scope.lookup_field_smartcast(expr.lhs.name()) {
			// println('## 2 found smartcast for ${expr.lhs.name()} - ${cast_type.type_name()} - ${cast_type.name()} - ${expr.rhs.name}')
			return c.find_field_or_method(cast_type, expr.rhs.name) or {
				c.error_with_pos('## smartcast field lookup error ' + err.msg(), expr.pos)
			}
		}
	}

	if expr.lhs is ast.Ident {
		lhs_obj := c.ident(expr.lhs)
		// c.log('LHS.RHS: $expr.lhs.name . $expr.rhs.name - $lhs_obj.typ().name()')
		match lhs_obj {
			Const {
				return c.find_field_or_method(lhs_obj.typ, expr.rhs.name) or {
					c.error_with_pos(err.msg(), expr.pos)
				}
			}
			Global {
				return c.find_field_or_method(lhs_obj.typ, expr.rhs.name) or {
					c.error_with_pos(err.msg(), expr.pos)
				}
			}
			Module {
				// return lhs_type.scope.lookup_parent(expr.rhs.name)
				mut mod_scope := lhs_obj.scope
				rhs_obj := mod_scope.lookup_parent(expr.rhs.name, expr.rhs.pos) or {
					// TODO: proper
					if expr.lhs.name == 'C' {
						// c.log('assuming C constant: $expr.lhs.name . $expr.rhs.name')
						return int_
					}
					mod_scope.print(true)
					c.error_with_pos('missing ${expr.lhs.name}.${expr.rhs.name}', expr.pos)
				}
				return rhs_obj.typ()
			}
			Type {
				return c.find_field_or_method(lhs_obj, expr.rhs.name) or {
					c.error_with_pos(err.msg(), expr.pos)
				}
			}
			// SmartCastSelector {
			// 	c.log('@@ found smart cast selector: $expr.rhs.name')
			// 	if expr.rhs.name == lhs_obj.field {
			// 		return lhs_obj.cast_type
			// 		// return c.find_field_or_method(lhs_obj.cast_type, expr.rhs.name) or { c.error_with_pos(err.msg(), expr.pos) }
			// 	}
			// 	return c.find_field_or_method(lhs_obj.origin, expr.rhs.name) or { c.error_with_pos(err.msg(), expr.pos) }
			// }
			else {
				c.error_with_pos('unsupported ${lhs_obj.type_name()} - ${expr.rhs.name}',
					0)
			}
		}
	}
	// else {
	// 	panic('unexpected expr.lhs: $expr.lhs.type_name()')
	// }
	// TODO: this will be removed
	// unset when checking lhs, only needed for rightmost selector
	expecting_method := c.expecting_method
	c.expecting_method = false
	lhs_type := c.expr(expr.lhs)
	c.expecting_method = expecting_method
	return c.find_field_or_method(lhs_type, expr.rhs.name) or {
		c.error_with_pos(err.msg(), expr.pos)
	}
}

fn (mut c Checker) find_field_or_method(t Type, name string) !Type {
	// TODO: is this the best way to do this?
	// this whole field/method search needs to be cleaned up
	// fields and method with same name should not be allowed to begin with!
	if c.expecting_method {
		if method := c.find_method(t, name) {
			return method
		}
	}
	match t {
		Alias {
			// if field_or_method_type := c.find_field_or_method(t, name) {
			// 	return field_or_method_type
			// }
			if field_or_method_type := c.find_field_or_method(t.base_type, name) {
				return field_or_method_type
			}
		}
		Array {
			// TODO: has to be a better way
			// there is probably no reason to look these up, and just do what we do above
			mut builtin_scope := c.get_module_scope('builtin', universe)
			at := builtin_scope.lookup_parent('array', 0) or { panic('missing builtin array type') }
			at_type := at.typ()
			// c.log('ARRAY: looking for field or method $name on $t.name()')
			// // dump(t)
			if field_or_method_type := c.find_field_or_method(at_type, name) {
				c.log('found ${name}')
				if field_or_method_type is FnType {
					if name in ['clone', 'map', 'filter'] {
						// c.log('SNEAKY: $t.name()')
						// return t
						return FnType{
							...field_or_method_type
							return_type: t
						}
					}
					// TODO: proper PLEASE! :D
					else if name in ['first', 'last'] {
						return FnType{
							...field_or_method_type
							return_type: t.elem_type
						}
					}
				}
				return field_or_method_type
			}
		}
		ArrayFixed {
			if name in ['clone', 'map', 'filter'] {
				return FnType{
					return_type: t
				}
			} else if name in ['first', 'last'] {
				return FnType{
					return_type: t.elem_type
				}
			} else if name == 'len' {
				return int_
			}
		}
		Channel {
			println('channel . ${name}')
			// TODO: sync must be imported when channels are used
			mut sync_scope := c.get_module_scope('sync', universe)
			at := sync_scope.lookup_parent('Channel', 0) or { panic('missing builtin chan type') }
			at_type := at.typ()
			if field_or_method_type := c.find_field_or_method(at_type, name) {
				return field_or_method_type
			}
		}
		Enum {
			for field in t.fields {
				if field.name == name {
					return t
					// return int_
				}
			}
			// flags - compiler magic (currently)
			if name == 'has' {
				return bool_
			} else if name == 'all' {
				return int_
			} else if name in ['clear', 'set'] {
				return void_
			}
		}
		Interface {
			// c.log('looking for fields on interface ${t.name()}')
			for field in t.fields {
				if field.name == name {
					// c.log('found field ${name} for interface ${t.name()}')
					if c.expecting_method && field.typ !is FnType {
						continue
					}
					return field.typ
				}
			}
		}
		Map {
			mut builtin_scope := c.get_module_scope('builtin', universe)
			at := builtin_scope.lookup_parent('map', 0) or { panic('missing builtin map type') }
			at_type := at.typ()
			// c.log('MAP: looking for field or method $name on $t.name()')
			if field_or_method_type := c.find_field_or_method(at_type, name) {
				if name == 'keys' {
					if field_or_method_type is FnType {
						return FnType{
							...field_or_method_type
							return_type: Array{
								elem_type: t.key_type
							}
						}
					}
				}
				return field_or_method_type
			}
		}
		Pointer {
			return c.find_field_or_method(t.base_type, name)!
		}
		ResultType {
			return c.find_field_or_method(t.base_type, name)!
		}
		// TODO:
		// currently should be handled at bottom by find_method call
		// Primitive {
		// 	c.log('#### method on ptimitive: ${name} on ${t.name()}')
		// 	// if name in ['free'] { return FnType{return_type: void_} }
		// }
		String {
			if name == 'len' {
				return int_
			}
			if o := c.scope.lookup_parent('string', 0) {
				// c.log(o)
				return c.find_field_or_method(o.typ(), name)
			}
		}
		Struct {
			for field in t.fields {
				// c.log('comparing field $field.name with $name for $t.name')
				if field.name == name {
					c.log('found field ${name} for ${t.name}: ${field.typ.name()} (${field.typ.type_name()})')
					return field.typ
				}
			}
			for embedded_type in t.embedded {
				if embedded_field_or_method_type := c.find_field_or_method(embedded_type,
					name)
				{
					return embedded_field_or_method_type
				}
			}
		}
		SumType {
			// if !c.expecting_method {
			mut prev_type := Type(nil_)
			for i, variant in t.variants {
				if variant is Struct {
					mut has_field := false
					for field in variant.fields {
						if field.name == name {
							has_field = true
							if i > 0 && field.typ != prev_type {
								return error('field ${name} must have the same type for all variants of ${t.name()}')
							}
							prev_type = field.typ
							break
						}
					}
					if !has_field {
						return error('not all variants of ${t.name()} have the field ${name}')
					}
				}
			}
			return prev_type
			// }
		}
		Thread {
			// TODO:
			if name == 'wait' {
				c.open_scope()
				fn_type := FnType{
					return_type: t.elem_type or { t }
				}
				c.close_scope()
				return fn_type
			}
		}
		else {
			c.log('find_field_or_method: unhandled ${t.name()}')
		}
	}
	// else if t is FnType {
	// 			c.log('FnType: $t.name')
	// }

	// // dump(t)
	// c.log('returning none for $t.type_name() - $name')
	if method := c.find_method(t, name) {
		return method
	}
	if t is Struct {
		// dump(t.fields)
	}
	base_type := t.base_type()
	return error('cannot find field or method: `${name}` for type ${t.type_name()} - ${t.name()} - (base: ${base_type.type_name()})')
}

fn (mut c Checker) find_method(t Type, name string) !Type {
	c.log('looking for method `${name}` on type `${t.name()}`')
	// TODO: do we need to look for methods on the non base type first?
	// I think we will for aliases, probably not other types. we might
	// need to differentiate then for base_type / base_type in this case
	mut base_type := t.base_type()
	// TODO: interface methods
	// if base_type is Interface { base_type = t }
	base_type_name := if t is Pointer { base_type.name() } else { t.name() }
	// c.log('base_type_name: $base_type_name - $t.type_name() - $base_type.type_name()')
	if methods := c.env.methods[base_type_name] {
		for method in methods {
			// c.log('# $method.name - $name')
			if method.name == name {
				c.log('found method ${name} for ${t.name()}')
				return method.typ
			}
		}
	}
	return error('cannot find method `${name}` for `${t.name()}`')
}

// fn (mut c &Checker) open_scope(node ast.Node, comment string) {
// 	scope := new_scope(c.scope, node.Pos(), node.End(), comment)
// 	c.record_scope(node, scope)
// 	c.scope = scope
// }
fn (mut c Checker) open_scope() {
	c.scope = new_scope(c.scope)
}

fn (mut c Checker) close_scope() {
	c.scope = c.scope.parent
}

// so we can customize the error message used by warn & error
fn (mut c Checker) error_message(msg string, kind errors.Kind, pos token.Position, file &token.File) {
	errors.error(msg, errors.details(file, pos, 2), kind, pos)
}

// fn (mut c Checker) warn(msg string) {
// 	c.error_message(msg, .warning, p.current_position())
// }

// [noreturn]
// fn (mut c Checker) error(msg string) {
// 	c.error_with_position(msg, p.current_position())
// }

@[noreturn]
fn (mut c Checker) error_with_pos(msg string, pos token.Pos) {
	file := c.file_set.file(pos)
	c.error_with_position(msg, file.position(pos), file)
}

@[noreturn]
fn (mut c Checker) error_with_position(msg string, pos token.Position, file &token.File) {
	c.error_message(msg, .error, pos, file)
	exit(1)
}

@[if verbose ?]
fn (c Checker) log[T](s T) {
	println(s)
}

@[if verbose ?]
fn (c Checker) elog[T](s T) {
	eprintln(s)
}
