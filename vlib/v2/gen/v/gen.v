// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v

import v2.ast
import v2.pref
import strings
import time

// tabs = build_tabs(16)
const tabs = build_tabs(24)

struct Gen {
	pref &pref.Preferences
mut:
	file       ast.File
	out        strings.Builder
	indent     int
	on_newline bool
	in_init    bool
}

fn build_tabs(tabs_len int) []string {
	mut tabs_arr := []string{len: tabs_len, cap: tabs_len}
	mut indent := ''
	for i in 1 .. tabs_len {
		indent += '\t'
		tabs_arr[i] = indent
	}
	return tabs_arr
}

pub fn new_gen(prefs &pref.Preferences) &Gen {
	unsafe {
		return &Gen{
			pref:   prefs
			out:    strings.new_builder(1000)
			indent: -1
		}
	}
}

pub fn (mut g Gen) reset() {
	g.out.go_back_to(0)
	g.indent = -1
	g.on_newline = false
}

pub fn (mut g Gen) gen(file ast.File) {
	// clear in case we are reusing gen instance
	if g.out.len > 1 {
		g.reset()
	}
	if !g.pref.verbose {
		unsafe {
			goto start_no_time
		}
	}
	mut sw := time.new_stopwatch()
	start_no_time:
	g.file = file
	g.stmt_list(g.file.stmts)
	if g.pref.verbose {
		gen_time := sw.elapsed()
		println('gen (v) ${file.name}: ${gen_time.milliseconds()}ms (${gen_time.microseconds()}µs)')
	}
}

fn (mut g Gen) stmt_list(stmts []ast.Stmt) {
	for stmt in stmts {
		g.indent++
		g.stmt(stmt)
		g.indent--
	}
}

fn (mut g Gen) stmt(stmt ast.Stmt) {
	match stmt {
		ast.AsmStmt {
			g.writeln('asm ${stmt.arch} {')
			g.writeln('}')
		}
		ast.AssertStmt {
			g.write('assert ')
			g.expr(stmt.expr)
			if stmt.extra !is ast.EmptyExpr {
				g.write(', ')
				g.expr(stmt.extra)
			}
			g.writeln('')
		}
		ast.AssignStmt {
			g.expr_list(stmt.lhs, ', ')
			g.write(' ${stmt.op} ')
			g.expr_list(stmt.rhs, ', ')
			if !g.in_init {
				g.writeln('')
			}
		}
		[]ast.Attribute {
			g.attributes(stmt)
		}
		ast.BlockStmt {
			g.writeln('{')
			g.stmt_list(stmt.stmts)
			g.writeln('}')
		}
		ast.ConstDecl {
			for field in stmt.fields {
				if stmt.is_public {
					g.write('pub ')
				}
				g.writeln('const ')
				g.write(field.name)
				g.write(' = ')
				g.expr(field.value)
				g.writeln('')
			}
		}
		ast.ComptimeStmt {
			g.write('$')
			g.stmt(stmt.stmt)
		}
		ast.DeferStmt {
			g.write('defer {')
			g.stmt_list(stmt.stmts)
			g.writeln('}')
		}
		ast.Directive {
			g.write('#')
			g.write(stmt.name)
			g.write(' ')
			g.writeln(stmt.value)
		}
		ast.EmptyStmt {}
		ast.EnumDecl {
			if stmt.attributes.len > 0 {
				g.attributes(stmt.attributes)
				g.writeln('')
			}
			if stmt.is_public {
				g.write('pub ')
			}
			g.write('enum ')
			g.write(stmt.name)
			if stmt.as_type !is ast.EmptyExpr {
				g.write(' as ')
				g.expr(stmt.as_type)
			}
			g.writeln(' {')
			g.indent++
			for field in stmt.fields {
				g.write('${field.name}')
				g.expr(field.typ)
				if field.value !is ast.EmptyExpr {
					g.write(' = ')
					g.expr(field.value)
				}
				if field.attributes.len > 0 {
					g.write(' ')
					g.attributes(field.attributes)
				}
				g.writeln('')
			}
			g.indent--
			g.writeln('}')
		}
		ast.ExprStmt {
			g.expr(stmt.expr)
			if !g.in_init {
				g.writeln('')
			}
		}
		ast.FlowControlStmt {
			if stmt.label.len > 0 {
				g.write(stmt.op.str())
				g.write(' ')
				g.writeln(stmt.label)
			} else {
				g.writeln(stmt.op.str())
			}
		}
		ast.FnDecl {
			if stmt.attributes.len > 0 {
				g.attributes(stmt.attributes)
				g.writeln('')
			}
			if stmt.is_public {
				g.write('pub ')
			}
			g.write('fn ')
			if stmt.is_method {
				if !stmt.is_static {
					g.write('(')
					g.write(stmt.receiver.name)
					g.write(' ')
					g.expr(stmt.receiver.typ)
					g.write(') ')
				} else {
					g.expr(stmt.receiver.typ)
					g.write('.')
				}
			}
			if stmt.language != .v {
				g.write(stmt.language.str())
				g.write('.')
			}
			g.write(stmt.name)
			g.fn_type(stmt.typ)
			// C fn definition |
			// v fns with compiler implementations eg. `pub fn (a array) filter(predicate fn (voidptr) bool) array`
			// NOTE: can we use generics for these fns, also make sure we parser error for normal fns without a body
			// TODO: is it the correct way to handle those cases (the fn definitions, not this code)?
			// if stmt.language == .c && stmt.stmts.len == 0 {
			if stmt.stmts.len == 0 {
				g.writeln('')
			}
			// normal v function
			else {
				g.writeln(' {')
				g.stmt_list(stmt.stmts)
				g.writeln('}')
			}
		}
		ast.ForStmt {
			g.write('for ')
			in_init := g.in_init
			g.in_init = true
			mut is_plain := true
			mut has_init := stmt.init !is ast.EmptyStmt
			if has_init && stmt.init is ast.ForInStmt {
				is_plain = false
				g.write('/* ForIn */')
				g.stmt(stmt.init)
			} else {
				mut has_post := stmt.post !is ast.EmptyStmt
				if has_init {
					is_plain = false
					g.stmt(stmt.init)
					g.write('; ')
				}
				if stmt.cond !is ast.EmptyExpr {
					is_plain = false
					g.write('/* cond: ${stmt.cond.type_name()} */')
					g.expr(stmt.cond)
				}
				if has_init || has_post {
					g.write('; ')
				}
				if has_post {
					is_plain = false
					g.stmt(stmt.post)
				}
			}
			g.in_init = in_init
			g.writeln(if is_plain { '{' } else { ' {' })
			g.stmt_list(stmt.stmts)
			g.writeln('}')
		}
		ast.ForInStmt {
			// if stmt.key.len > 0 {
			// 	g.write(stmt.key)
			// 	g.write(', ')
			// }
			// if stmt.value_is_mut {
			// 	g.write('mut ')
			// }
			// g.write(stmt.value)
			if stmt.key !is ast.EmptyExpr {
				g.expr(stmt.key)
				g.write(', ')
			}
			g.expr(stmt.value)
			g.write(' in ')
			g.expr(stmt.expr)
		}
		ast.GlobalDecl {
			g.writeln('__global (')
			g.indent++
			for field in stmt.fields {
				// TODO
				g.write(field.name)
				// if field.value != none {
				if field.value !is ast.EmptyExpr {
					g.write(' = ')
					g.expr(field.value)
				} else {
					g.write(' ')
					g.expr(field.typ)
				}
				g.writeln('')
			}
			g.indent--
			g.writeln(')')
		}
		ast.InterfaceDecl {
			if stmt.is_public {
				g.write('pub ')
			}
			g.write('interface ')
			g.write(stmt.name)
			if stmt.generic_params.len > 0 {
				g.generic_list(stmt.generic_params)
			}
			g.writeln(' {')
			g.indent++
			for embed in stmt.embedded {
				g.expr(embed)
				g.writeln('')
			}
			for field in stmt.fields {
				g.write(field.name)
				g.write(' ')
				g.expr(field.typ)
				// if field.typ is ast.Type {
				// 	if field.typ is ast.FnType {
				// 		g.fn_type(field.typ)
				// 	} else {
				// 		g.write(' ')
				// 		g.expr(field.typ)
				// 	}
				// }
				// // TODO/FIXME: because p.typ() is returning ident & selector currently
				// else {
				// 	g.write(' ')
				// 	g.expr(field.typ)
				// }
				g.writeln('')
			}
			g.indent--
			g.writeln('}')
		}
		ast.ImportStmt {
			g.write('import ')
			g.write(stmt.name)
			if stmt.is_aliased {
				g.write(' as ')
				g.write(stmt.alias)
			}
			if stmt.symbols.len > 0 {
				g.write(' { ')
				g.expr_list(stmt.symbols, ', ')
				g.write(' }')
			}
			g.writeln('')
		}
		ast.LabelStmt {
			g.write(stmt.name)
			g.writeln(':')
			if stmt.stmt != ast.empty_stmt {
				g.stmt(stmt.stmt)
			}
		}
		ast.ModuleStmt {
			g.write('module ')
			g.writeln(stmt.name)
		}
		ast.ReturnStmt {
			g.write('return ')
			g.expr_list(stmt.exprs, ', ')
			g.writeln('')
		}
		ast.StructDecl {
			g.struct_decl(stmt)
		}
		ast.TypeDecl {
			g.write('type ')
			if stmt.language != .v {
				g.write(stmt.language.str())
				g.write('.')
			}
			g.write(stmt.name)
			if stmt.generic_params.len > 0 {
				g.generic_list(stmt.generic_params)
			}
			if stmt.variants.len > 0 {
				g.write(' = ')
				g.expr_list(stmt.variants, ' | ')
			} else {
				g.write(' ')
				g.expr(stmt.base_type)
			}
			g.writeln('')
		}
	}
	// g.writeln('')
}

fn (mut g Gen) expr(expr ast.Expr) {
	match expr {
		ast.ArrayInitExpr {
			if expr.exprs.len > 0 {
				g.write('[')
				g.expr_list(expr.exprs, ', ')
				g.write(']')
				// TODO: better way to handle this
				if expr.len !is ast.EmptyExpr {
					g.write('!')
				}
			} else {
				has_init := expr.init !is ast.EmptyExpr
				has_len := expr.len !is ast.EmptyExpr
				has_cap := expr.cap !is ast.EmptyExpr
				g.expr(expr.typ)
				g.write('{')
				// if expr.init != none {
				if has_init {
					g.write('init: ')
					g.expr(expr.init)
					if has_len || has_cap {
						g.write(', ')
					}
				}
				// if expr.len != none {
				if has_len {
					g.write('len: ')
					g.expr(expr.len)
					if has_cap {
						g.write(', ')
					}
				}
				// if expr.cap != none {
				if has_cap {
					g.write('cap: ')
					g.expr(expr.cap)
				}
				g.write('}')
			}
		}
		ast.AsCastExpr {
			g.expr(expr.expr)
			g.write(' as ')
			g.expr(expr.typ)
		}
		ast.AssocExpr {
			g.expr(expr.typ)
			g.writeln('{')
			g.indent++
			g.write('...')
			g.expr(expr.expr)
			g.writeln('')
			for field in expr.fields {
				g.write(field.name)
				g.write(': ')
				g.expr(field.value)
				g.writeln('')
			}
			g.indent--
			g.write('}')
		}
		ast.BasicLiteral {
			if expr.kind == .char {
				g.write('`')
				g.write(expr.value)
				g.write('`')
			} else {
				g.write(expr.value)
			}
		}
		ast.CallExpr {
			g.expr(expr.lhs)
			g.write('(')
			g.expr_list(expr.args, ', ')
			g.write(')')
		}
		ast.CallOrCastExpr {
			g.expr(expr.lhs)
			g.write('(')
			g.expr(expr.expr)
			g.write(')')
		}
		ast.CastExpr {
			g.expr(expr.typ)
			g.write('(')
			g.expr(expr.expr)
			g.write(')')
		}
		ast.ComptimeExpr {
			g.write('$')
			g.expr(expr.expr)
		}
		ast.EmptyExpr {}
		// TODO: should this be handled like this
		ast.FieldInit {
			g.write(expr.name)
			g.write(': ')
			g.expr(expr.value)
		}
		ast.FnLiteral {
			g.write('fn')
			if expr.captured_vars.len > 0 {
				g.write(' [')
				g.expr_list(expr.captured_vars, ', ')
				g.write('] ')
			}
			g.fn_type(expr.typ)
			g.writeln(' {')
			g.stmt_list(expr.stmts)
			g.write('}')
		}
		ast.GenericArgs {
			// g.write('/* ast.GenericArgs */')
			g.expr(expr.lhs)
			g.generic_list(expr.args)
		}
		ast.GenericArgOrIndexExpr {
			// g.write('/* ast.GenericArgOrIndexExpr */')
			g.expr(expr.lhs)
			g.generic_list([expr.expr])
		}
		ast.Ident {
			g.write(expr.name)
		}
		ast.IfExpr {
			g.if_expr(expr)
		}
		ast.IfGuardExpr {
			g.stmt(expr.stmt)
		}
		ast.IndexExpr {
			g.expr(expr.lhs)
			g.write('[')
			g.expr(expr.expr)
			g.write(']')
		}
		ast.InfixExpr {
			g.expr(expr.lhs)
			g.write(' ')
			g.write(expr.op.str())
			g.write(' ')
			g.expr(expr.rhs)
		}
		ast.InitExpr {
			g.expr(expr.typ)
			// with field names
			if expr.fields.len > 0 && expr.fields[0].name != '' {
				g.writeln('{')
				in_init := g.in_init
				g.in_init = true
				g.indent++
				for i, field in expr.fields {
					g.write(field.name)
					g.write(': ')
					g.expr(field.value)
					if i < expr.fields.len - 1 {
						g.writeln(',')
					} else {
						g.writeln('')
					}
				}
				g.indent--
				g.in_init = in_init
			}
			// without field names, or empty init `Struct{}`
			else {
				g.write('{')
				for i, field in expr.fields {
					g.expr(field.value)
					if i < expr.fields.len - 1 {
						g.write(', ')
					}
				}
			}
			g.write('}')
		}
		ast.Keyword {
			g.write(expr.tok.str())
		}
		ast.KeywordOperator {
			g.write(expr.op.str())
			if expr.op in [.key_go, .key_spawn] {
				g.expr(expr.exprs[0])
			} else {
				g.write('(')
				g.expr_list(expr.exprs, ', ')
				g.write(')')
			}
		}
		ast.LambdaExpr {
			g.write('|')
			for i, arg in expr.args {
				g.write(arg.name)
				if i < expr.args.len - 1 {
					g.write(', ')
				}
			}
			g.write('| ')
			g.expr(expr.expr)
		}
		ast.LockExpr {
			has_lock_exprs := expr.lock_exprs.len > 0
			has_rlock_exprs := expr.rlock_exprs.len > 0
			if !has_lock_exprs && !has_rlock_exprs {
				g.write('lock')
			} else {
				if has_lock_exprs {
					g.write('lock ')
					g.expr_list(expr.lock_exprs, ', ')
					if has_rlock_exprs {
						g.write('; ')
					}
				}
				if has_rlock_exprs {
					g.write('rlock ')
					g.expr_list(expr.rlock_exprs, ', ')
				}
			}
			g.writeln(' {')
			g.stmt_list(expr.stmts)
			g.writeln('}')
		}
		ast.MapInitExpr {
			// long syntax
			if expr.typ !is ast.EmptyExpr {
				g.expr(expr.typ)
				g.write('{}')
			}
			// shorthand syntax
			else if expr.keys.len > 0 {
				g.write('{')
				for i, key in expr.keys {
					val := expr.vals[i]
					g.expr(key)
					g.write(': ')
					g.expr(val)
					if i < expr.keys.len - 1 {
						g.write(', ')
					}
				}
				g.write('}')
			}
			// empty {}
			else {
				g.write('{}')
			}
		}
		ast.MatchExpr {
			g.write('match ')
			g.expr(expr.expr)
			g.writeln(' {')
			g.indent++
			for branch in expr.branches {
				if branch.cond.len > 0 {
					g.expr_list(branch.cond, ', ')
				} else {
					g.write('else')
				}
				g.writeln(' {')
				g.stmt_list(branch.stmts)
				g.writeln('}')
			}
			g.indent--
			g.write('}')
			// g.writeln(' ==== DeSugared MatchExpr  ==== ')
			// g.expr(expr.desugar())
		}
		ast.ModifierExpr {
			g.write(expr.kind.str())
			g.write(' ')
			g.expr(expr.expr)
		}
		ast.OrExpr {
			g.expr(expr.expr)
			g.writeln(' or {')
			g.stmt_list(expr.stmts)
			g.writeln('}')
			// g.writeln('==== DeSugared OrExpr ====')
			// g.expr(expr.desugar())
		}
		ast.ParenExpr {
			g.write('(')
			g.expr(expr.expr)
			g.write(')')
		}
		ast.PostfixExpr {
			g.expr(expr.expr)
			g.write(expr.op.str())
		}
		ast.PrefixExpr {
			g.write(expr.op.str())
			g.expr(expr.expr)
		}
		ast.RangeExpr {
			g.write('RangeExpr[ ')
			g.expr(expr.start)
			// g.write(' ')
			g.write(expr.op.str())
			// g.write(' ')
			g.expr(expr.end)
			g.write(' ]')
		}
		ast.SelectExpr {
			g.writeln('select {')
			g.indent++
			g.select_expr(expr)
			g.indent--
			g.write('}')
		}
		ast.SelectorExpr {
			g.expr(expr.lhs)
			g.write('.')
			g.expr(expr.rhs)
		}
		ast.StringInterLiteral {
			if expr.kind != .v {
				g.write(expr.kind.str())
			}
			// TODO: smart quote
			// quote_str := "'"
			// g.write(quote_str)
			for i, value in expr.values {
				g.write(value)
				if inter := expr.inters[i] {
					g.write('\${')
					g.expr(inter.expr)
					if inter.format != .unformatted {
						g.write(':')
						g.expr(inter.format_expr)
						g.write(inter.format.str())
					}
					g.write('}')
				}
			}
			// g.write(quote_str)
		}
		ast.StringLiteral {
			if expr.kind != .v {
				g.write(expr.kind.str())
			}
			// TODO: smart quote
			// quote_str := "'"
			// g.write(quote_str)
			g.write(expr.value)
			// g.write(quote_str)
		}
		ast.SqlExpr {
			g.write('sql ')
			g.expr(expr.expr)
			g.writeln(' {')
			g.write('}')
		}
		ast.Tuple {
			g.expr_list(expr.exprs, ', ')
		}
		ast.UnsafeExpr {
			one_liner := g.in_init || expr.stmts.len == 1
			if one_liner {
				g.write('unsafe { ')
			} else {
				g.writeln('unsafe {')
			}
			in_init := g.in_init
			g.in_init = one_liner
			g.stmt_list(expr.stmts)
			g.in_init = in_init
			if one_liner {
				g.write(' }')
			} else {
				g.write('}')
			}
		}
		// Type Nodes
		// TODO: I really would like to allow matching the nested sumtypes like TS
		ast.Type {
			match expr {
				ast.AnonStructType {
					g.write('struct')
					if expr.generic_params.len > 0 {
						g.generic_list(expr.generic_params)
					}
					g.struct_decl_fields(expr.embedded, expr.fields)
				}
				ast.ArrayType {
					g.write('[]')
					g.expr(expr.elem_type)
				}
				ast.ArrayFixedType {
					g.write('[')
					if expr.len !is ast.EmptyExpr {
						g.expr(expr.len)
					}
					g.write(']')
					g.expr(expr.elem_type)
				}
				ast.ChannelType {
					g.write('chan ')
					g.expr(expr.elem_type)
				}
				ast.FnType {
					g.write('fn')
					g.fn_type(expr)
				}
				ast.GenericType {
					g.expr(expr.name)
					g.generic_list(expr.params)
				}
				ast.MapType {
					g.write('map[')
					g.expr(expr.key_type)
					g.write(']')
					g.expr(expr.value_type)
				}
				ast.NilType {
					g.write('nil')
				}
				ast.NoneType {
					g.write('none')
				}
				ast.OptionType {
					g.write('?')
					if expr.base_type !is ast.EmptyExpr {
						g.expr(expr.base_type)
					}
				}
				ast.ResultType {
					g.write('!')
					if expr.base_type !is ast.EmptyExpr {
						g.expr(expr.base_type)
					}
				}
				ast.ThreadType {
					g.write('thread')
					if expr.elem_type !is ast.EmptyExpr {
						g.write(' ')
						g.expr(expr.elem_type)
					}
				}
				ast.TupleType {
					g.write('(')
					g.expr_list(expr.types, ', ')
					g.write(')')
				}
				// TODO: v bug since all variants are accounted for
				// this should not be required?
				// ast.Type {}
			}
		}
	}
}

// NOTE: I purposefully left out `$` from every branch of comptime if
// Hopefully this will be removed from the syntax. if not ill add it.
fn (mut g Gen) if_expr(expr ast.IfExpr) {
	if expr.cond !is ast.EmptyExpr {
		g.write('if ')
		in_init := g.in_init
		g.in_init = true
		g.expr(expr.cond)
		g.in_init = in_init
		g.write(' ')
	}
	g.writeln('{')
	g.stmt_list(expr.stmts)
	g.write('}')
	if expr.else_expr is ast.IfExpr {
		g.write(' else ')
		g.if_expr(expr.else_expr)
	}
}

fn (mut g Gen) select_expr(expr ast.SelectExpr) {
	// TODO: fix missing writeln after block due to `p.in_init`
	in_init := g.in_init
	g.in_init = true
	g.stmt(expr.stmt)
	g.in_init = in_init
	if expr.stmts.len > 0 {
		g.writeln(' {')
		g.stmt_list(expr.stmts)
		g.write('}')
	}
	g.writeln('')
	if expr.next is ast.SelectExpr {
		g.select_expr(expr.next)
	}
}

fn (mut g Gen) attributes(attributes []ast.Attribute) {
	g.write('@[')
	for i, attribute in attributes {
		if attribute.comptime_cond !is ast.EmptyExpr {
			g.write('if ')
			g.expr(attribute.comptime_cond)
		} else {
			if attribute.name != '' {
				g.write(attribute.name)
				g.write(': ')
			}
			g.expr(attribute.value)
			if i < attributes.len - 1 {
				g.write('; ')
			}
		}
	}
	g.write(']')
}

fn (mut g Gen) fn_type(typ ast.FnType) {
	if typ.generic_params.len > 0 {
		g.generic_list(typ.generic_params)
	}
	g.write('(')
	for i, param in typ.params {
		if param.name != '' {
			g.write(param.name)
			g.write(' ')
		}
		g.expr(param.typ)
		if i < typ.params.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
	if typ.return_type !is ast.EmptyExpr {
		g.write(' ')
		g.expr(typ.return_type)
	}
}

fn (mut g Gen) struct_decl(stmt ast.StructDecl) {
	if stmt.attributes.len > 0 {
		g.attributes(stmt.attributes)
		g.writeln('')
	}
	if stmt.is_public {
		g.write('pub ')
	}
	g.write('struct ')
	if stmt.language != .v {
		g.write(stmt.language.str())
		g.write('.')
	}
	g.write(stmt.name)
	if stmt.generic_params.len > 0 {
		g.generic_list(stmt.generic_params)
	}
	g.struct_decl_fields(stmt.embedded, stmt.fields)
}

fn (mut g Gen) struct_decl_fields(embedded []ast.Expr, fields []ast.FieldDecl) {
	if fields.len > 0 {
		g.writeln(' {')
	} else {
		g.write(' {')
	}
	g.indent++
	for embed in embedded {
		g.expr(embed)
		g.writeln('')
	}
	for field in fields {
		g.write(field.name)
		g.write(' ')
		g.expr(field.typ)
		// if field.value != none {
		if field.value !is ast.EmptyExpr {
			g.write(' = ')
			g.expr(field.value)
		}
		if field.attributes.len > 0 {
			g.write(' ')
			g.attributes(field.attributes)
		}
		g.writeln('')
	}
	g.indent--
	g.writeln('}')
}

@[inline]
fn (mut g Gen) expr_list(exprs []ast.Expr, separator string) {
	for i, expr in exprs {
		g.expr(expr)
		if i < exprs.len - 1 {
			g.write(separator)
		}
	}
}

@[inline]
fn (mut g Gen) generic_list(exprs []ast.Expr) {
	g.write('[')
	g.expr_list(exprs, ', ')
	g.write(']')
}

@[inline]
fn (mut g Gen) write(str string) {
	if g.on_newline {
		g.out.write_string(tabs[g.indent])
	}
	g.out.write_string(str)
	g.on_newline = false
}

@[inline]
fn (mut g Gen) writeln(str string) {
	if g.on_newline {
		g.out.write_string(tabs[g.indent])
	}
	g.out.writeln(str)
	g.on_newline = true
}

pub fn (g &Gen) print_output() {
	println(g.out)
}
