// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import os
import v.ast
import v.pref
import v.table
import vweb.tmpl

// #flag darwin -I.
const (
	supported_platforms  = ['windows', 'mac', 'macos', 'darwin', 'linux', 'freebsd', 'openbsd',
		'netbsd', 'dragonfly', 'android', 'js', 'solaris', 'haiku', 'linux_or_macos']
	supported_ccompilers = ['tinyc', 'clang', 'mingw', 'msvc', 'gcc']
)

// // #include, #flag, #v
fn (mut p Parser) hash() ast.HashStmt {
	mut val := p.tok.lit
	p.next()
	return ast.HashStmt{
		val: val
		mod: p.mod
		pos: p.prev_tok.position()
	}
}

fn (mut p Parser) vweb() ast.ComptimeCall {
	p.check(.dollar)
	p.check(.name) // skip `vweb.html()` TODO
	p.check(.dot)
	p.check(.name)
	p.check(.lpar)
	p.check(.rpar)
	// Compile vweb html template to V code, parse that V code and embed the resulting V function
	// that returns an html string.
	fn_path := p.cur_fn_name.split('_')
	html_name := '${fn_path.last()}.html'
	// Looking next to the vweb program
	dir := os.dir(p.scanner.file_path)
	mut path := os.join_path(dir, fn_path.join('/'))
	path += '.html'
	if !os.exists(path) {
		// can be in `templates/`
		path = os.join_path(dir, 'templates', fn_path.join('/'))
		path += '.html'
		if !os.exists(path) {
			p.error('vweb HTML template "$path" not found')
		}
		// println('path is now "$path"')
	}
	if p.pref.is_verbose {
		println('>>> compiling vweb HTML template "$path"')
	}
	v_code := tmpl.compile_file(path, p.cur_fn_name)
	mut scope := &ast.Scope{
		start_pos: 0
		parent: p.global_scope
	}
	mut file := parse_text(v_code, p.table, p.pref, scope, p.global_scope)
	if p.pref.is_verbose {
		println('\n\n')
		println('>>> vweb template for $path:')
		println(v_code)
		println('>>> end of vweb template END')
		println('\n\n')
	}
	file = {
		file |
		path: html_name
	}
	// copy vars from current fn scope into vweb_tmpl scope
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.name == 'main.vweb_tmpl_$p.cur_fn_name' {
				mut tmpl_scope := file.scope.innermost(stmt.body_pos.pos)
				for _, obj in p.scope.objects {
					if obj is ast.Var {
						mut v := obj
						v.pos = stmt.body_pos
						tmpl_scope.register(v.name, *v)
						// set the controller action var to used
						// if its unused in the template it will warn
						v.is_used = true
					}
				}
				break
			}
		}
	}
	return ast.ComptimeCall{
		is_vweb: true
		vweb_tmpl: file
	}
}

fn (mut p Parser) comp_for() ast.CompFor {
	// p.comp_for() handles these special forms:
	// $for method in App(methods) {
	// $for field in App(fields) {
	p.next()
	p.check(.key_for)
	val_var := p.check_name()
	p.check(.key_in)
	lang := p.parse_language()
	typ := p.parse_any_type(lang, false, false)
	p.check(.dot)
	for_val := p.check_name()
	mut kind := ast.CompForKind.methods
	if for_val == 'methods' {
		p.scope.register(val_var, ast.Var{
			name: val_var
			typ: p.table.find_type_idx('FunctionData')
		})
	} else if for_val == 'fields' {
		p.scope.register(val_var, ast.Var{
			name: val_var
			typ: p.table.find_type_idx('FieldData')
		})
		kind = .fields
	} else {
		p.error('unknown kind `$for_val`, available are: `methods` or `fields`')
	}
	stmts := p.parse_block()
	return ast.CompFor{
		val_var: val_var
		stmts: stmts
		kind: kind
		typ: typ
	}
}

fn (mut p Parser) comp_if() ast.Stmt {
	pos := p.tok.position()
	p.next()
	// if p.tok.kind == .name && p.tok.lit == 'vweb' {
	// return p.vweb()
	// }
	p.check(.key_if)
	mut is_not := p.tok.kind == .not
	inversion_pos := p.tok.position()
	if is_not {
		p.next()
	}
	//
	name_pos_start := p.tok.position()
	mut val := ''
	mut tchk_expr := ast.Expr{}
	if p.peek_tok.kind == .dot {
		vname := p.parse_ident(.v)
		cobj := p.scope.find(vname.name) or {
			p.error_with_pos('unknown variable `$vname.name`', name_pos_start)
			return ast.Stmt{}
		}
		if cobj is ast.Var {
			tchk_expr = p.dot_expr(vname)
			val = vname.name
			if tchk_expr is ast.SelectorExpr as tchk_expr2 {
				if p.tok.kind == .lsbr && tchk_expr2.field_name == 'args' {
					tchk_expr = p.index_expr(tchk_expr)
					if p.tok.kind == .dot && p.peek_tok.lit == 'Type' {
						tchk_expr = p.dot_expr(tchk_expr)
					} else {
					p.error_with_pos('only the `Type` field is supported for arguments',
						p.peek_tok.position())
					}
				} else if tchk_expr2.field_name !in ['Type', 'ReturnType'] {
					p.error_with_pos('only the `Type` and `ReturnType` fields are supported for now',
						name_pos_start)
				}
			}
		} else {
			p.error_with_pos('`$vname.name` is not a variable', name_pos_start)
		}
	} else {
		val = p.check_name()
	}
	name_pos := name_pos_start.extend(p.tok.position())
	//
	mut stmts := []ast.Stmt{}
	mut skip_os := false
	mut skip_cc := false
	if val in supported_platforms {
		os := os_from_string(val)
		if (!is_not && os != p.pref.os) || (is_not && os == p.pref.os) {
			skip_os = true
		}
	} else if val in supported_ccompilers {
		if p.pref.ccompiler.len == 2 && p.pref.ccompiler == 'cc' {
			// we just do not know, so we can not skip:
			skip_cc = false
		}else {
			cc := cc_from_string(val)
			user_cc := cc_from_string(p.pref.ccompiler)
			if (!is_not && cc != user_cc) || (is_not && cc == user_cc) {
				skip_cc = true
			}
		}
	}
	mut skip := skip_os || skip_cc
	// `$if os {` or `$if compiler {` for a different target, skip everything inside
	// to avoid compilation errors (like including <windows.h> or calling WinAPI fns
	// on non-Windows systems)
	if !p.pref.is_fmt && !p.pref.output_cross_c && skip {
		p.check(.lcbr)
		// p.warn('skipping $if $val os=$os p.pref.os=$p.pref.os')
		mut stack := 1
		for {
			if p.tok.kind == .key_return {
				p.returns = true
			}
			if p.tok.kind == .lcbr {
				stack++
			} else if p.tok.kind == .rcbr {
				stack--
			}
			if p.tok.kind == .eof {
				break
			}
			if stack <= 0 && p.tok.kind == .rcbr {
				// p.warn('exiting $stack')
				p.next()
				break
			}
			p.next()
		}
	} else {
		skip = false
	}
	mut is_opt := false
	mut is_typecheck := false
	mut tchk_type := table.Type(0)
	if p.tok.kind == .question {
		p.next()
		is_opt = true
	} else if p.tok.kind in [.key_is, .not_is] {
		typecheck_inversion := p.tok.kind == .not_is
		p.next()
		tchk_type = p.parse_type()
		is_typecheck = true
		if is_not {
			name := p.table.get_type_name(tchk_type)
			p.error_with_pos('use `\$if $tchk_expr !is $name {`, not `\$if !$tchk_expr is $name {`',
			inversion_pos)
		}
		is_not = typecheck_inversion
	}
	if !skip {
		stmts = p.parse_block()
	}
	if !is_typecheck && val.len == 0 {
		p.error_with_pos('Only `\$if compvarname.field is type {}` is supported', name_pos)
	}
	if is_typecheck {
		match tchk_expr {
			ast.SelectorExpr {}
			else { p.error_with_pos('Only compvarname.field is supported', name_pos) }
		}
	}
	mut node := ast.CompIf{
		is_not: is_not
		is_opt: is_opt
		kind: if is_typecheck { ast.CompIfKind.typecheck } else { ast.CompIfKind.platform }
		pos: pos
		val: val
		tchk_type: tchk_type
		tchk_expr: tchk_expr
		stmts: stmts
	}
	if p.tok.kind == .dollar && p.peek_tok.kind == .key_else {
		p.next()
		p.next()
		node.has_else = true
		node.else_stmts = p.parse_block()
	}
	return node
}

// TODO import warning bug
const (
	todo_delete_me = pref.OS.linux
)

fn os_from_string(os string) pref.OS {
	match os {
		'linux' {
			return .linux
		}
		'windows' {
			return .windows
		}
		'ios' {
			return .ios
		}
		'mac' {
			return .mac
		}
		'macos' {
			return .mac
		}
		'freebsd' {
			return .freebsd
		}
		'openbsd' {
			return .openbsd
		}
		'netbsd' {
			return .netbsd
		}
		'dragonfly' {
			return .dragonfly
		}
		'js' {
			return .js
		}
		'solaris' {
			return .solaris
		}
		'android' {
			return .android
		}
		'msvc' {
			// notice that `-os msvc` became `-cc msvc`
			verror('use the flag `-cc msvc` to build using msvc')
		}
		'haiku' {
			return .haiku
		}
		'linux_or_macos' {
			return .linux
		}
		else {
			panic('bad os $os')
		}
	}
	// println('bad os $os') // todo panic?
	return .linux
}

// Helper function to convert string names to CC enum
pub fn cc_from_string(cc_str string) pref.CompilerType {
	if cc_str.len == 0 {
		return .gcc
	}
	cc := cc_str.replace('\\', '/').split('/').last().all_before('.')
	if 'tcc' in cc {
		return .tinyc
	}
	if 'tinyc' in cc {
		return .tinyc
	}
	if 'clang' in cc {
		return .clang
	}
	if 'mingw' in cc {
		return .mingw
	}
	if 'msvc' in cc {
		return .msvc
	}
	return .gcc
}

// `app.$action()` (`action` is a string)
// `typ` is `App` in this example
// fn (mut p Parser) comptime_method_call(typ table.Type) ast.ComptimeCall {
fn (mut p Parser) comptime_method_call(left ast.Expr) ast.ComptimeCall {
	p.check(.dollar)
	method_name := p.check_name()
	/*
	mut j := 0
	sym := p.table.get_type_symbol(typ)
	if sym.kind != .struct_ {
		p.error('not a struct')
	}
	// info := sym.info as table.Struct
	for method in sym.methods {
		if method.return_type != table.void_type {
			continue
		}
		/*
		receiver := method.args[0]
		if !p.expr_var.ptr {
			p.error('`$p.expr_var.name` needs to be a reference')
		}
		amp := if receiver.is_mut && !p.expr_var.ptr { '&' } else { '' }
		if j > 0 {
			p.gen(' else ')
		}
		p.genln('if (string_eq($method_name, _STR("$method.name")) ) ' + '${typ.name}_$method.name ($amp $p.expr_var.name);')
		*/
		j++
	}
	*/
	p.check(.lpar)
	mut args_var := ''
	if p.tok.kind == .name {
		args_var = p.tok.lit
		p.next()
	}
	p.check(.rpar)
	if p.tok.kind == .key_orelse {
		p.check(.key_orelse)
		// p.genln('else {')
		p.check(.lcbr)
		// p.statements()
	}
	return ast.ComptimeCall{
		left: left
		method_name: method_name
		args_var: args_var
	}
}
