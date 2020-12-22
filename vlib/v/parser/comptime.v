// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import os
import v.ast
import v.pref
import v.table
import v.token
import vweb.tmpl

// // #include, #flag, #v
fn (mut p Parser) hash() ast.HashStmt {
	mut pos := p.prev_tok.position()
	val := p.tok.lit
	kind := val.all_before(' ')
	p.next()
	mut main_str := ''
	mut msg := ''
	content := val.all_after('$kind ').all_before('//')
	if content.contains(' #') {
		main_str = content.all_before(' #').trim_space()
		msg = content.all_after(' #').trim_space()
	} else {
		main_str = content.trim_space()
		msg = ''
	}
	// p.trace('a.v', 'kind: ${kind:-10s} | pos: ${pos:-45s} | hash: $val')
	return ast.HashStmt{
		mod: p.mod
		val: val
		kind: kind
		main: main_str
		msg: msg
		pos: pos
	}
}

fn (mut p Parser) vweb() ast.ComptimeCall {
	p.check(.dollar)
	error_msg := 'only `\$tmpl()` and `\$vweb.html()` comptime functions are supported right now'
	if p.peek_tok.kind == .dot {
		n := p.check_name() // skip `vweb.html()` TODO
		if n != 'vweb' {
			p.error(error_msg)
			return ast.ComptimeCall{}
		}
		p.check(.dot)
	}
	n := p.check_name() // (.name)
	if n != 'html' && n != 'tmpl' {
		p.error(error_msg)
		return ast.ComptimeCall{}
	}
	is_html := n == 'html'
	p.check(.lpar)
	s := if is_html { '' } else { p.tok.lit }
	if !is_html {
		p.check(.string)
	}
	p.check(.rpar)
	// Compile vweb html template to V code, parse that V code and embed the resulting V function
	// that returns an html string.
	fn_path := p.cur_fn_name.split('_')
	tmpl_path := if is_html { '${fn_path.last()}.html' } else { s }
	// Looking next to the vweb program
	dir := os.dir(p.scanner.file_path)
	mut path := os.join_path(dir, fn_path.join('/'))
	path += '.html'
	if !is_html {
		path = tmpl_path
	}
	if !os.exists(path) {
		// can be in `templates/`
		if is_html {
			path = os.join_path(dir, 'templates', fn_path.join('/'))
			path += '.html'
		}
		if !os.exists(path) {
			if is_html {
				p.error('vweb HTML template "$path" not found')
				return ast.ComptimeCall{}
			} else {
				p.error('template file "$path" not found')
				return ast.ComptimeCall{}
			}
		}
		// println('path is now "$path"')
	}
	if p.pref.is_verbose {
		println('>>> compiling comptime template file "$path"')
	}
	tmp_fn_name := p.cur_fn_name.replace('.', '__')
	v_code := tmpl.compile_file(path, tmp_fn_name)
	$if print_vweb_template_expansions ? {
		lines := v_code.split('\n')
		for i, line in lines {
			println('$path:${i + 1}: $line')
		}
	}
	mut scope := &ast.Scope{
		start_pos: 0
		parent: p.global_scope
	}
	if p.pref.is_verbose {
		println('\n\n')
		println('>>> vweb template for $path:')
		println(v_code)
		println('>>> end of vweb template END')
		println('\n\n')
	}
	mut file := parse_comptime(v_code, p.table, p.pref, scope, p.global_scope)
	file = {
		file |
		path: tmpl_path
	}
	// copy vars from current fn scope into vweb_tmpl scope
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.name == 'main.vweb_tmpl_$tmp_fn_name' {
				// mut tmpl_scope := file.scope.innermost(stmt.body_pos.pos)
				mut tmpl_scope := stmt.scope
				for _, obj in p.scope.objects {
					if obj is ast.Var {
						mut v := obj
						v.pos = stmt.body_pos
						tmpl_scope.register(v)
						// set the controller action var to used
						// if it's unused in the template it will warn
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
		method_name: n
		args_var: s
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
		p.scope.register(ast.Var{
			name: val_var
			typ: p.table.find_type_idx('FunctionData')
		})
	} else if for_val == 'fields' {
		p.scope.register(ast.Var{
			name: val_var
			typ: p.table.find_type_idx('FieldData')
		})
		kind = .fields
	} else {
		p.error('unknown kind `$for_val`, available are: `methods` or `fields`')
		return ast.CompFor{}
	}
	spos := p.tok.position()
	stmts := p.parse_block()
	return ast.CompFor{
		val_var: val_var
		stmts: stmts
		kind: kind
		typ: typ
		pos: spos.extend(p.tok.position())
	}
}

// @FN, @STRUCT, @MOD etc. See full list in token.valid_at_tokens
fn (mut p Parser) at() ast.AtExpr {
	name := p.tok.lit
	kind := match name {
		'@FN' { token.AtKind.fn_name }
		'@MOD' { token.AtKind.mod_name }
		'@STRUCT' { token.AtKind.struct_name }
		'@VEXE' { token.AtKind.vexe_path }
		'@FILE' { token.AtKind.file_path }
		'@LINE' { token.AtKind.line_nr }
		'@COLUMN' { token.AtKind.column_nr }
		'@VHASH' { token.AtKind.vhash }
		'@VMOD_FILE' { token.AtKind.vmod_file }
		else { token.AtKind.unknown }
	}
	p.next()
	return ast.AtExpr{
		name: name
		pos: p.tok.position()
		kind: kind
	}
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
		'macos' {
			return .macos
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
