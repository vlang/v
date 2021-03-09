// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import os
import v.ast
import v.pref
import v.table
import v.token
import vweb.tmpl

const (
	supported_comptime_calls = ['html', 'tmpl', 'env', 'embed_file']
)

// // #include, #flag, #v
fn (mut p Parser) hash() ast.HashStmt {
	pos := p.tok.position()
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
		source_file: p.file_name
		val: val
		kind: kind
		main: main_str
		msg: msg
		pos: pos
	}
}

fn (mut p Parser) comp_call() ast.ComptimeCall {
	err_node := ast.ComptimeCall{
		scope: 0
	}
	p.check(.dollar)
	start_pos := p.prev_tok.position()
	error_msg := 'only `\$tmpl()`, `\$env()`, `\$embed_file()` and `\$vweb.html()` comptime functions are supported right now'
	if p.peek_tok.kind == .dot {
		n := p.check_name() // skip `vweb.html()` TODO
		if n != 'vweb' {
			p.error(error_msg)
			return err_node
		}
		p.check(.dot)
	}
	n := p.check_name() // (.name)
	if n !in parser.supported_comptime_calls {
		p.error(error_msg)
		return err_node
	}
	is_embed_file := n == 'embed_file'
	is_html := n == 'html'
	// $env('ENV_VAR_NAME')
	if n == 'env' {
		p.check(.lpar)
		spos := p.tok.position()
		s := p.tok.lit
		p.check(.string)
		p.check(.rpar)
		return ast.ComptimeCall{
			scope: 0
			method_name: n
			args_var: s
			is_env: true
			env_pos: spos
		}
	}
	p.check(.lpar)
	spos := p.tok.position()
	literal_string_param := if is_html { '' } else { p.tok.lit }
	path_of_literal_string_param := literal_string_param.replace('/', os.path_separator)
	if !is_html {
		p.check(.string)
	}
	p.check(.rpar)
	// $embed_file('/path/to/file')
	if is_embed_file {
		mut epath := path_of_literal_string_param
		// Validate that the epath exists, and that it is actually a file.
		if epath == '' {
			p.error_with_pos('supply a valid relative or absolute file path to the file to embed',
				spos)
			return err_node
		}
		if !p.pref.is_fmt {
			abs_path := os.real_path(epath)
			// check absolute path first
			if !os.exists(abs_path) {
				// ... look relative to the source file:
				epath = os.real_path(os.join_path(os.dir(p.file_name), epath))
				if !os.exists(epath) {
					p.error_with_pos('"$epath" does not exist so it cannot be embedded',
						spos)
					return err_node
				}
				if !os.is_file(epath) {
					p.error_with_pos('"$epath" is not a file so it cannot be embedded',
						spos)
					return err_node
				}
			} else {
				epath = abs_path
			}
		}
		p.register_auto_import('v.embed_file')
		return ast.ComptimeCall{
			scope: 0
			is_embed: true
			embed_file: ast.EmbeddedFile{
				rpath: literal_string_param
				apath: epath
			}
		}
	}
	// Compile vweb html template to V code, parse that V code and embed the resulting V function
	// that returns an html string.
	fn_path := p.cur_fn_name.split('_')
	fn_path_joined := fn_path.join(os.path_separator)
	compiled_vfile_path := os.real_path(p.scanner.file_path.replace('/', os.path_separator))
	tmpl_path := if is_html { '${fn_path.last()}.html' } else { path_of_literal_string_param }
	// Looking next to the vweb program
	dir := os.dir(compiled_vfile_path)
	mut path := os.join_path(dir, fn_path_joined)
	path += '.html'
	path = os.real_path(path)
	if !is_html {
		path = os.join_path(dir, tmpl_path)
	}
	if !os.exists(path) {
		// can be in `templates/`
		if is_html {
			path = os.join_path(dir, 'templates', fn_path_joined)
			path += '.html'
		}
		if !os.exists(path) {
			if p.pref.is_fmt {
				return ast.ComptimeCall{
					scope: 0
					is_vweb: true
					method_name: n
					pos: start_pos.extend(p.prev_tok.position())
				}
			}
			if is_html {
				p.error('vweb HTML template "$path" not found')
			} else {
				p.error('template file "$path" not found')
			}
			return err_node
		}
		// println('path is now "$path"')
	}
	tmp_fn_name := p.cur_fn_name.replace('.', '__')
	$if trace_comptime ? {
		println('>>> compiling comptime template file "$path" for $tmp_fn_name')
	}
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
	$if trace_comptime ? {
		println('')
		println('>>> vweb template for $path:')
		println(v_code)
		println('>>> end of vweb template END')
		println('')
	}
	mut file := parse_comptime(v_code, p.table, p.pref, scope, p.global_scope)
	file = ast.File{
		...file
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
						tmpl_scope.register(ast.Var{
							...v
							is_used: true
						})
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
		scope: 0
		is_vweb: true
		vweb_tmpl: file
		method_name: n
		args_var: literal_string_param
		pos: start_pos.extend(p.prev_tok.position())
	}
}

fn (mut p Parser) comp_for() ast.CompFor {
	// p.comp_for() handles these special forms:
	// $for method in App(methods) {
	// $for field in App(fields) {
	p.next()
	p.check(.key_for)
	var_pos := p.tok.position()
	val_var := p.check_name()
	p.check(.key_in)
	mut typ_pos := p.tok.position()
	lang := p.parse_language()
	typ := p.parse_any_type(lang, false, false)
	typ_pos = typ_pos.extend(p.prev_tok.position())
	p.check(.dot)
	for_val := p.check_name()
	mut kind := ast.CompForKind.methods
	p.open_scope()
	if for_val == 'methods' {
		p.scope.register(ast.Var{
			name: val_var
			typ: p.table.find_type_idx('FunctionData')
			pos: var_pos
		})
	} else if for_val == 'fields' {
		p.scope.register(ast.Var{
			name: val_var
			typ: p.table.find_type_idx('FieldData')
			pos: var_pos
		})
		kind = .fields
	} else {
		p.error_with_pos('unknown kind `$for_val`, available are: `methods` or `fields`',
			p.prev_tok.position())
		return ast.CompFor{}
	}
	spos := p.tok.position()
	stmts := p.parse_block()
	p.close_scope()
	return ast.CompFor{
		val_var: val_var
		stmts: stmts
		kind: kind
		typ: typ
		typ_pos: typ_pos
		pos: spos.extend(p.tok.position())
	}
}

// @FN, @STRUCT, @MOD etc. See full list in token.valid_at_tokens
fn (mut p Parser) at() ast.AtExpr {
	name := p.tok.lit
	kind := match name {
		'@FN' { token.AtKind.fn_name }
		'@METHOD' { token.AtKind.method_name }
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

fn (mut p Parser) comptime_selector(left ast.Expr) ast.Expr {
	p.check(.dollar)
	start_pos := p.prev_tok.position()
	if p.peek_tok.kind == .lpar {
		method_pos := p.tok.position()
		method_name := p.check_name()
		p.mark_var_as_used(method_name)
		// `app.$action()` (`action` is a string)
		p.check(.lpar)
		args := p.call_args()
		p.check(.rpar)
		if p.tok.kind == .key_orelse {
			p.check(.key_orelse)
			p.check(.lcbr)
		}
		return ast.ComptimeCall{
			left: left
			method_name: method_name
			method_pos: method_pos
			scope: p.scope
			args_var: ''
			args: args
			pos: start_pos.extend(p.prev_tok.position())
		}
	}
	mut has_parens := false
	if p.tok.kind == .lpar {
		p.check(.lpar)
		has_parens = true
	} else {
		p.warn_with_pos('use brackets instead e.g. `s.$(field.name)` - run vfmt', p.tok.position())
	}
	expr := p.expr(0)
	if has_parens {
		p.check(.rpar)
	}
	return ast.ComptimeSelector{
		has_parens: has_parens
		left: left
		field_expr: expr
		pos: start_pos.extend(p.prev_tok.position())
	}
}
