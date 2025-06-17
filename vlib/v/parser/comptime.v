// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import os
import v.ast
import v.token

const supported_comptime_calls = ['html', 'tmpl', 'env', 'embed_file', 'pkgconfig', 'compile_error',
	'compile_warn', 'd', 'res']
const comptime_types = ['map', 'array', 'array_dynamic', 'array_fixed', 'int', 'float', 'struct',
	'interface', 'enum', 'sumtype', 'alias', 'function', 'option', 'string', 'pointer', 'voidptr']

fn (mut p Parser) parse_comptime_type() ast.ComptimeType {
	pos := p.tok.pos()
	p.check(.dollar)
	name := p.check_name()
	if name !in comptime_types {
		p.error('unsupported compile-time type `${name}`: only ${comptime_types} are supported')
	}
	mut kind := ast.ComptimeTypeKind.unknown
	kind = match name {
		'map' {
			.map
		}
		'struct' {
			.struct
		}
		'interface' {
			.iface
		}
		'int' {
			.int
		}
		'float' {
			.float
		}
		'alias' {
			.alias
		}
		'function' {
			.function
		}
		'array' {
			.array
		}
		'array_fixed' {
			.array_fixed
		}
		'array_dynamic' {
			.array_dynamic
		}
		'enum' {
			.enum
		}
		'sumtype' {
			.sum_type
		}
		'option' {
			.option
		}
		'string' {
			.string
		}
		'pointer' {
			.pointer
		}
		'voidptr' {
			.voidptr
		}
		else {
			.unknown
		}
	}
	return ast.ComptimeType{
		kind: kind
		pos:  pos
	}
}

// // #include, #flag, #v
fn (mut p Parser) hash() ast.HashStmt {
	pos := p.tok.pos()
	val := p.tok.lit
	kind := val.all_before(' ')
	attrs := p.attrs
	p.next()
	mut main_str := ''
	mut msg := ''
	content := val.all_after('${kind} ').all_before('//')
	if content.contains(' #') {
		main_str = content.all_before(' #').trim_space()
		msg = content.all_after(' #').trim_space()
	} else {
		main_str = content.trim_space()
		msg = ''
	}

	mut is_use_once := false
	for fna in attrs {
		match fna.name {
			'use_once' { is_use_once = true }
			else {}
		}
	}

	return ast.HashStmt{
		mod:         p.mod
		source_file: p.file_path
		val:         val
		kind:        kind
		main:        main_str
		msg:         msg
		pos:         pos
		attrs:       attrs
		is_use_once: is_use_once
	}
}

const error_msg = 'only `\$tmpl()`, `\$env()`, `\$embed_file()`, `\$pkgconfig()`, `\$vweb.html()`, `\$compile_error()`, `\$compile_warn()`, `\$d()` and `\$res()` comptime functions are supported right now'

fn (mut p Parser) comptime_call() ast.ComptimeCall {
	err_node := ast.ComptimeCall{
		scope: unsafe { nil }
	}
	start_pos := p.tok.pos()
	p.check(.dollar)
	mut is_veb := false
	if p.peek_tok.kind == .dot {
		name := p.check_name() // skip `vweb.html()` TODO
		if name != 'vweb' && name != 'veb' {
			p.error(error_msg)
			return err_node
		}
		import_mods := p.ast_imports.map(it.mod)
		if name == 'vweb' {
			if 'vweb' !in import_mods && 'x.vweb' !in import_mods {
				p.error_with_pos('`\$vweb` cannot be used without importing vweb', start_pos.extend(p.prev_tok.pos()))
				return err_node
			}
			p.register_used_import('vweb')
		} else if name == 'veb' {
			if 'veb' !in import_mods {
				p.error_with_pos('`\$veb` cannot be used without importing veb', start_pos.extend(p.prev_tok.pos()))
				return err_node
			}
			p.register_used_import('veb')
			is_veb = true
		}
		p.check(.dot)
	}
	method_name := p.check_name()
	if method_name !in supported_comptime_calls {
		p.error(error_msg)
		return err_node
	}
	is_embed_file := method_name == 'embed_file'
	is_html := method_name == 'html'
	p.check(.lpar)
	arg_pos := p.tok.pos()
	if method_name in ['env', 'pkgconfig', 'compile_error', 'compile_warn'] {
		s := p.tok.lit
		p.check(.string)
		p.check(.rpar)
		return ast.ComptimeCall{
			scope:        unsafe { nil }
			method_name:  method_name
			args_var:     s
			is_env:       method_name == 'env'
			is_pkgconfig: method_name == 'pkgconfig'
			env_pos:      start_pos
			pos:          start_pos.extend(p.prev_tok.pos())
		}
	} else if method_name == 'res' {
		mut has_args := false
		mut type_index := ''
		if p.tok.kind == .number {
			has_args = true
			type_index = p.tok.lit
			p.check(.number)
		}
		p.check(.rpar)
		if has_args {
			return ast.ComptimeCall{
				scope:       unsafe { nil }
				method_name: method_name
				args_var:    type_index
				pos:         start_pos.extend(p.prev_tok.pos())
			}
		}
		return ast.ComptimeCall{
			scope:       unsafe { nil }
			method_name: method_name
			pos:         start_pos.extend(p.prev_tok.pos())
		}
	} else if method_name == 'd' {
		const_string := p.tok.lit
		// const_name_pos := p.tok.pos()
		p.check(.string)
		p.check(.comma)
		arg_expr := p.expr(0)
		args := [
			ast.CallArg{
				expr: arg_expr
				pos:  p.tok.pos()
			},
		]
		p.check(.rpar)
		return ast.ComptimeCall{
			scope:            unsafe { nil }
			is_compile_value: true
			method_name:      method_name
			args_var:         const_string
			args:             args
			pos:              start_pos.extend(p.prev_tok.pos())
		}
	}
	has_string_arg := p.tok.kind == .string
	mut literal_string_param := if is_html && !has_string_arg { '' } else { p.tok.lit }
	if p.tok.kind == .name {
		if var := p.scope.find_var(p.tok.lit) {
			if var.expr is ast.StringLiteral {
				literal_string_param = var.expr.val
			}
		} else if var := p.table.global_scope.find_const(p.mod + '.' + p.tok.lit) {
			if var.expr is ast.StringLiteral {
				literal_string_param = var.expr.val
			}
		}
	}
	path_of_literal_string_param := literal_string_param.replace('/', os.path_separator)
	mut arg := ast.CallArg{}
	if is_html && !(has_string_arg || p.tok.kind == .rpar) {
		p.error('expecting `\$vweb.html()` for a default template path or `\$vweb.html("/path/to/template.html")`')
	}
	if is_html && p.tok.kind != .string {
		// $vweb.html() can have no arguments
	} else {
		arg_expr := p.expr(0)
		arg = ast.CallArg{
			expr: arg_expr
		}
	}
	mut embed_compression_type := 'none'
	if is_embed_file {
		if p.tok.kind == .comma {
			p.next()
			p.check(.dot)
			embed_compression_type = p.check_name()
		}
	}
	p.check(.rpar)
	// $embed_file('/path/to/file')
	if is_embed_file {
		p.register_auto_import('v.preludes.embed_file')
		if embed_compression_type == 'zlib' {
			p.register_auto_import('v.preludes.embed_file.zlib')
		}
		return ast.ComptimeCall{
			scope:      unsafe { nil }
			is_embed:   true
			embed_file: ast.EmbeddedFile{
				compression_type: embed_compression_type
			}
			args:       [arg]
			pos:        start_pos.extend(p.prev_tok.pos())
		}
	}
	// Compile vweb html template to V code, parse that V code and embed the resulting V function
	// that returns an html string.
	fn_path := p.cur_fn_name.split('_')
	fn_path_joined := fn_path.join(os.path_separator)
	compiled_vfile_path := os.real_path(p.scanner.file_path.replace('/', os.path_separator))
	tmpl_path := if is_html && !has_string_arg {
		'${fn_path.last()}.html'
	} else {
		path_of_literal_string_param
	}
	// Looking next to the vweb program
	dir := os.dir(compiled_vfile_path)
	mut path := os.join_path_single(dir, fn_path_joined)
	path += '.html'
	path = os.real_path(path)
	if !is_html || has_string_arg {
		if os.is_abs_path(tmpl_path) {
			path = tmpl_path
		} else {
			path = os.join_path_single(dir, tmpl_path)
		}
	}
	if !os.exists(path) {
		if is_html {
			// can be in `templates/`
			path = os.join_path(dir, 'templates', fn_path_joined)
			path += '.html'
		}
		if !os.exists(path) {
			if p.pref.is_fmt {
				return ast.ComptimeCall{
					scope:       unsafe { nil }
					is_vweb:     true
					is_veb:      is_veb
					method_name: method_name
					args_var:    literal_string_param
					args:        [arg]
					pos:         start_pos.extend(p.prev_tok.pos())
				}
			}
			if is_html {
				p.error_with_pos('veb HTML template "${tmpl_path}" not found', arg_pos)
			} else {
				p.error_with_pos('template file "${tmpl_path}" not found', arg_pos)
			}
			return err_node
		}
		// println('path is now "$path"')
	}
	tmp_fn_name := p.cur_fn_name.replace('.', '__') + start_pos.pos.str()
	$if trace_comptime ? {
		println('>>> compiling comptime template file "${path}" for ${tmp_fn_name}')
	}
	v_code := p.compile_template_file(path, tmp_fn_name)
	$if print_veb_template_expansions ? {
		lines := v_code.split('\n')
		for i, line in lines {
			println('${path}:${i + 1}: ${line}')
		}
	}
	$if trace_comptime ? {
		println('')
		println('>>> template for ${path}:')
		println(v_code)
		println('>>> end of template END')
		println('')
	}
	// the tmpl inherits all parent scopes. previous functionality was just to
	// inherit the scope from which the comptime call was made and no parents.
	// this is much simpler and allows access to globals. can be changed if needed.
	p.open_scope()
	defer {
		p.close_scope()
	}
	mut file := parse_comptime(tmpl_path, v_code, mut p.table, p.pref, mut p.scope)
	file.path = tmpl_path
	return ast.ComptimeCall{
		scope:       unsafe { nil }
		is_vweb:     true
		is_veb:      is_veb
		veb_tmpl:    file
		method_name: method_name
		args_var:    literal_string_param
		args:        [arg]
		pos:         start_pos.extend(p.prev_tok.pos())
	}
}

fn (mut p Parser) comptime_for() ast.ComptimeFor {
	// p.comptime_for() handles these special forms:
	// `$for method in App.methods {`
	// `$for val in App.values {`
	// `$for field in App.fields {`
	// `$for attr in App.attributes {`
	// `$for variant in App.variants {`
	p.next()
	p.check(.key_for)
	var_pos := p.tok.pos()
	val_var := p.check_name()
	p.check(.key_in)
	mut expr := ast.empty_expr
	mut typ_pos := p.tok.pos()
	lang := p.parse_language()
	mut typ := ast.void_type

	if p.tok.lit.len == 0 {
		p.error('invalid expr, use `${p.peek_tok.lit}` instead')
		return ast.ComptimeFor{}
	}
	if p.tok.lit[0].is_capital() || p.tok.lit in p.imports {
		typ = p.parse_any_type(lang, false, true, false)
	} else {
		expr = p.ident(lang)
		p.scope.mark_var_as_used((expr as ast.Ident).name)
	}
	typ_pos = typ_pos.extend(p.prev_tok.pos())
	p.check(.dot)
	for_val := p.check_name()
	mut kind := ast.ComptimeForKind.methods
	p.open_scope()
	defer {
		p.close_scope()
	}
	match for_val {
		'params' {
			p.scope.register(ast.Var{
				name: val_var
				typ:  p.table.find_type('MethodParam')
				pos:  var_pos
			})
			kind = .params
		}
		'methods' {
			p.scope.register(ast.Var{
				name: val_var
				typ:  p.table.find_type('FunctionData')
				pos:  var_pos
			})
		}
		'values' {
			p.scope.register(ast.Var{
				name: val_var
				typ:  p.table.find_type('EnumData')
				pos:  var_pos
			})
			kind = .values
		}
		'fields' {
			p.scope.register(ast.Var{
				name: val_var
				typ:  p.table.find_type('FieldData')
				pos:  var_pos
			})
			kind = .fields
		}
		'variants' {
			p.scope.register(ast.Var{
				name: val_var
				typ:  p.table.find_type('VariantData')
				pos:  var_pos
			})
			kind = .variants
		}
		'attributes' {
			p.scope.register(ast.Var{
				name: val_var
				typ:  p.table.find_type('VAttribute')
				pos:  var_pos
			})
			kind = .attributes
		}
		else {
			p.error_with_pos('unknown kind `${for_val}`, available are: `methods`, `fields`, `values`, `variants`, `attributes` or `params`',
				p.prev_tok.pos())
			return ast.ComptimeFor{}
		}
	}
	spos := p.tok.pos()
	stmts := p.parse_block()
	return ast.ComptimeFor{
		val_var: val_var
		stmts:   stmts
		kind:    kind
		typ:     typ
		expr:    expr
		typ_pos: typ_pos
		pos:     spos.extend(p.tok.pos())
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
		'@FILE' { token.AtKind.file_path }
		'@DIR' { token.AtKind.file_dir }
		'@LINE' { token.AtKind.line_nr }
		'@FILE_LINE' { token.AtKind.file_path_line_nr }
		'@LOCATION' { token.AtKind.location }
		'@COLUMN' { token.AtKind.column_nr }
		'@VCURRENTHASH' { token.AtKind.v_current_hash }
		'@VHASH' { token.AtKind.vhash }
		'@VMOD_FILE' { token.AtKind.vmod_file }
		'@VEXE' { token.AtKind.vexe_path }
		'@VEXEROOT' { token.AtKind.vexeroot_path }
		'@VMODROOT' { token.AtKind.vmodroot_path }
		'@VMODHASH' { token.AtKind.vmod_hash }
		'@VROOT' { token.AtKind.vroot_path } // deprecated, use @VEXEROOT or @VMODROOT
		'@BUILD_DATE' { token.AtKind.build_date }
		'@BUILD_TIME' { token.AtKind.build_time }
		'@BUILD_TIMESTAMP' { token.AtKind.build_timestamp }
		else { token.AtKind.unknown }
	}
	expr := ast.AtExpr{
		name: name
		pos:  p.tok.pos()
		kind: kind
	}
	p.next()
	return expr
}

fn (mut p Parser) comptime_selector(left ast.Expr) ast.Expr {
	p.check(.dollar)
	start_pos := p.prev_tok.pos()
	if p.peek_tok.kind == .lpar {
		method_pos := p.tok.pos()
		method_name := p.check_name()
		p.scope.mark_var_as_used(method_name)
		// `app.$action()` (`action` is a string)
		p.check(.lpar)
		args := p.call_args()
		p.check(.rpar)
		mut or_kind := ast.OrKind.absent
		mut or_pos := p.tok.pos()
		mut or_stmts := []ast.Stmt{}
		if p.tok.kind == .key_orelse {
			// `$method() or {}``
			or_kind = .block
			or_stmts, or_pos = p.or_block(.with_err_var)
		}
		return ast.ComptimeCall{
			left:        left
			method_name: method_name
			method_pos:  method_pos
			scope:       p.scope
			args_var:    ''
			args:        args
			pos:         start_pos.extend(p.prev_tok.pos())
			or_block:    ast.OrExpr{
				stmts: or_stmts
				kind:  or_kind
				pos:   or_pos
			}
		}
	}
	mut has_parens := false
	if p.tok.kind == .lpar {
		p.next()
		has_parens = true
	} else {
		p.warn_with_pos('use brackets instead e.g. `s.$(field.name)` - run vfmt', p.tok.pos())
	}
	expr := p.expr(0)
	if has_parens {
		p.check(.rpar)
	}
	return ast.ComptimeSelector{
		has_parens: has_parens
		left:       left
		field_expr: expr
		pos:        start_pos.extend(p.prev_tok.pos())
		or_block:   ast.OrExpr{
			stmts: []ast.Stmt{}
			kind:  if p.tok.kind == .question { .propagate_option } else { .absent }
			pos:   p.tok.pos()
		}
	}
}
