// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import os
import v.ast
import v.pref
import v.token

const (
	supported_comptime_calls = ['html', 'tmpl', 'env', 'embed_file', 'pkgconfig']
	comptime_types           = ['Map', 'Array', 'Int', 'Float', 'Struct', 'Interface', 'Enum',
		'Sumtype']
)

pub fn (mut p Parser) parse_comptime_type() ast.ComptimeType {
	mut node := ast.ComptimeType{ast.ComptimeTypeKind.map_, p.tok.pos()}

	p.check(.dollar)
	name := p.check_name()
	if name !in parser.comptime_types {
		p.error('unsupported compile-time type `$name`: only $parser.comptime_types are supported')
	}
	mut cty := ast.ComptimeTypeKind.map_
	match name {
		'Map' {
			cty = .map_
		}
		'Struct' {
			cty = .struct_
		}
		'Interface' {
			cty = .iface
		}
		'Int' {
			cty = .int
		}
		'Float' {
			cty = .float
		}
		'Array' {
			cty = .array
		}
		'Enum' {
			cty = .enum_
		}
		'Sumtype' {
			cty = .sum_type
		}
		else {}
	}
	node = ast.ComptimeType{cty, node.pos}
	return node
}

// // #include, #flag, #v
fn (mut p Parser) hash() ast.HashStmt {
	pos := p.tok.pos()
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

fn (mut p Parser) comptime_call() ast.ComptimeCall {
	err_node := ast.ComptimeCall{
		scope: 0
	}
	p.check(.dollar)
	start_pos := p.prev_tok.pos()
	error_msg := 'only `\$tmpl()`, `\$env()`, `\$embed_file()`, `\$pkgconfig()` and `\$vweb.html()` comptime functions are supported right now'
	if p.peek_tok.kind == .dot {
		name := p.check_name() // skip `vweb.html()` TODO
		if name != 'vweb' {
			p.error(error_msg)
			return err_node
		}
		p.check(.dot)
	}
	method_name := p.check_name() // (.name)
	if method_name !in parser.supported_comptime_calls {
		p.error(error_msg)
		return err_node
	}
	is_embed_file := method_name == 'embed_file'
	is_html := method_name == 'html'
	// $env('ENV_VAR_NAME')
	p.check(.lpar)
	spos := p.tok.pos()
	if method_name == 'env' {
		s := p.tok.lit
		p.check(.string)
		p.check(.rpar)
		return ast.ComptimeCall{
			scope: 0
			method_name: method_name
			args_var: s
			is_env: true
			env_pos: spos
			pos: spos.extend(p.prev_tok.pos())
		}
	}
	if method_name == 'pkgconfig' {
		s := p.tok.lit
		p.check(.string)
		p.check(.rpar)
		return ast.ComptimeCall{
			scope: 0
			method_name: method_name
			args_var: s
			is_pkgconfig: true
			env_pos: spos
			pos: spos.extend(p.prev_tok.pos())
		}
	}
	literal_string_param := if is_html { '' } else { p.tok.lit }
	path_of_literal_string_param := literal_string_param.replace('/', os.path_separator)
	if !is_html {
		p.check(.string)
	}
	mut embed_compression_type := 'none'
	if is_embed_file {
		if p.tok.kind == .comma {
			p.check(.comma)
			p.check(.dot)
			embed_compression_type = p.check_name()
		}
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
				epath = os.real_path(os.join_path_single(os.dir(p.file_name), epath))
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
		p.register_auto_import('v.preludes.embed_file')
		if embed_compression_type == 'zlib'
			&& (p.pref.is_prod || 'debug_embed_file_in_prod' in p.pref.compile_defines) {
			p.register_auto_import('v.preludes.embed_file.zlib')
		}
		return ast.ComptimeCall{
			scope: 0
			is_embed: true
			embed_file: ast.EmbeddedFile{
				rpath: literal_string_param
				apath: epath
				compression_type: embed_compression_type
			}
			pos: start_pos.extend(p.prev_tok.pos())
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
	mut path := os.join_path_single(dir, fn_path_joined)
	path += '.html'
	path = os.real_path(path)
	if !is_html {
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
					scope: 0
					is_vweb: true
					method_name: method_name
					args_var: literal_string_param
					pos: start_pos.extend(p.prev_tok.pos())
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
	tmp_fn_name := p.cur_fn_name.replace('.', '__') + start_pos.pos.str()
	$if trace_comptime ? {
		println('>>> compiling comptime template file "$path" for $tmp_fn_name')
	}
	v_code := p.compile_template_file(path, tmp_fn_name)
	$if print_vweb_template_expansions ? {
		lines := v_code.split('\n')
		for i, line in lines {
			println('$path:${i + 1}: $line')
		}
	}
	$if trace_comptime ? {
		println('')
		println('>>> template for $path:')
		println(v_code)
		println('>>> end of template END')
		println('')
	}
	// the tmpl inherits all parent scopes. previous functionality was just to
	// inherit the scope from which the comptime call was made and no parents.
	// this is much simpler and allws access to globals. can be changed if needed.
	mut file := parse_comptime(tmpl_path, v_code, p.table, p.pref, p.scope)
	file.path = tmpl_path
	return ast.ComptimeCall{
		scope: 0
		is_vweb: true
		vweb_tmpl: file
		method_name: method_name
		args_var: literal_string_param
		pos: start_pos.extend(p.prev_tok.pos())
	}
}

fn (mut p Parser) comptime_for() ast.ComptimeFor {
	// p.comptime_for() handles these special forms:
	// $for method in App(methods) {
	// $for field in App(fields) {
	p.next()
	p.check(.key_for)
	var_pos := p.tok.pos()
	val_var := p.check_name()
	p.check(.key_in)
	mut typ_pos := p.tok.pos()
	lang := p.parse_language()
	typ := p.parse_any_type(lang, false, false)
	typ_pos = typ_pos.extend(p.prev_tok.pos())
	p.check(.dot)
	for_val := p.check_name()
	mut kind := ast.ComptimeForKind.methods
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
	} else if for_val == 'attributes' {
		p.scope.register(ast.Var{
			name: val_var
			typ: p.table.find_type_idx('StructAttribute')
			pos: var_pos
		})
		kind = .attributes
	} else {
		p.error_with_pos('unknown kind `$for_val`, available are: `methods`, `fields` or `attributes`',
			p.prev_tok.pos())
		return ast.ComptimeFor{}
	}
	spos := p.tok.pos()
	stmts := p.parse_block()
	p.close_scope()
	return ast.ComptimeFor{
		val_var: val_var
		stmts: stmts
		kind: kind
		typ: typ
		typ_pos: typ_pos
		pos: spos.extend(p.tok.pos())
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
		'@LINE' { token.AtKind.line_nr }
		'@COLUMN' { token.AtKind.column_nr }
		'@VHASH' { token.AtKind.vhash }
		'@VMOD_FILE' { token.AtKind.vmod_file }
		'@VEXE' { token.AtKind.vexe_path }
		'@VEXEROOT' { token.AtKind.vexeroot_path }
		'@VMODROOT' { token.AtKind.vmodroot_path }
		'@VROOT' { token.AtKind.vroot_path } // deprecated, use @VEXEROOT or @VMODROOT
		else { token.AtKind.unknown }
	}
	p.next()
	return ast.AtExpr{
		name: name
		pos: p.tok.pos()
		kind: kind
	}
}

fn (mut p Parser) comptime_selector(left ast.Expr) ast.Expr {
	p.check(.dollar)
	start_pos := p.prev_tok.pos()
	if p.peek_tok.kind == .lpar {
		method_pos := p.tok.pos()
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
			pos: start_pos.extend(p.prev_tok.pos())
		}
	}
	mut has_parens := false
	if p.tok.kind == .lpar {
		p.check(.lpar)
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
		left: left
		field_expr: expr
		pos: start_pos.extend(p.prev_tok.pos())
	}
}
