// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	vweb.tmpl  // for `$vweb_html()`
	os
	strings
)

fn (p mut Parser) comp_time() {
	p.check(.dollar)
	if p.tok == .key_if {
		p.check(.key_if)
		p.fspace()
		not := p.tok == .not
		if not {
			p.check(.not)
		}
		name := p.check_name()
		p.fspace()
		if name in supported_platforms {
			ifdef_name := os_name_to_ifdef(name)
			if not {
				p.genln('#ifndef $ifdef_name')
			}
			else {
				p.genln('#ifdef $ifdef_name')
			}
			p.check(.lcbr)
			os := os_from_string(name)
			if p.fileis('runtime.v') && os != p.os {
				// `$if os {` for a different target, skip everything inside
				// to avoid compilation errors (like including <windows.h>
				// on non-Windows systems)
				mut stack := 1
				for {
					if p.tok == .lcbr {
						stack++
					} else if p.tok == .rcbr {
						stack--
					}	
					if p.tok == .eof {
						break
					}	
					if stack <= 0 && p.tok == .rcbr {
						//p.warn('exiting $stack')
						p.next()
						break
					}	
					p.next()
				}	
			}	 else {
				p.statements_no_rcbr()
			}
			if ! (p.tok == .dollar && p.peek() == .key_else) {
				p.genln('#endif')
			}
		}
		else if name == 'debug' {
			p.genln('#ifdef VDEBUG')
			p.check(.lcbr)
			p.statements_no_rcbr()
			p.genln('#endif')
		}
		else if name == 'tinyc' {
			p.genln('#ifdef __TINYC__')
			p.check(.lcbr)
			p.statements_no_rcbr()
			p.genln('#endif')
		}
		else {
			println('Supported platforms:')
			println(supported_platforms)
			p.error('unknown platform `$name`')
		}
		if_returns := p.returns
		p.returns = false
		//p.gen('/* returns $p.returns */')
		if p.tok == .dollar && p.peek() == .key_else {
			p.next()
			p.next()
			p.check(.lcbr)
			p.genln('#else')
			p.statements_no_rcbr()
			p.genln('#endif')
			else_returns := p.returns
			p.returns = if_returns && else_returns
			//p.gen('/* returns $p.returns */')
		}
	}
	else if p.tok == .key_for {
		p.next()
		name := p.check_name()
		if name != 'field' {
			p.error('for field only')
		}
		p.check(.key_in)
		p.check_name()
		p.check(.dot)
		p.check_name()// fields
		p.check(.lcbr)
		// for p.tok != .rcbr && p.tok != .eof {
		res_name := p.check_name()
		println(res_name)
		p.check(.dot)
		p.check(.dollar)
		p.check(.name)
		p.check(.assign)
		p.cgen.start_tmp()
		p.bool_expression()
		val := p.cgen.end_tmp()
		println(val)
		p.check(.rcbr)
		// }
	}
	// $vweb.html()
	// Compile vweb html template to V code, parse that V code and embed the resulting V functions
	// that returns an html string
	else if p.tok == .name && p.lit == 'vweb' {
		mut path := p.cur_fn.name + '.html'
		if p.pref.is_debug {
			println('compiling tmpl $path')
		}
		if !os.file_exists(path) {
			// Can't find the template file in current directory,
			// try looking next to the vweb program, in case it's run with
			// v path/to/vweb_app.v
			path = os.dir(p.scanner.file_path) + '/' + path
			if !os.file_exists(path) {
				p.error('vweb HTML template "$path" not found')
			}
		}
		p.check(.name)  // skip `vweb.html()` TODO
		p.check(.dot)
		p.check(.name)
		p.check(.lpar)
		p.check(.rpar)
		v_code := tmpl.compile_template(path)
		if os.file_exists('.vwebtmpl.v') {
			os.rm('.vwebtmpl.v')
		}
		os.write_file('.vwebtmpl.v', v_code.clone()) // TODO don't need clone, compiler bug
		p.genln('')
		// Parse the function and embed resulting C code in current function so that
		// all variables are available.
		pos := p.cgen.lines.len - 1
		mut pp := p.v.new_parser_from_file('.vwebtmpl.v')
		if !p.pref.is_debug {
			os.rm('.vwebtmpl.v')
		}
		pp.is_vweb = true
		pp.set_current_fn( p.cur_fn ) // give access too all variables in current function
		pp.parse(.main)
		pp.v.add_parser(pp)
		tmpl_fn_body := p.cgen.lines.slice(pos + 2, p.cgen.lines.len).join('\n').clone()
		end_pos := tmpl_fn_body.last_index('Builder_str( sb )')  + 19 // TODO
		p.cgen.lines = p.cgen.lines.left(pos)
		p.genln('/////////////////// tmpl start')
		p.genln(tmpl_fn_body.left(end_pos))
		p.genln('/////////////////// tmpl end')
		// `app.vweb.html(index_view())`
		receiver := p.cur_fn.args[0]
		dot := if receiver.is_mut { '->' } else { '.' }
		p.genln('vweb__Context_html($receiver.name $dot vweb, tmpl_res)')
	}
	else {
		p.error('bad comptime expr')
	}
}

// #include, #flag, #v
fn (p mut Parser) chash() {
	hash := p.lit.trim_space()
	// println('chsh() file=$p.file  hash="$hash"')
	p.next()
	if hash.starts_with('flag ') {
		mut flag := hash.right(5)
		// expand `@VROOT` `@VMOD` to absolute path
		flag = flag.replace('@VROOT', p.vroot)
		flag = flag.replace('@VMOD', v_modules_path)
		//p.log('adding flag "$flag"')
		p.table.parse_cflag(flag, p.mod)
		return
	}
	if hash.starts_with('include') {
		if p.first_pass() && !p.is_vh {
			if p.file_pcguard.len != 0 {
				//println('p: $p.file_platform $p.file_pcguard')
				p.cgen.includes << '$p.file_pcguard\n#$hash\n#endif'
				return
			}
			p.cgen.includes << '#$hash'
			return
		}
	}
	// TODO remove after ui_mac.m is removed
	else if hash.contains('embed') {
		pos := hash.index('embed') + 5
		file := hash.right(pos)
		if p.pref.build_mode != BuildMode.default_mode {
			p.genln('#include $file')
		}
	}
	else if hash.contains('define') {
		// Move defines on top
		p.cgen.includes << '#$hash'
	}
	else if hash == 'v' {
		println('v script')
		//p.v_script = true
	}
	// Don't parse a non-JS V file (`#-js` flag)
	else if hash == '-js'  {
		$if js {
			for p.tok != .eof {
				p.next()
			}	
		} $else {
			p.next()
		}	
	}
	else {
		$if !js {
			if !p.can_chash {
				println('hash="$hash"')
				println(hash.starts_with('include'))
				p.error('bad token `#` (embedding C code is no longer supported)')
			}
		}
		p.genln(hash)
	}
}

// `user.$method()` (`method` is a string)
fn (p mut Parser) comptime_method_call(typ Type) {
	p.cgen.cur_line = ''
	p.check(.dollar)
	var := p.check_name()
	for i, method in typ.methods {
		if method.typ != 'void' {
			continue
		}
		receiver := method.args[0]
		amp := if receiver.is_mut { '&' } else { '' }
		if i > 0 {
			p.gen(' else ')
		}
		p.gen('if ( string_eq($var, _STR("$method.name")) ) ${typ.name}_$method.name($amp $p.expr_var.name);')
	}
	p.check(.lpar)
	p.check(.rpar)
	if p.tok == .key_orelse {
		p.check(.key_orelse)
		p.genln('else {')
		p.check(.lcbr)
		p.statements()
	}
}

fn (p mut Parser) gen_array_str(typ Type) {
	p.add_method(typ.name, Fn{
		name: 'str'
		typ: 'string'
		args: [Var{typ: typ.name, is_arg:true}]
		is_method: true
		is_public: true
		receiver_typ: typ.name
	})
	elm_type := typ.name.right(6)
	elm_type2 := p.table.find_type(elm_type)
	if p.typ_to_fmt(elm_type, 0) == '' &&
		!p.table.type_has_method(elm_type2, 'str') {
		p.error('cant print ${elm_type}[], unhandled print of ${elm_type}')
	}
	p.v.vgen_buf.writeln('
fn (a $typ.name) str() string {
	mut sb := strings.new_builder(a.len * 3)
	sb.write("[")
	for i, elm in a {
		sb.write(elm.str())
		if i < a.len - 1 {
			sb.write(", ")
		}
	}
	sb.write("]")
	return sb.str()
}
	')
	p.cgen.fns << 'string ${typ.name}_str();'
}

// `Foo { bar: 3, baz: 'hi' }` => '{ bar: 3, baz: "hi" }'
fn (p mut Parser) gen_struct_str(typ Type) {
	p.add_method(typ.name, Fn{
		name: 'str'
		typ: 'string'
		args: [Var{typ: typ.name, is_arg:true}]
		is_method: true
		is_public: true
		receiver_typ: typ.name
	})
	
	mut sb := strings.new_builder(typ.fields.len * 20)
	sb.writeln('fn (a $typ.name) str() string {\nreturn')
	sb.writeln("'{")
	for field in typ.fields {
		sb.writeln('\t$field.name: $' + 'a.${field.name}')
	}
	sb.writeln("}'")
	sb.writeln('}')
	p.v.vgen_buf.writeln(sb.str())
	// Need to manually add the definition to `fns` so that it stays
	// at the top of the file.
	// This function will get parsee by V after the main pass.
	p.cgen.fns << 'string ${typ.name}_str();'
}
