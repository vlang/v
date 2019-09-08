// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	vweb.tmpl  // for `$vweb_html()`
	os
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
		if name in SupportedPlatforms {
			ifdef_name := os_name_to_ifdef(name)
			if not {
				p.genln('#ifndef $ifdef_name')
			}
			else {
				p.genln('#ifdef $ifdef_name')
			}
			p.check(.lcbr)
			p.statements_no_rcbr()
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
		else {
			println('Supported platforms:')
			println(SupportedPlatforms)
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
		path := p.cur_fn.name + '.html'
		if p.pref.is_debug {
			println('compiling tmpl $path')
		}
		if !os.file_exists(path) {
			p.error('vweb HTML template "$path" not found')
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
		mut pp := p.v.new_parser('.vwebtmpl.v')
		if !p.pref.is_debug {
			os.rm('.vwebtmpl.v')
		}
		pp.is_vweb = true
		pp.set_current_fn( p.cur_fn ) // give access too all variables in current function
		pp.parse(.main)
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
	// println('chsh() file=$p.file  is_sig=${p.is_sig()} hash="$hash"')
	p.next()
	is_sig := p.is_sig()
	if hash.starts_with('flag ') {
		mut flag := hash.right(5)
		// expand `@VROOT` `@VMOD` to absolute path
		flag = flag.replace('@VROOT', p.vroot)
		flag = flag.replace('@VMOD', ModPath)
		p.log('adding flag "$flag"')
		p.table.parse_cflag(flag)
		return
	}
	if hash.starts_with('include') {
		if p.first_pass() && !is_sig {
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
	else {
		if !p.can_chash {
			p.error('bad token `#` (embedding C code is no longer supported)')
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
	//println('gen array str "$typ.name"')
	p.table.add_method(typ.name, Fn{
		name: 'str',
		typ: 'string'
		args: [Var{typ: typ.name, is_arg:true}]
		is_method: true
		is_public: true
		receiver_typ: typ.name
	})
	/*
	tt := p.table.find_type(typ.name)
	for m in tt.methods {
		println(m.name + ' ' + m.typ)
		}
		*/
	t := typ.name
	elm_type := t.right(6)
	elm_type2 := p.table.find_type(elm_type)
	if p.typ_to_fmt(elm_type, 0) == '' &&
		!p.table.type_has_method(elm_type2, 'str') {
		p.error('cant print ${elm_type}[], unhandled print of ${elm_type}')
	}
	p.cgen.fns << '
string ${t}_str($t a) {
	strings__Builder sb = strings__new_builder(a.len * 3);
	strings__Builder_write(&sb, tos2("[")) ;
	for (int i = 0; i < a.len; i++) {
		strings__Builder_write(&sb, ${elm_type}_str( (($elm_type *) a.data)[i]));

	if (i < a.len - 1) {
		strings__Builder_write(&sb, tos2(", ")) ;
		
	}
}
strings__Builder_write(&sb, tos2("]")) ;
return strings__Builder_str(sb);
} '
}

fn (p mut Parser) parse_t() {

}



