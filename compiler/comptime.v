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
		else {
			println('Supported platforms:')
			println(SupportedPlatforms)
			p.error('unknown platform `$name`')
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
	else if p.tok == .key_else {
		p.next()
		p.check(.lcbr)
		p.genln('#else')
		p.statements_no_rcbr()
		p.genln('#endif')
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
		mut pp := p.v.new_parser('.vwebtmpl.v', Pass.main)  
		if !p.pref.is_debug { 
			os.rm('.vwebtmpl.v') 
		} 
		pp.is_vweb = true 
		pp.cur_fn = p.cur_fn // give access too all variables in current function 
		pp.parse() 
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
		// No the right os? Skip!
		// mut ok := true
		if hash.contains('linux') && p.os != .linux {
			return
		}
		else if hash.contains('darwin') && p.os != .mac {  
			return
		}
		else if hash.contains('windows') && (p.os != .windows && p.os != .msvc) {
			return
		}
		// Remove "linux" etc from flag
		if flag.contains('linux') || flag.contains('darwin') || flag.contains('windows') {
			pos := flag.index(' ')
			flag = flag.right(pos)
		}
		has_vroot := flag.contains('@VROOT') 
		flag = flag.trim_space().replace('@VROOT', p.vroot)
		if p.table.flags.contains(flag) {
			return
		}
		p.log('adding flag "$flag"')
		// `@VROOT/thirdparty/glad/glad.o`, make sure it exists, otherwise build it 
		if has_vroot && flag.contains('.o') {
			if p.os == .msvc {
				build_thirdparty_obj_file_with_msvc(flag)
			} 
			else {
				build_thirdparty_obj_file(flag)
			}
		} 
		p.table.flags << flag 
		return
	}
	if hash.starts_with('include') {
		if p.first_pass() && !is_sig {
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
	for method in typ.methods {
		if method.typ != 'void' {
			continue 
		} 
		receiver := method.args[0] 
		amp := if receiver.is_mut { '&' } else { '' } 
		p.gen('if ( string_eq($var, _STR("$method.name")) ) ${typ.name}_$method.name($amp $p.expr_var.name);') 
	} 
	p.check(.lpar) 
	p.check(.rpar) 
} 

