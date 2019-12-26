// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import (
	vweb.tmpl // for `$vweb_html()`
	os
	strings
	filepath
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
			if name == 'mac' {
				p.warn('use `macos` instead of `mac`')
			}
			if not {
				p.genln('#ifndef $ifdef_name')
			}
			else {
				p.genln('#ifdef $ifdef_name')
			}
			p.check(.lcbr)
			os := os_from_string(name)
			if ((!not && os != p.os) || (not && os == p.os)) && !p.scanner.is_fmt && !p.pref.output_cross_c {
				// `$if os {` for a different target, skip everything inside
				// to avoid compilation errors (like including <windows.h>
				// on non-Windows systems)
				mut stack := 1
				for {
					if p.tok == .key_return {
						p.returns = true
					}
					if p.tok == .lcbr {
						stack++
					}
					else if p.tok == .rcbr {
						stack--
					}
					if p.tok == .eof {
						break
					}
					if stack <= 0 && p.tok == .rcbr {
						// p.warn('exiting $stack')
						p.next()
						break
					}
					p.next()
				}
			}
			else {
				p.statements_no_rcbr()
			}
			if !(p.tok == .dollar && p.peek() == .key_else) {
				p.genln('#endif')
			}
		}
		else if name == 'x64' {
			p.comptime_if_block('TARGET_IS_64BIT')
		}
		else if name == 'x32' {
			p.comptime_if_block('TARGET_IS_32BIT')
		}
		else if name == 'big_endian' {
			p.comptime_if_block('TARGET_ORDER_IS_BIG')
		}
		else if name == 'little_endian' {
			p.comptime_if_block('TARGET_ORDER_IS_LITTLE')
		}
		else if name == 'debug' {
			p.comptime_if_block('VDEBUG')
		}
		else if name == 'prealloc' {
			p.comptime_if_block('VPREALLOC')
		}
		else if name == 'tinyc' {
			p.comptime_if_block('__TINYC__')
		}
		else if name == 'glibc' {
			p.comptime_if_block('__GLIBC__')
		}
		else if name == 'mingw' {
			p.comptime_if_block('__MINGW32__')
		}
		else if name == 'msvc' {
			p.comptime_if_block('_MSC_VER')
		}
		else if name == 'clang' {
			p.comptime_if_block('__clang__')
		}
		else if p.v.compile_defines_all.len > 0 && name in p.v.compile_defines_all {
			p.comptime_if_block('CUSTOM_DEFINE_${name}')
		}
		else {
			println('Supported platforms:')
			println(supported_platforms)
			p.error('unknown platform `$name`')
		}
		if_returns := p.returns
		p.returns = false
		// p.gen('/* returns $p.returns */')
		if p.tok == .dollar && p.peek() == .key_else {
			p.fspace()
			p.next()
			p.next()
			p.fspace() // spaces before and after $else
			p.check(.lcbr)
			p.genln('#else')
			p.statements_no_rcbr()
			p.genln('#endif')
			else_returns := p.returns
			p.returns = if_returns && else_returns
			// p.gen('/* returns $p.returns */')
		}
		else if p.tok == .key_else {
			p.error('use `$' + 'else` instead of `else` in comptime if statements')
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
		p.check_name() // fields
		p.check(.lcbr)
		// for p.tok != .rcbr && p.tok != .eof {
		res_name := p.check_name()
		println(res_name)
		p.check(.dot)
		p.check(.dollar)
		p.check(.name)
		p.check(.assign)
		_,val := p.tmp_expr()
		// p.bool_expression()
		// val := p.cgen.end_tmp()
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
		if !os.exists(path) {
			// Can't find the template file in current directory,
			// try looking next to the vweb program, in case it's run with
			// v path/to/vweb_app.v
			path = filepath.dir(p.scanner.file_path) + '/' + path
			if !os.exists(path) {
				p.error('vweb HTML template "$path" not found')
			}
		}
		p.check(.name) // skip `vweb.html()` TODO
		p.check(.dot)
		p.check(.name)
		p.check(.lpar)
		p.check(.rpar)
		v_code := tmpl.compile_template(path)
		if p.pref.is_verbose {
			println('vweb template:')
			println(v_code)
		}
		is_strings_imorted := p.import_table.known_import('strings')
		if !is_strings_imorted {
			p.register_import('strings', 0) // used by v_code
		}
		p.import_table.register_used_import('strings')
		p.genln('/////////////////// tmpl start')
		p.scanner.file_path = path
		p.scanner.line_nr = 0
		p.statements_from_text(v_code, false)
		p.genln('/////////////////// tmpl end')
		receiver := p.cur_fn.args[0]
		dot := if receiver.is_mut || receiver.ptr || receiver.typ.ends_with('*') { '->' } else { '.' }
		p.genln('vweb__Context_html( & $receiver.name /*!*/$dot vweb, tmpl_res)')
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
	p.fgen_nl()
	if hash.starts_with('flag ') {
		if p.first_pass() {
			mut flag := hash[5..]
			// expand `@VROOT` `@VMOD` to absolute path
			flag = flag.replace('@VROOT', p.vroot)
			flag = flag.replace('@VPATH', p.pref.vpath)
			flag = flag.replace('@VLIB_PATH', p.pref.vlib_path)
			flag = flag.replace('@VMOD', v_modules_path)
			// p.log('adding flag "$flag"')
			_ = p.table.parse_cflag(flag, p.mod, p.v.compile_defines_all ) or {
				p.error_with_token_index(err, p.cur_tok_index() - 1)
				return
			}
		}
		return
	}
	if hash.starts_with('include') {
		if p.first_pass() && !p.is_vh {
			/*
			if !p.pref.building_v && !p.fileis('vlib') {
				p.warn('C #includes will soon be removed from the language' +
				'\ndefine the C structs and functions in V')
			}
			*/
			if p.file_pcguard.len != 0 {
				// println('p: $p.file_platform $p.file_pcguard')
				p.cgen.includes << '$p.file_pcguard\n#$hash\n#endif'
				return
			}
			p.cgen.includes << '#$hash'
			return
		}
	}
	// TODO remove after ui_mac.m is removed
	else if hash.contains('embed') {
		pos := hash.index('embed') or {
			return
		}
		file := hash[pos + 5..]
		// if p.pref.build_mode != .default_mode {
		p.genln('#include $file')
		// }
	}
	else if hash.contains('define') {
		// Move defines on top
		if p.first_pass() {
			p.cgen.includes << '#$hash'
		}
	}
	// // Don't parse a non-JS V file (`#-js` flag)
	else if hash == '-js' {
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
				if hash.starts_with('include') {
					println('include')
				}
				else {
				}
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
	mut j := 0
	for method in typ.methods {
		if method.typ != 'void' {
			continue
		}
		receiver := method.args[0]
		if !p.expr_var.ptr {
			p.error('`$p.expr_var.name` needs to be a reference')
		}
		amp := if receiver.is_mut && !p.expr_var.ptr { '&' } else { '' }
		if j > 0 {
			p.gen(' else ')
		}
		p.genln('if ( string_eq($var, _STR("$method.name")) ) ' + '${typ.name}_$method.name ($amp $p.expr_var.name);')
		j++
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
	if typ.has_method('str') {
		return
	}
	p.add_method(typ.name, Fn{
		name: 'str'
		typ: 'string'
		args: [Var{
			typ: typ.name
			is_arg: true
		}]
		is_method: true
		is_public: true
		receiver_typ: typ.name
	})
	elm_type := parse_pointer(typ.name[6..])
	elm_type2 := p.table.find_type(elm_type)
	is_array := elm_type.starts_with('array_')
	if is_array {
		p.gen_array_str(elm_type2)
	}
	else if p.typ_to_fmt(elm_type, 0) == '' && !p.table.type_has_method(elm_type2, 'str') {
		p.error('cant print ${elm_type}[], unhandled print of ${elm_type}')
	}
	p.v.vgen_buf.writeln('
pub fn (a $typ.name) str() string {
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
		args: [Var{
			typ: typ.name
			is_arg: true
		}]
		is_method: true
		is_public: true
		receiver_typ: typ.name
	})
	mut sb := strings.new_builder(typ.fields.len * 20)
	sb.writeln('pub fn (a $typ.name) str() string {\nreturn')
	sb.writeln("'{")
	for field in typ.fields {
		sb.writeln('\t$field.name: $' + 'a.${field.name}')
	}
	sb.writeln("}'")
	sb.writeln('}')
	p.v.vgen_buf.writeln(sb.str())
	// Need to manually add the definition to `fns` so that it stays
	// at the top of the file.
	// This function will get parsed by V after the main pass.
	p.cgen.fns << 'string ${typ.name}_str();'
}

fn (p mut Parser) gen_varg_str(typ Type) {
	elm_type := typ.name[5..]
	elm_type2 := p.table.find_type(elm_type)
	is_array := elm_type.starts_with('array_')
	if is_array {
		p.gen_array_str(elm_type2)
	}
	else if elm_type2.cat == .struct_ {
		p.gen_struct_str(elm_type2)
	}
	p.v.vgen_buf.writeln('
pub fn (a $typ.name) str() string {
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
}')
	p.cgen.fns << 'string ${typ.name}_str();'
}

fn (p mut Parser) gen_array_filter(str_typ string, method_ph int) {
	/*
		// V
		a := [1,2,3,4]
		b := a.filter(it % 2 == 0)

		// C
		array_int a = ...;
		array_int tmp2 = new_array(0, 4, 4);
		for (int i = 0; i < a.len; i++) {
			int it = ((int*)a.data)[i];
			if (it % 2 == 0) array_push(&tmp2, &it);
		}
		array_int b = tmp2;
	*/
	val_type := parse_pointer(str_typ[6..])
	p.open_scope()
	p.register_var(Var{
		name: 'it'
		typ: val_type
	})
	p.next()
	p.check(.lpar)
	p.cgen.resetln('')
	tmp := p.get_tmp()
	a := p.expr_var.name
	p.cgen.set_placeholder(method_ph, '\n$str_typ $tmp = new_array(0, $a .len,sizeof($val_type));\n')
	p.genln('for (int i = 0; i < ${a}.len; i++) {')
	p.genln('$val_type it = (($val_type*)${a}.data)[i];')
	p.gen('if (')
	p.bool_expression()
	p.genln(') array_push(&$tmp, &it);')
	// p.genln(') array_push(&$tmp, &((($val_type*)${a}.data)[i]));')
	// p.genln(') array_push(&$tmp, ${a}.data + i * ${a}.element_size);')
	p.genln('}')
	p.gen(tmp) // TODO why does this `gen()` work?
	p.check(.rpar)
	p.close_scope()
}

fn (p mut Parser) gen_array_map(str_typ string, method_ph int) string {
	/*
		// V
		a := [1,2,3,4]
		b := a.map(it * 2)

		// C
		array_int a = ...;
		array_int tmp2 = new_array(0, 4, 4);
		for (int i = 0; i < a.len; i++) {
			int it = ((int*)a.data)[i];
			_PUSH(tmp2, it * 2, tmp3, int)
		}
		array_int b = tmp2;
	*/
	val_type := parse_pointer(str_typ[6..])
	p.open_scope()
	p.register_var(Var{
		name: 'it'
		typ: val_type
	})
	p.next()
	p.check(.lpar)
	p.cgen.resetln('')
	tmp := p.get_tmp()
	tmp_elm := p.get_tmp()
	a := p.expr_var.name
	map_type,expr := p.tmp_expr()
	p.cgen.set_placeholder(method_ph, '\narray $tmp = new_array(0, $a .len, ' + 'sizeof($map_type));\n')
	p.genln('for (int i = 0; i < ${a}.len; i++) {')
	p.genln('$val_type it = (($val_type*)${a}.data)[i];')
	p.genln('_PUSH(&$tmp, $expr, $tmp_elm, $map_type)')
	p.genln('}')
	p.gen(tmp) // TODO why does this `gen()` work?
	p.check(.rpar)
	p.close_scope()
	return 'array_' + stringify_pointer(map_type)
}

fn (p mut Parser) comptime_if_block(name string) {
	p.genln('#ifdef $name')
	p.check(.lcbr)
	p.statements_no_rcbr()
	if !(p.tok == .dollar && p.peek() == .key_else) {
		p.genln('#endif')
	}
}

fn (p mut Parser) gen_enum_flag_methods(typ mut Type) {
	for method in ['set', 'clear', 'toggle', 'has'] {
		typ.methods << Fn{
			name: method
			typ: if method == 'has' { 'bool' } else { 'void' }
			args: [Var{
				typ: typ.name
				is_mut: true
				is_arg: true
			}, Var{
				typ: typ.name
				is_arg: true
			}]
			is_method: true
			is_public: true
			receiver_typ: typ.name
		}
	}
	p.v.vgen_buf.writeln('
pub fn (e mut $typ.name) set(flag $typ.name)      { *e = int(*e) | (1 << int(flag)) }
pub fn (e mut $typ.name) clear(flag $typ.name)    { *e = int(*e) &~ (1 << int(flag)) }
pub fn (e mut $typ.name) toggle(flag $typ.name)   { *e = int(*e) ^ (1 << int(flag)) }
pub fn (e &$typ.name) has(flag $typ.name) bool { return int(*e)&(1 << int(flag)) != 0 }')
	p.cgen.fns << 'void ${typ.name}_set($typ.name *e, $typ.name flag);'
	p.cgen.fns << 'void ${typ.name}_clear($typ.name *e, $typ.name flag);'
	p.cgen.fns << 'void ${typ.name}_toggle($typ.name *e, $typ.name flag);'
	p.cgen.fns << 'bool ${typ.name}_has($typ.name *e, $typ.name flag);'
}

