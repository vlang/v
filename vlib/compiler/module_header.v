// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import (
	strings
	os
)

/*
	.vh generation logic.
	.vh files contain only function signatures, consts, and types.
	They are used together with pre-compiled modules.
*/

// `mod` == "vlib/os"
fn generate_vh(mod string) {
	println('\n\n\n\nGenerating a V header file for module `$mod`')
	vexe := os.executable()
	full_mod_path := os.dir(vexe) + '/' + mod
	
	mod_path := mod.replace('.', os.path_separator)
	dir := if mod.starts_with('vlib') {
		'$compiler.v_modules_path${os.path_separator}$mod'
	} else {
		'$compiler.v_modules_path${os.path_separator}$mod'
	}
	path := dir + '.vh'
	pdir := dir.all_before_last(os.path_separator)
	if !os.dir_exists(pdir) {
		os.mkdir_all(pdir)
		// os.mkdir(os.realpath(dir))
	}
	out := os.create(path) or { panic(err) }
	// Consts
	println(full_mod_path)
	mut vfiles := os.walk_ext(full_mod_path, '.v')
	filtered := vfiles.filter(!it.ends_with('test.v') && !it.ends_with('_win.v')) // TODO merge once filter allows it
	println(filtered)
	mut v := new_v(['foo.v'])
	//v.pref.generating_vh = true
	for file in filtered {
		mut p := v.new_parser_from_file(file)
		p.scanner.is_vh = true
		println('kek $file')
		p.parse(.decl)
		for i, tok in p.tokens {
			if !p.tok.is_decl() {
				continue
			}	
			match tok.tok {
				TokenKind.key_fn {	generate_fn(out, p.tokens, i)	}
				TokenKind.key_const {	generate_const(out, p.tokens, i)	}
			}	
		}	
	}	
}

fn generate_fn(file os.File, tokens []Token, i int) {
	mut out := strings.new_builder(100)
	mut next := tokens[i+1]
	if tokens[i-1].tok != .key_pub {
		// Skip private fns
		return
	}
	
	if next.tok == .name && next.lit == 'C' {
		println('skipping C')
		return
	}	
	//out.write('pub ')
	mut tok := tokens[i]
	for i < tokens.len && tok.tok != .lcbr {
		next = tokens[i+1]
		
		out.write(tok.str())
		if tok.tok != .lpar  && !(next.tok in [.comma, .rpar]) {
			// No space after (), [], etc
			out.write(' ')
		}
		i++
		tok = tokens[i]
	}	
	file.writeln(out.str())
}	

fn generate_const(file os.File, tokens []Token, i int) {
	mut out := strings.new_builder(100)
	mut tok := tokens[i]
	for i < tokens.len && tok.tok != .rpar {
		out.write(tok.str())
		out.write(' ')
		if tokens[i+2].tok == .assign {
			out.write('\n\t')
			
		}	
		i++
		tok = tokens[i]
	}
	out.writeln('\n)')
	file.writeln(out.str())
}

/*
fn (v &V) generate_vh_old() {
	println('\n\n\n\nGenerating a V header file for module `$v.mod`')
	mod_path := v.mod.replace('.', os.path_separator)
	dir := if v.dir.starts_with('vlib') {
		'$v_modules_path${os.path_separator}$v.dir'
	} else {
		'$v_modules_path${os.path_separator}$mod_path'
	}
	path := dir + '.vh'
	pdir := dir.all_before_last(os.path_separator)
	if !os.dir_exists(pdir) {
		os.mkdir_all(pdir)
		// os.mkdir(os.realpath(dir))
	}
	file := os.create(path) or { panic(err) }
	// Consts
	mod_def := if v.mod.contains('.') { v.mod.all_after('.') } else { v.mod }
	file.writeln('// $v.mod module header \n')
	file.writeln('module $mod_def')
	file.writeln('// Consts')
	if v.table.consts.len > 0 {
		file.writeln('const (')
		for i, c in v.table.consts {
			if c.mod != v.mod {
				continue
			}	
			// println('$i $c.name')
			//if !c.name.contains('__') {
				//continue
			//}
			name := c.name.all_after('__')
			typ := v_type_str(c.typ)
			file.writeln('\t$name $typ')
		}	
		file.writeln(')\n')
		// Globals
		for var in v.table.consts {
			if var.mod != v.mod {
				continue
			}	
			if !var.is_global {
				continue
			}	
			name := var.name.all_after('__')
			typ := v_type_str(var.typ)
			file.writeln('__global $name $typ')
		}
		file.writeln('\n')
	}
	// Types
	file.writeln('// Types')
	for _, typ in v.table.typesmap {
		//println(typ.name)
		if typ.mod != v.mod && typ.mod != ''{ // int, string etc mod == ''
			// println('skipping type "$typ.name"')
			continue
		}	
		if typ.name.contains('_V_MulRet') {
			continue
		}	
		mut name := typ.name
		if typ.name.contains('__') {
			name = typ.name.all_after('__')
		}	
		// type alias
		if typ.parent != '' && typ.cat == .alias {
			parent := v_type_str(typ.parent)
			file.writeln('type $typ.name $parent')
		}
		if typ.cat in [TypeCategory.struct_, .c_struct] {
			c := if typ.is_c { 'C.' } else { '' }
			file.writeln('struct ${c}$name {')
			// Private fields
			for field in typ.fields {
				if field.access_mod == .public {
					continue
				}	
				field_type := v_type_str(field.typ).replace('*', '&')
				file.writeln('\t$field.name $field_type')
			}	
			//file.writeln('pub:')
			mut public_str := ''
			for field in typ.fields {
				if field.access_mod == .private {
					continue
				}	
				field_type := v_type_str(field.typ).replace('*', '&')
				public_str += '\t$field.name $field_type\n'
				//file.writeln('\t$field.name $field_type')
			}	
			if public_str != '' {
				file.writeln('pub:' + public_str)
			}	
			file.writeln('}\n')
		}
	}	
	// Functions & methods
	file.writeln('// Functions')
	// Public first
	mut fns := []Fn
	// TODO fns := v.table.fns.filter(.mod == v.mod)
	for _, f in v.table.fns {
		if f.mod == v.mod || f.mod == ''{
			fns << f
		}	 else {
			//println('skipping fn $f.name mod=$f.mod')
		}	
	}
	for _, f in fns {
		if !f.is_public {
			continue
		}	
		file.writeln(f.v_definition())
	}	
	// Private
	for _, f in fns {
		if f.is_public {
			continue
		}	
		file.writeln(f.v_definition())
	}	
	// Methods
	file.writeln('\n// Methods //////////////////')
	for _, typ in v.table.typesmap {
		if typ.mod != v.mod && !(v.mod == 'builtin' && typ.mod == '') {
			// println('skipping method typ $typ.name mod=$typ.mod')
			continue
		}	
		for method in typ.methods {
			file.writeln(method.v_definition())
		}	
	}	
	file.close()
	
	/*
	for i, p in v.parsers {
		if v.parsers[i].vh_lines.len > 0 {
			os.write_file(p.file_name +'.vh', v.parsers[i].vh_lines.join('\n'))
		}
	}	
	*/
}	
*/

