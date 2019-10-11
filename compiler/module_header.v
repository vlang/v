// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	strings
	os
)

/*
	.vh generation logic.
	.vh files contain only function signatures, consts, and types.
	They are used together with pre-compiled modules.
*/

// "fn foo(a int) string"
fn (f &Fn) v_definition() string {
	//t :=time.ticks()
	mut sb := strings.new_builder(100)
	if f.is_public {
		sb.write('pub ')
	}	
	sb.write('fn ')
	if f.is_c {
		sb.write('C.')
	}	
	if f.is_method {
		recv := f.args[0]
		typ := v_type_str(recv.typ).replace('*', '')
		mut mu := if recv.is_mut { 'mut' } else { '' }
		if recv.ref {
			mu = '&'
		}	
		sb.write('($recv.name $mu $typ) ')
	}	
	if f.name.contains('__') {
		sb.write(f.name.all_after('__') + '(')
	} else {
		sb.write('$f.name(')
	}
	for i, arg in f.args {
		if i == 0 && f.is_method { // skip the receiver
			continue
		}	
		typ := v_type_str(arg.typ).replace('*', '&')
		if arg.name == '' {
			sb.write(typ)
		}	 else {
			sb.write('$arg.name $typ')
		}
		if i != f.args.len - 1 {
			sb.write(', ')
		}
	}
	sb.write(')')
	if f.typ != 'void' {
		typ := v_type_str(f.typ).replace('*', '&')
		sb.write(' ')
		sb.write(typ)
		sb.writeln(' ')
	}
	//println('ms: ${time.ticks() - t}')
	return sb.str()
}

fn v_type_str(typ_ string) string {
	mut typ := if typ_.ends_with('*') {
		'*' + typ_.left(typ_.len - 1)
	} else {
		typ_
	}	
	typ = typ.replace('Option_', '?')
	if typ.contains('_V_MulRet') {
		words := typ.replace('_V_MulRet_', '').split('_V_')
		typ = '('
		for i in 0 .. words.len {
			typ += words[i]
			if i != words.len - 1 {
				typ += ','
			}	
		}	
		typ += ')'
		return typ
	}	
	//println('"$typ"')
	if typ == '*void' {
		return 'voidptr'
	}	
	if typ == '*byte' {
		return 'byteptr'
	}	
	if typ.starts_with('array_') {
		return '[]' + typ.right(6)
	}	
	if typ.contains('__') {
		opt := typ.starts_with('?')
		typ = typ.all_after('__')
		if opt {
			typ = '?' + typ
		}	
	}	
	return typ
}	

fn (v &V) generate_vh() {
	println('\n\n\n\nGenerating a V header file for module `$v.mod`')
	mod_path := v.mod.replace('.', os.PathSeparator)
	dir := '$v_modules_path${os.PathSeparator}$mod_path'
	path := dir + '.vh'
	if !os.dir_exists(dir) {
		// create recursive
		mut mkpath := v_modules_path
		for subdir in mod_path.split(os.PathSeparator) {
			mkpath += os.PathSeparator + subdir
			if !os.dir_exists(mkpath) {
				os.mkdir(mkpath)
			}
		}
		// os.mkdir(os.realpath(dir))
	}
	println(path)
	
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
			println('$i $c.name')
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
			//println('skipping type "$typ.name"')
			continue
		}	
		if typ.name.contains('_V_MulRet') {
			continue
		}	
		mut name := typ.name
		if typ.name.contains('__') {
			name = typ.name.all_after('__')
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
			println('skipping method typ $typ.name mod=$typ.mod')
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

