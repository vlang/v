// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	strings
	os
)

/* .vh generation logic.
 .vh files contains only function signatures, consts, and types.
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
		typ := v_type_str(recv.typ)
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
		typ := v_type_str(arg.typ)
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
		typ := v_type_str(f.typ)
		sb.write(' ')
		sb.write(typ)
		sb.writeln(' ')
	}
	//println('ms: ${time.ticks() - t}')
	return sb.str()
}

fn v_type_str(typ_ string) string {
	typ := if typ_.ends_with('*') {
		'*' + typ_.left(typ_.len - 1)
	} else {
		typ_
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
		return typ.all_after('__')
	}	
	return typ.replace('Option_', '?')
}	

fn (v &V) generate_vh() {
	println('Generating a V header file for module `$v.mod`')
	path := v_modules_path + v.mod + '.vh'
	println(path)
	
	file := os.create(path) or { panic(err) }
	// Consts
	file.writeln('// $v.mod module header \n')
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
		if typ.mod != v.mod {
			continue
		}	
		if typ.name.contains('__') {
			continue
		}	
		if typ.cat == .struct_ {
			file.writeln('struct $typ.name {')
			// Private fields
			for field in typ.fields {
				if field.access_mod == .public {
					continue
				}	
				field_type := v_type_str(field.typ)
				file.writeln('\t$field.name $field_type')
			}	
			file.writeln('pub:')
			for field in typ.fields {
				if field.access_mod == .private {
					continue
				}	
				field_type := v_type_str(field.typ)
				file.writeln('\t$field.name $field_type')
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
		if f.mod == v.mod {
			fns << f
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
		if typ.mod != v.mod {
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

