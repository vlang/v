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
	mod_def := if mod.contains('/') { mod.all_after('/') } else { mod } // "os"
	out.writeln('// $mod module header \n')
	out.writeln('module $mod_def\n')
	// Consts
	println(full_mod_path)
	mut vfiles := os.walk_ext(full_mod_path, '.v')
	//mut vfiles := os.ls(full_mod_path) or {
		//exit(1)
	//}	
	filtered := vfiles.filter(it.ends_with('.v') && !it.ends_with('test.v') &&
		!it.ends_with('_windows.v') && !it.ends_with('_win.v') &&
		!it.ends_with('_lin.v') &&
		!it.contains('/examples') &&
		!it.contains('/js')) // TODO merge once filter allows it
	println(filtered)
	mut v := new_v(['foo.v'])
	//v.pref.generating_vh = true
	mut consts := strings.new_builder(100)
	mut fns := strings.new_builder(100)
	mut types := strings.new_builder(100)
	for file in filtered {
		mut p := v.new_parser_from_file(file)
		p.scanner.is_vh = true
		p.parse(.decl)
		for i, tok in p.tokens {
			if !p.tok.is_decl() {
				continue
			}	
			match tok.tok {
				.key_fn     {	fns.writeln(generate_fn(p.tokens, i))	}
				.key_const  {	consts.writeln(generate_const(p.tokens, i))	}
				.key_struct {	types.writeln(generate_type(p.tokens, i))	}
				.key_type   {	types.writeln(generate_alias(p.tokens, i))	}
			}	
		}	
	}	
	result := consts.str() + types.str() +
		fns.str().replace('\n\n\n', '\n').replace('\n\n', '\n')
	
	out.writeln(result.replace('[ ] ', '[]').replace('? ', '?'))
	out.close()
}

fn generate_fn(tokens []Token, i int) string {
	mut out := strings.new_builder(100)
	mut next := tokens[i+1]
	if tokens[i-1].tok != .key_pub {
		// Skip private fns
		return ''
	}
	
	if next.tok == .name && next.lit == 'C' {
		println('skipping C')
		return ''
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
	return out.str()
}	

fn generate_alias(tokens []Token, i int) string {
	mut out := strings.new_builder(100)
	mut tok := tokens[i]
	for i < tokens.len-1 {
		out.write(tok.str())
		out.write(' ')
		if tok.line_nr != tokens[i+1].line_nr {
			break
		}	
		i++
		tok = tokens[i]
	}
	out.writeln('\n')
	return out.str()
}

fn generate_const(tokens []Token, i int) string {
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
	return out.str()
}

fn generate_type(tokens []Token, i int) string {
	mut out := strings.new_builder(100)
	mut tok := tokens[i]
	for i < tokens.len && tok.tok != .rcbr {
		out.write(tok.str())
		out.write(' ')
		if tokens[i+1].line_nr != tokens[i].line_nr {
			out.write('\n\t')
		}	
		i++
		tok = tokens[i]
	}
	out.writeln('\n}')
	return out.str()
}


