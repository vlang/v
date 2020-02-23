// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import (
	strings
	os
	filepath
	v.pref
)
/*
	.vh generation logic.
	.vh files contain only function signatures, consts, and types.
	They are used together with pre-compiled modules.
*/


struct VhGen {
mut:
	i      int // token index
	consts strings.Builder
	fns    strings.Builder
	types  strings.Builder
	tokens []Token
}

// `mod` == "vlib/os"
fn generate_vh(mod string) {
	println('\n\n\n\nGenerating a V header file for module `$mod`')
	vexe := pref.vexe_path()
	full_mod_path := filepath.join(filepath.dir(vexe),mod)
	dir := if mod.starts_with('vlib') { '$compiler.v_modules_path${filepath.separator}$mod' } else { mod }
	path := dir + '.vh'
	pdir := dir.all_before_last(filepath.separator)
	if !os.is_dir(pdir) {
		os.mkdir_all(pdir)
		// os.mkdir(os.realpath(dir)) or { panic(err) }
	}
	mut out := os.create(path)or{
		panic(err)
	}
	mod_path := mod.replace('\\', '/')
	out.writeln('// $mod_path module header\n')
	mod_def := if mod_path.contains('/') { mod_path.all_after('/') } else { mod_path } // "os"
	out.writeln('module $mod_def\n')
	// Consts
	println(full_mod_path)
	vfiles := os.walk_ext(full_mod_path, '.v')
	// mut vfiles := os.ls(full_mod_path) or {
	// exit(1)
	// }
	filtered := vfiles.filter(it.ends_with('.v') && !it.ends_with('test.v') && !it.ends_with('_windows.v') && !it.ends_with('_win.v') && !it.ends_with('_lin.v') && !it.contains('${filepath.separator}examples') && !it.contains('_js.v') && !it.contains('_bare.v') && !it.contains('${filepath.separator}js')) // TODO merge once filter allows it
	// println('f:')
	// println(filtered)
	mut pref := &pref.Preferences {
		path: 'foo.v'
	}
	pref.fill_with_defaults()
	mut v := new_v(pref)
	// v.pref.generating_vh = true
	mut g := VhGen{
		consts: strings.new_builder(1000)
		fns: strings.new_builder(1000)
		types: strings.new_builder(1000)
	}
	for file in filtered {
		mut p := v.new_parser_from_file(file)
		p.scanner.is_vh = true
		p.parse(.decl)
		g.tokens = p.tokens
		g.i = 0
		for ; g.i < p.tokens.len; g.i++ {
			if !p.tokens[g.i].tok.is_decl() {
				continue
			}
			match g.tokens[g.i].tok {
				.key_fn {
					g.generate_fn()
				}
				.key_const {
					g.generate_const()
				}
				.key_struct {
					g.generate_type()
				}
				.key_type {
					g.generate_alias()
				}
				else {
				}}
		}
	}
	result := g.types.str() + g.consts.str() + g.fns.str().replace('\n\n\n', '\n').replace('\n\n', '\n')
	out.writeln(result.replace('[ ] ', '[]').replace('? ', '?'))
	out.close()
}

fn (g mut VhGen) generate_fn() {
	if g.i >= g.tokens.len - 2 {
		return
	}
	mut next := g.tokens[g.i + 1]
	if g.i > 0 && g.tokens[g.i - 1].tok != .key_pub {
		// Skip private fns
		// return ''
	}
	if next.tok == .name && next.lit == 'C' {
		// println('skipping C')
		return
	}
	// out.write('pub ')
	mut tok := g.tokens[g.i]
	for g.i < g.tokens.len - 1 && tok.tok != .lcbr {
		next = g.tokens[g.i + 1]
		g.fns.write(tok.str())
		if tok.tok != .lpar && !(next.tok in [.comma, .rpar]) {
			// No space after (), [], etc
			g.fns.write(' ')
		}
		g.i++
		tok = g.tokens[g.i]
	}
	g.fns.writeln('')
	// g.i--
}

fn (g mut VhGen) generate_alias() {
	mut tok := g.tokens[g.i]
	for g.i < g.tokens.len - 1 {
		g.types.write(tok.str())
		g.types.write(' ')
		if tok.line_nr != g.tokens[g.i + 1].line_nr {
			break
		}
		g.i++
		tok = g.tokens[g.i]
	}
	g.types.writeln('\n')
	// g.i--
}

fn (g mut VhGen) generate_const() {
	mut tok := g.tokens[g.i]
	for g.i < g.tokens.len && tok.tok != .rpar {
		g.consts.write(tok.str())
		g.consts.write(' ')
		if g.tokens[g.i + 2].tok == .assign {
			g.consts.write('\n\t')
		}
		g.i++
		tok = g.tokens[g.i]
	}
	g.consts.writeln('\n)')
	// g.i--
}

fn (g mut VhGen) generate_type() {
	// old := g.i
	mut tok := g.tokens[g.i]
	for g.i < g.tokens.len && tok.tok != .rcbr {
		g.types.write(tok.str())
		g.types.write(' ')
		if g.tokens[g.i + 1].line_nr != g.tokens[g.i].line_nr {
			g.types.write('\n\t')
		}
		g.i++
		tok = g.tokens[g.i]
	}
	g.types.writeln('\n}')
	// g.i = old
	// g.i--
}
