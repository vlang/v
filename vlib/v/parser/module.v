// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

// return true if file being parsed imports `mod`
pub fn (p &Parser) known_import(mod string) bool {
	return mod in p.imports
}

fn (p &Parser) prepend_mod(name string) string {
	// println('prepend_mod() name=$name p.mod=$p.mod expr_mod=$p.expr_mod')
	if p.expr_mod != '' {
		return p.expr_mod + '.' + name
	}
	if p.builtin_mod {
		return name
	}
	return p.mod + '.' + name
}

fn (p &Parser) is_used_import(alias string) bool {
	return alias in p.used_imports
}

fn (mut p Parser) register_used_import(alias string) {
	if !p.is_used_import(alias) {
		p.used_imports << alias
	}
}

fn (mut p Parser) register_auto_import(alias string) {
	if alias !in p.imports {
		p.imports[alias] = alias
		p.table.imports << alias
		node := ast.Import{
			pos: p.tok.pos()
			mod: alias
			alias: alias
		}
		p.ast_imports << node
	}
	if alias !in p.auto_imports {
		p.auto_imports << alias
	}
	p.register_used_import(alias)
}

fn (mut p Parser) check_unused_imports() {
	if p.pref.is_repl || p.pref.is_fmt {
		// The REPL should be much more liberal, and should not warn about
		// unused imports, because they probably will be in the next few lines...
		// vfmt doesn't care about unused imports either
		return
	}
	for import_m in p.ast_imports {
		alias := import_m.alias
		mod := import_m.mod
		if !p.is_used_import(alias) {
			mod_alias := if alias == mod { alias } else { '${alias} (${mod})' }
			p.warn_with_pos("module '${mod_alias}' is imported but never used", import_m.mod_pos)
		}
	}
}
