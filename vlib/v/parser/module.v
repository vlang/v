// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

// return true if file being parsed imports `mod`
pub fn (p &Parser) known_import(mod string) bool {
	return mod in p.imports
}

fn (p &Parser) prepend_mod(name string) string {
	if p.expr_mod != '' {
		return p.expr_mod + '.' + name
	}
	if p.builtin_mod || p.mod == 'main' {
		return name
	}
	return '${p.mod}.$name'
}

fn (p &Parser) is_used_import(alias string) bool {
	return alias in p.used_imports
}

fn (mut p Parser) register_used_import(alias string) {
	if alias !in p.used_imports {
		p.used_imports << alias
	}
}

fn (mut p Parser) check_unused_imports() {
	mut output := ''
	for alias, mod in p.imports {
		if !p.is_used_import(alias) {
			mod_alias := if alias == mod { alias } else { '$alias ($mod)' }
			output += '\n * $mod_alias'
		}
	}
	if output == '' {
		return
	}
	if p.pref.is_repl {
		// The REPL should be much more liberal, and should not warn about
		// unused imports, because they probably will be in the next few lines...
		return
	}
	eprintln('`$p.file_name` warning: the following imports were never used: $output')
}
