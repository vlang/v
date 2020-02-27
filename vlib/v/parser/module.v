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
