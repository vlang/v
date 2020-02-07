// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

fn (p mut Parser) inline_asm() {
	if !p.inside_unsafe {
		p.error('asm() needs to be run inside `unsafe {}`')
	}
	p.next()
	p.check(.lcbr)
	s := p.check_string()
	p.genln('asm("$s"')
	for p.tok == .str {
		p.genln('"$p.lit"')
		p.next()
	}
	for p.tok == .colon {
		p.next()
		arg := p.check_string()
		p.gen(': "$arg"')
		if p.tok == .lpar {
			p.next()
			var_name := p.check_name()
			if !p.known_var(var_name) {
				p.error('unknown variable `$var_name`')
			}
			p.check(.rpar)
			p.genln('($var_name)')
		}
	}
	p.genln(');')
	p.check(.rcbr)
}

