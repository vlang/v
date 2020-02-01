// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import (
	strings
)

fn (p mut Parser) get_type2() Type {
	mut mul := false
	mut nr_muls := 0
	mut typ := ''
	cat := TypeCategory.struct_
	// fn type
	if p.tok == .key_fn {
		mut f := Fn{
			name: '_'
			mod: p.mod
		}
		p.next()
		line_nr := p.scanner.line_nr
		p.fn_args(mut f)
		// Same line, it's a return type
		if p.scanner.line_nr == line_nr {
			if p.tok in [.name, .mul, .amp, .lsbr, .question, .lpar] {
				f.typ = p.get_type()
			}
			else {
				f.typ = 'void'
			}
			// println('fn return typ=$f.typ')
		}
		else {
			f.typ = 'void'
		}
		// Register anon fn type
		fn_typ := Type{
			name: f.typ_str() // 'fn (int, int) string'

			mod: p.mod
			func: f
			cat: .func
		}
		p.table.register_type(fn_typ)
		return fn_typ
	}
	is_question := p.tok == .question
	if is_question {
		p.check(.question)
	}

	// multiple returns
	if p.tok == .lpar {
		// p.warn('`()` are no longer necessary in multiple returns' +
		// '\nuse `fn foo() int, int {` instead of `fn foo() (int, int) {`')
		// if p.inside_tuple {p.error('unexpected (')}
		// p.inside_tuple = true
		p.check(.lpar)
		mut types := []string
		for {
			types << p.get_type()
			if p.tok != .comma {
				break
			}
			p.check(.comma)
			p.fspace()
		}
		p.check(.rpar)
		// p.inside_tuple = false
		typ = p.register_multi_return_stuct(types)
		if is_question {
			typ = stringify_pointer(typ)
			typ = 'Option_$typ'
			p.table.register_type_with_parent(typ, 'Option')
		}
		return Type{
			name: typ
			mod: p.mod
			cat: cat
		}
	}

	// arrays ([]int)
	mut arr_level := 0
	for p.tok == .lsbr {
		p.check(.lsbr)
		// [10]int
		if p.tok == .number || (p.tok == .name && !p.inside_const) {
			if p.tok == .name {
				typ += '[${p.mod}__$p.lit]'
			}
			else {
				typ += '[$p.lit]'
			}
			p.next()
		}
		else {
			arr_level++
		}
		p.check(.rsbr)
	}
	// map[string]int
	if !p.builtin_mod && p.tok == .name && p.lit == 'map' {
		p.next()
		p.check(.lsbr)
		key_type := p.check_name()
		if key_type != 'string' {
			p.error('maps only support string keys for now')
		}
		p.check(.rsbr)
		val_type := stringify_pointer(p.get_type()) // p.check_name()
		typ = 'map_$val_type'
		p.register_map(typ)
		return Type{
			name: typ
		}
	}
	// ptr/ref
	mut warn := false
	for p.tok == .mul {
		if p.first_pass() {
			warn = true
		}
		mul = true
		nr_muls++
		p.check(.mul)
	}
	if p.tok == .amp {
		mul = true
		nr_muls++
		p.check(.amp)
	}
	// generic type check
	ti := p.generic_dispatch.inst
	if p.lit in ti.keys() {
		typ += ti[p.lit]
	}
	else {
		typ += p.lit
	}
	// C.Struct import
	if p.lit == 'C' && p.peek() == .dot {
		p.next()
		p.check(.dot)
		typ = p.lit
	}
	else {
		if warn && p.mod != 'ui' {
			p.warn('use `&Foo` instead of `*Foo`')
		}
		// Module specified? (e.g. gx.Image)
		if p.peek() == .dot {
			// try resolve full submodule
			if !p.builtin_mod && p.import_table.known_alias(typ) {
				mod := p.import_table.resolve_alias(typ)
				typ = if mod.contains('.') {
					mod_gen_name(mod)
				} else {
					mod
				}
			}
			p.next()
			p.check(.dot)
			typ += '__$p.lit'
		}
		mut t := p.table.find_type(typ)
		// "typ" not found? try "mod__typ"
		if t.name == '' && !p.builtin_mod {
			// && !p.first_pass() {
			if !typ.contains('array_') && p.mod != 'main' && !typ.contains('__') && !typ.starts_with('[') {
				typ = p.prepend_mod(typ)
			}
			t = p.table.find_type(typ)
			if t.name == '' && !p.pref.translated && !p.first_pass() && !typ.starts_with('[') {
				//println('get_type() bad type')
				// println('all registered types:')
				// for q in p.table.types {
				// println(q.name)
				// }
				mut t_suggest,tc_suggest := p.table.find_misspelled_type(typ, p, 0.50)
				if t_suggest.len > 0 {
					t_suggest = '. did you mean: ($tc_suggest) `$t_suggest`'
				}
				econtext := if p.pref.is_debug { '('+@FILE+':'+@LINE+')' } else {''}
				p.error('unknown type `$typ`$t_suggest $econtext')
			}
		}
		else if !t.is_public && t.mod != p.mod && !p.is_vgen && t.name != '' && !p.first_pass() {
			p.error('type `$t.name` is private')
		}
	}
	if typ == 'void' {
		p.error('unknown type `$typ`')
	}
	if mul {
		typ += strings.repeat(`*`, nr_muls)
	}
	// Register an []array type
	if arr_level > 0 {
		// p.log('ARR TYPE="$typ" run=$p.pass')
		// We come across "[]User" etc ?
		typ = stringify_pointer(typ)
		for i := 0; i < arr_level; i++ {
			typ = 'array_$typ'
		}
		p.register_array(typ)
	}
	p.next()
	if is_question {
		typ = stringify_pointer(typ)
		typ = 'Option_$typ'
		p.table.register_type_with_parent(typ, 'Option')
	}
	// Because the code uses * to see if it's a pointer
	if typ == 'byteptr' {
		typ = 'byte*'
	}
	if typ == 'voidptr' {
		// if !p.builtin_mod && p.mod != 'os' && p.mod != 'gx' && p.mod != 'gg' && !p.pref.translated {
		// p.error('voidptr can only be used in unsafe code')
		// }
		typ = 'void*'
	}
	/*
	TODO this is not needed?
	if typ.last_index('__') > typ.index('__') {
		p.error('2 __ in gettype(): typ="$typ"')
	}
	*/

	return Type{
		name: typ
		cat: cat
	}
}

fn parse_pointer(_typ string) string {
	if !_typ.starts_with('ptr_') {
		return _typ
	}
	mut typ := _typ.clone()
	for typ.starts_with('ptr_') {
		typ = typ[4..] + '*'
	}
	return typ
}

fn stringify_pointer(typ string) string {
	if !typ.ends_with('*') {
		return typ
	}
	count := typ.count('*')
	return 'ptr_'.repeat(count) + typ.trim_right('*')
}
