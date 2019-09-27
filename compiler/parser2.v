module main

import strings

fn (p mut Parser) get_type2() Type {
	mut mul := false
	mut nr_muls := 0
	mut typ := ''
	mut cat := TypeCategory.struct_
	// fn type
	if p.tok == .func {
		mut f := Fn{name: '_', mod: p.mod}
		p.next()
		line_nr := p.scanner.line_nr
		p.fn_args(mut f)
		// Same line, it's a return type
		if p.scanner.line_nr == line_nr {
			if p.tok == .name {
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
		fn_typ := Type {
			name: f.typ_str()// 'fn (int, int) string'
			mod: p.mod
			func: f
			cat: TypeCategory.func
		}
		p.table.register_type2(fn_typ)
		return fn_typ
	}
	// arrays ([]int)
	mut is_arr := false
	mut is_arr2 := false// [][]int TODO remove this and allow unlimited levels of arrays
	is_question := p.tok == .question
	if is_question {
		p.check(.question)
	}
	if p.tok == .lsbr {
		p.check(.lsbr)
		// [10]int
		if p.tok == .number {
			typ = '[$p.lit]'
			p.next()
		}
		else {
			is_arr = true
		}
		p.check(.rsbr)
		// [10][3]int
		if p.tok == .lsbr {
			p.next()
			if p.tok == .number {
				typ += '[$p.lit]'
				p.check(.number)
			}
			else {
				is_arr2 = true
			}
			p.check(.rsbr)
		}
		cat = .array
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
		val_type := p.get_type()// p.check_name()
		typ = 'map_$val_type'
		p.register_map(typ)
		return Type{name: typ}
	}
	//
	for p.tok == .mul {
		if p.first_pass() {
			p.warn('use `&Foo` instead of `*Foo`')
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
	typ += p.lit
	if !p.is_struct_init {
		// Otherwise we get `foo := FooFoo{` because `Foo` was already
		// generated in name_expr()
		p.fgen(p.lit)
	}
	// C.Struct import
	if p.lit == 'C' && p.peek() == .dot {
		p.next()
		p.check(.dot)
		typ = p.lit
	}
	else {
		// Module specified? (e.g. gx.Image)
		if p.peek() == .dot {
			// try resolve full submodule
			if !p.builtin_mod && p.import_table.known_alias(typ) {
				mod := p.import_table.resolve_alias(typ)
				if mod.contains('.') {
					typ = mod.replace('.', '_dot_')
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
			if !typ.contains('array_') && p.mod != 'main' && !typ.contains('__') &&
				!typ.starts_with('[') {
				typ = p.prepend_mod(typ)
			}
			t = p.table.find_type(typ)
			if t.name == '' && !p.pref.translated && !p.first_pass() && !typ.starts_with('[') {
				println('get_type() bad type')
				// println('all registered types:')
				// for q in p.table.types {
				// println(q.name)
				// }
				p.error('unknown type `$typ`')
			}
		}
	}
	if typ == 'void' {
		p.error('unknown type `$typ`')
	}
	if mul {
		typ += strings.repeat(`*`, nr_muls)
	}
	// Register an []array type
	if is_arr2 {
		typ = 'array_array_$typ'
		p.register_array(typ)
	}
	else if is_arr {
		typ = 'array_$typ'
		// We come across "[]User" etc ?
		p.register_array(typ)
	}
	p.next()
	if is_question {
		typ = 'Option_$typ'
		p.table.register_type_with_parent(typ, 'Option')
	}
	if typ.last_index('__') > typ.index('__') {
		p.error('2 __ in gettype(): typ="$typ"')
	}
	return Type{name: typ, cat: cat}
}
