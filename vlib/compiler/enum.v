// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

fn (p mut Parser) enum_decl(_enum_name string) {
	mut enum_name := _enum_name
	// Specify full type name
	if !p.builtin_mod && p.mod != 'main' {
		enum_name = p.prepend_mod(enum_name)
	}
	// Skip empty enums
	if enum_name != 'int' && !p.first_pass() {
		p.cgen.typedefs << 'typedef int $enum_name;'
	}
	p.check(.lcbr)
	mut val := 0
	mut fields := []string
	for p.tok == .name {
		field := p.check_name()
		fields << field
		p.fgenln('')
		name := '${mod_gen_name(p.mod)}__${enum_name}_$field'
		if p.pass == .main {
			p.cgen.consts << '#define $name $val'
		}
		if p.tok == .comma {
			p.next()
		}
		// !!!! NAME free
		if p.first_pass() {
			p.table.register_const(name, enum_name, p.mod)
		}
		val++
	}
	p.table.register_type2(Type {
		name: enum_name
		mod: p.mod
		parent: 'int'
		cat: TypeCategory.enum_
		enum_vals: fields.clone()
	})
	p.check(.rcbr)
	p.fgenln('\n')
}

fn (p mut Parser) check_enum_member_access() {
	T := p.find_type(p.expected_type)
	if T.cat == .enum_ {
		p.check(.dot)
		val := p.check_name()
		// Make sure this enum value exists
		if !T.has_enum_val(val) {
			p.error('enum `$T.name` does not have value `$val`')
		}
		p.gen(mod_gen_name(T.mod) + '__' + p.expected_type + '_' + val)
	} else {
		p.error('`$T.name` is not an enum')
	}
}


