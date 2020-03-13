// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

fn (p mut Parser) enum_decl(no_name bool) {
	is_pub := p.tok == .key_pub
	if is_pub {
		p.next()
		p.fspace()
	}
	p.check(.key_enum)
	p.fspace()
	mut enum_name := p.check_name()
	is_c := enum_name == 'C' && p.tok == .dot
	if is_c {
		p.check(.dot)
		enum_name = p.check_name()
	}
	// Specify full type name
	if !p.builtin_mod && p.mod != 'main' {
		enum_name = p.prepend_mod(enum_name)
	}
	p.fspace()
	p.check(.lcbr)
	mut val := 0
	mut fields := []string
	mut tuple_variants := []string
	for p.tok == .name {
		field := p.check_name()
		if p.pass == .decl && p.tok != .lpar && contains_capital(field) {
			p.warn('enum values cannot contain uppercase letters, use snake_case instead (`$field`)')
		}
		fields << field
		name := '${mod_gen_name(p.mod)}__${enum_name}_$field'
		if p.tok == .assign {
			p.fspace()
			mut enum_assign_tidx := p.cur_tok_index()
			next := p.peek()
			if next in [.number, .minus] {
				p.next()
				p.fspace()
				is_neg := p.tok == .minus
				if is_neg {
					p.next()
				}
				val = p.lit.int()
				if is_neg {
					val = -val
				}
				p.next()
			}
			else {
				p.next()
				enum_assign_tidx = p.cur_tok_index()
				p.error_with_token_index('only numbers are allowed in enum initializations', enum_assign_tidx)
			}
		}
		// `BoolExpr(bool)`
		else if p.tok == .lpar {
			if !field[0].is_capital() {
				p.error('sum types must be capitalized')
			}
			p.check(.lpar)
			tuple_variants << p.get_type()
			p.check(.rpar)
			if p.pass == .main {
				p.cgen.consts << '#define ${field}_type $val // LOL'
			}
		}
		if p.pass == .main {
			p.cgen.consts << '#define $name $val'
		}
		if p.tok == .comma {
			p.next()
			p.fremove_last()
		}
		p.fgen_nl()
		val++
	}
	is_flag := p.attr == 'flag'
	if is_flag && fields.len > 32 {
		p.error('when an enum is used as bit field, it must have a max of 32 fields')
	}
	mut T := Type{
		name: enum_name
		mod: p.mod
		parent: 'int'
		cat: .enum_
		enum_vals: fields.clone()
		is_public: is_pub
		is_flag: is_flag
	}
	p.table.tuple_variants[enum_name] = tuple_variants
	if is_flag && !p.first_pass() {
		p.gen_enum_flag_methods(mut T)
	}
	if p.pass == .decl || is_flag {
		p.table.register_type(T)
	}
	// Register `Expression` enum
	if tuple_variants.len > 0 && p.pass == .main {
		p.cgen.typedefs << 'typedef struct {
void* obj;
int typ;
} $enum_name;
'
	}
	// Skip nameless enums
	else if !no_name && !p.first_pass() {
		p.cgen.typedefs << 'typedef int $enum_name;'
	}
	p.check(.rcbr)
	p.fgen_nl()
	p.fgen_nl()
	if !no_name && fields.len == 0 {
		p.error('Empty enums are not allowed.')
	}
}

fn (p mut Parser) check_enum_member_access() {
	if p.expected_type.starts_with('Option_') {
		p.expected_type = p.expected_type[7..]
	}
	tt := p.find_type(p.expected_type)
	if tt.cat == .enum_ {
		p.check(.dot)
		val := p.check_name()
		// Make sure this enum value exists
		if !tt.has_enum_val(val) {
			p.error('enum `$tt.name` does not have value `$val`')
		}
		p.gen(mod_gen_name(tt.mod) + '__' + p.expected_type + '_' + val)
	}
	else {
		p.error('`$tt.name` is not an enum')
	}
}

/*

enum Expression {
Boolean(bool),
Integer(i32),
}

fn main() {
    let expr = Expression::Integer(10);
    let mut val = Expression::Boolean(true);
    val = expr;
    match val {
        Expression::Integer(n) => println!("INT {}", n),
        Expression::Boolean(b) => println!("BOOL {}", b),
    }

    //println!("HELLO {}", val);
}
*/

