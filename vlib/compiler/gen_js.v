module compiler

import strings

const (
	dot_ptr = '.'
)

fn (p mut Parser) gen_var_decl(name string, is_static bool) string {
	p.gen('var $name /* typ */ = ')
	mut typ := p.bool_expression()
	if typ.starts_with('...') { typ = typ[3..] }
	or_else := p.tok == .key_orelse
	if or_else {
		// return p.gen_handle_option_or_else(typ, name, pos)
	}
	return typ
}

fn (p mut Parser) gen_fn_decl(f Fn, typ, _str_args string) {
	mut str_args := ''
	for i, arg in f.args   {
		str_args += ' /** @type { $arg.typ } **/ ' + arg.name
		if i < f.args.len - 1 {
			str_args += ', '
		}
	}
	name := p.table.fn_gen_name(f)
	if f.is_method {
		//p.genln('\n${f.receiver_typ}.prototype.${name} = function($str_args) {')
		p.genln('function ${f.receiver_typ}_$name($str_args) {')
	}	 else {
		p.genln('/** @return { $typ } **/\nfunction $name($str_args) {')
	}
}

fn (p mut Parser) gen_blank_identifier_assign() {
	assign_error_tok_idx := p.token_idx
	p.check_name()
	p.check_space(.assign)
	is_indexer := p.peek() == .lsbr
	is_fn_call, next_expr := p.is_expr_fn_call(p.token_idx)
	p.bool_expression()
	if !is_indexer && !is_fn_call {
		p.error_with_token_index('assigning `$next_expr` to `_` is redundant', assign_error_tok_idx)
	}
	or_else := p.tok == .key_orelse
	if or_else {
		// return p.gen_handle_option_or_else(typ, '', pos)
	}
}

// TODO: optionals
fn (p mut Parser) gen_handle_option_or_else(_typ, name string, fn_call_ph int) string {
	return _typ
}

fn types_to_c(types []Type, table &Table) string {
	mut sb := strings.new_builder(10)
	for t in types {
		if t.cat != .union_ && t.cat != .struct_ {
			continue
		}
		sb.write('\n/**\n')
		sb.write('* @typedef { object } $t.name' + 'Type\n')
		for field in t.fields {
			sb.writeln('* @property { $field.typ' + '= } $field.name')
		}
		sb.writeln('**/\n')
		sb.writeln('/** @type { function & $t.name' + 'Type } **/')
		sb.writeln('var $t.name = function() {}')
	}
	return sb.str()
}

fn (p mut Parser) index_get(typ string, fn_ph int, cfg IndexConfig) {
	p.cgen.cur_line = p.cgen.cur_line.replace(',', '[') + ']'
}

fn (table &Table) fn_gen_name(f &Fn) string {
	mut name := f.name
	if f.is_method {
		name = name.replace(' ', '')
		name = name.replace('*', '')
		name = name.replace('+', 'plus')
		name = name.replace('-', 'minus')
		return name
	}
	// Avoid name conflicts (with things like abs(), print() etc).
	// Generate b_abs(), b_print()
	// TODO duplicate functionality
	if f.mod == 'builtin' && f.name in CReserved {
		return 'v_$name'
	}
	return name
}

//fn (p mut Parser) gen_method_call(receiver &Var, receiver_type string,
	//ftyp string,	cgen_name string, receiver Var,method_ph int)
fn (p mut Parser) gen_method_call(receiver &Var, receiver_type string,
	cgen_name string, ftyp string, method_ph int)
{
	// TODO  js methods have been broken from the start
	
	//mut cgen_name := p.table.fn_gen_name(f)
	//mut method_call := cgen_name + '('
	//p.gen('/*2*/.' + cgen_name.all_after('_') + '(')
	t := receiver_type.replace('*', '')
	p.cgen.set_placeholder(method_ph, '${t}_$cgen_name(')
	//p.cgen.set_placeholder(method_ph, '$cast kKE $method_call')
	//return method_call
}


fn (p mut Parser) gen_array_at(typ string, is_arr0 bool, fn_ph int) {
	p.gen('[')
}	

fn (p mut Parser) gen_for_header(i, tmp, var_typ, val string) {
	p.genln('for (var $i = 0; $i < ${tmp}.length; $i++) {')
	if val == '_' { return }
	p.genln('var $val = $tmp [$i];')
}

fn (p mut Parser) gen_for_range_header(i, range_end, tmp, var_type, val string) {
	p.genln(';\nfor (var $i = $tmp; $i < $range_end; $i++) {')
	if val == '_' { return }
	p.genln('var /*$var_type*/ $val = $i;')
}

fn (p mut Parser) gen_for_str_header(i, tmp, var_typ, val string) {
	p.genln('for (var $i = 0; $i < $tmp .length; $i ++) {')
	if val == '_' { return }
	p.genln('var $val = $tmp[$i];')
}

fn (p mut Parser) gen_for_map_header(i, tmp, var_typ, val, typ string) {
	p.genln('for (var $i in $tmp) {')
	if val == '_' { return }
	p.genln('var $val = $tmp[$i];')
}

fn (p mut Parser) gen_for_varg_header(i, varg, var_typ, val string) {
	p.genln('for (var $i = 0; $i < ${varg}.len; $i++) {')
	if val == '_' { return }
	p.genln('var $val = ${varg}.args[$i];')
}

fn (p mut Parser) gen_array_init(typ string, no_alloc bool, new_arr_ph int, nr_elems int) {
	p.cgen.set_placeholder(new_arr_ph,	'[')
	p.gen(']')
}

fn (p mut Parser) gen_array_set(typ string, is_ptr, is_map bool,fn_ph, assign_pos int, is_cao bool) {
	mut val := p.cgen.cur_line[assign_pos..]
	p.cgen.resetln(p.cgen.cur_line[..assign_pos])
	p.gen('] =')
	cao_tmp := p.cgen.cur_line
	if is_cao  {
		val = cao_tmp + val.all_before('=') +	val.all_after('=')
	}
	p.gen(val)
}

// returns true in case of an early return
fn (p mut Parser) gen_struct_init(typ string, t &Type) bool {
	p.next()
	p.check(.lcbr)
	ptr := typ.contains('*')
	if !ptr {
			p.gen('{')
	}
	else {
		// TODO tmp hack for 0 pointers init
		// &User{!} ==> 0
		if p.tok == .not {
			p.next()
			p.gen('}')
			p.check(.rcbr)
			return true
		}
	}
	return false
}

fn (p mut Parser) gen_struct_field_init(field string) {
	p.gen('$field : ')
}

fn (p mut Parser) gen_empty_map(typ string) {
	p.gen('{}')
}

fn (p mut Parser) cast(typ string) string {
	p.next()
	pos := p.cgen.add_placeholder()
	if p.tok == .rpar {
		p.next()
	}
	p.check(.lpar)
	p.bool_expression()
	if typ == 'string' {
		if p.tok == .comma {
			p.check(.comma)
			p.cgen.set_placeholder(pos, 'tos(')
			//p.gen('tos(')
			p.gen(', ')
			p.expression()
			p.gen(')')
		}
	}
	p.check(.rpar)
	return typ
}

fn type_default(typ string) string {
	if typ.starts_with('array_') {
		return '[]'
	}
	// Always set pointers to 0
	if typ.ends_with('*') {
		return '0'
	}
	// User struct defined in another module.
	if typ.contains('__') {
		return '{}'
	}
	// Default values for other types are not needed because of mandatory initialization
	match typ {
		'bool'{ return '0'}
		'string'{ return 'tos("")'}
		'i8'{ return '0'}
		'i16'{ return '0'}
		'i64'{ return '0'}
		'u16'{ return '0'}
		'u32'{ return '0'}
		'u64'{ return '0'}
		'byte'{ return '0'}
		'int'{ return '0'}
		'rune'{ return '0'}
		'f32'{ return '0.0'}
		'f64'{ return '0.0'}
		'byteptr'{ return '0'}
		'voidptr'{ return '0'}
	}
	return '{}'
}

fn (p mut Parser) gen_array_push(ph int, typ, expr_type, tmp, tmp_typ string) {
	push_array := typ == expr_type
	if push_array {
		p.cgen.set_placeholder(ph, 'push(&' )
		p.gen('), $tmp, $typ)')
	}  else {
		p.check_types(expr_type, tmp_typ)
		p.gen(')')
		p.cgen.cur_line = p.cgen.cur_line.replace(',', '.push')
	}
}

