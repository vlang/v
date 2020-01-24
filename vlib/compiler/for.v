// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

fn (p mut Parser) for_st() {
	p.check(.key_for)
	p.for_expr_cnt++
	next_tok := p.peek()
	if p.tok != .lcbr {
		p.fspace()
	}
	// debug := p.scanner.file_path.contains('r_draw')
	p.open_scope()
	//mut label := 0
	mut to := 0
	if p.tok == .lcbr {
		// Infinite loop
		p.gen('while (1) {')
	}
	else if p.tok == .key_mut {
		p.error('`mut` is not required in for loops')
	}
	// for i := 0; i < 10; i++ {
	else if next_tok == .decl_assign || next_tok == .assign || p.tok == .semicolon {
		p.genln('for (')
		if next_tok == .decl_assign {
			p.check_not_reserved()
			p.var_decl()
		}
		else if p.tok != .semicolon {
			// allow `for ;; i++ {`
			// Allow `for i = 0; i < ...`
			p.statement(false)
		}
		p.check(.semicolon)
		p.gen(' ; ')
		p.fspace()
		if p.tok != .semicolon {
			p.bool_expression()
		}
		p.check(.semicolon)
		p.gen(' ; ')
		p.fspace()
		if p.tok != .lcbr {
			p.statement(false)
		}
		p.genln(') { ')
	}
	// for i, val in array
	else if p.peek() == .comma {
		/*
		`for i, val in array {`
		==>
		```
		 array_int tmp = array;
		 for (int i = 0; i < tmp.len; i++) {
		 int val = tmp[i];
		```
		*/
		i := p.check_name()
		p.check(.comma)
		p.fspace()
		val := p.check_name()
		if i == '_' && val == '_' {
			p.error('no new variables on the left side of `in`')
		}
		p.fspace()
		p.check(.key_in)
		p.fspace()
		tmp := p.get_tmp()
		mut typ,expr := p.tmp_expr()
		is_arr := typ.starts_with('array_')
		is_map := typ.starts_with('map_')
		is_str := typ == 'string'
		is_variadic_arg := typ.starts_with('varg_')
		if !is_arr && !is_str && !is_map && !is_variadic_arg {
			p.error('cannot range over type `$typ`')
		}
		if !is_variadic_arg {
			if p.is_js {
				p.genln('var $tmp = $expr;')
			}
			else {
				p.genln('$typ $tmp = $expr;')
			}
		}
		// typ = strings.Replace(typ, "_ptr", "*", -1)
		mut i_var_type := 'int'
		if is_variadic_arg {
			typ = typ[5..]
			p.gen_for_varg_header(i, expr, typ, val)
		}
		else if is_arr {
			typ = parse_pointer(typ[6..])
			p.gen_for_header(i, tmp, typ, val)
		}
		else if is_map {
			i_var_type = 'string'
			typ = parse_pointer(typ[4..])
			p.gen_for_map_header(i, tmp, typ, val, typ)
		}
		else if is_str {
			typ = 'byte'
			p.gen_for_str_header(i, tmp, typ, val)
		}
		// Register temp vars
		if i != '_' {
			if p.known_var(i) {
				p.error('redefinition of `$i`')
			}
			p.register_var(Var{
				name: i
				typ: i_var_type
				is_mut: true
				is_changed: true
			})
		}
		if val != '_' {
			if p.known_var(val) {
				p.error('redefinition of `$val`')
			}
			p.register_var(Var{
				name: val
				typ: typ
				ptr: typ.contains('*')
			})
		}
	}
	// `for val in vals`
	else if p.peek() == .key_in || p.peek() == .left_arrow {
		p.check_not_reserved()
		val := p.check_name()
		p.fspace()
		//p.check(.key_in)
		p.next()
		p.fspace()
		tmp := p.get_tmp()
		mut typ,expr := p.tmp_expr()
		is_range := p.tok == .dotdot
		is_variadic_arg := typ.starts_with('varg_')
		mut range_end := ''
		if is_range {
			p.check_types(typ, 'int')
			p.check_space(.dotdot)
			if p.pref.x64 {
				to = p.lit.int()
			}
			range_typ,range_expr := p.tmp_expr()
			p.check_types(range_typ, 'int')
			range_end = range_expr
			if p.pref.x64 {
				//label = p.x64.gen_loop_start(expr.int())
				// to  = range_expr.int() // TODO why empty?
			}
		}
		is_arr := typ.contains('array')
		is_fixed := typ.starts_with('[')
		is_str := typ == 'string'
		if !is_arr && !is_str && !is_range && !is_fixed && !is_variadic_arg {
			p.error('cannot range over type `$typ`')
		}
		if !is_variadic_arg {
			if p.is_js {
				p.genln('var $tmp = $expr;')
			}
			else if !is_fixed {
				// Don't copy if it's a fixed array
				p.genln('$typ $tmp = $expr;')
			}
		}
		// TODO var_type := if...
		i := p.get_tmp()
		if is_variadic_arg {
			typ = typ[5..]
			p.gen_for_varg_header(i, expr, typ, val)
		}
		else if is_range {
			typ = 'int'
			p.gen_for_range_header(i, range_end, tmp, typ, val)
		}
		else if is_arr {
			typ = parse_pointer(typ[6..]) // all after `array_`
			p.gen_for_header(i, tmp, typ, val)
		}
		else if is_str {
			typ = 'byte'
			p.gen_for_str_header(i, tmp, typ, val)
		}
		else if is_fixed {
			typ = typ.all_after(']')
			p.gen_for_fixed_header(i, expr, typ, val)
		}
		// println('for typ=$typ vartyp=$var_typ')
		// Register temp var
		if val != '_' {
			if p.known_var(val) {
				p.error('redefinition of `$val`')
			}
			p.register_var(Var{
				name: val
				typ: typ
				ptr: typ.contains('*')
				is_changed: true
				is_mut: false
				is_for_var: true
			})
		}
	}
	else {
		// `for a < b {`
		p.gen('while (')
		p.check_types(p.bool_expression(), 'bool')
		p.genln(') {')
	}
	p.fspace()
	p.check(.lcbr)
	p.genln('') // TODO why is this needed?
	p.statements()
	p.close_scope()
	p.for_expr_cnt--
	p.returns = false // TODO handle loops that are guaranteed to return
	//if label > 0 {
		//p.x64.gen_loop_end(to, label)
	//}
}

