// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import (
	strings
)
// Returns type if used as expression


fn (p mut Parser) match_statement(is_expr bool) string {
	p.check(.key_match)
	p.fspace()
	is_mut := p.tok == .key_mut
	if is_mut {
		p.next()
		p.fspace()
	}
	typ,expr := p.tmp_expr()
	if typ.starts_with('array_') {
		p.error('arrays cannot be compared')
	}
	is_sum_type := typ in p.table.sum_types
	mut sum_child_type := ''
	// is it safe to use p.cgen.insert_before ???
	tmp_var := p.get_tmp()
	p.cgen.insert_before('$typ $tmp_var = $expr;')
	p.fspace()
	p.check(.lcbr)
	mut i := 0
	mut all_cases_return := true
	// stores typ of resulting variable
	mut res_typ := ''
	defer {
		p.check(.rcbr)
	}
	for p.tok != .rcbr {
		if p.tok == .key_else {
			p.check(.key_else)
			if p.tok == .arrow {
				p.error(warn_match_arrow)
			}
			// unwrap match if there is only else
			if i == 0 {
				p.fspace()
				if is_expr {
					// statements are dissallowed (if match is expression) so user cant declare variables there and so on
					// allow braces is else
					got_brace := p.tok == .lcbr
					if got_brace {
						p.fspace()
						p.check(.lcbr)
					}
					p.gen('( ')
					res_typ = p.bool_expression()
					p.gen(' )')
					// allow braces in else
					if got_brace {
						p.check(.rcbr)
					}
					return res_typ
				}
				else {
					p.returns = false
					p.check(.lcbr)
					p.genln('{ ')
					p.statements()
					p.returns = all_cases_return && p.returns
					return ''
				}
			}
			if is_expr {
				// statements are dissallowed (if match is expression) so
				// user cant declare variables there and so on
				p.gen(':(')
				// allow braces is else
				got_brace := p.tok == .lcbr
				if got_brace {
					p.fspace()
					p.check(.lcbr)
				}
				p.check_types(p.bool_expression(), res_typ)
				// allow braces in else
				if got_brace {
					p.check(.rcbr)
				}
				p.gen(strings.repeat(`)`, i + 1))
				return res_typ
			}
			else {
				p.returns = false
				p.genln('else // default:')
				p.fspace()
				p.check(.lcbr)
				p.genln('{ ')
				p.statements()
				p.returns = all_cases_return && p.returns
				return ''
			}
		}
		if i > 0 {
			if is_expr {
				p.gen(': (')
			}
			else {
				p.gen('else ')
			}
		}
		else if is_expr {
			p.gen('(')
		}
		if is_expr {
			p.gen('(')
		}
		else {
			p.gen('if (')
		}
		ph := p.cgen.add_placeholder()
		// Multiple checks separated by comma
		p.open_scope()
		mut got_comma := false
		for {
			if got_comma {
				p.gen(') || (')
			}
			mut got_string := false
			if typ == 'string' {
				got_string = true
				p.gen('string_eq($tmp_var, ')
			}
			else if is_sum_type {
				p.gen('${tmp_var}.typ == ')
			}
			else {
				p.gen('$tmp_var == ')
			}
			p.expected_type = typ
			// `match node { ast.BoolExpr { it := node as BoolExpr ... } }`
			if is_sum_type {
				sum_child_type = p.get_type2().name
				tt := sum_child_type.all_after('_')
				p.gen('SumType_${typ}_$tt')
				// println('got child $sum_child_type')
				p.register_var(Var{
					name: 'it'
					typ: sum_child_type+'*'
					is_mut: is_mut
					ptr: true
				})
			}
			else {
				p.check_types(p.bool_expression(), typ)
			}
			p.expected_type = ''
			if got_string {
				p.gen(')')
			}
			if p.tok != .comma {
				if got_comma {
					p.gen(') ')
					p.cgen.set_placeholder(ph, '(')
				}
				break
			}
			p.check(.comma)
			p.fspace()
			got_comma = true
		}
		p.gen(')')
		if p.tok == .arrow {
			p.error(warn_match_arrow)
			p.check(.arrow)
		}
		// statements are dissallowed (if match is expression) so user cant declare variables there and so on
		if is_expr {
			p.gen('? (')
			// braces are required for now
			p.check(.lcbr)
			if i == 0 {
				// on the first iteration we set value of res_typ
				res_typ = p.bool_expression()
			}
			else {
				// later on we check that the value is of res_typ type
				p.check_types(p.bool_expression(), res_typ)
			}
			// braces are required for now
			p.fgen_nl()
			p.check(.rcbr)
			p.gen(')')
		}
		else {
			p.returns = false
			p.fspace()
			p.check(.lcbr)
			p.genln('{ ')
			if is_sum_type {
				//p.genln(' $sum_child_type it = *($sum_child_type*)$tmp_var .obj ;')
				p.genln(' $sum_child_type* it = ($sum_child_type*)${tmp_var}.obj ;')
			}
			p.statements()
			all_cases_return = all_cases_return && p.returns
			// p.gen(')')
		}
		i++
		p.fgen_nl()
		p.close_scope()
	}
	p.error('match must be exhaustive')
	// p.returns = false // only get here when no default, so return is not guaranteed
	return ''
}

fn (p mut Parser) switch_statement() {
	p.error('`switch` statement has been removed, use `match` instead:\n' + 'https://vlang.io/docs#match')
}

fn (p mut Parser) if_statement(is_expr bool, elif_depth int) string {
	if is_expr {
		// if p.fileis('if_expr') {
		// println('IF EXPR')
		// }
		p.inside_if_expr = true
		p.gen('((')
	}
	else {
		p.gen('if (')
	}
	p.next()
	p.fspace()
	if p.tok == .name && p.peek() == .assign {
		p.error('cannot assign on if-else statement')
	}
	if p.tok == .name && (p.peek() == .inc || p.peek() == .dec) {
		p.error('`${p.peek().str()}` is a statement')
	}
	// `if a := opt() { }` syntax
	if p.tok == .name && p.peek() == .decl_assign {
		p.check_not_reserved()
		option_tmp := p.get_tmp()
		var_name := p.lit
		if p.known_var(var_name) {
			p.error('redefinition of `$var_name`')
		}
		p.open_scope()
		p.next()
		p.fspace()
		p.check(.decl_assign)
		p.fspace()
		p.is_var_decl = true
		option_type,expr := p.tmp_expr() // := p.bool_expression()
		if !option_type.starts_with('Option_') {
			p.error('`if x := opt() {` syntax requires a function that returns an optional value')
		}
		p.is_var_decl = false
		typ := parse_pointer(option_type[7..])
		// Option_User tmp = get_user(1);
		// if (tmp.ok) {
		// User user = *(User*)tmp.data;
		// [statements]
		// }
		p.cgen.insert_before('$option_type $option_tmp = $expr; ')
		p.fspace()
		p.check(.lcbr)
		p.genln(option_tmp + '.ok) {')
		p.genln('$typ $var_name = *($typ*) $option_tmp . data;')
		p.register_var(Var{
			name: var_name
			typ: typ
			is_mut: false // TODO

			is_used: true // TODO
			// is_alloc: p.is_alloc || typ.starts_with('array_')
			// line_nr: p.tokens[ var_token_idx ].line_nr
			// token_idx: var_token_idx

		})
		p.statements()
		p.close_scope()
		p.returns = false
		if p.tok == .key_else {
			p.next()
			p.genln('else {')
			p.check(.lcbr)
			p.statements()
		}
		return 'void'
	}
	else {
		p.check_types(p.bool_expression(), 'bool')
	}
	if is_expr {
		p.gen(') ? (')
	}
	else {
		p.genln(') {')
	}
	p.fspace()
	p.check(.lcbr)
	if p.inside_if_expr {
		p.fspace()
	}
	mut typ := ''
	// if { if hack
	if p.tok == .key_if && p.inside_if_expr {
		typ = p.factor()
		p.next()
	}
	else {
		typ = p.statements()
	}
	if_returns := p.returns
	p.returns = false
	if p.tok == .key_else {
		if p.inside_if_expr {
			p.fspace()
		}
		else {
			p.fgen_nl()
		}
		p.check(.key_else)
		p.fspace()
		if p.tok == .key_if {
			if is_expr {
				p.gen(') : (')
				nested := p.if_statement(is_expr, elif_depth + 1)
				nested_returns := p.returns
				p.returns = if_returns && nested_returns
				return nested
			}
			else {
				p.gen(' else ')
				nested := p.if_statement(is_expr, 0)
				nested_returns := p.returns
				p.returns = if_returns && nested_returns
				return nested
			}
			// return ''
		}
		if is_expr {
			p.gen(') : (')
		}
		else {
			p.genln(' else { ')
		}
		p.check(.lcbr)
		if is_expr {
			p.fspace()
		}
		// statements() returns the type of the last statement
		first_typ := typ
		typ = p.statements()
		p.inside_if_expr = false
		if is_expr {
			p.check_types(first_typ, typ)
			p.gen(strings.repeat(`)`, 2 * (elif_depth + 1)))
		}
		else_returns := p.returns
		p.returns = if_returns && else_returns
		return typ
	}
	p.inside_if_expr = false
	if p.fileis('test_test') {
		println('if ret typ="$typ" line=$p.scanner.line_nr')
	}
	return typ
}
