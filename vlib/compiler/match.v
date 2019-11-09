// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import (
	strings
)

// Returns typ if used as expression
fn (p mut Parser) match_statement(is_expr bool) string {
	p.check(.key_match)
	p.cgen.start_tmp()
	typ := p.bool_expression()
	if typ.starts_with('array_') {
		p.error('arrays cannot be compared')
	}	
	expr := p.cgen.end_tmp()

	// is it safe to use p.cgen.insert_before ???
	tmp_var := p.get_tmp()
	p.cgen.insert_before('$typ $tmp_var = $expr;')

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
				p.warn(warn_match_arrow)
				p.check(.arrow)
			}	

			// unwrap match if there is only else
			if i == 0 {
				if is_expr {
					// statements are dissallowed (if match is expression) so user cant declare variables there and so on

					// allow braces is else
					got_brace := p.tok == .lcbr
					if got_brace {
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
				} else {
					p.returns = false
					p.check(.lcbr)

					p.genln('{ ')
					p.statements()
					p.returns = all_cases_return && p.returns
					return ''
				}
			}

			if is_expr {
				// statements are dissallowed (if match is expression) so user cant declare variables there and so on
				p.gen(':(')

				// allow braces is else
				got_brace := p.tok == .lcbr
				if got_brace {
					p.check(.lcbr)
				}

				p.check_types(p.bool_expression(), res_typ)

				// allow braces in else
				if got_brace {
					p.check(.rcbr)
				}

				p.gen(strings.repeat(`)`, i+1))

				return res_typ
			} else {
				p.returns = false
				p.genln('else // default:')

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
			} else {
				p.gen('else ')
			}
		} else if is_expr {
			p.gen('(')
		}

		if is_expr {
			p.gen('(')
		} else {
			p.gen('if (')
		}

		ph := p.cgen.add_placeholder()

		// Multiple checks separated by comma
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
			else {
				p.gen('$tmp_var == ')
			}

			p.expected_type = typ
			p.check_types(p.bool_expression(), typ)
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
			got_comma = true
		}
		p.gen(')')

		if p.tok == .arrow {
			p.warn(warn_match_arrow)
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
			} else {
				// later on we check that the value is of res_typ type
				p.check_types(p.bool_expression(), res_typ)
			}

			// braces are required for now
			p.check(.rcbr)

			p.gen(')')
		}
		else {
			p.returns = false
			p.check(.lcbr)

			p.genln('{ ')
			p.statements()

			all_cases_return = all_cases_return && p.returns
			// p.gen(')')
		}
		i++
	}

	if is_expr {
		// we get here if no else found, ternary requires "else" branch
		p.error('Match expression requires "else"')
	}

	p.returns = false // only get here when no default, so return is not guaranteed

	return ''
}

fn (p mut Parser) switch_statement() {
	p.error('`switch` statement has been removed, use `match` instead:\n' +
		'https://vlang.io/docs#match')
}


