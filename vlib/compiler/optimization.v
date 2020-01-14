module compiler
// `a in [1,2,3]` => `a == 1 || a == 2 || a == 3`
// avoid allocation
// `typ` is the type of `a`
// `ph` is for string_eq()
fn (p mut Parser) in_optimization(typ string, ph int) {
	p.check(.lsbr)
	if p.tok == .rsbr {
		p.error('`x in []` is always false')
	}
	mut i := 0
	// Get `a` expr value (can be a string literal, not a variable)
	expr := p.cgen.cur_line[ph..]
	is_str := typ == 'string'
	// println('!! $p.expr_var.name => $name ($typ)')
	for p.tok != .rsbr && p.tok != .eof {
		if i > 0 {
			if is_str {
				p.gen(' || string_eq($expr, ')
			}
			else {
				p.gen(' || $expr == ')
			}
		}
		if i == 0 {
			if is_str {
				p.cgen.set_placeholder(ph, ' (string_eq(')
				p.gen(', ')
			}
			else {
				p.cgen.set_placeholder(ph, ' (')
				p.gen(' ==')
			}
		}
		p.check_types(p.bool_expression(), typ)
		if is_str {
			p.gen(')')
		}
		if p.tok != .rsbr {
			p.check(.comma)
			p.fspace()
		}
		i++
	}
	p.gen(')')
	p.check(.rsbr)
}

