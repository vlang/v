// Q: What's this?
// A: A simple arithmetic calculator, similar to examples/mini_calculator.v,
// but *without* an explicit stack. Instead, it re-uses the one from the host language.
// It also demonstrates how to use textscanner.TextScanner from the V standart library.
// See https://modules.vlang.io/strings.textscanner.html ,
// and also https://en.wikipedia.org/wiki/Recursive_descent_parser .
module main

import os
import strconv
import strings.textscanner

struct Parser {
	textscanner.TextScanner
}

fn (mut p Parser) expr() !f64 {
	mut result := p.term()!
	for {
		p.skip_whitespace()
		c := p.peek_u8()
		if c == `+` {
			p.next()
			result += p.term()!
		} else if c == `-` {
			p.next()
			result -= p.term()!
		} else {
			break
		}
	}
	return result
}

fn (mut p Parser) term() !f64 {
	mut result := p.factor()!
	for {
		p.skip_whitespace()
		c := p.peek_u8()
		if c == `*` {
			p.next()
			result *= p.factor()!
		} else if c == `/` {
			p.next()
			result /= p.factor()!
		} else {
			break
		}
	}
	return result
}

fn (mut p Parser) factor() !f64 {
	p.skip_whitespace()
	c := p.peek_u8()
	if c.is_digit() {
		return p.number()
	} else if c == `(` {
		p.next()
		result := p.expr()!
		p.skip_whitespace()
		if p.next() != `)` {
			return error('Expected closing parenthesis')
		}
		return result
	}
	return error('Expected number or opening parenthesis')
}

fn (mut p Parser) number() !f64 {
	start := p.pos
	for {
		c := p.peek_u8()
		if c.is_digit() || c == `.` || c == `e` || c == `E` {
			p.next()
			continue
		}
		break
	}
	return strconv.atof64(p.input[start..p.pos]) or { error('Invalid number') }
}

println('Enter expressions to calculate, e.g. `2 * (5-1)` or `exit` to quit.')
for i := 1; true; i++ {
	input := os.input_opt('[${i}] ') or {
		println('')
		break
	}.trim_space()
	if input.to_upper() == 'EXIT' {
		break
	}
	mut parser := Parser{textscanner.new(input)}
	result := parser.expr() or {
		println('Error: ${err}')
		continue
	}
	println(result)
}
