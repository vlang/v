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

fn (mut p Parser) parse_expression() !f64 {
	mut result := p.parse_term()!
	for {
		p.skip_whitespace()
		c := p.peek_u8()
		if c == `+` {
			p.next()
			result += p.parse_term()!
		} else if c == `-` {
			p.next()
			result -= p.parse_term()!
		} else {
			break
		}
	}
	return result
}

fn (mut p Parser) parse_term() !f64 {
	mut result := p.parse_factor()!
	for {
		p.skip_whitespace()
		c := p.peek_u8()
		if c == `*` {
			p.next()
			result *= p.parse_factor()!
		} else if c == `/` {
			p.next()
			result /= p.parse_factor()!
		} else {
			break
		}
	}
	return result
}

fn (mut p Parser) parse_factor() !f64 {
	p.skip_whitespace()
	c := p.peek_u8()
	if c.is_digit() {
		return p.parse_number()
	} else if c == `(` {
		p.next()
		result := p.parse_expression()!
		p.skip_whitespace()
		if p.next() != `)` {
			return error('Expected closing parenthesis')
		}
		return result
	}
	return error('Expected number or opening parenthesis')
}

fn (mut p Parser) parse_number() !f64 {
	start := p.pos
	for {
		c := p.peek_u8()
		if c.is_digit() || c == `.` || c == `e` || c == `E` {
			p.next()
			continue
		}
		break
	}
	num_str := p.input[start..p.pos]
	return strconv.atof64(num_str) or { return error('Invalid number') }
}

println('Please enter the expression you want to calculate, e.g. `2 * (5-1)` .')
println("Enter 'exit' or 'EXIT' to quit.")
mut expr_count := 0
for {
	expr_count++
	input := os.input_opt('[${expr_count}] ') or {
		println('')
		break
	}.trim_space()
	if input in ['exit', 'EXIT'] {
		break
	}
	mut parser := Parser{textscanner.new(input)}
	result := parser.parse_expression() or {
		println('Error: ${err}')
		continue
	}
	println(result)
}
