module parser

import math
import x.kdl.document

fn (mut p Parser) parse_document() !document.Document {
	mut nodes := []document.Node{}
	for {
		if p.tok.kind == .eof {
			if p.tok.lit.len > 0 && p.tok.lit.starts_with('kdl:') {
				return kdl_err(p.tok.line, p.tok.col, p.tok.lit)
			}
			break
		}
		if p.tok.kind == .slashdash {
			p.skip_commented_node()
			continue
		}
		if p.tok.kind == .newline {
			p.advance()
			continue
		}
		if p.tok.kind == .semicolon {
			p.advance()
			continue
		}
		nodes << p.parse_node()!
		for p.tok.kind in [.newline, .semicolon] {
			p.advance()
		}
	}
	return document.Document{
		nodes: nodes
	}
}

fn (mut p Parser) skip_commented_node() {
	p.advance()
	p.skip_node()
}

fn (mut p Parser) skip_commented_entry() {
	p.advance()
	p.skip_entry()
}

fn (mut p Parser) skip_node() {
	if p.tok.kind == .type_annotation { p.advance() }
	if p.tok.kind in [.identifier, .string_val] { p.advance() }
	for p.tok.kind in [.identifier, .string_val, .int_val, .float_val, .bool_val, .null_val,
		.type_annotation, .slashdash, .suffixed_decimal] {
		if p.tok.kind == .slashdash {
			p.skip_commented_entry()
			continue
		}
		p.skip_entry()
	}
	if p.tok.kind == .l_brace {
		p.advance()
		for p.tok.kind != .r_brace && p.tok.kind != .eof {
			if p.tok.kind == .slashdash {
				p.skip_commented_node()
				continue
			}
			if p.tok.kind in [.newline, .semicolon] {
				p.advance()
				continue
			}
			p.skip_node()
		}
		if p.tok.kind == .r_brace { p.advance() }
	}
}

fn (mut p Parser) skip_entry() {
	if p.tok.kind == .type_annotation { p.advance() }
	if p.tok.kind in [.identifier, .string_val] {
		p.advance()
		if p.tok.kind == .equals {
			p.advance()
			if p.tok.kind == .type_annotation { p.advance() }
			if p.tok.kind in [.identifier, .string_val, .int_val, .float_val, .bool_val, .null_val,
				.suffixed_decimal] {
				p.advance()
			}
			return
		}
		return
	}
	if p.tok.kind in [.int_val, .float_val, .bool_val, .null_val, .suffixed_decimal] { p.advance() }
}

fn (mut p Parser) parse_node() !document.Node {
	// Check for scanner-level errors (bare keywords, invalid strings, etc.)
	if p.tok.kind == .eof && p.tok.lit.len > 0 {
		return kdl_err(p.tok.line, p.tok.col, p.tok.lit)
	}

	mut node := document.Node{}

	// Capture before-comment if parse_comments is enabled
	if p.scan.parse_comments && p.scan.comment.len > 0 {
		node.comment = document.Comment{
			before: p.scan.comment
		}
		p.scan.comment = ''
	}

	if p.tok.kind == .type_annotation {
		node.type_name = p.tok.lit
		p.advance()
	}
	if p.tok.kind in [.identifier, .string_val] {
		node.name = p.tok.lit
		p.advance()
	} else {
		return kdl_err(p.tok.line, p.tok.col, 'expected node name')
	}
	for p.tok.kind in [.identifier, .string_val, .int_val, .float_val, .bool_val, .null_val,
		.type_annotation, .slashdash, .suffixed_decimal] {
		if p.tok.kind == .slashdash {
			p.skip_commented_entry()
			continue
		}
		node.entries << p.parse_entry()!
	}
	// Propagate scanner errors from invalid tokens
	if p.tok.kind == .eof && p.tok.lit.len > 0 {
		return kdl_err(p.tok.line, p.tok.col, p.tok.lit)
	}
	if p.tok.kind == .l_brace {
		p.advance()
		for p.tok.kind != .r_brace && p.tok.kind != .eof {
			if p.tok.kind == .slashdash {
				p.skip_commented_node()
				continue
			}
			if p.tok.kind in [.newline, .semicolon] {
				p.advance()
				continue
			}
			node.children << p.parse_node()!
		}
		if p.tok.kind == .r_brace {
			p.advance()
		} else {
			return kdl_err(p.tok.line, p.tok.col, 'unterminated children block')
		}
	}

	// Capture after-comment if parse_comments is enabled
	if p.scan.parse_comments && p.scan.comment.len > 0 {
		if node.comment != none {
			mut c := node.comment or { document.Comment{} }
			c.after = p.scan.comment
			node.comment = c
		} else {
			node.comment = document.Comment{
				after: p.scan.comment
			}
		}
		p.scan.comment = ''
	}

	return node
}

fn (mut p Parser) parse_entry() !document.Entry {
	mut type_ann := ''
	if p.tok.kind == .type_annotation {
		type_ann = p.tok.lit
		p.advance()
	}
	if p.tok.kind in [.identifier, .string_val] {
		key_lit := p.tok.lit
		if p.peek().kind == .equals {
			p.advance()
			p.advance()
			mut val_type_ann := type_ann
			if p.tok.kind == .type_annotation {
				val_type_ann = p.tok.lit
				p.advance()
			}
			value := p.parse_value()!
			return document.Property{
				type_name: val_type_ann
				key:       key_lit
				value:     value
			}
		}
		p.advance()
		return document.Argument{
			type_name: type_ann
			value:     document.StringVal{
				value: key_lit
				flag:  .bare
			}
		}
	}
	value := p.parse_value()!
	return document.Argument{
		type_name: type_ann
		value:     value
	}
}

fn (mut p Parser) parse_value() !document.Value {
	match p.tok.kind {
		.string_val {
			v := document.StringVal{
				value: p.tok.lit
				flag:  .quoted
			}
			p.advance()
			return v
		}
		.identifier {
			v := document.StringVal{
				value: p.tok.lit
				flag:  .bare
			}
			p.advance()
			return v
		}
		.int_val {
			v := parse_int(p.tok.lit)
			p.advance()
			return v
		}
		.float_val {
			flag := if p.tok.lit.contains('e') || p.tok.lit.contains('E') {
				document.ValueFlag.scientific
			} else {
				document.ValueFlag.none
			}
			mut val := 0.0
			if p.tok.lit == 'inf' || p.tok.lit == '+inf' {
				val = math.inf(1)
			} else if p.tok.lit == '-inf' {
				val = math.inf(-1)
			} else if p.tok.lit == 'nan' || p.tok.lit == '+nan' {
				val = math.nan()
			} else {
				val = p.tok.lit.f64()
			}
			v := document.FloatVal{
				value: val
				flag:  flag
			}
			p.advance()
			return v
		}
		.suffixed_decimal {
			v := parse_suffixed(p.tok.lit)
			p.advance()
			return v
		}
		.bool_val {
			v := document.BoolVal{
				value: p.tok.lit == 'true'
			}
			p.advance()
			return v
		}
		.null_val {
			p.advance()
			return document.NullVal{}
		}
		else {
			return kdl_err(p.tok.line, p.tok.col, 'expected value')
		}
	}
}

fn parse_int(lit string) document.Value {
	mut result := i64(0)
	mut flag := document.ValueFlag.none
	mut neg := false
	mut start := 0
	if lit.len > 0 && (lit[0] == 45 || lit[0] == 43) {
		neg = lit[0] == 45
		start = 1
	}
	if lit.len > start + 1 && lit[start] == 48 {
		if lit[start + 1] == 120 || lit[start + 1] == 88 {
			flag = .hex
			for i in start + 2 .. lit.len {
				c := lit[i]
				result <<= 4
				if c >= 48 && c <= 57 {
					result |= i64(c - 48)
				} else if c >= 97 && c <= 102 {
					result |= i64(c - 97 + 10)
				} else if c >= 65 && c <= 70 {
					result |= i64(c - 65 + 10)
				}
			}
			if neg { result = -result }
		} else if lit[start + 1] == 111 || lit[start + 1] == 79 {
			flag = .octal
			for i in start + 2 .. lit.len {
				result <<= 3
				result |= i64(lit[i] - 48)
			}
			if neg { result = -result }
		} else if lit[start + 1] == 98 || lit[start + 1] == 66 {
			flag = .binary
			for i in start + 2 .. lit.len {
				result <<= 1
				result |= i64(lit[i] - 48)
			}
			if neg { result = -result }
		}
	} else {
		result = lit.i64()
	}
	return document.IntVal{
		value: result
		flag:  flag
	}
}

fn parse_suffixed(lit string) document.Value {
	mut i := 0
	for i < lit.len && ((lit[i] >= 48 && lit[i] <= 57) || lit[i] == 46
		|| lit[i] == 101 || lit[i] == 69 || lit[i] == 43 || lit[i] == 45) {
		i++
	}
	return document.StringVal{
		value: lit
		flag:  .bare
	}
}
