module parser

import math
import x.kdl.document

fn (mut p Parser) parse_document() !document.Document {
	mut nodes := []document.Node{}
	for {
		if p.tok.kind == .eof {
			if p.tok.lit.len > 0 {
				return kdl_err(p.tok.line, p.tok.col, p.tok.lit)
			}
			break
		}
		if p.tok.kind == .slashdash {
			p.skip_commented_node()!
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

fn (mut p Parser) skip_commented_node() ! {
	p.advance_after_slashdash()!
	p.skip_node()!
}

fn (mut p Parser) advance_after_slashdash() ! {
	p.advance()
	for p.tok.kind == .newline {
		p.advance()
	}
	if p.tok.kind == .slashdash {
		return kdl_err(p.tok.line, p.tok.col,
			'kdl: slashdash may not be followed by another slashdash')
	}
}

fn (mut p Parser) skip_commented_entry_or_child() !bool {
	p.advance_after_slashdash()!
	if p.tok.kind == .l_brace {
		p.skip_children_block()!
		return true
	}
	if p.skip_entry()! {
		return false
	}
	return kdl_err(p.tok.line, p.tok.col,
		'kdl: expected argument, property, or children block after slashdash')
}

fn (mut p Parser) skip_commented_child() ! {
	p.advance_after_slashdash()!
	if p.tok.kind != .l_brace {
		return kdl_err(p.tok.line, p.tok.col,
			'kdl: only children blocks may follow a children block')
	}
	p.skip_children_block()!
}

fn (mut p Parser) skip_node() ! {
	if p.tok.kind == .type_annotation { p.advance() }
	if p.tok.kind in [.identifier, .string_val] {
		p.advance()
	} else {
		return kdl_err(p.tok.line, p.tok.col, 'kdl: expected node name after slashdash')
	}
	for p.tok.kind in [.identifier, .string_val, .int_val, .float_val, .bool_val, .null_val,
		.type_annotation, .slashdash, .suffixed_decimal] {
		if p.tok.kind == .slashdash {
			is_child := p.skip_commented_entry_or_child()!
			if is_child {
				break
			}
			continue
		}
		if !p.skip_entry()! {
			return kdl_err(p.tok.line, p.tok.col, 'kdl: expected entry after slashdash')
		}
	}
	for p.tok.kind == .slashdash {
		p.skip_commented_child()!
	}
	if p.tok.kind == .l_brace {
		p.skip_children_block()!
	}
	for p.tok.kind == .slashdash {
		p.skip_commented_child()!
	}
	if p.tok.kind !in [.newline, .semicolon, .r_brace, .eof] {
		return kdl_err(p.tok.line, p.tok.col,
			'kdl: expected newline, semicolon, } or EOF after slashdashed node')
	}
}

fn (mut p Parser) skip_children_block() ! {
	p.advance()
	for p.tok.kind != .r_brace && p.tok.kind != .eof {
		if p.tok.kind == .slashdash {
			p.skip_commented_node()!
			continue
		}
		if p.tok.kind in [.newline, .semicolon] {
			p.advance()
			continue
		}
		p.skip_node()!
	}
	if p.tok.kind == .eof {
		return kdl_err(p.tok.line, p.tok.col, 'unterminated children block')
	}
	p.advance()
}

fn (mut p Parser) skip_entry() !bool {
	mut has_type_ann := false
	if p.tok.kind == .type_annotation {
		has_type_ann = true
		p.advance()
	}
	if p.tok.kind in [.identifier, .string_val] {
		p.advance()
		if p.tok.kind in [.equals, .colon] {
			if has_type_ann {
				return kdl_err(p.tok.line, p.tok.col,
					'kdl: type annotation before property key is not allowed')
			}
			p.advance()
			if p.tok.kind == .type_annotation { p.advance() }
			if p.tok.kind in [.identifier, .string_val, .int_val, .float_val, .bool_val, .null_val,
				.suffixed_decimal] {
				p.advance()
				return true
			}
			return kdl_err(p.tok.line, p.tok.col, 'kdl: expected property value after slashdash')
		}
		return true
	}
	if has_type_ann {
		if p.tok.kind in [.int_val, .float_val, .bool_val, .null_val, .suffixed_decimal] {
			p.advance()
			return true
		}
		return kdl_err(p.tok.line, p.tok.col, 'kdl: expected entry after slashdash')
	}
	if p.tok.kind in [.int_val, .float_val, .bool_val, .null_val, .suffixed_decimal] {
		p.advance()
		return true
	}
	return false
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
	mut saw_slashdashed_child := false
	for p.tok.kind in [.identifier, .string_val, .int_val, .float_val, .bool_val, .null_val,
		.type_annotation, .slashdash, .suffixed_decimal] {
		if p.tok.kind == .slashdash {
			is_child := p.skip_commented_entry_or_child()!
			if is_child {
				saw_slashdashed_child = true
				break
			}
			continue
		}
		node.entries << p.parse_entry()!
	}
	// Propagate scanner errors from invalid tokens
	if p.tok.kind == .eof && p.tok.lit.len > 0 {
		return kdl_err(p.tok.line, p.tok.col, p.tok.lit)
	}
	if saw_slashdashed_child {
		for p.tok.kind == .slashdash {
			p.skip_commented_child()!
		}
	}
	if p.tok.kind == .l_brace {
		p.advance()
		for p.tok.kind != .r_brace && p.tok.kind != .eof {
			if p.tok.kind == .slashdash {
				p.skip_commented_node()!
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
			for p.tok.kind == .slashdash {
				p.skip_commented_child()!
			}
			if p.tok.kind !in [.newline, .semicolon, .r_brace, .eof] {
				return kdl_err(p.tok.line, p.tok.col,
					'kdl: expected newline, semicolon, } or EOF after children block')
			}
		} else {
			return kdl_err(p.tok.line, p.tok.col, 'unterminated children block')
		}
	}
	if saw_slashdashed_child && p.tok.kind !in [.newline, .semicolon, .r_brace, .eof] {
		return kdl_err(p.tok.line, p.tok.col,
			'kdl: expected newline, semicolon, } or EOF after slashdashed children block')
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
		if p.peek().kind in [.equals, .colon] {
			if type_ann.len > 0 {
				return kdl_err(p.tok.line, p.tok.col,
					'kdl: type annotation before property key is not allowed')
			}
			p.advance()
			p.advance()
			mut val_type_ann := ''
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
			v := parse_int(p.tok.lit) or { return kdl_err(p.tok.line, p.tok.col, err.msg()) }
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

fn parse_int(lit string) !document.Value {
	mut flag := document.ValueFlag.none
	mut neg := false
	mut start := 0
	if lit.len > 0 && (lit[0] == 45 || lit[0] == 43) {
		neg = lit[0] == 45
		start = 1
	}
	mut base := u64(10)
	mut digit_start := start
	if lit.len > start + 1 && lit[start] == 48 {
		if lit[start + 1] == 120 || lit[start + 1] == 88 {
			flag = .hex
			base = 16
			digit_start = start + 2
		} else if lit[start + 1] == 111 || lit[start + 1] == 79 {
			flag = .octal
			base = 8
			digit_start = start + 2
		} else if lit[start + 1] == 98 || lit[start + 1] == 66 {
			flag = .binary
			base = 2
			digit_start = start + 2
		}
	}
	result := parse_i64_digits(lit, digit_start, base, neg)!
	return document.IntVal{
		value: result
		flag:  flag
	}
}

fn parse_i64_digits(lit string, start int, base u64, neg bool) !i64 {
	i64_min_abs := u64(1) << 63
	max_abs := if neg { i64_min_abs } else { i64_min_abs - 1 }
	mut acc := u64(0)
	for i in start .. lit.len {
		digit := digit_value(lit[i])
		if digit >= base {
			return error('kdl: invalid digit in integer literal')
		}
		if acc > (max_abs - digit) / base {
			return error('kdl: integer literal out of i64 range')
		}
		acc = acc * base + digit
	}
	if neg {
		if acc == i64_min_abs {
			return -9223372036854775807 - 1
		}
		return -i64(acc)
	}
	return i64(acc)
}

fn digit_value(c u8) u64 {
	if c >= 48 && c <= 57 {
		return u64(c - 48)
	}
	if c >= 97 && c <= 102 {
		return u64(c - 97 + 10)
	}
	if c >= 65 && c <= 70 {
		return u64(c - 65 + 10)
	}
	return u64(255)
}

fn parse_suffixed(lit string) document.Value {
	return document.StringVal{
		value: lit
		flag:  .bare
	}
}
