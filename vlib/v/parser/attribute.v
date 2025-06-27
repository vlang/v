module parser

import v.ast
import v.token

fn (mut p Parser) parse_attr(is_at bool) ast.Attr {
	mut kind := ast.AttrKind.plain
	p.inside_attr_decl = true
	defer {
		p.inside_attr_decl = false
	}
	apos := if is_at { p.peek_token(-2).pos() } else { p.prev_tok.pos() }
	if p.tok.kind == .key_unsafe {
		p.next()
		return ast.Attr{
			name:   'unsafe'
			kind:   kind
			pos:    apos.extend(p.tok.pos())
			has_at: is_at
		}
	}
	mut name := ''
	mut has_arg := false
	mut arg := ''
	mut comptime_cond := ast.empty_expr
	mut comptime_cond_opt := false
	if p.tok.kind == .key_if {
		kind = .comptime_define
		p.next()
		p.comptime_if_cond = true
		p.inside_if_expr = true
		p.inside_ct_if_expr = true
		comptime_cond = p.expr(0)
		p.comptime_if_cond = false
		p.inside_if_expr = false
		p.inside_ct_if_expr = false
		if comptime_cond is ast.PostfixExpr {
			comptime_cond_opt = true
		}
		name = comptime_cond.str()
	} else if p.tok.kind == .string {
		name = p.tok.lit
		kind = .string
		p.next()
	} else {
		name = p.check_name()
		// support dot prefix `module.name: arg`
		if p.tok.kind == .dot {
			p.next()
			name += '.'
			name += p.check_name()
		}
		if p.tok.kind == .colon {
			has_arg = true
			p.next()
			if p.tok.kind == .name { // `name: arg`
				kind = .plain
				arg = p.check_name()
			} else if p.tok.kind == .number { // `name: 123`
				kind = .number
				arg = p.tok.lit
				p.next()
			} else if p.tok.kind == .string { // `name: 'arg'`
				kind = .string
				arg = p.tok.lit
				p.next()
			} else if p.tok.kind == .key_true || p.tok.kind == .key_false { // `name: true`
				kind = .bool
				arg = p.tok.kind.str()
				p.next()
			} else if token.is_key(p.tok.lit) { // // `name: keyword`
				kind = .plain
				arg = p.check_name()
			} else {
				p.unexpected(additional_msg: 'an argument is expected after `:`')
			}
		}
	}
	return ast.Attr{
		name:    name
		has_arg: has_arg
		arg:     arg
		kind:    kind
		ct_expr: comptime_cond
		ct_opt:  comptime_cond_opt
		pos:     apos.extend(p.tok.pos())
		has_at:  is_at
	}
}

fn (mut p Parser) is_attributes() bool {
	if p.tok.kind != .lsbr {
		return false
	}
	mut i := 0
	for {
		tok := p.peek_token(i)
		if tok.kind == .eof || tok.line_nr != p.tok.line_nr {
			return false
		}
		if tok.kind == .rsbr {
			break
		}
		i++
	}
	peek_rsbr_tok := p.peek_token(i + 1)
	if peek_rsbr_tok.line_nr == p.tok.line_nr && peek_rsbr_tok.kind != .rcbr {
		return false
	}
	return true
}

// when is_top_stmt is true, attrs are added to p.attrs
fn (mut p Parser) attributes() {
	start_pos := p.tok.pos()
	mut is_at := false
	if p.tok.kind == .lsbr {
		if p.pref.is_fmt {
		} else {
			p.error('`[attr]` has been deprecated, use `@[attr]` instead')
		}
		// [attr]
		p.check(.lsbr)
	} else if p.tok.kind == .at {
		// @[attr]
		p.check(.at)
		p.check(.lsbr)
		is_at = true
	}
	mut has_ctdefine := false
	for p.tok.kind != .rsbr {
		attr_start_pos := p.tok.pos()
		attr := p.parse_attr(is_at)
		if p.attrs.contains(attr.name) && attr.name != 'wasm_export' {
			p.error_with_pos('duplicate attribute `${attr.name}`', attr_start_pos.extend(p.prev_tok.pos()))
			return
		}
		if attr.kind == .comptime_define {
			if has_ctdefine {
				p.error_with_pos('only one `[if flag]` may be applied at a time `${attr.name}`',
					attr_start_pos.extend(p.prev_tok.pos()))
				return
			} else {
				has_ctdefine = true
			}
		}
		p.attrs << attr
		if p.tok.kind != .semicolon {
			if p.tok.kind == .rsbr {
				p.next()
				break
			}
			p.unexpected(expecting: '`;`')
			return
		}
		p.next()
	}
	if p.attrs.len == 0 {
		p.error_with_pos('attributes cannot be empty', start_pos.extend(p.tok.pos()))
		return
	}
	// TODO: remove when old attr syntax is removed
	if p.inside_struct_attr_decl && p.tok.kind == .lsbr {
		p.error_with_pos('multiple attributes should be in the same [], with ; separators',
			p.prev_tok.pos().extend(p.tok.pos()))
		return
	} else if p.inside_struct_attr_decl && p.tok.kind == .at {
		p.error_with_pos('multiple attributes should be in the same @[], with ; separators',
			p.prev_tok.pos().extend(p.tok.pos()))
		return
	}
}
