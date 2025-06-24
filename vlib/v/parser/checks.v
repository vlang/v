module parser

import v.ast
import v.util
import v.token

// peek token `type Fn = fn () int`
fn (p &Parser) is_fn_type_decl() bool {
	mut n := 1
	mut tok := p.tok
	mut prev_tok := p.tok
	cur_ln := p.tok.line_nr
	for {
		tok = p.scanner.peek_token(n)
		if tok.kind == .eof {
			break
		}
		if tok.kind in [.lpar, .rpar] {
			n++
			prev_tok = tok
			continue
		}
		if tok.kind == .pipe {
			if tok.pos - prev_tok.pos > prev_tok.len {
				return false
			}
		}
		if tok.line_nr > cur_ln {
			break
		}
		prev_tok = tok
		n++
	}
	return true
}

fn (p &Parser) has_prev_newline() bool {
	mut tok := p.tok
	mut prev_tok := p.prev_tok
	mut idx := -1

	for {
		if tok.line_nr - prev_tok.line_nr - prev_tok.lit.count('\n') > 1 {
			return true
		}
		if prev_tok.kind == .comment {
			idx--
			tok = prev_tok
			prev_tok = p.peek_token(idx)
			continue
		}
		break
	}
	return false
}

fn (p &Parser) has_prev_line_comment_or_label() bool {
	return p.prev_tok.kind == .colon || (p.prev_tok.kind == .comment
		&& p.tok.line_nr - p.prev_tok.line_nr == 1
		&& p.prev_tok.line_nr - p.peek_token(-2).line_nr > 0)
}

fn (p &Parser) is_array_type() bool {
	mut i := 1
	mut tok := p.tok
	line_nr := p.tok.line_nr

	for {
		tok = p.peek_token(i)
		if tok.line_nr != line_nr {
			return false
		}
		if tok.kind in [.name, .amp] {
			return true
		}
		if tok.kind == .eof {
			break
		}
		i++
		if tok.kind == .lsbr || tok.kind != .rsbr {
			continue
		}
	}
	return false
}

fn (mut p Parser) is_following_concrete_types() bool {
	if !(p.tok.kind in [.lt, .lsbr] && p.tok.is_next_to(p.prev_tok)) {
		return false
	}
	mut i := 1
	for {
		cur_tok := p.peek_token(i)
		if cur_tok.kind == .eof {
			return false
		} else if cur_tok.kind == .rsbr {
			break
		} else if cur_tok.kind == .name {
			if p.peek_token(i + 1).kind == .dot {
				if p.is_typename(cur_tok) {
					return false
				}
				i++
			} else if !(p.is_typename(cur_tok) && !(cur_tok.lit.len == 1
				&& !cur_tok.lit[0].is_capital())) {
				return false
			}
		} else if cur_tok.kind != .comma {
			return false
		}
		i++
	}
	return true
}

@[direct_array_access]
fn (p &Parser) is_generic_struct_init() bool {
	lit0_is_capital := p.tok.kind != .eof && p.tok.lit.len > 0 && p.tok.lit[0].is_capital()
	if !lit0_is_capital || p.peek_tok.kind !in [.lt, .lsbr] {
		return false
	}
	if p.peek_tok.kind == .lt {
		return true
	} else {
		mut i := 2
		mut nested_sbr_count := 0
		for {
			cur_tok := p.peek_token(i)
			if cur_tok.kind == .eof
				|| cur_tok.kind !in [.amp, .dot, .comma, .name, .lpar, .rpar, .lsbr, .rsbr, .key_fn] {
				break
			}
			if cur_tok.kind == .lsbr {
				nested_sbr_count++
			} else if cur_tok.kind == .rsbr {
				if nested_sbr_count > 0 {
					nested_sbr_count--
				} else {
					if p.peek_token(i + 1).kind == .lcbr {
						return true
					}
					break
				}
			}
			i++
		}
	}
	return false
}

@[direct_array_access; inline]
fn (p &Parser) is_typename(t token.Token) bool {
	return t.kind == .name && (t.lit[0].is_capital() || p.table.known_type(t.lit))
}

// heuristics to detect `func<T>()` from `var < expr`
// 1. `f<[]` is generic(e.g. `f<[]int>`) because `var < []` is invalid
// 2. `f<map[` is generic(e.g. `f<map[string]string>)
// 3. `f<foo>` is generic because `v1 < foo > v2` is invalid syntax
// 4. `f<foo<bar` is generic when bar is not generic T (f<foo<T>(), in contrast, is not generic!)
// 5. `f<Foo,` is generic when Foo is typename.
//	   otherwise it is not generic because it may be multi-value (e.g. `return f < foo, 0`).
// 6. `f<mod.Foo>` is same as case 3
// 7. `f<mod.Foo,` is same as case 5
// 8. if there is a &, ignore the & and see if it is a type
// 9. otherwise, it's not generic
// see also test_generic_detection in vlib/v/tests/generics_test.v
@[direct_array_access]
fn (p &Parser) is_generic_call() bool {
	lit0_is_capital := p.tok.kind != .eof && p.tok.lit.len > 0 && p.tok.lit[0].is_capital()
	if lit0_is_capital || p.peek_tok.kind !in [.lt, .lsbr] {
		return false
	}
	mut tok2 := p.peek_token(2)
	mut tok3 := p.peek_token(3)
	mut tok4 := p.peek_token(4)
	mut tok5 := p.peek_token(5)
	mut kind2, mut kind3, mut kind4, mut kind5 := tok2.kind, tok3.kind, tok4.kind, tok5.kind
	if kind2 == .amp { // if there is a & in front, shift everything left
		tok2 = tok3
		kind2 = kind3
		tok3 = tok4
		kind3 = kind4
		tok4 = tok5
		kind4 = kind5
		tok5 = p.peek_token(6)
		kind5 = tok5.kind
	}

	if kind2 == .lsbr {
		// case 1 (array or fixed array type)
		return tok3.kind == .rsbr || (tok4.kind == .rsbr && p.is_typename(tok5))
	}

	if kind2 == .name {
		if kind3 == .lsbr && tok2.lit == 'map' {
			// case 2
			return true
		}
		if p.peek_tok.kind == .lt {
			return match kind3 {
				.gt { true } // case 3
				.lt { !(tok4.lit.len == 1 && tok4.lit[0].is_capital()) } // case 4
				.comma { p.is_typename(tok2) } // case 5
				// case 6 and 7
				.dot { kind4 == .name && (kind5 == .gt || (kind5 == .comma && p.is_typename(tok4))) }
				else { false }
			}
		} else if p.peek_tok.kind == .lsbr {
			mut i := 3
			mut nested_sbr_count := 0
			for {
				cur_tok := p.peek_token(i)
				if cur_tok.kind == .eof
					|| cur_tok.kind !in [.amp, .dot, .comma, .name, .lpar, .rpar, .lsbr, .rsbr, .key_fn] {
					break
				}
				if cur_tok.kind == .lsbr {
					nested_sbr_count++
				}
				if cur_tok.kind == .rsbr {
					if nested_sbr_count > 0 {
						nested_sbr_count--
					} else {
						prev_tok := p.peek_token(i - 1)
						// `funcs[i]()` is not generic call
						if !(p.is_typename(prev_tok) || prev_tok.kind == .rsbr) {
							return false
						}
						if p.peek_token(i + 1).kind == .lpar {
							return true
						}
						break
					}
				}
				i++
			}
		}
	}
	return false
}

const valid_tokens_inside_types = [token.Kind.lsbr, .rsbr, .name, .dot, .comma, .key_fn, .lt]

fn (mut p Parser) is_generic_cast() bool {
	if !ast.type_can_start_with_token(&p.tok) {
		return false
	}
	mut i := 0
	mut level := 0
	mut lt_count := 0
	for {
		i++
		tok := p.peek_token(i)

		if tok.kind in [.lt, .lsbr] {
			lt_count++
			level++
		} else if tok.kind in [.gt, .rsbr] {
			level--
		}
		if lt_count > 0 && level == 0 {
			break
		}

		if i > 20 || tok.kind !in valid_tokens_inside_types {
			return false
		}
	}
	next_tok := p.peek_token(i + 1)
	// `next_tok` is the token following the closing `>` of the generic type: MyType<int>{
	//                                                                                   ^
	// if `next_tok` is a left paren, then the full expression looks something like
	// `Foo<string>(` or `Foo<mod.Type>(`, which are valid type casts - return true
	if next_tok.kind == .lpar {
		return true
	}
	// any other token is not a valid generic cast, however
	return false
}

// is_generic_name returns true if the current token is a generic name.
@[inline]
fn (p &Parser) is_generic_name() bool {
	return p.tok.kind == .name && util.is_generic_type_name(p.tok.lit)
}
