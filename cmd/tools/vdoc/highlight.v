module main

import term
import strings
import v.scanner
import v.ast
import v.token
import v.pref

const highlight_builtin_types = ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'isize',
	'byte', 'u8', 'u16', 'u32', 'u64', 'usize', 'u128', 'rune', 'f32', 'f64', 'byteptr', 'voidptr',
	'any']

fn color_highlight(code string, tb &ast.Table) string {
	highlight_code := fn (tok token.Token, typ HighlightTokenTyp) string {
		mut lit := ''
		match typ {
			.unone, .operator, .punctuation {
				lit = tok.kind.str()
			}
			.string {
				use_double_quote := tok.lit.contains("'") && !tok.lit.contains('"')
				unescaped_val := tok.lit.replace('\\\\', '\x01').replace_each(["\\'", "'", '\\"',
					'"'])
				if use_double_quote {
					s := unescaped_val.replace_each(['\x01', '\\\\', '"', '\\"'])
					lit = term.yellow('"${s}"')
				} else {
					s := unescaped_val.replace_each(['\x01', '\\\\', "'", "\\'"])
					lit = term.yellow("'${s}'")
				}
			}
			.char {
				lit = term.yellow('`${tok.lit}`')
			}
			.comment {
				lit = if tok.lit != '' && tok.lit[0] == 1 {
					term.gray('//${tok.lit[1..]}')
				} else {
					term.gray('//${tok.lit}')
				}
			}
			.keyword {
				lit = term.bright_blue(tok.lit)
			}
			.builtin, .symbol {
				lit = term.green(tok.lit)
			}
			.function {
				lit = term.cyan(tok.lit)
			}
			.number, .module_ {
				lit = term.bright_blue(tok.lit)
			}
			.boolean {
				lit = term.bright_magenta(tok.lit)
			}
			.none {
				lit = term.red(tok.lit)
			}
			.prefix {
				lit = term.magenta(tok.lit)
			}
			else {
				lit = tok.lit
			}
		}
		return lit
	}
	mut s := scanner.new_scanner(code, .parse_comments, &pref.Preferences{ is_fmt: true })
	mut prev_prev := token.Token{}
	mut prev := token.Token{}
	mut tok := s.scan()
	mut next_tok := s.scan()
	mut buf := strings.new_builder(200)
	mut i := 0
	for i < code.len {
		if i == tok.pos {
			mut tok_typ := HighlightTokenTyp.unone
			match tok.kind {
				.name {
					if (tok.lit in highlight_builtin_types || tb.known_type(tok.lit))
						&& (next_tok.kind != .lpar || prev.kind !in [.key_fn, .rpar]) {
						tok_typ = .builtin
					} else if
						(next_tok.kind in [.lcbr, .rpar, .eof, .name, .rcbr, .assign, .key_pub, .key_mut, .pipe, .comma, .comment, .lt, .lsbr]
						&& next_tok.lit !in highlight_builtin_types)
						&& (prev.kind in [.name, .amp, .lcbr, .rsbr, .key_type, .assign, .dot, .not, .question, .rpar, .key_struct, .key_enum, .pipe, .key_interface, .comment, .ellipsis, .comma]
						&& prev.lit !in highlight_builtin_types)
						&& ((tok.lit != '' && tok.lit[0].is_capital())
						|| prev_prev.lit in ['C', 'JS']) {
						tok_typ = .symbol
					} else if tok.lit[0].is_capital() && prev.kind == .lpar
						&& next_tok.kind == .comma {
						tok_typ = .symbol
					} else if next_tok.kind == .lpar
						|| (!(tok.lit != '' && tok.lit[0].is_capital())
						&& next_tok.kind in [.lt, .lsbr] && next_tok.pos == tok.pos + tok.lit.len) {
						tok_typ = .function
					} else if next_tok.kind == .dot {
						if tok.lit in ['C', 'JS'] {
							tok_typ = .prefix
						} else {
							if tok.lit != '' && tok.lit[0].is_capital() {
								tok_typ = .symbol
							} else {
								tok_typ = .module_
							}
						}
					} else if tok.lit in ['r', 'c'] && next_tok.kind == .string {
						tok_typ = .prefix
					} else {
						tok_typ = .name
					}
				}
				.comment {
					tok_typ = .comment
				}
				.chartoken {
					tok_typ = .char
				}
				.string {
					tok_typ = .string
				}
				.number {
					tok_typ = .number
				}
				.key_true, .key_false {
					tok_typ = .boolean
				}
				.lpar, .lcbr, .rpar, .rcbr, .lsbr, .rsbr, .semicolon, .colon, .comma, .dot,
				.dotdot, .ellipsis {
					tok_typ = .punctuation
				}
				.key_none {
					tok_typ = .none
				}
				else {
					if token.is_key(tok.lit) || token.is_decl(tok.kind) {
						tok_typ = .keyword
					} else if tok.kind.is_assign() || tok.is_unary() || tok.kind.is_relational()
						|| tok.kind.is_infix() || tok.kind.is_postfix() {
						tok_typ = .operator
					}
				}
			}
			buf.write_string(highlight_code(tok, tok_typ))
			if prev_prev.kind == .eof || prev.kind == .eof || next_tok.kind == .eof {
				break
			}
			prev_prev = prev
			prev = tok
			i = tok.pos + tok.len
			tok = next_tok
			next_tok = s.scan()
		} else {
			buf.write_u8(code[i])
			i++
		}
	}
	return buf.str()
}
