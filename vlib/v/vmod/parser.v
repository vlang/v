module vmod

import os

const err_label = 'vmod:'

enum TokenKind {
	module_keyword
	field_key
	lcbr
	rcbr
	labr
	rabr
	comma
	colon
	eof
	str
	ident
	unknown
}

pub struct Manifest {
pub mut:
	name         string
	version      string
	description  string
	dependencies []string
	license      string
	repo_url     string
	author       string
	unknown      map[string][]string
}

struct Scanner {
mut:
	pos         int
	line        int = 1
	text        string
	inside_text bool
	tokens      []Token
}

struct Parser {
mut:
	file_path string
	scanner   Scanner
}

struct Token {
	typ  TokenKind
	val  string
	line int
}

pub fn from_file(vmod_path string) !Manifest {
	if !os.exists(vmod_path) {
		return error('v.mod: v.mod file not found.')
	}
	contents := os.read_file(vmod_path) or { '' }
	return decode(contents)
}

pub fn decode(contents string) !Manifest {
	mut parser := Parser{
		scanner: Scanner{
			pos: 0
			text: contents
		}
	}
	return parser.parse()
}

fn (mut s Scanner) tokenize(t_type TokenKind, val string) {
	s.tokens << Token{t_type, val, s.line}
}

fn (mut s Scanner) skip_whitespace() {
	for s.pos < s.text.len && s.text[s.pos].is_space() {
		s.pos++
	}
}

fn is_name_alpha(chr u8) bool {
	return chr.is_letter() || chr == `_`
}

fn (mut s Scanner) create_string(q u8) string {
	mut str := ''
	for s.pos < s.text.len && s.text[s.pos] != q {
		if s.text[s.pos] == `\\` && s.text[s.pos + 1] == q {
			str += s.text[s.pos..s.pos + 1]
			s.pos += 2
		} else {
			str += s.text[s.pos].ascii_str()
			s.pos++
		}
	}
	return str
}

fn (mut s Scanner) create_ident() string {
	mut text := ''
	for s.pos < s.text.len && is_name_alpha(s.text[s.pos]) {
		text += s.text[s.pos].ascii_str()
		s.pos++
	}
	return text
}

fn (s Scanner) peek_char(c u8) bool {
	return s.pos - 1 < s.text.len && s.text[s.pos - 1] == c
}

fn (mut s Scanner) scan_all() {
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if c.is_space() || c == `\\` {
			s.pos++
			if c == `\n` {
				s.line++
			}
			continue
		}
		if is_name_alpha(c) {
			name := s.create_ident()
			if name == 'Module' {
				s.tokenize(.module_keyword, name)
				s.pos++
				continue
			} else if s.pos < s.text.len && s.text[s.pos] == `:` {
				s.tokenize(.field_key, name + ':')
				s.pos += 2
				continue
			} else {
				s.tokenize(.ident, name)
				s.pos++
				continue
			}
		}
		if c in [`'`, `\"`] && !s.peek_char(`\\`) {
			s.pos++
			str := s.create_string(c)
			s.tokenize(.str, str)
			s.pos++
			continue
		}
		match c {
			`{` { s.tokenize(.lcbr, c.ascii_str()) }
			`}` { s.tokenize(.rcbr, c.ascii_str()) }
			`[` { s.tokenize(.labr, c.ascii_str()) }
			`]` { s.tokenize(.rabr, c.ascii_str()) }
			`:` { s.tokenize(.colon, c.ascii_str()) }
			`,` { s.tokenize(.comma, c.ascii_str()) }
			else { s.tokenize(.unknown, c.ascii_str()) }
		}
		s.pos++
	}
	s.tokenize(.eof, 'eof')
}

fn get_array_content(tokens []Token, st_idx int) !([]string, int) {
	mut vals := []string{}
	mut idx := st_idx
	if tokens[idx].typ != .labr {
		return error('${vmod.err_label} not a valid array, at line ${tokens[idx].line}')
	}
	idx++
	for {
		tok := tokens[idx]
		match tok.typ {
			.str {
				vals << tok.val
				if tokens[idx + 1].typ !in [.comma, .rabr] {
					return error('${vmod.err_label} invalid separator "${tokens[idx + 1].val}", at line ${tok.line}')
				}
				idx += if tokens[idx + 1].typ == .comma { 2 } else { 1 }
			}
			.rabr {
				idx++
				break
			}
			else {
				return error('${vmod.err_label} invalid token "${tok.val}", at line ${tok.line}')
			}
		}
	}
	return vals, idx
}

fn (mut p Parser) parse() !Manifest {
	if p.scanner.text.len == 0 {
		return error('${vmod.err_label} no content.')
	}
	p.scanner.scan_all()
	tokens := p.scanner.tokens
	mut mn := Manifest{}
	if tokens[0].typ != .module_keyword {
		return error('${vmod.err_label} v.mod files should start with Module, at line ${tokens[0].line}')
	}
	mut i := 1
	for i < tokens.len {
		tok := tokens[i]
		match tok.typ {
			.lcbr {
				if tokens[i + 1].typ !in [.field_key, .rcbr] {
					return error('${vmod.err_label} invalid content after opening brace, at line ${tok.line}')
				}
				i++
				continue
			}
			.rcbr {
				break
			}
			.field_key {
				field_name := tok.val.trim_right(':')
				if tokens[i + 1].typ !in [.str, .labr] {
					return error('${vmod.err_label} value of field "${field_name}" must be either string or an array of strings, at line ${tok.line}')
				}
				field_value := tokens[i + 1].val
				match field_name {
					'name' {
						mn.name = field_value
					}
					'version' {
						mn.version = field_value
					}
					'license' {
						mn.license = field_value
					}
					'repo_url' {
						mn.repo_url = field_value
					}
					'description' {
						mn.description = field_value
					}
					'author' {
						mn.author = field_value
					}
					'dependencies' {
						deps, idx := get_array_content(tokens, i + 1)!
						mn.dependencies = deps
						i = idx
						continue
					}
					else {
						if tokens[i + 1].typ == .labr {
							vals, idx := get_array_content(tokens, i + 1)!
							mn.unknown[field_name] = vals
							i = idx
							continue
						}
						mn.unknown[field_name] = [field_value]
					}
				}
				i += 2
				continue
			}
			.comma {
				if tokens[i - 1].typ !in [.str, .rabr] || tokens[i + 1].typ != .field_key {
					return error('${vmod.err_label} invalid comma placement, at line ${tok.line}')
				}
				i++
				continue
			}
			else {
				return error('${vmod.err_label} invalid token "${tok.val}", at line ${tok.line}')
			}
		}
	}
	return mn
}
