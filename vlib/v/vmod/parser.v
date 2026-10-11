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
	number
	unknown
}

pub struct Manifest {
pub mut:
	name         string
	base_url     string
	description  string
	version      string
	license      string
	repo_url     string
	repo_branch  string = 'master'
	author       string
	dependencies []string
	unknown      map[string][]string
	catalog      map[string]string
	workspaces   []string
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
			pos:  0
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
	for s.pos < s.text.len && (is_name_alpha(s.text[s.pos]) || s.text[s.pos].is_digit()
		|| s.text[s.pos] == `.`) {
		text += s.text[s.pos].ascii_str()
		s.pos++
	}
	return text
}

fn (mut s Scanner) create_number() string {
	start := s.pos
	for s.pos < s.text.len && (s.text[s.pos].is_digit() || s.text[s.pos] == `.`) {
		s.pos++
	}
	return s.text[start..s.pos]
}

fn (s &Scanner) peek_char(c u8) bool {
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
				continue
			} else if s.pos < s.text.len && s.text[s.pos] == `:` {
				s.tokenize(.field_key, name + ':')
				s.pos++
				continue
			} else {
				s.tokenize(.ident, name)
				continue
			}
		}
		if c.is_digit() {
			s.tokenize(.number, s.create_number())
			continue
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

fn get_array_content(tokens []Token, st_idx int, allow_legacy_dependencies bool) !([]string, int) {
	mut vals := []string{}
	mut idx := st_idx
	if tokens[idx].typ != .labr {
		return error('${err_label} not a valid array, at line ${tokens[idx].line}')
	}
	idx++
	for {
		tok := tokens[idx]
		match tok.typ {
			.str, .ident, .field_key {
				if tok.typ != .str && !allow_legacy_dependencies {
					return error('${err_label} invalid token "${tok.val}", at line ${tok.line}')
				}
				mut value := tok.val
				idx++
				if allow_legacy_dependencies && (tok.typ == .field_key || tokens[idx].typ == .colon) {
					if tok.typ == .field_key {
						value = value.trim_right(':')
					} else {
						idx++
					}
					// Manifest.dependencies stores names only, so ignore legacy version values.
					if tokens[idx].typ !in [.str, .number] {
						return error('${err_label} invalid token "${tokens[idx].val}", at line ${tokens[idx].line}')
					}
					idx++
				}
				vals << value
				if tokens[idx].typ !in [.comma, .rabr] {
					return error('${err_label} invalid separator "${tokens[idx].val}", at line ${tok.line}')
				}
				if tokens[idx].typ == .comma {
					idx++
				}
			}
			.rabr {
				idx++
				break
			}
			else {
				return error('${err_label} invalid token "${tok.val}", at line ${tok.line}')
			}
		}
	}
	return vals, idx
}

fn (mut p Parser) parse() !Manifest {
	if p.scanner.text.len == 0 {
		return error('${err_label} no content.')
	}
	p.scanner.scan_all()
	tokens := p.scanner.tokens
	mut mn := Manifest{}
	if tokens[0].typ != .module_keyword {
		return error('${err_label} v.mod files should start with Module, at line ${tokens[0].line}')
	}
	mut i := 1
	for i < tokens.len {
		tok := tokens[i]
		match tok.typ {
			.lcbr {
				if tokens[i + 1].typ !in [.field_key, .rcbr] {
					return error('${err_label} invalid content after opening brace, at line ${tok.line}')
				}
				i++
				continue
			}
			.rcbr {
				break
			}
			.field_key {
				field_name := tok.val.trim_right(':')
				if i + 1 >= tokens.len {
					return error('${err_label} missing value for field "${field_name}"')
				}
				if tokens[i + 1].typ !in [.str, .labr]
					&& !(field_name == 'catalog' && tokens[i + 1].typ == .lcbr) {
					return error('${err_label} value of field "${field_name}" must be either string or an array of strings, at line ${tok.line}')
				}
				field_value := tokens[i + 1].val
				match field_name {
					'name' {
						mn.name = field_value
					}
					'base_url' {
						mn.base_url = field_value
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
					'repo_branch' {
						mn.repo_branch = field_value
					}
					'description' {
						mn.description = field_value
					}
					'author' {
						mn.author = field_value
					}
					'dependencies' {
						deps, idx := get_array_content(tokens, i + 1, true)!
						mn.dependencies = deps
						i = idx
						continue
					}
					'catalog' {
						if tokens[i + 1].typ != .lcbr {
							return error('${err_label} value of field "catalog" must be an object, at line ${tok.line}')
						}
						mut catalog := map[string]string{}
						mut j := i + 2
						for j < tokens.len && tokens[j].typ != .rcbr {
							mut key := ''
							if tokens[j].typ == .field_key {
								key = tokens[j].val.trim_right(':')
								j++
							} else if tokens[j].typ == .str && j + 1 < tokens.len && tokens[j + 1].typ == .colon {
								key = tokens[j].val
								j += 2
							} else {
								return error('${err_label} invalid catalog entry at line ${tokens[j].line}')
							}
							if key == '' || key in catalog {
								return error('${err_label} empty or duplicate catalog key "${key}"')
							}
							if j >= tokens.len || tokens[j].typ != .str {
								return error('${err_label} catalog value for "${key}" must be a string')
							}
							catalog[key] = tokens[j].val
							j++
							if j < tokens.len && tokens[j].typ == .comma { j++ }
						}
						if j >= tokens.len {
							return error('${err_label} unterminated catalog object')
						}
						if j + 1 >= tokens.len {
							return error('${err_label} unterminated Module object after catalog')
						}
						mn.catalog = catalog
						i = j + 1
						continue
					}
					'workspaces' {
						ws, idx := get_array_content(tokens, i + 1, false)!
						mn.workspaces = ws
						i = idx
						continue
					}
					else {
						if tokens[i + 1].typ == .labr {
							vals, idx := get_array_content(tokens, i + 1, false)!
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
				if tokens[i - 1].typ !in [.str, .rabr, .rcbr] || tokens[i + 1].typ != .field_key {
					return error('${err_label} invalid comma placement, at line ${tok.line}')
				}
				i++
				continue
			}
			else {
				return error('${err_label} invalid token "${tok.val}", at line ${tok.line}')
			}
		}
	}
	return mn
}
