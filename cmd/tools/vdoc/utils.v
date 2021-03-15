module main

import os
import v.doc
import term
import v.table
import v.scanner
import v.token
import strings
import v.pref

[inline]
fn slug(title string) string {
	return title.replace(' ', '-')
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '\t', '\\t'])
}

fn get_sym_name(dn doc.DocNode) string {
	sym_name := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'($dn.parent_name) $dn.name'
	} else {
		dn.name
	}
	return sym_name
}

fn get_node_id(dn doc.DocNode) string {
	tag := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'${dn.parent_name}.$dn.name'
	} else {
		dn.name
	}
	return slug(tag)
}

fn is_module_readme(dn doc.DocNode) bool {
	if dn.comments.len > 0 && dn.content == 'module $dn.name' {
		return true
	}
	return false
}

fn trim_doc_node_description(description string) string {
	mut dn_description := description.replace_each(['\r\n', '\n', '"', '\\"'])
	// 80 is enough to fill one line
	if dn_description.len > 80 {
		dn_description = dn_description[..80]
	}
	if '\n' in dn_description {
		dn_description = dn_description.split('\n')[0]
	}
	// if \ is last character, it ends with \" which leads to a JS error
	if dn_description.ends_with('\\') {
		dn_description = dn_description.trim_right('\\')
	}
	return dn_description
}

fn set_output_type_from_str(format string) OutputType {
	output_type := match format {
		'htm', 'html' { OutputType.html }
		'md', 'markdown' { OutputType.markdown }
		'json' { OutputType.json }
		'stdout' { OutputType.stdout }
		else { OutputType.plaintext }
	}
	return output_type
}

fn get_ignore_paths(path string) ?[]string {
	ignore_file_path := os.join_path(path, '.vdocignore')
	ignore_content := os.read_file(ignore_file_path) or {
		return error_with_code('ignore file not found.', 1)
	}
	mut res := []string{}
	if ignore_content.trim_space().len > 0 {
		rules := ignore_content.split_into_lines().map(it.trim_space())
		mut final := []string{}
		for rule in rules {
			if rule.contains('*.') || rule.contains('**') {
				println('vdoc: Wildcards in ignore rules are not allowed for now.')
				continue
			}
			final << rule
		}
		res = final.map(os.join_path(path, it.trim_right('/')))
	} else {
		mut dirs := os.ls(path) or { return []string{} }
		res = dirs.map(os.join_path(path, it)).filter(os.is_dir(it))
	}
	return res.map(it.replace('/', os.path_separator))
}

fn is_included(path string, ignore_paths []string) bool {
	if path.len == 0 {
		return true
	}
	for ignore_path in ignore_paths {
		if ignore_path !in path {
			continue
		}
		return false
	}
	return true
}

fn get_modules_list(path string, ignore_paths2 []string) []string {
	files := os.ls(path) or { return []string{} }
	mut ignore_paths := get_ignore_paths(path) or { []string{} }
	ignore_paths << ignore_paths2
	mut dirs := []string{}
	for file in files {
		fpath := os.join_path(path, file)
		if os.is_dir(fpath) && is_included(fpath, ignore_paths) && !os.is_link(path) {
			dirs << get_modules_list(fpath, ignore_paths.filter(it.starts_with(fpath)))
		} else if fpath.ends_with('.v') && !fpath.ends_with('_test.v') {
			if path in dirs {
				continue
			}
			dirs << path
		}
	}
	dirs.sort()
	return dirs
}

fn gen_footer_text(d &doc.Doc, include_timestamp bool) string {
	footer_text := 'Powered by vdoc.'
	if !include_timestamp {
		return footer_text
	}
	generated_time := d.time_generated
	time_str := '$generated_time.day $generated_time.smonth() $generated_time.year $generated_time.hhmmss()'
	return '$footer_text Generated on: $time_str'
}

fn color_highlight(code string, tb &table.Table) string {
	builtin := ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'byte', 'u16', 'u32', 'u64',
		'u128', 'rune', 'f32', 'f64', 'int_literal', 'float_literal', 'byteptr', 'voidptr', 'any']
	highlight_code := fn (tok token.Token, typ HighlightTokenTyp) string {
		lit := match typ {
			.unone, .operator, .punctuation {
				tok.kind.str()
			}
			.string {
				term.yellow("'$tok.lit'")
			}
			.char {
				term.yellow('`$tok.lit`')
			}
			.keyword {
				term.blue(tok.lit)
			}
			.builtin, .symbol {
				term.green(tok.lit)
			}
			.function {
				term.cyan(tok.lit)
			}
			.number {
				term.bright_blue(tok.lit)
			}
			else {
				tok.lit
			}
		}
		return lit
	}
	mut s := scanner.new_scanner(code, .parse_comments, &pref.Preferences{})
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
					if (tok.lit in builtin || tb.known_type(tok.lit))
						&& (next_tok.kind != .lpar || prev.kind != .key_fn) {
						tok_typ = .builtin
					} else if next_tok.kind in [.lcbr, .rpar, .eof]
						&& (next_tok.kind != .rpar || prev.kind in [.name, .amp]) {
						tok_typ = .symbol
					} else if next_tok.kind in [.lpar, .lt] {
						tok_typ = .function
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
				.lpar, .lcbr, .rpar, .rcbr, .lsbr, .rsbr, .semicolon, .colon, .comma, .dot {
					tok_typ = .punctuation
				}
				else {
					if token.is_key(tok.lit) || token.is_decl(tok.kind) {
						tok_typ = .keyword
					} else if tok.kind == .decl_assign || tok.kind.is_assign() || tok.is_unary()
						|| tok.kind.is_relational() || tok.kind.is_infix() {
						tok_typ = .operator
					}
				}
			}
			buf.write_string(highlight_code(tok, tok_typ))
			if prev.kind != .eof {
				prev = tok
			} else {
				break
			}
			if next_tok.kind != .eof {
				i = tok.pos + tok.len
				tok = next_tok
				next_tok = s.scan()
			} else {
				break
			}
		} else {
			buf.write_b(code[i])
			i++
		}
	}
	return buf.str()
}
