module main

import strings
import term
import v.pref
import v.scanner
import v.token

const highlight_builtin_types = ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'isize',
	'u8', 'u16', 'u32', 'u64', 'uint', 'usize', 'u128', 'rune', 'f32', 'f64', 'byteptr', 'voidptr',
	'any']

struct ScannedToken {
	kind  token.Token
	start int
	end   int
	lit   string
}

fn scan_code(code string) []ScannedToken {
	mut fs := token.FileSet.new()
	mut file := fs.add_file('vdoc-snippet.v', code.len)
	file.index_lines(code)
	prefs := pref.new_preferences()
	mut scanner_ := scanner.new_scanner(prefs, .scan_comments)
	scanner_.init(file, code)
	mut tokens := []ScannedToken{}
	for {
		kind := scanner_.scan()
		if kind == .eof {
			break
		}
		if kind != .semicolon && scanner_.offset > scanner_.pos {
			tokens << ScannedToken{
				kind:  kind
				start: scanner_.pos
				end:   scanner_.offset
				lit:   scanner_.lit
			}
		}
	}
	return tokens
}

// HighlightTokenTyp is the kind of code that a token is, as far as highlighting goes.
// `highlight_tokens` decides it once, and each output format only maps it to its own styling.
enum HighlightTokenTyp {
	boolean
	builtin
	char
	comment
	function
	keyword
	name
	number
	operator
	punctuation
	string
	string_interp
	none
	module_
	type_name
}

struct HighlightedToken {
	typ   HighlightTokenTyp
	start int
	end   int
}

// highlight_tokens scans `code`, and decides for every token what kind of code it is. Both the
// terminal output (`color_highlight`) and the HTML output (`html_highlight`) use it, so that
// they highlight the same things, and only differ in how each kind is displayed.
fn highlight_tokens(code string) []HighlightedToken {
	tokens := scan_code(code)
	mut highlighted := []HighlightedToken{cap: tokens.len}
	// The brace depth, and the depths at which a string interpolation `${...}` was opened.
	// The scanner reports `'a${x}b'` as `'a`, `$`, `{`, `x`, `}`, `b'`, so the `{` and `}`
	// that delimit an interpolation have to be told apart from those inside the expression.
	mut brace_depth := 0
	mut interpolation_depths := []int{}
	for i, scanned in tokens {
		prev_kind := if i > 0 { tokens[i - 1].kind } else { token.Token.unknown }
		next_kind := if i + 1 < tokens.len { tokens[i + 1].kind } else { token.Token.eof }
		mut is_interpolation_brace := false
		if scanned.kind == .lcbr {
			brace_depth++
			if prev_kind == .str_dollar {
				interpolation_depths << brace_depth
				is_interpolation_brace = true
			}
		} else if scanned.kind == .rcbr {
			if interpolation_depths.len > 0 && interpolation_depths.last() == brace_depth {
				interpolation_depths.delete_last()
				is_interpolation_brace = true
			}
			brace_depth--
		}
		typ := match scanned.kind {
			.str_dollar {
				HighlightTokenTyp.string_interp
			}
			.lcbr, .rcbr {
				if is_interpolation_brace {
					HighlightTokenTyp.string_interp
				} else {
					HighlightTokenTyp.punctuation
				}
			}
			.comment {
				HighlightTokenTyp.comment
			}
			.string {
				HighlightTokenTyp.string
			}
			.char {
				HighlightTokenTyp.char
			}
			.number {
				HighlightTokenTyp.number
			}
			.key_true, .key_false {
				HighlightTokenTyp.boolean
			}
			.key_none {
				HighlightTokenTyp.none
			}
			.name {
				if scanned.lit in highlight_builtin_types || scanned.lit == 'chan' {
					HighlightTokenTyp.builtin
				} else if next_kind == .lpar {
					HighlightTokenTyp.function
				} else if scanned.lit.len > 0 && scanned.lit[0].is_capital() {
					HighlightTokenTyp.type_name
				} else if next_kind == .dot {
					HighlightTokenTyp.module_
				} else {
					HighlightTokenTyp.name
				}
			}
			else {
				if scanned.kind.is_keyword() {
					HighlightTokenTyp.keyword
				} else if scanned.kind == .question || scanned.kind.is_assignment()
					|| scanned.kind.is_prefix() || scanned.kind.is_infix()
					|| scanned.kind.is_postfix() {
					HighlightTokenTyp.operator
				} else {
					HighlightTokenTyp.punctuation
				}
			}
		}
		highlighted << HighlightedToken{
			typ:   typ
			start: scanned.start
			end:   scanned.end
		}
	}
	return highlighted
}

// ansi_highlight returns `raw` with the terminal color for the kind of code `typ`.
fn ansi_highlight(typ HighlightTokenTyp, raw string) string {
	return match typ {
		.comment { term.gray(raw) }
		.string, .char { term.yellow(raw) }
		.number, .keyword, .module_ { term.bright_blue(raw) }
		.boolean, .string_interp { term.bright_magenta(raw) }
		.none { term.red(raw) }
		.builtin, .type_name { term.green(raw) }
		.function { term.cyan(raw) }
		.operator { term.magenta(raw) }
		else { raw }
	}
}

fn color_highlight(code string) string {
	mut out := strings.new_builder(code.len + 32)
	mut offset := 0
	for highlighted in highlight_tokens(code) {
		if highlighted.start > offset {
			out.write_string(code[offset..highlighted.start])
		}
		out.write_string(ansi_highlight(highlighted.typ, code[highlighted.start..highlighted.end]))
		offset = highlighted.end
	}
	if offset < code.len {
		out.write_string(code[offset..])
	}
	return out.str()
}
