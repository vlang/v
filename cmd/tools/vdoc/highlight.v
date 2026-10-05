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

fn color_highlight(code string) string {
	tokens := scan_code(code)
	mut out := strings.new_builder(code.len + 32)
	mut offset := 0
	// The brace depth, and the depths at which a string interpolation `${...}` was opened.
	// The scanner reports `'a${x}b'` as `'a`, `$`, `{`, `x`, `}`, `b'`, so the `{` and `}`
	// that delimit an interpolation have to be told apart from those inside the expression.
	mut brace_depth := 0
	mut interpolation_depths := []int{}
	for i, scanned in tokens {
		if scanned.start > offset {
			out.write_string(code[offset..scanned.start])
		}
		raw := code[scanned.start..scanned.end]
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
		colored := match scanned.kind {
			.str_dollar {
				term.bright_magenta(raw)
			}
			.lcbr, .rcbr {
				if is_interpolation_brace { term.bright_magenta(raw) } else { raw }
			}
			.comment { term.gray(raw) }
			.string, .char { term.yellow(raw) }
			.number { term.bright_blue(raw) }
			.key_true, .key_false { term.bright_magenta(raw) }
			.key_none { term.red(raw) }
			.name {
				if scanned.lit in highlight_builtin_types {
					term.green(raw)
				} else if next_kind == .lpar {
					term.cyan(raw)
				} else if scanned.lit.len > 0 && scanned.lit[0].is_capital() {
					term.green(raw)
				} else if next_kind == .dot {
					term.bright_blue(raw)
				} else {
					raw
				}
			}
			else {
				if scanned.kind.is_keyword() {
					term.bright_blue(raw)
				} else if scanned.kind.is_assignment() || scanned.kind.is_prefix()
					|| scanned.kind.is_infix() || scanned.kind.is_postfix() {
					term.magenta(raw)
				} else {
					raw
				}
			}
		}
		out.write_string(colored)
		offset = scanned.end
	}
	if offset < code.len {
		out.write_string(code[offset..])
	}
	return out.str()
}
