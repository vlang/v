module main

import strings
import term
import v.pref
import v.scanner
import v.token

const highlight_builtin_types = ['bool', 'string', 'i8', 'i16', 'int', 'i64', 'i128', 'isize',
	'u8', 'u16', 'u32', 'u64', 'uint', 'usize', 'u128', 'rune', 'f32', 'f64', 'byteptr', 'voidptr',
	'any']!

struct ScannedToken {
	kind                        token.Token
	start                       int
	end                         int
	lit                         string
	inside_string_interpolation bool
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
		inside_interpolation := scanner_.in_str_inter || scanner_.in_str_incomplete
			|| scanner_.str_parent_quotes.len > 0
		kind := scanner_.scan()
		if kind == .eof {
			break
		}
		if kind != .semicolon && scanner_.offset > scanner_.pos {
			tokens << ScannedToken{
				kind:                        kind
				start:                       scanner_.pos
				end:                         scanner_.offset
				lit:                         scanner_.lit
				inside_string_interpolation: inside_interpolation || scanner_.in_str_inter
					|| scanner_.in_str_incomplete || scanner_.str_parent_quotes.len > 0
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
	enum_value
	attribute
}

// highlight_value_end_kinds are the tokens that can end a value, so that a `.` right after one
// of them is a field or method access, and not the start of an enum value like `.closed`.
const highlight_value_end_kinds = [token.Token.name, .rpar, .rsbr, .rcbr, .string, .char, .number,
	.question, .not]!

// highlight_reflection_fields are what compile time reflection reads from a type, like
// `T.fields` in `$for f in T.fields`. They are not enum values, despite following a type name.
const highlight_reflection_fields = ['fields', 'methods', 'values', 'attributes', 'variants']!

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
	interpolation_parts := find_interpolation_parts(tokens)
	enum_values := find_enum_values(tokens)
	attribute_words := find_attribute_words(tokens)
	mut highlighted := []HighlightedToken{cap: tokens.len}
	for i, scanned in tokens {
		next_kind := if i + 1 < tokens.len { tokens[i + 1].kind } else { token.Token.eof }
		typ := if interpolation_parts[i] {
			HighlightTokenTyp.string_interp
		} else if enum_values[i] {
			HighlightTokenTyp.enum_value
		} else if attribute_words[i] {
			HighlightTokenTyp.attribute
		} else {
			highlight_token_kind(scanned, next_kind)
		}
		highlighted << HighlightedToken{
			typ:   typ
			start: scanned.start
			end:   scanned.end
		}
	}
	return highlighted
}

// find_interpolation_parts returns, for every token, whether it belongs to the interpolation
// syntax of a string, like `$`, `{`, `:5.2f` and `}` in `'${x:5.2f}'`. The interpolated
// expression, like `x`, does not.
fn find_interpolation_parts(tokens []ScannedToken) []bool {
	mut parts := []bool{len: tokens.len}
	// The brace depth, and the depths at which an interpolation was opened. The scanner reports
	// `'a${x}b'` as `'a`, `$`, `{`, `x`, `}`, `b'`, so the `{` and `}` that delimit an
	// interpolation have to be told apart from those inside the expression.
	mut brace_depth := 0
	mut interpolation_depths := []int{}
	// The `(` and `[` depth, and that depth for every open interpolation. A `:` is only a format
	// spec directly inside `${...}`, not inside parentheses, like the named argument in `${f(a: 1)}`.
	// The `@[` of an attribute counts as a `[` too, since a plain `]` closes it.
	mut group_depth := 0
	mut interpolation_group_depths := []int{}
	// Whether the tokens are inside the format spec of an interpolation, like `:5.2f` in `${x:5.2f}`.
	mut in_format_spec := false
	for i, scanned in tokens {
		if scanned.kind == .str_dollar {
			parts[i] = true
		} else if scanned.kind == .lcbr {
			brace_depth++
			if i > 0 && tokens[i - 1].kind == .str_dollar {
				interpolation_depths << brace_depth
				interpolation_group_depths << group_depth
				parts[i] = true
			}
		} else if scanned.kind == .rcbr {
			if interpolation_depths.len > 0 && interpolation_depths.last() == brace_depth {
				interpolation_depths.delete_last()
				interpolation_group_depths.delete_last()
				in_format_spec = false
				parts[i] = true
			}
			brace_depth--
		} else if scanned.kind in [.lpar, .lsbr, .attribute] {
			group_depth++
		} else if scanned.kind in [.rpar, .rsbr] {
			group_depth--
		} else if scanned.kind == .colon && interpolation_depths.len > 0
			&& interpolation_depths.last() == brace_depth
			&& interpolation_group_depths.last() == group_depth {
			in_format_spec = true
		}
		if in_format_spec {
			parts[i] = true
		}
	}
	return parts
}

// find_enum_values returns, for every token, whether it belongs to an enum value, like the `.`
// and `closed` of `.closed`.
fn find_enum_values(tokens []ScannedToken) []bool {
	mut values := []bool{len: tokens.len}
	for i in 0 .. tokens.len {
		if starts_enum_value(tokens, values, i) {
			values[i] = true
			values[i + 1] = true
		}
	}
	return values
}

// starts_enum_value reports whether the token at `i` is the `.` of an enum value, like `.closed`
// in `state = .closed`, `f(.closed)` or `State.closed`.
fn starts_enum_value(tokens []ScannedToken, values []bool, i int) bool {
	if tokens[i].kind != .dot || i + 1 >= tokens.len {
		return false
	}
	dot := tokens[i]
	name := tokens[i + 1]
	// The value's name follows directly.
	if name.kind != .name || name.start != dot.end {
		return false
	}
	// A called name is a method, like `.map()` or `Foo.new()`.
	if i + 2 < tokens.len && tokens[i + 2].kind == .lpar {
		return false
	}
	if i == 0 {
		return true
	}
	prev := tokens[i - 1]
	if prev.kind !in highlight_value_end_kinds {
		return true
	}
	// A separated enum value or branch body can precede another enum value. Whitespace before
	// a field selector, including a newline, does not otherwise change its meaning.
	if prev.end != dot.start && (values[i - 1]
		|| (prev.kind == .rcbr && closes_enum_branch_body(tokens, values, i - 1))) {
		return true
	}
	// After a value, it is a field, like `foo.bar`, unless the value is a type name. `C.` and
	// `JS.` are the namespaces of foreign names instead, which are mostly types, like `C.FILE`.
	return prev.kind == .name && is_type_name(prev.lit) && prev.lit !in ['C', 'JS']
		&& name.lit !in highlight_reflection_fields
}

// closes_enum_branch_body distinguishes a match arm's body from a struct or match expression.
fn closes_enum_branch_body(tokens []ScannedToken, values []bool, end int) bool {
	mut depth := 0
	for i := end; i >= 0; i-- {
		if tokens[i].kind == .rcbr {
			depth++
		} else if tokens[i].kind == .lcbr {
			depth--
			if depth == 0 {
				return i > 0 && values[i - 1]
			}
		}
	}
	return false
}

// find_attribute_words returns, for every token, whether it belongs to an attribute itself, like
// `@[`, `deprecated` and `]` in `@[deprecated: 'use y']`. The arguments, like `'use y'`, do not.
fn find_attribute_words(tokens []ScannedToken) []bool {
	mut words := []bool{len: tokens.len}
	// The `[` depth inside an attribute, or 0 outside of one.
	mut depth := 0
	mut argument_tokens := 0
	mut paren_depth := 0
	for i, scanned in tokens {
		if scanned.inside_string_interpolation {
			// A split string argument includes its expression and trailing string segment.
			argument_tokens = 0
			continue
		}
		if scanned.kind == .attribute {
			depth = 1
			argument_tokens = 0
			paren_depth = 0
			words[i] = true
		} else if depth == 0 {
			continue
		} else if scanned.kind == .lsbr {
			depth++
		} else if scanned.kind == .rsbr {
			depth--
			words[i] = depth == 0
		} else if scanned.kind == .lpar {
			paren_depth++
		} else if scanned.kind == .rpar {
			paren_depth--
		} else if argument_tokens > 0 {
			if scanned.kind != .comment {
				argument_tokens--
			}
		} else if scanned.kind == .colon && depth == 1 && paren_depth == 0 {
			argument_tokens = if i + 1 < tokens.len && tokens[i + 1].kind == .dot { 2 } else { 1 }
		} else if scanned.kind == .name || scanned.kind.is_keyword() {
			// The words of the attribute, like `deprecated` or `if debug`, not names in arguments.
			words[i] = depth == 1 && paren_depth == 0
		}
	}
	return words
}

// is_type_name reports whether the name `lit` looks like a type, like `Foo`. V type names are
// capitalized, unlike the names of variables, functions and modules.
fn is_type_name(lit string) bool {
	return lit != '' && lit[0].is_capital()
}

// highlight_token_kind returns the kind of a token that does not depend on what surrounds it,
// apart from the token after it.
fn highlight_token_kind(scanned ScannedToken, next_kind token.Token) HighlightTokenTyp {
	return match scanned.kind {
		// The `$` of compile time code, like `$if`, belongs to the keyword after it.
		// `!in` and `!is` are keywords, just like `in` and `is`.
		.dollar, .not_in, .not_is {
			HighlightTokenTyp.keyword
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
			// `map` is only a type in `map[K]V`; otherwise it is usually the `.map()` method.
			if scanned.lit in highlight_builtin_types || scanned.lit == 'chan'
				|| (scanned.lit == 'map' && next_kind == .lsbr) {
				HighlightTokenTyp.builtin
			} else if next_kind == .lpar {
				HighlightTokenTyp.function
			} else if is_type_name(scanned.lit) {
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
}

// ansi_highlight returns `raw` with the terminal color for the kind of code `typ`.
fn ansi_highlight(typ HighlightTokenTyp, raw string) string {
	return match typ {
		.comment, .attribute { term.gray(raw) }
		.string, .char { term.yellow(raw) }
		.number, .keyword, .module_ { term.bright_blue(raw) }
		.boolean, .string_interp, .enum_value { term.bright_magenta(raw) }
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
