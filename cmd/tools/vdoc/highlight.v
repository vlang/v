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
	// starts_line is whether a newline separates the token from the one before it.
	starts_line bool
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
		mut kind := scanner_.scan()
		if kind == .eof {
			break
		}
		mut start := scanner_.pos
		// The scanner reports a C string, like `c'hi'`, as a char literal that starts after the `c`.
		if kind == .char && scanner_.lit.starts_with('c:') && start > 0 && code[start - 1] == `c`
			&& code[start] in [`'`, `"`] {
			kind = .string
			start--
		}
		if kind != .semicolon && scanner_.offset > start {
			prev_end := if tokens.len > 0 { tokens.last().end } else { start }
			tokens << ScannedToken{
				kind:                        kind
				start:                       start
				end:                         scanner_.offset
				lit:                         scanner_.lit
				inside_string_interpolation: inside_interpolation || scanner_.in_str_inter
					|| scanner_.in_str_incomplete || scanner_.str_parent_quotes.len > 0
				starts_line:                 code[prev_end..start].contains_u8(`\n`)
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
	escape
	none
	module_
	type_name
	enum_value
	attribute
}

// highlight_value_end_kinds are the tokens that can end a value, so that a `.` right after one
// of them is a field or method access, and not the start of an enum value like `.closed`.
// The kinds are spelled out, because the C backend resolves the shorthands in a const fixed array,
// like `.rcbr`, to a value of any enum with that name: https://github.com/vlang/v/issues/29846
const highlight_value_end_kinds = [token.Token.name, token.Token.rpar, token.Token.rsbr,
	token.Token.rcbr, token.Token.string, token.Token.char, token.Token.number, token.Token.question,
	token.Token.not]!

// highlight_reflection_fields are what compile time reflection reads from a type, like
// `T.fields` in `$for f in T.fields`. They are not enum values, despite following a type name.
const highlight_reflection_fields = ['fields', 'methods', 'values', 'attributes', 'variants']!

// highlight_implicit_variables are the variables that V declares by itself, like `it` in
// `a.map(it * 2)`, `err` in an `or` block, and `a` and `b` in `a.sort(a > b)`.
const highlight_implicit_variables = ['it', 'err', 'a', 'b']!

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
	module_names := find_module_names(tokens)
	mut highlighted := []HighlightedToken{cap: tokens.len}
	for i, scanned in tokens {
		next_kind := if i + 1 < tokens.len { tokens[i + 1].kind } else { token.Token.eof }
		typ := if interpolation_parts[i] {
			HighlightTokenTyp.string_interp
		} else if enum_values[i] {
			HighlightTokenTyp.enum_value
		} else if attribute_words[i] {
			HighlightTokenTyp.attribute
		} else if module_names[i] {
			HighlightTokenTyp.module_
		} else {
			highlight_token_kind(scanned, next_kind)
		}
		if typ == .string {
			add_string_parts(mut highlighted, code, scanned)
			continue
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
		// An attribute ends with its line, even when its `]` is missing.
		if scanned.starts_line {
			depth = 0
		}
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

// add_string_parts adds the string token `scanned` of `code` to `highlighted`, with each escape
// sequence in it, like `\n` or `\x41`, as a separate `.escape` part. Raw strings, like `r'\n'`,
// have no escape sequences.
fn add_string_parts(mut highlighted []HighlightedToken, code string, scanned ScannedToken) {
	// A raw string ends with its opening quote, unlike a part of a string after an interpolation
	// that starts with `r`, like `r"\n'` in `'${x}r"\n'`.
	is_raw := scanned.end - scanned.start > 2 && code[scanned.start] == `r`
		&& code[scanned.start + 1] in [`'`, `"`] && code[scanned.end - 1] == code[scanned.start + 1]
	mut start := scanned.start
	mut i := scanned.start
	for !is_raw && i < scanned.end {
		if code[i] != `\\` {
			i++
			continue
		}
		end := escape_end(code, i, scanned.end)
		if i > start {
			highlighted << HighlightedToken{.string, start, i}
		}
		highlighted << HighlightedToken{.escape, i, end}
		start = end
		i = end
	}
	if start < scanned.end {
		highlighted << HighlightedToken{.string, start, scanned.end}
	}
}

// escape_end returns where the escape sequence that starts with the `\` at `i` ends, before
// `limit`. `\x`, `\u` and `\U` take 2, 4 and 8 hex digits, and `\0` up to 3 octal digits.
fn escape_end(code string, i int, limit int) int {
	if i + 1 >= limit {
		return limit
	}
	is_hex := code[i + 1] in [`x`, `u`, `U`]
	max_digits, start := match code[i + 1] {
		`x` { 2, i + 2 }
		`u` { 4, i + 2 }
		`U` { 8, i + 2 }
		`0`...`7` { 3, i + 1 }
		else { 0, i + 2 }
	}
	mut end := start
	for end < limit && end < start + max_digits {
		if !(code[end].is_oct_digit() || (is_hex && code[end].is_hex_digit())) {
			break
		}
		end++
	}
	return end
}

// find_module_names returns, for every token, whether it is the name of a module, like `os` in
// `os.args` or `http` in `http.Request`. That is a lowercase name right before a `.`, unless the
// code declares a variable with that name, like `a` in `a := [1]` before `a.len`. The names in
// `module` and `import` lines, like `net`, `http` and `h` in `import net.http as h`, are too.
fn find_module_names(tokens []ScannedToken) []bool {
	variables := find_variable_names(tokens)
	mut modules := []bool{len: tokens.len}
	mut brace_depth := 0
	for i, scanned in tokens {
		if scanned.kind == .lcbr {
			brace_depth++
		} else if scanned.kind == .rcbr {
			brace_depth--
		}
		// `module` and `import` start a top level line, unlike fields with those names, like
		// `node.module` or `module string` in a struct.
		if scanned.kind in [.key_module, .key_import] && brace_depth == 0
			&& (i == 0 || scanned.starts_line) {
			mark_module_path(mut modules, tokens, i + 1)
			continue
		}
		if modules[i] || scanned.kind != .name || i + 1 >= tokens.len || tokens[i + 1].kind != .dot
			|| tokens[i + 1].start != scanned.end {
			continue
		}
		// After a `.`, it is a field, like `b` in `a.b.c`.
		if i > 0 && tokens[i - 1].kind == .dot {
			continue
		}
		// `@` names, like `@FN`, are compile time pseudo variables, or keywords used as names.
		modules[i] = !is_type_name(scanned.lit) && !scanned.lit.starts_with('@')
			&& scanned.lit !in highlight_builtin_types && scanned.lit !in variables
	}
	return modules
}

// mark_module_path marks the module path that starts at `start`, like `net.http` or
// `net.http as h`, in `modules`.
fn mark_module_path(mut modules []bool, tokens []ScannedToken, start int) {
	mut i := start
	for i < tokens.len && tokens[i].kind == .name {
		modules[i] = true
		if i + 2 < tokens.len && tokens[i + 1].kind in [.dot, .key_as] {
			i += 2
		} else {
			break
		}
	}
}

// find_variable_names returns the names that `tokens` declare as variables, like `x` in `x := 1`,
// `for i, x in xs`, `fn (x Foo) f(y int)`, `|x| x * 2`, `const x = 1`, `if x is Foo` and
// `match x {`, the
// symbols of a selective import, like `args` in `import os { args }`, and the implicit ones,
// like `it`. A declaration counts for the whole snippet, not just for its scope.
fn find_variable_names(tokens []ScannedToken) map[string]bool {
	closing := find_closing_brackets(tokens)
	mut names := map[string]bool{}
	for name in highlight_implicit_variables {
		names[name] = true
	}
	for i, scanned in tokens {
		match scanned.kind {
			.decl_assign {
				add_declared_names(mut names, tokens, i)
			}
			.key_for {
				add_loop_names(mut names, tokens, i)
			}
			.key_fn {
				add_parameter_names(mut names, tokens, closing, i)
			}
			.pipe {
				add_closure_parameter_names(mut names, tokens, i)
			}
			.key_const {
				add_const_names(mut names, tokens, closing, i)
			}
			// A smart cast, like `w` in `match w { Mars { w.dust_storm() } }`.
			.key_match {
				if i + 2 < tokens.len && tokens[i + 1].kind == .name
					&& tokens[i + 2].kind != .dot {
					names[tokens[i + 1].lit] = true
				}
			}
			// A smart cast, like `w` in `if w is Mars { w.dust_storm() }`.
			.key_is, .not_is, .key_as {
				if i > 0 && tokens[i - 1].kind == .name && (i < 2 || tokens[i - 2].kind != .dot)
					&& !is_import_line(tokens, i - 1) {
					names[tokens[i - 1].lit] = true
				}
			}
			.lcbr {
				if i > 0 && tokens[i - 1].kind == .name && is_import_line(tokens, i - 1) {
					add_names_in_list(mut names, tokens, i + 1)
				}
			}
			else {}
		}
	}
	return names
}

// is_import_line reports whether the token at `end` ends the module path of an `import`, like
// `os` in `import os { args }` or `h` in `import net.http as h`.
fn is_import_line(tokens []ScannedToken, end int) bool {
	mut i := end
	for i >= 2 && tokens[i - 1].kind in [.dot, .key_as] && tokens[i - 2].kind == .name {
		i -= 2
	}
	return i > 0 && tokens[i - 1].kind == .key_import && (i == 1 || tokens[i - 1].starts_line)
}

// find_closing_brackets returns, for every `(`, `[` and `@[` in `tokens`, the index of the
// bracket that closes it, or -1 when none does, so that a group can be skipped without scanning
// it, and an unclosed one ignored.
fn find_closing_brackets(tokens []ScannedToken) []int {
	mut closing := []int{len: tokens.len, init: -1}
	mut open := []int{}
	for i, scanned in tokens {
		if scanned.kind in [.lpar, .lsbr, .attribute] {
			open << i
		} else if scanned.kind in [.rpar, .rsbr] && open.len > 0 {
			closing[open.pop()] = i
		}
	}
	return closing
}

// add_names_in_list adds the names of the list that starts at `start`, like `a` and `b` in
// `a, mut b`, to `names`.
fn add_names_in_list(mut names map[string]bool, tokens []ScannedToken, start int) {
	for i := start; i < tokens.len && tokens[i].kind in [.name, .comma, .key_mut]; i++ {
		if tokens[i].kind == .name {
			names[tokens[i].lit] = true
		}
	}
}

// add_declared_names adds the names before the `:=` at `end`, like `x` and `y` in
// `x, mut y := ...`, to `names`. The list ends at the first name that no comma precedes, since a
// `;` or a newline before it, like in `import m as h; x := 1`, is not a token.
fn add_declared_names(mut names map[string]bool, tokens []ScannedToken, end int) {
	mut i := end - 1
	for i >= 0 && tokens[i].kind == .name {
		names[tokens[i].lit] = true
		i--
		if i >= 0 && tokens[i].kind in [.key_mut, .key_shared] {
			i--
		}
		if i < 0 || tokens[i].kind != .comma {
			return
		}
		i--
	}
}

// add_loop_names adds the variables of the `for` at `start`, like `i` and `x` in
// `for i, mut x in xs`, to `names`. A condition, like `os.args.len > 0`, declares none.
fn add_loop_names(mut names map[string]bool, tokens []ScannedToken, start int) {
	mut i := start + 1
	for i < tokens.len && tokens[i].kind in [.name, .comma, .key_mut] {
		i++
	}
	if i < tokens.len && tokens[i].kind == .key_in {
		add_names_in_list(mut names, tokens, start + 1)
	}
}

// add_parameter_names adds the parameters of the `fn` at `fn_index` to `names`, including its
// receiver, like `s` and `x` in `fn (s Foo) bar(x int)`, and the variables that a closure
// captures, like `x` in `fn [x] () {}`.
fn add_parameter_names(mut names map[string]bool, tokens []ScannedToken, closing []int, fn_index int) {
	mut i := fn_index + 1
	if i < tokens.len && tokens[i].kind == .lsbr && closing[i] > 0 {
		add_names_in_list(mut names, tokens, i + 1)
		i = closing[i] + 1
	}
	mut groups := 0
	for groups < 2 && i < tokens.len {
		match tokens[i].kind {
			.lpar {
				add_group_parameter_names(mut names, tokens, closing, i)
				groups++
			}
			// The name of the function, like `bar`, or of its module, like `C` in `fn C.puts()`.
			.name, .dot {
				i++
				continue
			}
			// Generic parameters, like `[T]`.
			.lsbr {}
			else {
				break
			}
		}
		if closing[i] < 0 {
			break
		}
		i = closing[i] + 1
	}
}

// add_group_parameter_names adds the parameter names in the parentheses that open at `start`,
// like `a`, `b` and `c` in `(a, b int, mut c Foo)`, to `names`. A parameter type, like `int`
// in `fn (int) string`, gets added too, which is harmless, since a type is no module either.
fn add_group_parameter_names(mut names map[string]bool, tokens []ScannedToken, closing []int, start int) {
	end := closing[start]
	mut starts_parameter := true
	mut i := start + 1
	for i < end {
		scanned := tokens[i]
		if scanned.kind == .comma {
			starts_parameter = true
		} else if starts_parameter && scanned.kind in [.key_mut, .key_shared] {
			// The parameter's name follows.
		} else {
			if starts_parameter && scanned.kind == .name && tokens[i + 1].kind != .dot {
				names[scanned.lit] = true
			}
			starts_parameter = false
			// Skip nested groups, like the parameters of `cb fn (int)`, or `[]int`.
			if scanned.kind in [.lpar, .lsbr] && closing[i] > 0 {
				i = closing[i]
			}
		}
		i++
	}
}

// add_closure_parameter_names adds the parameters of a short closure, like `x` and `y` in
// `|x, y| x + y`, to `names`, when the `|` at `start` opens one.
fn add_closure_parameter_names(mut names map[string]bool, tokens []ScannedToken, start int) {
	// A `|` that follows a value is the bitwise or operator, like in `a | b`.
	if start > 0 && tokens[start - 1].kind in highlight_value_end_kinds {
		return
	}
	mut parameters := []string{}
	for i := start + 1; i < tokens.len; i++ {
		match tokens[i].kind {
			.name {
				parameters << tokens[i].lit
			}
			.comma, .key_mut {}
			.pipe {
				for name in parameters {
					names[name] = true
				}
				return
			}
			else {
				return
			}
		}
	}
}

// add_const_names adds the names that the `const` at `start` declares, like `x` in `const x = 1`,
// or `x` and `y` in `const ( x = 1 y = 2 )`, to `names`.
fn add_const_names(mut names map[string]bool, tokens []ScannedToken, closing []int, start int) {
	if start + 1 >= tokens.len {
		return
	}
	if tokens[start + 1].kind == .name {
		names[tokens[start + 1].lit] = true
		return
	}
	if tokens[start + 1].kind != .lpar || closing[start + 1] < 0 {
		return
	}
	for i in start + 2 .. closing[start + 1] {
		if tokens[i].kind == .name && tokens[i + 1].kind == .assign {
			names[tokens[i].lit] = true
		}
	}
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
			// Compile time pseudo variables, like `@FN`, are like the `$` of compile time code.
			// A lowercase name after `@`, like `@type`, is a keyword used as a plain name instead.
			if scanned.lit.len > 1 && scanned.lit[0] == `@` && scanned.lit[1].is_capital() {
				return HighlightTokenTyp.keyword
			}
			// `map` is only a type in `map[K]V`; otherwise it is usually the `.map()` method.
			if scanned.lit in highlight_builtin_types || scanned.lit == 'chan'
				|| (scanned.lit == 'map' && next_kind == .lsbr) {
				HighlightTokenTyp.builtin
			} else if next_kind == .lpar {
				HighlightTokenTyp.function
			} else if is_type_name(scanned.lit) {
				HighlightTokenTyp.type_name
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
		.number, .keyword { term.bright_blue(raw) }
		.module_ { term.bold(raw) }
		.boolean, .string_interp, .escape, .enum_value { term.bright_magenta(raw) }
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
