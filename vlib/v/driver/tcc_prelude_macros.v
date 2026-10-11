module driver

import strings

// What `tcc -E -dD` prints for a prelude is not all that TinyCC knows of its
// macros when it compiles the prelude itself (see tcc_prelude.v):
//
// - It prints the definition of a macro without what says that the macro takes
//   any number of arguments: `f(x, ...)` comes out as `f(x,__VA_ARGS__)`, and
//   `f(x, rest...)` as `f(x,rest)`.
// - It defines some macros for a compilation that it does not define for `-E`,
//   `__builtin_offsetof` among them, and one, `__TCC_PP__`, the other way round.
//   A header that asks whether such a macro is defined, defines it, or takes its
//   definition away, is read in one way by the preprocessor and in another by the
//   compiler.
//
// The text of the prelude and of the headers that it reads tells both: which
// macros name their variable argument, and which names a compilation may see
// otherwise than the preprocessor did.

// V3Directive is a preprocessing directive of a C text: its name, and what follows
// the name, with the lines that a backslash continues put together and without
// its comments.
struct V3Directive {
	name string
	rest string
}

// v3_c_skip_literal returns the position after the string or character literal
// that starts at `start` in `text`. A literal ends with its line at the latest.
fn v3_c_skip_literal(text string, start int) int {
	quote := text[start]
	mut i := start + 1
	for i < text.len && text[i] != quote && text[i] != `\n` {
		i += if text[i] == `\\` { 2 } else { 1 }
	}
	return if i < text.len && text[i] == quote { i + 1 } else { i }
}

// v3_c_directives returns the preprocessing directives of the C `text`. A line
// that starts with `#` inside a comment is none.
fn v3_c_directives(text string) []V3Directive {
	mut directives := []V3Directive{}
	mut at_line_start := true
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `\n` {
			at_line_start = true
			i++
			continue
		}
		if c in [` `, `\t`, `\r`] {
			i++
			continue
		}
		if c == `/` && i + 1 < text.len && text[i + 1] == `*` {
			end := text.index_after_('*/', i + 2)
			i = if end < 0 { text.len } else { end + 2 }
			continue
		}
		if c == `/` && i + 1 < text.len && text[i + 1] == `/` {
			end := text.index_after_('\n', i + 2)
			i = if end < 0 { text.len } else { end }
			continue
		}
		if c != `#` || !at_line_start {
			at_line_start = false
			i = if c in [`"`, `'`] { v3_c_skip_literal(text, i) } else { i + 1 }
			continue
		}
		mut line := strings.new_builder(64)
		i++
		for i < text.len && text[i] != `\n` {
			b := text[i]
			if b == `\\` && i + 1 < text.len && text[i + 1] == `\n` {
				line.write_u8(` `)
				i += 2
			} else if b == `\\` && i + 2 < text.len && text[i + 1] == `\r` && text[i + 2] == `\n` {
				line.write_u8(` `)
				i += 3
			} else if b == `/` && i + 1 < text.len && text[i + 1] == `*` {
				end := text.index_after_('*/', i + 2)
				line.write_u8(` `)
				i = if end < 0 { text.len } else { end + 2 }
			} else if b == `/` && i + 1 < text.len && text[i + 1] == `/` {
				end := text.index_after_('\n', i + 2)
				i = if end < 0 { text.len } else { end }
			} else if b in [`"`, `'`] {
				end := v3_c_skip_literal(text, i)
				line.write_string(text[i..end])
				i = end
			} else {
				line.write_u8(b)
				i++
			}
		}
		words := line.str().trim_space()
		mut name_end := 0
		for name_end < words.len && (words[name_end] == `_` || words[name_end].is_alnum()) {
			name_end++
		}
		directives << V3Directive{
			name: words[..name_end]
			rest: words[name_end..]
		}
	}
	return directives
}

// v3_c_identifiers returns the identifiers of a line of C, in their order: what is
// in a literal or part of a number is none.
fn v3_c_identifiers(line string) []string {
	mut names := []string{}
	mut i := 0
	for i < line.len {
		c := line[i]
		if c in [`"`, `'`] {
			i = v3_c_skip_literal(line, i)
		} else if c.is_digit() {
			for i < line.len && (line[i].is_alnum() || line[i] in [`_`, `.`]) {
				i++
			}
		} else if c == `_` || c.is_letter() {
			start := i
			for i < line.len && (line[i] == `_` || line[i].is_alnum()) {
				i++
			}
			names << line[start..i]
		} else {
			i++
		}
	}
	return names
}

// V3PreludeMacros is what the text of a prelude and of its headers says of macros
// that the output of `tcc -E -dD` does not say.
struct V3PreludeMacros {
mut:
	// The names that a conditional asks about, and those that are defined or
	// whose definition is taken away: all that the preprocessor and the compiler
	// must see alike at the start of a unit.
	tested map[string]bool
	// For a macro, the last parameter of each definition of it that takes any
	// number of arguments under a name: `rest` of `#define f(x, rest...)`.
	named_variadic map[string][]string
	// For a macro, the last parameter of each definition of it that takes a fixed
	// number of arguments.
	fixed map[string][]string
}

// add takes the directives of the C `text` into `m`.
fn (mut m V3PreludeMacros) add(text string) {
	if !text.contains('#') {
		return
	}
	for directive in v3_c_directives(text) {
		match directive.name {
			'if', 'elif', 'ifdef', 'ifndef', 'elifdef', 'elifndef' {
				for name in v3_c_identifiers(directive.rest) {
					if name != 'defined' {
						m.tested[name] = true
					}
				}
			}
			'undef' {
				for name in v3_c_identifiers(directive.rest) {
					m.tested[name] = true
					break
				}
			}
			'define' {
				rest := directive.rest.trim_left(' \t')
				mut name_end := 0
				for name_end < rest.len && (rest[name_end] == `_` || rest[name_end].is_alnum()) {
					name_end++
				}
				if name_end == 0 {
					continue
				}
				name := rest[..name_end]
				m.tested[name] = true
				// A macro takes arguments when `(` follows its name at once.
				if name_end >= rest.len || rest[name_end] != `(` {
					continue
				}
				close := rest.index_after_(')', name_end)
				if close < 0 {
					continue
				}
				last := rest[name_end + 1..close].all_after_last(',').trim_space()
				if last.ends_with('...') {
					named := last[..last.len - 3].trim_space()
					if named.len > 0 && named !in m.named_variadic[name] {
						m.named_variadic[name] << named
					}
				} else if last.len > 0 && last !in m.fixed[name] {
					m.fixed[name] << last
				}
			}
			else {}
		}
	}
}

// with_variable_arguments returns `definitions`, the lines that TinyCC printed for
// the macros of a prelude, with the parameters that take any number of arguments
// marked as such again, and why that cannot be done, or ''. A parameter with the
// name `__VA_ARGS__` is what TinyCC prints for `...`. One with another name takes
// any number of arguments when the text defines the macro so, and cannot be told
// when the text also defines the macro with that parameter and a fixed number.
fn (m &V3PreludeMacros) with_variable_arguments(definitions string) (string, string) {
	if !definitions.contains('(') {
		return definitions, ''
	}
	mut out := strings.new_builder(definitions.len + 64)
	for line in definitions.split_into_lines() {
		if !line.starts_with('#define ') {
			out.writeln(line)
			continue
		}
		mut name_end := '#define '.len
		for name_end < line.len && (line[name_end] == `_` || line[name_end].is_alnum()) {
			name_end++
		}
		close := if name_end < line.len && line[name_end] == `(` {
			line.index_after_(')', name_end)
		} else {
			-1
		}
		if close < 0 {
			out.writeln(line)
			continue
		}
		name := line['#define '.len..name_end]
		parameters := line[name_end + 1..close]
		last := parameters.all_after_last(',').trim_space()
		before_last := parameters[..parameters.len - parameters.all_after_last(',').len]
		if last == '__VA_ARGS__' {
			out.writeln('${line[..name_end + 1]}${before_last}...${line[close..]}')
		} else if last.len > 0 && last in m.named_variadic[name] {
			if last in m.fixed[name] {
				return definitions, 'the macro `${name}` is defined with and without a variable number of arguments'
			}
			out.writeln('${line[..name_end + 1]}${before_last}${last}...${line[close..]}')
		} else {
			out.writeln(line)
		}
	}
	return out.str(), ''
}

// v3_tcc_prelude_defined_probe is the word before the number of a name that the
// preprocessor found defined.
const v3_tcc_prelude_defined_probe = '__v3_tcc_prelude_defined'

// v3_tcc_prelude_differs_probe is the word before a name that the compiler finds
// defined when the preprocessor did not, or the other way round.
const v3_tcc_prelude_differs_probe = '__v3_tcc_prelude_differs'

// v3_tcc_preprocessor_probe returns C that the preprocessor turns into one line
// for each of `names` that is a macro where the C starts: the probe word and the
// position of the name.
fn v3_tcc_preprocessor_probe(names []string) string {
	mut out := strings.new_builder(names.len * 48)
	for i, name in names {
		out.write_string('#ifdef ${name}\n${v3_tcc_prelude_defined_probe} ${i}\n#endif\n')
	}
	return out.str()
}

// v3_tcc_compiler_probe returns C that compiles when the compiler finds the same of
// `names` defined as the preprocessor did, those at the positions `defined`, and
// that stops with the name of one that it does not.
fn v3_tcc_compiler_probe(names []string, defined []int) string {
	mut is_defined := []bool{len: names.len}
	for position in defined {
		if position >= 0 && position < names.len {
			is_defined[position] = true
		}
	}
	mut out := strings.new_builder(names.len * 64)
	for i, name in names {
		directive := if is_defined[i] { 'ifndef' } else { 'ifdef' }
		out.write_string('#${directive} ${name}\n#error ${v3_tcc_prelude_differs_probe} ${name}\n#endif\n')
	}
	return out.str()
}

// v3_tcc_probe_difference returns the name that `output`, what TinyCC printed for
// the C of v3_tcc_compiler_probe, says is defined in one way only, or none.
fn v3_tcc_probe_difference(output string) ?string {
	found := output.index(v3_tcc_prelude_differs_probe) or { return none }
	names := v3_c_identifiers(output[found + v3_tcc_prelude_differs_probe.len..].all_before('\n'))
	if names.len == 0 {
		return none
	}
	return names[0]
}
