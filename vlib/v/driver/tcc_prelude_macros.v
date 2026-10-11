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
//   A header that asks whether such a macro is defined, in a condition or in a
//   macro that a condition expands, that defines it, or that takes its
//   definition away, is read in one way by the preprocessor and in another by the
//   compiler. A name that a condition pastes together with `##` is not seen here.
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
		if c == `/` {
			after := v3_c_skip_comment(text, i)
			if after > i {
				i = after
				continue
			}
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
			} else if b == `/` && v3_c_skip_comment(text, i) > i {
				// A comment of a directive can go on over lines, and one that
				// starts with `//` ends the directive.
				line.write_u8(` `)
				i = v3_c_skip_comment(text, i)
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
		name_end := v3_c_identifier_end(words, 0)
		directives << V3Directive{
			name: words[..name_end]
			rest: words[name_end..]
		}
	}
	return directives
}

// V3PreludeMacros is what the text of a prelude and of its headers says of macros
// that the output of `tcc -E -dD` does not say.
struct V3PreludeMacros {
mut:
	// The names that a `#define` gives a macro, or that an `#undef` takes one
	// from.
	defined map[string]bool
	// The names that a condition asks about: those of `#ifdef` and of `defined`.
	asked map[string]bool
	// The other identifiers of the conditions of `#if`, which it expands.
	expanded map[string]bool
	// For a macro, the names that its definitions ask about with `defined`, and
	// the other identifiers of what they expand to.
	bodies_ask    map[string][]string
	bodies_expand map[string][]string
	// For a macro, the last parameter of each definition of it that takes any
	// number of arguments under a name: `rest` of `#define f(x, rest...)`.
	named_variadic map[string][]string
	// For a macro, the last parameter of each definition of it that takes a fixed
	// number of arguments.
	fixed map[string][]string
}

// v3_c_condition_names returns the names that `text`, a condition of `#if` or what
// a macro expands to, asks about with `defined`, and its other identifiers, which
// a condition expands.
fn v3_c_condition_names(text string) ([]string, []string) {
	mut asked := []string{}
	mut expanded := []string{}
	mut after_defined := false
	for name in v3_c_identifiers(text) {
		if after_defined {
			after_defined = false
			if name !in asked {
				asked << name
			}
		} else if name == 'defined' {
			after_defined = true
		} else if name !in expanded {
			expanded << name
		}
	}
	return asked, expanded
}

// add takes the directives of the C `text` into `m`.
fn (mut m V3PreludeMacros) add(text string) {
	if !text.contains('#') {
		return
	}
	for directive in v3_c_directives(text) {
		match directive.name {
			'ifdef', 'ifndef', 'elifdef', 'elifndef' {
				for name in v3_c_identifiers(directive.rest) {
					m.asked[name] = true
					break
				}
			}
			'if', 'elif' {
				asked, expanded := v3_c_condition_names(directive.rest)
				for name in asked {
					m.asked[name] = true
				}
				for name in expanded {
					m.expanded[name] = true
				}
			}
			'undef' {
				for name in v3_c_identifiers(directive.rest) {
					m.defined[name] = true
					break
				}
			}
			'define' {
				rest := directive.rest.trim_left(' \t')
				name_end := v3_c_identifier_end(rest, 0)
				if name_end == 0 {
					continue
				}
				name := rest[..name_end]
				m.defined[name] = true
				mut body := rest[name_end..]
				// A macro takes arguments when `(` follows its name at once.
				if name_end < rest.len && rest[name_end] == `(` {
					close := rest.index_after_(')', name_end)
					if close < 0 {
						continue
					}
					body = rest[close + 1..]
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
				asked, expanded := v3_c_condition_names(body)
				for word in asked {
					if word !in m.bodies_ask[name] {
						m.bodies_ask[name] << word
					}
				}
				for word in expanded {
					if word !in m.bodies_expand[name] {
						m.bodies_expand[name] << word
					}
				}
			}
			else {}
		}
	}
}

// names_to_test returns the names that the preprocessor and the compiler must find
// defined alike where a unit starts, in the order of their bytes: those that the
// text defines or undefines, those that a condition asks about or expands, and
// those that a macro which a condition expands asks about or expands in its
// turn, as `__builtin_offsetof` of
// `#define HAS_OFFSETOF defined(__builtin_offsetof)` under `#if HAS_OFFSETOF`.
// What a macro expands to in the C, and not in a condition, is no question:
// `#ifdef offsetof` asks about `offsetof`, and not about what that stands for.
fn (m &V3PreludeMacros) names_to_test() []string {
	mut names := map[string]bool{}
	for name, _ in m.defined {
		names[name] = true
	}
	for name, _ in m.asked {
		names[name] = true
	}
	mut reached := map[string]bool{}
	mut waiting := m.expanded.keys()
	for waiting.len > 0 {
		name := waiting.pop()
		if reached[name] {
			continue
		}
		reached[name] = true
		names[name] = true
		for word in m.bodies_ask[name] {
			names[word] = true
		}
		for word in m.bodies_expand[name] {
			if !reached[word] {
				waiting << word
			}
		}
	}
	mut sorted := names.keys()
	sorted.sort()
	return sorted
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
		name_end := v3_c_identifier_end(line, '#define '.len)
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
