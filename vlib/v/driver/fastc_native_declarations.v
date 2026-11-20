module driver

// vfmt off
$if !skip_fastc ? {
	import strings
}
// vfmt on

$if !skip_fastc ? {
	// V3FastcNativeDeclaration is one top-level declaration of generated C: its span
	// in the text and the names it declares. One without names is always kept.
	struct V3FastcNativeDeclaration {
		start int
		end   int
		names []string
	}

	@[inline]
	fn v3_fastc_native_name_start(c u8) bool {
		return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
	}

	@[inline]
	fn v3_fastc_native_name_part(c u8) bool {
		return v3_fastc_native_name_start(c) || (c >= `0` && c <= `9`)
	}

	// v3_fastc_native_identifiers lists the identifiers of generated C, also those
	// inside its comments and literals: a caller only looks them up among declared names.
	fn v3_fastc_native_identifiers(text string) []string {
		mut names := []string{cap: text.len / 12 + 1}
		mut pos := 0
		for pos < text.len {
			if !v3_fastc_native_name_start(text[pos]) {
				// A number such as `0x1f` is not a name.
				if text[pos] >= `0` && text[pos] <= `9` {
					for pos < text.len && v3_fastc_native_name_part(text[pos]) {
						pos++
					}
					continue
				}
				pos++
				continue
			}
			start := pos
			for pos < text.len && v3_fastc_native_name_part(text[pos]) {
				pos++
			}
			names << text[start..pos]
		}
		return names
	}

	// v3_fastc_native_skip_literal returns the position after the string or character
	// literal, or the comment, that starts at `pos`; `pos` itself when none starts there.
	fn v3_fastc_native_skip_literal(text string, pos int) int {
		c := text[pos]
		if c == `"` || c == `'` {
			mut end := pos + 1
			for end < text.len && text[end] != c && text[end] != `\n` {
				if text[end] == `\\` { end++ }
				end++
			}
			return if end < text.len { end + 1 } else { text.len }
		}
		if c == `/` && pos + 1 < text.len && text[pos + 1] == `/` {
			mut end := pos + 2
			for end < text.len && text[end] != `\n` {
				end++
			}
			return end
		}
		if c == `/` && pos + 1 < text.len && text[pos + 1] == `*` {
			mut end := pos + 2
			for end + 1 < text.len && !(text[end] == `*` && text[end + 1] == `/`) {
				end++
			}
			return if end + 1 < text.len { end + 2 } else { text.len }
		}
		return pos
	}

	// v3_fastc_native_split_declarations splits generated C declarations (constants as
	// macros, types, objects, prototypes) at the top level. A preprocessor directive is
	// a declaration of its own; one inside a declaration stays in it.
	fn v3_fastc_native_split_declarations(text string) []V3FastcNativeDeclaration {
		mut declarations := []V3FastcNativeDeclaration{cap: text.len / 64 + 1}
		mut pos := 0
		for pos < text.len {
			for pos < text.len && text[pos] in [` `, `\t`, `\n`, `\r`] {
				pos++
			}
			if pos >= text.len { break }
			start := pos
			if text[pos] == `#` {
				// A directive runs to the end of its last continued line.
				for pos < text.len {
					mut end := pos
					for end < text.len && text[end] != `\n` {
						end++
					}
					continued := end > pos && text[end - 1] == `\\`
					pos = if end < text.len { end + 1 } else { end }
					if !continued { break }
				}
				declarations << V3FastcNativeDeclaration{ start: start, end: pos, names: v3_fastc_native_macro_names(text[start..pos]) }
				continue
			}
			mut depth := 0
			mut after_body := false
			for pos < text.len {
				skipped := v3_fastc_native_skip_literal(text, pos)
				if skipped != pos {
					pos = skipped
					continue
				}
				c := text[pos]
				if c == `{` {
					depth++
				} else if c == `}` {
					depth--
					after_body = depth == 0
				} else if depth == 0 {
					if c == `;` {
						pos++
						break
					}
					// A function body ends its line; `} name;` continues the declaration.
					if c == `\n` && after_body { break }
					if c !in [` `, `\t`, `\r`] {
						after_body = false
					}
				}
				pos++
			}
			if pos < text.len && text[pos] == `\n` {
				pos++
			}
			declarations << V3FastcNativeDeclaration{ start: start, end: pos, names: v3_fastc_native_declared_names(text[start..pos]) }
		}
		return declarations
	}

	// v3_fastc_native_macro_names names the macro a `#define` line declares. Every
	// other directive is unnamed.
	fn v3_fastc_native_macro_names(directive string) []string {
		mut pos := 1
		for pos < directive.len && directive[pos] in [` `, `\t`] {
			pos++
		}
		if !directive[pos..].starts_with('define') { return [] }
		pos += 6
		if pos >= directive.len || directive[pos] !in [` `, `\t`] { return [] }
		for pos < directive.len && directive[pos] in [` `, `\t`] {
			pos++
		}
		start := pos
		for pos < directive.len && v3_fastc_native_name_part(directive[pos]) {
			pos++
		}
		return if pos > start { [directive[start..pos]] } else { [] }
	}

	// v3_fastc_native_declared_names names what one declaration introduces: a typedef,
	// a struct, union or enum tag with its enumerators, a function or an object. An
	// empty result marks a declaration this does not recognize.
	fn v3_fastc_native_declared_names(declaration string) []string {
		mut names := []string{}
		// The words outside braces, with the nesting of parentheses and brackets.
		mut words := []string{cap: 8}
		mut word_parens := []int{cap: 8}
		mut word_ends := []int{cap: 8}
		mut braces := 0
		mut parens := 0
		mut brackets := 0
		mut body_start := -1
		mut body_end := -1
		mut first_paren := -1
		mut initializer := -1
		mut pos := 0
		for pos < declaration.len {
			skipped := v3_fastc_native_skip_literal(declaration, pos)
			if skipped != pos {
				pos = skipped
				continue
			}
			c := declaration[pos]
			if c == `{` {
				if braces == 0 && body_start < 0 {
					body_start = pos
				}
				braces++
			} else if c == `}` {
				braces--
				if braces == 0 && body_end < 0 {
					body_end = pos
				}
			} else if braces == 0 {
				if c == `(` {
					if parens == 0 && first_paren < 0 {
						first_paren = pos
					}
					parens++
				} else if c == `)` {
					parens--
				} else if c == `[` {
					brackets++
				} else if c == `]` {
					brackets--
				} else if c == `=` && parens == 0 && initializer < 0 {
					initializer = pos
				} else if v3_fastc_native_name_start(c) && initializer < 0 && brackets == 0 {
					start := pos
					for pos < declaration.len && v3_fastc_native_name_part(declaration[pos]) {
						pos++
					}
					words << declaration[start..pos]
					word_parens << parens
					word_ends << pos
					continue
				} else if c >= `0` && c <= `9` {
					for pos < declaration.len && v3_fastc_native_name_part(declaration[pos]) {
						pos++
					}
					continue
				}
			}
			pos++
		}
		if words.len == 0 { return names }
		is_typedef := words[0] == 'typedef'
		kind_index := if is_typedef { 1 } else { 0 }
		is_aggregate := kind_index < words.len && words[kind_index] in ['struct', 'union', 'enum']
		if is_aggregate && kind_index + 1 < words.len
			&& (body_start < 0 || word_ends[kind_index + 1] <= body_start) {
			// `struct Tag {`, `typedef struct Tag Name;`
			if !is_typedef || body_start >= 0 {
				names << words[kind_index + 1]
			}
		}
		if is_aggregate && words[kind_index] == 'enum' && body_start >= 0 && body_end > body_start {
			for item in declaration[body_start + 1..body_end].split(',') {
				enumerators := v3_fastc_native_identifiers(item.all_before('='))
				if enumerators.len > 0 {
					names << enumerators[0]
				}
			}
		}
		if is_typedef {
			// `typedef R (*Name)(P);` names the word in its first parentheses.
			if first_paren >= 0 {
				mut inner := first_paren + 1
				for inner < declaration.len && declaration[inner] in [` `, `\t`, `*`, `^`] {
					inner++
				}
				if inner > first_paren + 1 && inner < declaration.len
					&& v3_fastc_native_name_start(declaration[inner]) {
					mut end := inner
					for end < declaration.len && v3_fastc_native_name_part(declaration[end]) {
						end++
					}
					names << declaration[inner..end]
					return names
				}
			}
			for i := words.len - 1; i > kind_index; i-- {
				if word_parens[i] == 0 {
					if words[i] !in names {
						names << words[i]
					}
					break
				}
			}
			return names
		}
		if is_aggregate {
			// `struct Tag { ... } object;` also declares the object.
			if body_end >= 0 && word_ends[words.len - 1] > body_end && words[words.len - 1] !in names {
				names << words[words.len - 1]
			}
			return names
		}
		// A function is named before its parameters, an object by its last word.
		for i := words.len - 1; i >= 0; i-- {
			if word_parens[i] == 0 && (first_paren < 0 || word_ends[i] <= first_paren) {
				names << words[i]
				break
			}
		}
		return names
	}

	// v3_fastc_native_lean_declarations keeps the declarations of `text` that the seed
	// texts and names mention, with everything those declarations mention in turn, in
	// their original order. Every macro stays: a native source can test one with
	// `#ifdef` and compile either way, so only its presence keeps that choice. A
	// macro is no seed, since its body is text until something expands it. Other
	// directives and unrecognized declarations stay as well.
	fn v3_fastc_native_lean_declarations(text string, seed_texts []string, seed_names []string) string {
		declarations := v3_fastc_native_split_declarations(text)
		mut by_name := map[string][]int{}
		mut kept := []bool{len: declarations.len}
		for i, declaration in declarations {
			if text[declaration.start] == `#` {
				kept[i] = true
				continue
			}
			for name in declaration.names {
				by_name[name] << i
			}
		}
		mut queued := map[string]bool{}
		mut queue := []string{}
		for i, declaration in declarations {
			if kept[i] || declaration.names.len > 0 { continue }
			kept[i] = true
			for name in v3_fastc_native_identifiers(text[declaration.start..declaration.end]) {
				if name in by_name && !queued[name] {
					queued[name] = true
					queue << name
				}
			}
		}
		for seed in seed_texts {
			for name in v3_fastc_native_identifiers(seed) {
				if name in by_name && !queued[name] {
					queued[name] = true
					queue << name
				}
			}
		}
		for name in seed_names {
			if name in by_name && !queued[name] {
				queued[name] = true
				queue << name
			}
		}
		for queue.len > 0 {
			name := queue.pop()
			for index in by_name[name] {
				if kept[index] { continue }
				kept[index] = true
				declaration := declarations[index]
				for mentioned in v3_fastc_native_identifiers(text[declaration.start..declaration.end]) {
					if mentioned in by_name && !queued[mentioned] {
						queued[mentioned] = true
						queue << mentioned
					}
				}
			}
		}
		mut out := strings.new_builder(text.len / 2)
		for i, declaration in declarations {
			if !kept[i] { continue }
			out.write_string(text[declaration.start..declaration.end])
			if declaration.end > declaration.start && text[declaration.end - 1] != `\n` {
				out.write_u8(`\n`)
			}
		}
		return out.str()
	}

	// v3_fastc_native_missing_declarations lists the names quoted by a failed native
	// compilation that `text` declares and `known` lacks.
	fn v3_fastc_native_missing_declarations(message string, text string, known []string) []string {
		mut declared := map[string]bool{}
		for declaration in v3_fastc_native_split_declarations(text) {
			// Macros are never left out.
			if text[declaration.start] == `#` { continue }
			for name in declaration.names {
				declared[name] = true
			}
		}
		mut missing := []string{}
		mut pos := 0
		for pos < message.len {
			if message[pos] != `'` {
				pos++
				continue
			}
			start := pos + 1
			mut end := start
			for end < message.len && v3_fastc_native_name_part(message[end]) {
				end++
			}
			if end > start && end < message.len && message[end] == `'` {
				name := message[start..end]
				if declared[name] && name !in known && name !in missing {
					missing << name
				}
				pos = end + 1
				continue
			}
			pos = start
		}
		return missing
	}
}
