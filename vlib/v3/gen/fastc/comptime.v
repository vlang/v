module fastc

import os
import v3.pref
import v3.scanner
import v3.token

fn fastc_scan_comptime_unary(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeCondition {
	if first == .not {
		result := fastc_scan_comptime_unary(mut scan, scan.scan(), path, prefs)!
		return FastcComptimeCondition{
			value: !result.value
			tok: result.tok
		}
	}
	if first == .lpar {
		result := fastc_scan_comptime_or(mut scan, scan.scan(), path, prefs)!
		if result.tok != .rpar {
			return error('fastc parser does not support compile-time condition in ${path}')
		}
		return FastcComptimeCondition{
			value: result.value
			tok: scan.scan()
		}
	}
	if first == .key_true {
		return FastcComptimeCondition{
			value: true
			tok: scan.scan()
		}
	}
	if first == .key_false {
		return FastcComptimeCondition{
			value: false
			tok: scan.scan()
		}
	}
	if first == .dollar {
		// `$pkgconfig('lib')`: true when pkg-config reports the library present.
		if scan.scan() != .name || scan.lit != 'pkgconfig' {
			return error('fastc parser does not support compile-time condition `${scan.lit}` in ${path}')
		}
		if scan.scan() != .lpar {
			return error('fastc parser does not support `\$pkgconfig` condition in ${path}')
		}
		if scan.scan() != .string {
			return error('fastc parser does not support `\$pkgconfig` library name in ${path}')
		}
		library := scan.lit.trim('\'"')
		if scan.scan() != .rpar {
			return error('fastc parser does not support `\$pkgconfig` condition in ${path}')
		}
		return FastcComptimeCondition{
			value: pref.comptime_pkgconfig_value(library)
			tok: scan.scan()
		}
	}
	if first != .name {
		return error('fastc parser does not support compile-time condition `${first.str()}` in ${path}')
	}
	name := scan.lit
	mut tok := scan.scan()
	is_optional := tok == .question
	if is_optional {
		tok = scan.scan()
	}
	value := if is_optional {
		pref.comptime_optional_flag_value(prefs, name)
	} else {
		pref.comptime_flag_value(prefs, name)
	}
	return FastcComptimeCondition{
		value: value
		tok: tok
	}
}

fn fastc_scan_comptime_and(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeCondition {
	first_result := fastc_scan_comptime_unary(mut scan, first, path, prefs)!
	mut value := first_result.value
	mut tok := first_result.tok
	for tok == .and {
		right := fastc_scan_comptime_unary(mut scan, scan.scan(), path, prefs)!
		value = value && right.value
		tok = right.tok
	}
	return FastcComptimeCondition{
		value: value
		tok: tok
	}
}

fn fastc_scan_comptime_or(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeCondition {
	first_result := fastc_scan_comptime_and(mut scan, first, path, prefs)!
	mut value := first_result.value
	mut tok := first_result.tok
	for tok == .logical_or {
		right := fastc_scan_comptime_and(mut scan, scan.scan(), path, prefs)!
		value = value || right.value
		tok = right.tok
	}
	return FastcComptimeCondition{
		value: value
		tok: tok
	}
}

fn fastc_scan_comptime_block(mut scan scanner.Scanner, first token.Token, path string) !FastcComptimeBlock {
	if first != .lcbr {
		return error('fastc parser does not support compile-time branch without a block in ${path}')
	}
	start := scan.offset
	mut depth := 1
	mut tok := scan.scan()
	for {
		if tok == .eof {
			return error('fastc parser does not support unfinished compile-time block in ${path}')
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if depth == 0 {
				return FastcComptimeBlock{
					source: scan.src[start..scan.pos]
					tok: scan.scan()
				}
			}
		}
		tok = scan.scan()
	}
	return FastcComptimeBlock{}
}

fn fastc_scan_skip_semicolons(mut scan scanner.Scanner, first token.Token) token.Token {
	mut tok := first
	for tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}

fn fastc_scan_selected_comptime_branch(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeBlock {
	mut tok := first
	mut selected := ''
	mut branch_selected := false
	for {
		if tok != .key_if {
			return error('fastc parser does not support compile-time branch `${tok.str()}` in ${path}')
		}
		condition := fastc_scan_comptime_or(mut scan, scan.scan(), path, prefs)!
		block := fastc_scan_comptime_block(mut scan, condition.tok, path)!
		if condition.value && !branch_selected {
			selected = block.source
			branch_selected = true
		}
		tok = fastc_scan_skip_semicolons(mut scan, block.tok)
		if tok != .dollar {
			return FastcComptimeBlock{
				source: selected
				tok: tok
			}
		}
		mut lookahead := scan
		if lookahead.scan() != .key_else {
			return FastcComptimeBlock{
				source: selected
				tok: tok
			}
		}
		_ = scan.scan()
		tok = scan.scan()
		if tok == .dollar {
			tok = scan.scan()
			continue
		}
		else_block := fastc_scan_comptime_block(mut scan, tok, path)!
		if !branch_selected {
			selected = else_block.source
		}
		return FastcComptimeBlock{
			source: selected
			tok: fastc_scan_skip_semicolons(mut scan, else_block.tok)
		}
	}
	return FastcComptimeBlock{}
}

fn fastc_collect_selected_comptime_function_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_function_signatures(selected.source, path, header, prefs, []int{}, declared_types, declared_type_c_names, params_structs, mut functions)!
				}
				tok = selected.tok
				continue
			}
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		tok = scan.scan()
	}
}

fn (mut g Parser) parse_top_level_comptime_if() ! {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		g.parse_top_level_items(true)!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
		} else {
			g.parse_top_level_comptime_if()!
		}
		return
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
	} else {
		g.parse_top_level_items(true)!
	}
}

fn (mut g Parser) parse_comptime_if_statement() !bool {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	mut terminates := false
	if condition {
		terminates = g.parse_block_body()!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return terminates
	}
	g.next()
	if g.tok != .key_else {
		return g.unsupported('compile-time branch after `\$if`')
	}
	g.next()
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
			return terminates
		}
		return g.parse_comptime_if_statement()!
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
		return terminates
	}
	return g.parse_block_body()!
}

// parse_comptime_match_statement selects a type branch from `$match T` or
// `$match T.unaliased_typ`. Late generic instances contain a concrete type name in
// place of `T`, so only the selected branch needs to enter the ordinary statement
// parser. This is the statement form used by json2's numeric decoder.
fn (mut g Parser) parse_comptime_match_statement() !bool {
	g.expect(.dollar)!
	g.expect(.key_match)!
	if g.tok != .name && !(g.tok == .key_shared && g.shared_token_is_identifier(.key_match)) {
		return g.unsupported('compile-time `\$match` subject')
	}
	subject_name := g.lit
	mut subject_type := g.comptime_named_type(subject_name)
	g.next()
	if g.tok == .dot {
		g.next()
		if g.tok != .name || g.lit !in ['typ', 'unaliased_typ'] {
			return g.unsupported('compile-time `\$match` type member')
		}
		if g.lit == 'unaliased_typ' {
			subject_type = g.underlying_alias_type(subject_type)
		}
		g.next()
	}
	g.expect(.lcbr)!
	mut selected := false
	mut selected_terminates := false
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished compile-time `\$match`')
		}
		mut branch_matches := false
		if g.tok == .dollar {
			g.next()
			if g.tok != .key_else {
				return g.unsupported('compile-time `\$match` branch')
			}
			g.next()
			branch_matches = !selected
		} else {
			for {
				target_type := g.parse_type()!
				branch_matches = branch_matches || g.underlying_alias_type(subject_type) == g.underlying_alias_type(target_type)
				if g.tok != .comma {
					break
				}
				g.next()
			}
		}
		g.expect(.lcbr)!
		if branch_matches && !selected {
			selected = true
			selected_terminates = g.parse_block_body()!
		} else {
			g.skip_open_block()!
		}
		g.skip_semicolons()
	}
	g.next()
	g.skip_semicolons()
	return selected_terminates
}

// parse_comptime_for_statement unrolls `$for <var> in <Type>.fields { body }`.
// It resolves the concrete type's fields (monomorphization has already turned a
// generic `T.fields` into a concrete `User.fields`), captures the body source,
// and re-parses it once per field with a comptime-field context so `<var>.name`,
// `<var>.typ` and `t.$(<var>.name)` resolve.
fn (mut g Parser) parse_comptime_for_statement() !bool {
	g.expect(.dollar)!
	g.expect(.key_for)!
	if g.tok != .name && !(g.tok == .key_shared && g.shared_token_is_identifier(.key_for)) {
		return g.unsupported('`\$for` loop variable')
	}
	loop_var := g.lit
	g.next()
	g.expect(.key_in)!
	if g.tok != .name {
		return g.unsupported('`\$for` iterable')
	}
	first := g.lit
	g.next()
	if g.tok != .dot {
		return g.unsupported('`\$for` expects `Type.fields`')
	}
	g.next()
	if g.tok != .name {
		return g.unsupported('`\$for` iterable member')
	}
	mut type_key := ''
	mut member := g.lit
	if member in ['fields', 'values'] {
		type_key = g.resolve_declared_type_key(first) or {
			return g.unsupported('`\$for` over unknown type `${first}`')
		}
		g.next()
	} else {
		// Qualified `mod.Type.fields` / `mod.Type.values`.
		module_name := g.imports[first] or { first }
		type_key = fastc_type_key(module_name, g.lit)
		g.next()
		g.expect(.dot)!
		if g.tok != .name || g.lit !in ['fields', 'values'] {
			return g.unsupported('`\$for` only supports `.fields` or `.values`')
		}
		member = g.lit
		g.next()
	}
	g.expect(.lcbr)!
	c_type := fastc_c_declared_type_name(type_key)
	// Capture the body source (from the first body token up to the matching `}`).
	body_start := g.s.pos
	mut depth := 0
	mut body_end := -1
	for g.tok != .eof {
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			if depth == 0 {
				body_end = g.s.pos
				break
			}
			depth--
		}
		g.next()
	}
	if body_end < 0 {
		return g.unsupported('unterminated `\$for` block')
	}
	body_source := g.s.src[body_start..body_end]
	g.next() // consume the closing `}`; the main scanner now continues after it
	saved_tok := g.tok
	saved_lit := g.lit
	saved_s := g.s
	// Build one substituted body per iteration item: a struct field for `.fields`,
	// or an enum value for `.values`.
	mut substituted_bodies := []string{}
	if member == 'values' {
		for value_name in g.enum_field_names[c_type] {
			substituted_bodies << g.substitute_comptime_enum_value(body_source, loop_var, c_type, value_name)
		}
	} else {
		for field in g.struct_field_info[c_type] {
			substituted_bodies << g.substitute_comptime_field(body_source, loop_var, field)
		}
	}
	for substituted in substituted_bodies {
		// Re-scan each unrolled body as ordinary V — no comptime awareness is needed
		// in the renderer.
		file := token.File.unindexed('comptime_for', substituted.len)
		g.s = scanner.new_scanner(g.prefs, .normal)
		g.s.init(file, substituted)
		g.next()
		// A fresh C block scopes any locals the body declares, so re-parsing it per
		// item does not redeclare them in the enclosing scope.
		g.write_line('{')
		g.indent++
		outer_locals := g.locals.clone()
		for g.tok != .eof {
			g.parse_statement()!
			g.skip_semicolons()
		}
		g.type_memo.clear()
		g.locals = outer_locals.clone()
		g.indent--
		g.write_line('}')
	}
	g.s = saved_s
	g.tok = saved_tok
	g.lit = saved_lit
	return false
}

// substitute_comptime_field rewrites one `$for` body for a single field:
// `<var>.name` becomes the field-name string literal `'field'`; a computed
// selector `x.$(<var>.name)` becomes the static access `x.field`; and a comptime
// type test `<var>.typ is <Type>` becomes the literal `true`/`false` (so the
// enclosing `$if` takes/skips the branch). Other `<var>.<member>` forms are left
// as-is (they fail to parse, flagging the unsupported comptime feature). Uses
// token positions so nothing inside strings or comments is touched.
fn fastc_comptime_loop_var_token(tok token.Token) bool {
	return tok in [.name, .key_shared]
}

fn (g &Parser) substitute_comptime_field(body string, loop_var string, field FastcStructField) string {
	field_name := field.name
	file := token.File.unindexed('cf', body.len)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, body)
	mut edits := []FastcSourceEdit{}
	mut previous := token.Token.unknown
	mut tok := s.scan()
	for tok != .eof {
		if tok == .key_for {
			mut probe := s
			if probe.scan() == .name && probe.lit == 'attr' && probe.scan() == .key_in
				&& fastc_comptime_loop_var_token(probe.scan()) && probe.lit == loop_var
				&& probe.scan() == .dot && probe.scan() == .name && probe.lit == 'attrs'
				&& probe.scan() == .lcbr {
				loop_start := s.pos
				mut depth := 0
				mut part := s.scan()
				mut loop_end := s.offset
				for part != .eof {
					if part == .lcbr {
						depth++
					} else if part == .rcbr {
						depth--
						if depth == 0 {
							loop_end = s.offset
							break
						}
					}
					part = s.scan()
				}
				edits << FastcSourceEdit{
					start: loop_start
					end: loop_end
					replacement: if field.is_skip { 'is_skip = true' } else { '' }
				}
				previous = .rcbr
				tok = s.scan()
				continue
			}
		}
		if tok == .dollar && previous == .dot {
			// Potential computed selector `.$(<var>.name)`.
			dollar_pos := s.pos
			open := s.scan()
			if open == .lpar {
				name_tok := s.scan()
				if fastc_comptime_loop_var_token(name_tok) && s.lit == loop_var {
					dot_tok := s.scan()
					if dot_tok == .dot {
						member := s.scan()
						if member == .name && s.lit == 'name' {
							close := s.scan()
							if close == .rpar {
								edits << FastcSourceEdit{
									start: dollar_pos
									end: s.offset
									replacement: field_name
								}
								previous = .rpar
								tok = s.scan()
								continue
							}
						}
					}
				}
			}
			previous = .dollar
			tok = open
			continue
		}
		if fastc_comptime_loop_var_token(tok) && s.lit == loop_var {
			var_pos := s.pos
			after := s.scan()
			if after == .dot {
				member := s.scan()
				if member == .name && s.lit == 'name' {
					edits << FastcSourceEdit{
						start: var_pos
						end: s.offset
						replacement: "'${field_name}'"
					}
					previous = .name
					tok = s.scan()
					continue
				}
				if member == .name && s.lit == 'is_embed' {
					is_embed := field.name.starts_with('__embedded_')
					edits << FastcSourceEdit{
						start: var_pos
						end: s.offset
						replacement: if is_embed { 'true' } else { 'false' }
					}
					previous = if is_embed { token.Token.key_true } else { token.Token.key_false }
					tok = s.scan()
					continue
				}
				if member == .name && s.lit == 'attrs' {
					after_attrs := s.scan()
					if after_attrs == .dot {
						contains_tok := s.scan()
						contains_name := s.lit
						open_tok := s.scan()
						attr_tok := s.scan()
						attr_name := s.lit.trim('\'"')
						close_tok := s.scan()
						if contains_tok == .name && contains_name == 'contains' && open_tok == .lpar && attr_tok == .string && close_tok == .rpar {
							matches := field.is_skip && attr_name == 'skip'
							edits << FastcSourceEdit{
								start: var_pos
								end: s.offset
								replacement: if matches { 'true' } else { 'false' }
							}
							previous = if matches {
								token.Token.key_true
							} else {
								token.Token.key_false
							}
							tok = s.scan()
							continue
						}
					}
					edits << FastcSourceEdit{
						start: var_pos
						end: s.offset
						replacement: if field.is_skip { "['skip']" } else { "['']" }
					}
					previous = member
					tok = after_attrs
					continue
				}
				if member == .name && s.lit == 'typ' {
					is_tok := s.scan()
					if is_tok == .key_is {
						type_c, type_end, next_after := g.read_comptime_is_type(mut s)
						if type_c != '' {
							matches := if type_c.starts_with('?') {
								field.option_value_type == type_c[1..]
							} else if type_c.starts_with('@') {
								g.comptime_type_matches(field.typ, type_c[1..], true)
							} else {
								field.typ.trim_right('*') == type_c
							}
							edits << FastcSourceEdit{
								start: var_pos
								end: type_end
								replacement: if matches { 'true' } else { 'false' }
							}
							previous = if matches {
								token.Token.key_true
							} else {
								token.Token.key_false
							}
							tok = next_after
							continue
						}
						previous = is_tok
						tok = next_after
						continue
					}
					previous = member
					tok = is_tok
					continue
				}
				previous = member
				tok = s.scan()
				continue
			}
			previous = tok
			tok = after
			continue
		}
		previous = tok
		tok = s.scan()
	}
	return fastc_apply_source_edits(body, edits)
}

// substitute_comptime_enum_value rewrites one `$for x in Enum.values { ... }` body
// for a single enum value: `<var>.name` becomes the value-name string literal and
// `<var>.value` becomes the enum's C constant. Other `<var>.<member>` forms are left
// as-is so they surface as unsupported when re-parsed.
fn (g &Parser) substitute_comptime_enum_value(body string, loop_var string, enum_c_type string, value_name string) string {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('cv', body.len)
	file.index_lines_without_digest(body)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, body)
	mut edits := []FastcSourceEdit{}
	mut tok := s.scan()
	for tok != .eof {
		if fastc_comptime_loop_var_token(tok) && s.lit == loop_var {
			var_pos := s.pos
			after := s.scan()
			if after == .dot {
				member := s.scan()
				if member == .name && s.lit == 'name' {
					edits << FastcSourceEdit{
						start: var_pos
						end: s.offset
						replacement: "'${value_name}'"
					}
					tok = s.scan()
					continue
				}
				if member == .name && s.lit == 'value' {
					edits << FastcSourceEdit{
						start: var_pos
						end: s.offset
						replacement: '${enum_c_type}__${value_name}'
					}
					tok = s.scan()
					continue
				}
				tok = s.scan()
				continue
			}
			tok = after
			continue
		}
		tok = s.scan()
	}
	return fastc_apply_source_edits(body, edits)
}

// read_comptime_is_type reads the type on the right of `<var>.typ is <Type>` and
// returns its C spelling (empty if unrecognized), the source offset just past the
// type, and the already-scanned token that follows it.
fn (g &Parser) read_comptime_is_type(mut s scanner.Scanner) (string, int, token.Token) {
	first := s.scan()
	if first == .question {
		name_tok := s.scan()
		if name_tok != .name {
			return '', s.offset, s.scan()
		}
		name := s.lit
		name_end := s.offset
		next_token := s.scan()
		if next_token == .dot {
			qualified := s.scan()
			if qualified == .name {
				module_name := g.imports[name] or { name }
				key := fastc_type_key(module_name, s.lit)
				end := s.offset
				return '?${fastc_c_declared_type_name(key)}', end, s.scan()
			}
			return '', s.offset, s.scan()
		}
		return '?${g.resolve_type_name_c(name)}', name_end, next_token
	}
	if first == .dollar {
		group := s.scan()
		if group == .name || group.is_keyword() {
			name := s.lit
			end := s.offset
			return '@${name}', end, s.scan()
		}
		return '', s.offset, s.scan()
	}
	if first == .lsbr {
		bracket := s.scan()
		if bracket == .rsbr {
			element := s.scan()
			if element == .name {
				element_c := g.resolve_type_name_c(s.lit)
				end := s.offset
				return fastc_array_c_type(element_c), end, s.scan()
			}
		}
		return '', s.offset, s.scan()
	}
	if first != .name {
		return '', s.offset, s.scan()
	}
	name := s.lit
	name_end := s.offset
	next_token := s.scan()
	if next_token == .dot {
		qualified := s.scan()
		if qualified == .name {
			module_name := g.imports[name] or { name }
			key := fastc_type_key(module_name, s.lit)
			end := s.offset
			return fastc_c_declared_type_name(key), end, s.scan()
		}
		return '', s.offset, s.scan()
	}
	return g.resolve_type_name_c(name), name_end, next_token
}

// resolve_type_name_c resolves an unqualified type name to its C spelling: a
// primitive keeps its own name, a declared type maps through the type key.
fn (g &Parser) resolve_type_name_c(name string) string {
	if fastc_primitive_c_type(name) != none {
		return name
	}
	key := g.resolve_declared_type_key(name) or { return name }
	return fastc_c_declared_type_name(key)
}

fn (mut g Parser) parse_comptime_or() !bool {
	mut value := g.parse_comptime_and()!
	for g.tok == .logical_or {
		g.next()
		right := g.parse_comptime_and()!
		value = value || right
	}
	return value
}

fn (mut g Parser) parse_comptime_and() !bool {
	mut value := g.parse_comptime_unary()!
	for g.tok == .and {
		g.next()
		right := g.parse_comptime_unary()!
		value = value && right
	}
	return value
}

fn (mut g Parser) parse_comptime_unary() !bool {
	if g.tok == .not {
		g.next()
		return !g.parse_comptime_unary()!
	}
	if g.tok == .lpar {
		g.next()
		value := g.parse_comptime_or()!
		g.expect(.rpar)!
		return value
	}
	if g.tok == .key_true {
		g.next()
		return true
	}
	if g.tok == .key_false {
		g.next()
		return false
	}
	if g.tok == .string {
		actual := g.lit.trim('\'"')
		g.next()
		operator := g.tok
		if operator !in [.eq, .ne] {
			return g.unsupported('compile-time string comparison operator `${g.token_source()}`')
		}
		g.next()
		if g.tok != .string {
			return g.unsupported('compile-time string comparison value `${g.token_source()}`')
		}
		expected := g.lit.trim('\'"')
		g.next()
		return if operator == .eq { actual == expected } else { actual != expected }
	}
	if g.tok == .dollar {
		// `$pkgconfig('lib')`: true when pkg-config reports the library present.
		g.next()
		if g.tok != .name || g.lit != 'pkgconfig' {
			return g.unsupported('compile-time condition `${g.token_source()}`')
		}
		g.next()
		g.expect(.lpar)!
		if g.tok != .string {
			return g.unsupported('`\$pkgconfig` library name')
		}
		library := g.lit.trim('\'"')
		g.next()
		g.expect(.rpar)!
		return pref.comptime_pkgconfig_value(library)
	}
	if g.tok != .name {
		return g.unsupported('compile-time condition `${g.token_source()}`')
	}
	name := g.lit
	g.next()
	// `T.unaliased_typ is X` (json2's encoder dispatch): a comptime type test on the type
	// with aliases resolved. After monomorphization `T` is a concrete type name here, so
	// resolve the leading name's underlying (un-aliased) type before the `is` compare; for
	// a non-alias it is a no-op.
	mut unaliased := false
	mut indirections := false
	mut pointee_type := false
	if g.tok == .dot {
		g.next()
		if g.tok != .name || g.lit !in ['unaliased_typ', 'indirections', 'pointee_type'] {
			return g.unsupported('compile-time member `.${g.token_source()}`')
		}
		unaliased = g.lit == 'unaliased_typ'
		indirections = g.lit == 'indirections'
		pointee_type = g.lit == 'pointee_type'
		g.next()
	}
	if indirections {
		operator := g.tok
		if operator !in [.eq, .ne, .lt, .gt, .le, .ge] {
			return g.unsupported('compile-time `.indirections` comparison')
		}
		g.next()
		if g.tok != .number {
			return g.unsupported('compile-time `.indirections` value')
		}
		expected := g.lit.int()
		g.next()
		resolved := g.comptime_named_type(name)
		actual := resolved.len - resolved.trim_right('*').len
		return match operator {
			.eq { actual == expected }
			.ne { actual != expected }
			.lt { actual < expected }
			.gt { actual > expected }
			.le { actual <= expected }
			.ge { actual >= expected }
			else { false }
		}
	}
	if g.tok in [.key_is, .not_is] {
		operator := g.tok
		g.next()
		mut target_is_group := false
		mut target_type := ''
		if g.tok == .dollar {
			target_is_group = true
			g.next()
			if g.tok != .name && !g.tok.is_keyword() {
				return g.unsupported('compile-time type group `${g.token_source()}`')
			}
			target_type = g.lit
			g.next()
		} else {
			target_type = g.parse_type()!
		}
		mut left_type := g.comptime_named_type(name)
		if unaliased {
			left_type = g.underlying_alias_type(left_type)
		}
		if pointee_type {
			left_type = if left_type.ends_with('*') {
				left_type[..left_type.len - 1]
			} else {
				'void'
			}
		}
		matches := g.comptime_type_matches(left_type, target_type, target_is_group)
		return if operator == .key_is { matches } else { !matches }
	}
	if g.tok in [.eq, .ne] {
		// `$if @BACKEND == 'arm64'` etc.: compare a compile-time pseudo variable to a
		// string literal.
		operator := g.tok
		g.next()
		if g.tok != .string {
			return g.unsupported('compile-time comparison value `${g.token_source()}`')
		}
		expected := g.lit.trim('\'"')
		g.next()
		actual := g.comptime_pseudo_string(name) or {
			return g.unsupported('compile-time comparison of `${name}`')
		}
		return if operator == .eq { actual == expected } else { actual != expected }
	}
	is_optional := g.tok == .question
	if is_optional {
		g.next()
	}
	return if is_optional {
		pref.comptime_optional_flag_value(g.prefs, name)
	} else {
		pref.comptime_flag_value(g.prefs, name)
	}
}

// comptime_pseudo_string returns the plain string value of a compile-time pseudo
// variable used in a `$if … == '…'` comparison, or none when it is not comparable.
fn (g &Parser) comptime_pseudo_string(name string) ?string {
	return match name {
		'@BACKEND' { g.prefs.backend.str() }
		'@OS' { g.prefs.normalized_target_os() }
		'@CCOMPILER' { g.prefs.ccompiler }
		'@PLATFORM' { g.prefs.comptime_platform() }
		else { none }
	}
}

fn (g &Parser) comptime_named_type(name string) string {
	if local := g.locals[name] {
		return local.typ
	}
	if primitive := fastc_primitive_c_type(name) {
		return primitive
	}
	if key := g.resolve_declared_type_key(name) {
		return fastc_c_declared_type_name(key)
	}
	if name.len <= 3 && name[0].is_capital() {
		return 'voidptr'
	}
	return name
}

fn (g &Parser) comptime_type_matches(left_type string, target_type string, target_is_group bool) bool {
	left := g.underlying_alias_type(left_type)
	if !target_is_group {
		return left == g.underlying_alias_type(target_type)
	}
	left_key := g.semantic_type_key(left)
	original_key := g.semantic_type_key(left_type)
	return match target_type {
		'array' {
			left.starts_with('Array_') || left == 'array'
		}
		'array_fixed' {
			left.starts_with('FixedArray_')
		}
		'map' {
			left.starts_with('Map_') || left == 'map'
		}
		'string' {
			left == 'string'
		}
		'int' {
			left in ['i8', 'i16', 'i32', 'i64', 'int', 'isize', 'u8', 'u16', 'u32', 'u64', 'uint',
				'usize']
		}
		'float' {
			left in ['f32', 'f64']
		}
		'struct' {
			left_key in g.declared_kinds && g.declared_kinds[left_key] == .struct_
		}
		'enum' {
			left_key in g.declared_kinds && g.declared_kinds[left_key] == .enum_
		}
		'interface' {
			left_key in g.declared_kinds && g.declared_kinds[left_key] == .interface_
		}
		'alias' {
			original_key in g.declared_kinds && g.declared_kinds[original_key] == .alias_
		}
		else {
			false
		}
	}
}

fn (mut g Parser) skip_open_block() ! {
	mut depth := 1
	for depth > 0 {
		if g.tok == .eof {
			return g.unsupported('unfinished compile-time block')
		}
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			depth--
		}
		g.next()
	}
	g.skip_semicolons()
}

fn (mut g Parser) skip_comptime_if_chain() ! {
	if g.tok != .dollar {
		return g.unsupported('compile-time `\$else` branch')
	}
	g.next()

	g.expect(.key_if)!
	_ = g.parse_comptime_or()!
	g.expect(.lcbr)!
	g.skip_open_block()!
	if g.tok == .dollar && g.dollar_keyword_is('else') {
		g.next()
		g.expect(.key_else)!
		if g.tok == .dollar {
			g.skip_comptime_if_chain()!
		} else {
			g.expect(.lcbr)!
			g.skip_open_block()!
		}
	}
}

// read_comptime_d_expression lowers `$d('key', default)`, the compile-time
// define value. FastC does not transport `-d key=value` values into its fixed
// build, so it emits the default expression.
fn (mut g Parser) read_comptime_d_expression() !string {
	g.expect(.name)! // consume `d`
	g.expect(.lpar)!
	if g.tok != .string {
		return g.unsupported('`\$d` compile-time value key')
	}
	g.next() // the define name (only meaningful with `-d name=value`, not transported)
	g.expect(.comma)!
	default_value := g.read_expression([token.Token.rpar])!
	value_type := g.last_expression_type
	g.expect(.rpar)!
	g.last_expression_type = value_type
	g.last_expression = []FastcExpressionToken{}
	return default_value
}

// read_comptime_embed_file lowers `$embed_file('path')` by reading the file at
// generation time. The compiler chain uses the result as `.to_string()` /
// `.to_bytes()`, so those two accessors are supported directly; the file bytes
// become a C string literal or a `[]u8` array, exactly what the accessors return.
fn (mut g Parser) read_comptime_embed_file() !string {
	g.expect(.name)! // consume `embed_file`
	g.expect(.lpar)!
	if g.tok != .string {
		return g.unsupported('`\$embed_file` requires a string literal path')
	}
	path_literal := g.lit
	g.next()
	// Additional arguments (e.g. an `EmbedFileIndex` or compression option) do not
	// change what the two supported accessors return, so ignore them.
	for g.tok == .comma {
		g.next()
		g.read_expression([token.Token.comma, token.Token.rpar])!
	}
	g.expect(.rpar)!
	embed_path := fastc_string_literal_content(path_literal)
	resolved := if os.is_abs_path(embed_path) {
		embed_path
	} else {
		os.join_path(os.dir(g.path), embed_path)
	}
	content := os.read_file(resolved) or {
		return g.unsupported('`\$embed_file` cannot read `${resolved}`')
	}
	if g.tok != .dot {
		return g.unsupported('`\$embed_file` is only supported with a `.to_string()` or `.to_bytes()` accessor')
	}
	g.next()
	if g.tok != .name {
		return g.unsupported('`\$embed_file` accessor')
	}
	accessor := g.lit
	g.next()
	g.expect(.lpar)!
	g.expect(.rpar)!
	match accessor {
		'to_string', 'str' {
			g.last_expression_type = 'string'
			g.last_expression = []FastcExpressionToken{}
			return '_S(${fastc_c_string_value(content)})'
		}
		else {
			return g.unsupported('`\$embed_file` accessor `.${accessor}()`')
		}
	}
}

// fastc_string_literal_content returns the raw text inside a scanned string
// literal token, stripping an optional `r`/`c` prefix and the surrounding quotes.
// Embed paths contain no escape sequences, so no unescaping is required.
fn fastc_string_literal_content(literal string) string {
	mut raw := literal
	if raw.len >= 3 && raw[0] in [`r`, `c`] && raw[1] in [`'`, `"`] {
		raw = raw[1..]
	}
	if raw.len >= 2 && raw[0] in [`'`, `"`, `\``] && raw[raw.len - 1] == raw[0] {
		return raw[1..raw.len - 1]
	}
	return raw
}

fn (g &Parser) dollar_keyword_is(keyword string) bool {
	mut offset := g.s.offset
	for offset < g.s.src.len && g.s.src[offset] in [` `, `\t`] {
		offset++
	}
	return offset + keyword.len <= g.s.src.len && g.s.src[offset..offset + keyword.len] == keyword
}

fn (g &Parser) comptime_pseudo_expression(name string) ?string {
	line, column := fastc_line_column(g.s.src, g.s.pos)
	module_name := if g.module_name == '' { 'main' } else { g.module_name }
	function_name := g.current_function
	receiver_name := g.current_receiver.all_after_last('.')
	method_name := if receiver_name != '' {
		'${receiver_name}.${function_name}'
	} else {
		function_name
	}
	location_method := if receiver_name == '' {
		'${module_name}.${function_name}'
	} else if g.current_method_is_static {
		'${module_name}.${receiver_name}.${function_name} (static)'
	} else {
		'${module_name}.${receiver_name}{}.${function_name}'
	}
	value := match name {
		'@FN' {
			function_name
		}
		'@METHOD' {
			method_name
		}
		'@STRUCT' {
			receiver_name
		}
		'@MOD' {
			module_name
		}
		'@FILE' {
			g.path
		}
		'@DIR' {
			os.dir(g.path)
		}
		'@LINE' {
			line.str()
		}
		'@COLUMN' {
			column.str()
		}
		'@FILE_LINE' {
			'${os.file_name(g.path)}:${line}'
		}
		'@LOCATION' {
			'${g.path}:${line}, ${location_method}'
		}
		'@VEXEROOT', '@VROOT' {
			g.prefs.vroot
		}
		'@VMODROOT' {
			fastc_vmod_root_for_file(g.path)
		}
		'@VEXE' {
			g.prefs.vexe
		}
		'@VMOD_FILE' {
			vmod_file := os.join_path_single(fastc_vmod_root_for_file(g.path), 'v.mod')
			content := os.read_file(vmod_file) or { return none }
			content.replace('\r\n', '\n')
		}
		'@VHASH' {
			g.prefs.vhash
		}
		'@VCURRENTHASH' {
			g.prefs.vcurrent_hash
		}
		'@BUILD_DATE' {
			g.prefs.build_date
		}
		'@BUILD_TIME' {
			g.prefs.build_time
		}
		'@BUILD_TIMESTAMP' {
			g.prefs.build_timestamp
		}
		'@OS' {
			g.prefs.normalized_target_os()
		}
		'@CCOMPILER' {
			g.prefs.ccompiler
		}
		'@BACKEND' {
			g.prefs.backend
		}
		'@PLATFORM' {
			g.prefs.comptime_platform()
		}
		else {
			return none
		}
	}
	return '_S(${fastc_c_string_value(value)})'
}

fn (mut g Parser) read_comptime_if_expression() !string {
	g.expect(.dollar)!
	if g.tok == .name && g.lit == 'd' {
		return g.read_comptime_d_expression()!
	}
	if g.tok == .name && g.lit == 'embed_file' {
		return g.read_comptime_embed_file()!
	}
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
		value_type := g.last_expression_type
		g.skip_semicolons()
		g.expect(.rcbr)!
		if g.tok == .dollar {
			g.next()
			g.expect(.key_else)!
			if g.tok == .dollar {
				g.skip_comptime_if_chain()!
			} else {
				g.expect(.lcbr)!
				g.skip_open_block()!
			}
		}
		g.last_expression_type = value_type
		g.last_expression = []FastcExpressionToken{}
		return value
	}
	g.skip_open_block()!
	if g.tok != .dollar {
		return g.unsupported('compile-time if expression without `\$else`')
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		return g.read_comptime_if_expression()!
	}
	g.expect(.lcbr)!
	value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	value_type := g.last_expression_type
	g.skip_semicolons()
	g.expect(.rcbr)!
	g.last_expression_type = value_type
	g.last_expression = []FastcExpressionToken{}
	return value
}

// --- veb `$veb.html()` template compilation -----------------------------------
// FastC compiles a handler's HTML template into V that accumulates the rendered
// output into a string builder local, mirroring the v3 parser's `compile_template_file`
// but only for the constructs gitly's templates use so far: plain text, `@{expr}` /
// `@ident` interpolation (HTML-escaped via `veb.filter_html`), `%key` i18n shorthand,
// `@include`, and `@if` / `@for` / `@else` control flow. Other `@` directives are
// reported as unsupported.
fn fastc_veb_ident_start(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
}

fn fastc_veb_ident_part(c u8) bool {
	return fastc_veb_ident_start(c) || (c >= `0` && c <= `9`)
}

// fastc_veb_balanced_brace returns the index of the `}` matching the `{` at open_index,
// or -1 if unbalanced.
fn fastc_veb_balanced_brace(s string, open_index int) int {
	mut depth := 0
	mut i := open_index
	for i < s.len {
		if s[i] == `{` {
			depth++
		} else if s[i] == `}` {
			depth--
			if depth == 0 {
				return i
			}
		}
		i++
	}
	return -1
}

// fastc_veb_balanced_pair returns the index just past the `close` matching the `open`
// at open_index (or -1 if unbalanced). Single/double-quoted spans are skipped so
// brackets inside string literals (`@get('/a]b')`) do not affect nesting.
fn fastc_veb_balanced_pair(s string, open_index int, open u8, close u8) int {
	mut depth := 0
	mut i := open_index
	mut in_single := false
	mut in_double := false
	for i < s.len {
		ch := s[i]
		if ch == `\\` {
			i += 2
			continue
		}
		if in_single {
			if ch == `'` {
				in_single = false
			}
			i++
			continue
		}
		if in_double {
			if ch == `"` {
				in_double = false
			}
			i++
			continue
		}
		if ch == `'` {
			in_single = true
		} else if ch == `"` {
			in_double = true
		} else if ch == open {
			depth++
		} else if ch == close {
			depth--
			if depth == 0 {
				return i + 1
			}
		}
		i++
	}
	return -1
}

// fastc_veb_at_expr_end returns the index just past a bare `@` expression beginning at
// `start` (the char after `@`): an identifier plus any `.field` member accesses,
// `[index]` subscripts and `(call)` suffixes (`@a.b.c`, `@t.id`, `@f(x)`), mirroring the
// v parser's `find_tmpl_complex_at_expr_end`. Returns `start` if not an identifier.
fn fastc_veb_at_expr_end(s string, start int) int {
	if start >= s.len || !fastc_veb_ident_start(s[start]) {
		return start
	}
	mut i := start + 1
	for i < s.len && fastc_veb_ident_part(s[i]) {
		i++
	}
	for i < s.len {
		if s[i] == `[` {
			end := fastc_veb_balanced_pair(s, i, `[`, `]`)
			if end == -1 {
				return i
			}
			i = end
			continue
		}
		if s[i] == `(` {
			end := fastc_veb_balanced_pair(s, i, `(`, `)`)
			if end == -1 {
				return i
			}
			i = end
			continue
		}
		if i + 1 < s.len && s[i] == `.` && fastc_veb_ident_start(s[i + 1]) {
			i += 2
			for i < s.len && fastc_veb_ident_part(s[i]) {
				i++
			}
			continue
		}
		break
	}
	return i
}

// fastc_veb_expand_tr rewrites veb's `%key` / `%raw key` translation shorthand into
// `@{...}` interpolations of `veb.tr(...)`, mirroring the v3 parser's shorthand.
fn fastc_veb_expand_tr(line string, ctx_name string) string {
	if !line.contains('%') {
		return line
	}
	mut out := ''
	mut i := 0
	for i < line.len {
		if line[i] == `%` {
			mut is_raw := false
			mut start := i + 1
			if i + 5 <= line.len && line[i + 1..i + 5] == 'raw ' {
				is_raw = true
				start = i + 5
			}
			mut end := start
			for end < line.len && fastc_veb_ident_part(line[end]) {
				end++
			}
			key := line[start..end]
			if key.len > 0 {
				if is_raw {
					out += '@{veb.raw(veb.tr(${ctx_name}.lang.str(), "${key}"))}'
				} else {
					out += '@{veb.tr(${ctx_name}.lang.str(), "${key}")}'
				}
				i = end
				continue
			}
		}
		out += line[i].ascii_str()
		i++
	}
	return out
}

// fastc_veb_line_content converts one template text line into the content of a V
// single-quoted string literal: literal characters are escaped, `@{expr}` / `@ident`
// become `${veb.filter_html(expr)}` interpolations, and `@@` an literal `@`.
fn fastc_veb_line_content(line string, ctx_name string) string {
	expanded := fastc_veb_expand_tr(line, ctx_name)
	mut out := ''
	mut i := 0
	for i < expanded.len {
		ch := expanded[i]
		if ch == `\\` {
			out += '\\\\'
			i++
			continue
		}
		if ch == `'` {
			out += "\\'"
			i++
			continue
		}
		if ch == `$` {
			// A literal `$` in the template must not become interpolation in the
			// generated V string.
			out += r'\$'
			i++
			continue
		}
		if ch == `@` {
			if i + 1 < expanded.len && expanded[i + 1] == `@` {
				out += '@'
				i += 2
				continue
			}
			if i + 1 < expanded.len && expanded[i + 1] == `{` {
				close := fastc_veb_balanced_brace(expanded, i + 1)
				if close != -1 {
					expr := expanded[i + 2..close]
					out += r'\$' + '{veb.filter_html(' + expr + ')}'
					i = close + 1
					continue
				}
			}
			if i + 1 < expanded.len && fastc_veb_ident_start(expanded[i + 1]) {
				// A bare `@expr` interpolation covers the whole member/subscript/call
				// chain (`@author.username`, `@t.id`, `@f(x)`), not just the leading name.
				end := fastc_veb_at_expr_end(expanded, i + 1)
				expr := expanded[i + 1..end]
				out += r'\$' + '{veb.filter_html(' + expr + ')}'
				i = end
				continue
			}
			out += '@'
			i++
			continue
		}
		out += ch.ascii_str()
		i++
	}
	return out
}

// fastc_veb_read_template_lines reads a template file, inlining `@include 'path'`
// directives relative to the including file's directory.
fn fastc_veb_read_template_lines(path string, depth int) ![]string {
	if depth > 32 {
		return error('`@include` recursion too deep')
	}
	raw := os.read_lines(path) or { return error('cannot read template `${path}`') }
	base_dir := os.dir(os.real_path(path))
	mut out := []string{}
	for line in raw {
		trimmed := line.trim_space()
		if trimmed.starts_with('@include ') {
			mut inc := trimmed['@include '.len..].trim_space()
			inc = inc.trim_left('\'"').trim_right('\'"')
			inc_path := os.join_path_single(base_dir, inc)
			included := fastc_veb_read_template_lines(inc_path, depth + 1)!
			for included_line in included {
				out << included_line
			}
			continue
		}
		out << line
	}
	return out
}

// fastc_veb_compile_template compiles an HTML template into V source that accumulates
// the rendered output into `bname`.
// fastc_veb_append_stmt returns `<bname> += '<content>'` as a V statement. The single
// quotes are concatenated rather than written as `\'` inside an interpolated string
// literal, which FastC's own string lowering mis-renders.
fn fastc_veb_append_stmt(bname string, content string) string {
	q := "'"
	return '${bname} += ' + q + content + q + '\n'
}

fn fastc_veb_compile_template(path string, bname string, ctx_name string) !string {
	lines := fastc_veb_read_template_lines(path, 0)!
	mut out := 'mut ${bname} := ' + "''" + '\n'
	mut current := ''
	// Distinguishes control-flow braces (`@if`/`@for`, emitted as V `{`/`}`) from HTML
	// block shorthands (`.class {` / `span.x {` / `#id {`, emitted as <div>/<span> text)
	// so a closing `}` line resolves to the right one, mirroring the v parser's
	// `brace_block_kinds` stack.
	mut block_kinds := []string{}
	for line in lines {
		trimmed := line.trim_space()
		if trimmed.starts_with('@if ') {
			out += fastc_veb_append_stmt(bname, current)
			current = ''
			header := trimmed['@if '.len..].trim_right('{').trim_space()
			out += 'if ${header} {\n'
			block_kinds << 'control'
			continue
		}
		if trimmed.starts_with('@for ') {
			out += fastc_veb_append_stmt(bname, current)
			current = ''
			header := trimmed['@for '.len..].trim_right('{').trim_space()
			out += 'for ${header} {\n'
			block_kinds << 'control'
			continue
		}
		if trimmed == '@else' || trimmed == '@else {' || trimmed == '@else{' || trimmed.starts_with('@else if ') {
			out += fastc_veb_append_stmt(bname, current)
			current = ''
			rest := trimmed['@else'.len..].trim_right('{').trim_space()
			out += '} else ${rest} {\n'
			continue
		}
		if trimmed == '@end' {
			out += fastc_veb_append_stmt(bname, current)
			current = ''
			out += '}\n'
			if block_kinds.len > 0 && block_kinds.last() == 'control' {
				block_kinds.delete_last()
			}
			continue
		}
		if trimmed == '}' {
			// A closing HTML block (`</div>` / `</span>` text) unless it matches a
			// control block opened with a trailing `{`, which closes as V `}`.
			if block_kinds.len > 0 && block_kinds.last() == 'span' {
				current += '</span>' + r'\n'
				block_kinds.delete_last()
			} else if block_kinds.len > 0 && block_kinds.last() == 'div' {
				current += '</div>' + r'\n'
				block_kinds.delete_last()
			} else {
				out += fastc_veb_append_stmt(bname, current)
				current = ''
				out += '}\n'
				if block_kinds.len > 0 && block_kinds.last() == 'control' {
					block_kinds.delete_last()
				}
			}
			continue
		}
		if trimmed.starts_with('span.') && trimmed.ends_with('{') {
			// `span.header {` => `<span class="header">`
			class := trimmed['span.'.len..trimmed.len - 1].trim_space()
			current += '<span class="${class}">' + r'\n'
			block_kinds << 'span'
			continue
		}
		if trimmed.starts_with('.') && trimmed.ends_with('{') {
			// `.header {` => `<div class="header">`
			class := trimmed[1..trimmed.len - 1].trim_space()
			current += '<div class="${class}">' + r'\n'
			block_kinds << 'div'
			continue
		}
		if trimmed.starts_with('#') && trimmed.ends_with('{') {
			// `#header {` => `<div id="header">`
			id := trimmed[1..trimmed.len - 1].trim_space()
			current += '<div id="${id}">' + r'\n'
			block_kinds << 'div'
			continue
		}
		if trimmed.starts_with('@css ') || trimmed.starts_with('@js ') {
			// `@css 'url'` / `@js 'url'` expand to a literal <link>/<script> tag whose
			// URL is emitted verbatim (mirrors the v parser's tmpl.v), so treat the
			// generated HTML as ordinary template content.
			q := "'"
			is_css := trimmed.starts_with('@css ')
			prefix := if is_css { '@css '.len } else { '@js '.len }
			url := trimmed[prefix..].trim_space().trim(q)
			line_html := if is_css {
				'<link href="${url}" rel="stylesheet" type="text/css">'
			} else {
				'<script src="${url}"></script>'
			}
			current += fastc_veb_line_content(line_html, ctx_name) + r'\n'
			continue
		}
		// A standalone `@ident` line (control directives `@if`/`@for`/... are handled above)
		// is a bare interpolation like `@source`, not a directive — render it as content.
		// Only a `@` followed by a non-identifier is a malformed/unsupported directive.
		if trimmed.starts_with('@') && !trimmed.starts_with('@@') && !trimmed.starts_with('@{') && !(trimmed.len > 1 && fastc_veb_ident_start(trimmed[1])) {
			return error('unsupported template directive `${trimmed}`')
		}
		current += fastc_veb_line_content(line, ctx_name) + r'\n'
	}
	out += fastc_veb_append_stmt(bname, current)
	return out
}

// fastc_veb_template_path resolves the current handler's HTML template file
// (`templates/<fn>.html`, with a directory-relative and a vmod-root fallback).
fn (g &Parser) fastc_veb_template_path() ?string {
	dir := os.dir(os.real_path(g.path))
	fn_name := g.current_function
	mut candidates := [
		os.join_path(dir, 'templates', '${fn_name}.html'),
		os.join_path_single(dir, '${fn_name}.html'),
	]
	vmod_root := fastc_vmod_root_for_file(g.path)
	if vmod_root != '' && vmod_root != dir {
		candidates << os.join_path(vmod_root, 'templates', '${fn_name}.html')
	}
	for candidate in candidates {
		if os.exists(candidate) {
			return candidate
		}
	}
	return none
}

// fastc_veb_resolve_explicit_path resolves an explicit `$veb.html('path')` template path,
// relative to the source file's directory (and the vmod root), or as given.
fn (g &Parser) fastc_veb_resolve_explicit_path(path string) ?string {
	dir := os.dir(os.real_path(g.path))
	mut candidates := [
		os.join_path_single(dir, path),
		path,
	]
	vmod_root := fastc_vmod_root_for_file(g.path)
	if vmod_root != '' && vmod_root != dir {
		candidates << os.join_path_single(vmod_root, path)
	}
	for candidate in candidates {
		if os.exists(candidate) {
			return candidate
		}
	}
	return none
}

// fastc_veb_context_name returns the veb `Context` argument name in scope (the handler's
// `mut ctx Context` parameter), defaulting to `ctx`.
fn (g &Parser) fastc_veb_context_name() string {
	if 'ctx' in g.locals {
		return 'ctx'
	}
	for name, local in g.locals {
		base := local.typ.trim_right('*')
		if base == 'Context' || base.ends_with('__Context') {
			return name
		}
	}
	return 'ctx'
}

// parse_veb_html_return lowers `return $veb.html()`: it compiles the handler's template
// into builder statements, then returns `<ctx>.html(<builder>)`.
fn (mut g Parser) parse_veb_html_return() !bool {
	g.next() // consume `$`
	if g.tok != .name || g.lit != 'veb' {
		return g.unsupported('comptime `\$` expression')
	}
	g.next() // consume `veb`
	g.expect(.dot)!
	if g.tok != .name || g.lit != 'html' {
		return g.unsupported('`\$veb.${g.lit}()` is not supported (only `\$veb.html()`)')
	}
	g.next() // consume `html`
	g.expect(.lpar)!
	mut explicit_path := ''
	if g.tok == .string {
		explicit_path = g.lit.trim('\'"')
		g.next()
	}
	if g.tok != .rpar {
		return g.unsupported('`\$veb.html(...)` argument must be a string-literal path')
	}
	g.expect(.rpar)!
	g.consume_statement_end()
	tmpl_path := if explicit_path != '' {
		g.fastc_veb_resolve_explicit_path(explicit_path) or {
			return g.unsupported('veb template `${explicit_path}` not found')
		}
	} else {
		g.fastc_veb_template_path() or {
			return g.unsupported('veb template for `${g.current_function}` not found')
		}
	}
	ctx_name := g.fastc_veb_context_name()
	bname := '__v_fastc_veb_tmpl'
	mut lowering := fastc_veb_compile_template(tmpl_path, bname, ctx_name) or {
		return g.unsupported('veb template `${tmpl_path}`: ${err.msg()}')
	}
	lowering += 'return ${ctx_name}.html(${bname})\n'
	g.write_line('{')
	g.indent++
	g.emit_orm_lowering_statements(lowering)!
	g.indent--
	g.write_line('}')
	return true
}
