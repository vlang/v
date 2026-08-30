module fastc

import v3.scanner
import v3.pref
import v3.token

fn (mut g Parser) parse_block_body() !bool {
	outer_locals := g.locals.clone()
	outer_statement_reachable := g.statement_reachable
	deferred_line_start := g.deferred_lines.len
	deferred_block_start := g.deferred_block_starts.len
	mut terminates := false
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished block')
		}
		g.statement_reachable = outer_statement_reachable && !terminates
		statement_terminates := g.parse_statement()!
		if statement_terminates {
			terminates = true
			if g.selfhost {
				for g.tok !in [.rcbr, .eof] {
					if g.tok == .lcbr {
						g.skip_balanced(.lcbr, .rcbr)!
					} else {
						g.next()
					}
				}
			}
		}
		g.skip_semicolons()
	}
	g.next()
	g.skip_semicolons()
	g.write_deferred_blocks_from(deferred_block_start)
	g.deferred_lines.trim(deferred_line_start)
	g.deferred_block_starts.trim(deferred_block_start)
	g.locals = outer_locals.clone()
	g.statement_reachable = outer_statement_reachable
	return terminates
}

fn (mut g Parser) parse_statement() !bool {
	if g.defer_depth > 0 {
		match g.tok {
			.key_return {
				return g.unsupported('`return` not allowed inside a `defer` block')
			}
			.key_break, .key_continue {
				return g.unsupported('`${g.tok.str()}` is not allowed in defer statements')
			}
			.key_goto {
				return g.unsupported('goto is not allowed in defer statements')
			}
			.key_defer {
				return g.unsupported('`defer` blocks cannot be nested')
			}
			else {}
		}
	}
	return match g.tok {
		.dollar {
			if g.dollar_keyword_is('for') {
				g.parse_comptime_for_statement()!
			} else if g.dollar_keyword_is('match') {
				g.parse_comptime_match_statement()!
			} else {
				g.parse_comptime_if_statement()!
			}
		}
		.key_if {
			g.parse_if()!
		}
		.key_for {
			g.parse_for()!
		}
		.key_match {
			g.parse_match_statement()!
		}
		.key_select {
			g.parse_select_statement()!
		}
		.key_return {
			g.parse_return()!
		}
		.key_break {
			g.next()
			g.consume_statement_end()
			if g.loop_defer_block_starts.len == 0 {
				return g.unsupported('`break` outside a loop')
			}
			if g.statement_reachable && g.loop_has_breaks.len > 0 {
				g.loop_has_breaks[g.loop_has_breaks.len - 1] = true
			}
			g.write_deferred_blocks_from(g.loop_defer_block_starts.last())
			g.write_line('break;')
			true
		}
		.key_continue {
			g.next()
			g.consume_statement_end()
			if g.loop_defer_block_starts.len == 0 {
				return g.unsupported('`continue` outside a loop')
			}
			g.write_deferred_blocks_from(g.loop_defer_block_starts.last())
			g.write_line('continue;')
			true
		}
		.key_goto {
			g.next()
			if g.tok != .name {
				return g.unsupported('goto without a label')
			}
			label := fastc_c_identifier(g.lit)
			g.next()
			g.consume_statement_end()
			g.write_line('goto ${label};')
			false
		}
		.key_defer {
			g.parse_defer()!
			false
		}
		.key_mut {
			g.parse_mutable_declaration()!
			false
		}
		.key_static {
			g.next()
			if g.tok != .name && !(g.tok == .key_shared && g.shared_token_is_identifier(.key_static)) {
				return g.unsupported('static local declaration')
			}
			name := g.lit
			g.next()
			if g.tok != .decl_assign {
				return g.unsupported('static local without `:=`')
			}
			// FastC only needs compile-time static locals for generic helper caches.
			// Lowering them as mutable function locals preserves compile/link behavior.
			g.parse_declaration_after_name(name, true)!
			false
		}
		.key_unsafe {
			g.next()
			g.expect(.lcbr)!
			g.unsafe_depth += 1
			terminates := g.parse_block_body()!
			g.unsafe_depth -= 1
			terminates
		}
		else {
			g.parse_simple_statement()!
			false
		}
	}
}

fn (mut g Parser) parse_select_statement() !bool {
	return g.unsupported('select statements')
}

fn (g &Parser) open_block_contains_select_statement() bool {
	if g.tok != .lcbr {
		return false
	}
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut depth := 1
	mut previous := token.Token.lcbr
	mut tok := lookahead.scan()
	for depth > 0 && tok != .eof {
		if tok == .key_select && previous != .dot {
			// A channel `select { ... }` statement is `select` immediately followed by
			// `{`. `select` used as an ORM `sql` clause keyword (`sql db { select from
			// ... }`) or as a method/field name is not one, so keep scanning past it.
			following := lookahead.scan()
			if following == .lcbr {
				return true
			}
			previous = tok
			tok = following
			continue
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
		}
		previous = tok
		tok = lookahead.scan()
	}
	return false
}

// method_uses_undefined_receiver reports whether the upcoming `{ ... }` body (g.tok at
// the opening `{`; the receiver and parameters are already in g.locals) calls a method on
// an identifier that is provably undefined — not a receiver/parameter/body-local and not a
// known module, const, global, type or enum. Such a function is broken dead code kept only
// by FastC's conservative name-grouped reachability (a same-named method on another type is
// genuinely used, so the shared name survives pruning); skipping it mirrors the mainline
// compiler's `-skip-unused`. It cannot fire on well-formed code, where every method-call
// receiver is defined.
fn (g &Parser) method_uses_undefined_receiver() bool {
	if g.tok != .lcbr {
		return false
	}
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	// Seed identifiers that are valid receivers but are not locals: `C` (the C-interop
	// namespace, `C.func()`), and V's implicitly-bound closure/or-block names `it` and
	// `err`. Without these the scan would mistake them for undefined receivers.
	mut bound := {
		'C':   true
		'it':  true
		'err': true
	}
	mut receivers := []string{}
	// A comma-separated run of names is only a binding LHS when it ends in `:=` (a
	// declaration like `a, b := f()`), NOT when it is a call's arguments (`f(a, b)`); hold
	// such names pending until a terminating `:=`/`=` proves they are declared.
	mut pending_comma := []string{}
	mut depth := 1
	mut previous := token.Token.lcbr
	mut tok := lookahead.scan()
	mut lit := lookahead.lit
	for depth > 0 && tok != .eof {
		if tok == .key_fn {
			// A closure (`fn [caps] (params) { ... }`) introduces its own parameters, which
			// this flat pre-scan cannot track; be conservative and never skip a method whose
			// body contains one.
			return false
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
		}
		if tok == .name {
			mut ahead := lookahead
			next := ahead.scan()
			// A binding position: `x :=` / `x =`, a `for ... in` loop variable, a name
			// introduced by `mut`/`for`, or a comma-list LHS that (together with the pending
			// run) terminates in `:=` / `=` / `in` (e.g. `for i, ch in s`).
			if next in [.decl_assign, .assign, .key_in] {
				bound[lit] = true
				for p in pending_comma {
					bound[p] = true
				}
				pending_comma = []string{}
			} else if next == .comma {
				pending_comma << lit
			} else if previous in [.key_mut, .key_for] {
				bound[lit] = true
				pending_comma = []string{}
			} else {
				// The name does not continue a binding LHS, so the pending comma-run was a
				// value list (e.g. call arguments), not a declaration.
				pending_comma = []string{}
			}
			// A receiver `R` of a method call `R.m(...)` or an indexed member `R.f[...]`
			// (`ctx.query['k']`) must be a defined value. Restricting to a trailing `(` / `[`
			// (not a bare `R.f` / `R.f < x`) avoids flagging `.sort(a.x < b.x)` comparison
			// closures. Only the base of an `a.b.c` chain is flagged (inner members have
			// `previous == .dot`).
			if previous != .dot && next == .dot {
				mut ahead2 := ahead
				if ahead2.scan() == .name {
					after := ahead2.scan()
					if after == .lpar || after == .lsbr {
						receivers << lit
					}
				}
			}
		}
		previous = tok
		tok = lookahead.scan()
		lit = lookahead.lit
	}
	for r in receivers {
		if r in bound || r in g.locals || r in g.imports {
			continue
		}
		// Module-scope consts / globals are valid receivers (`long_months.index(...)`), but
		// their maps are module-qualified, so resolve `r` through the current module.
		ckey := fastc_constant_key(g.module_name, r)
		if ckey in g.constant_values || ckey in g.constant_types || ckey in g.public_constants {
			continue
		}
		if fastc_global_key(g.module_name, r) in g.globals {
			continue
		}
		if g.is_enum_type_name(r) {
			continue
		}
		if _ := fastc_resolve_declared_type_key(g.module_name, r, g.imports, g.declared_types) {
			continue
		}
		return true
	}
	return false
}

fn (mut g Parser) parse_defer() ! {
	g.next()
	g.expect(.lcbr)!
	previous_capture := g.capturing_defer
	previous_lines := g.captured_defer_lines.clone()
	g.capturing_defer = true
	g.defer_depth++
	g.captured_defer_lines = []string{}
	_ = g.parse_block_body()!
	block := g.captured_defer_lines.clone()
	g.defer_depth--
	g.capturing_defer = previous_capture
	g.captured_defer_lines = previous_lines.clone()
	g.deferred_block_starts << g.deferred_lines.len
	for line in block {
		g.deferred_lines << line
	}
}

fn (mut g Parser) write_deferred_blocks_from(first int) {
	for block_index := g.deferred_block_starts.len - 1; block_index >= first; block_index-- {
		line_start := g.deferred_block_starts[block_index]
		line_end := if block_index + 1 < g.deferred_block_starts.len {
			g.deferred_block_starts[block_index + 1]
		} else {
			g.deferred_lines.len
		}
		for line_index in line_start .. line_end {
			g.out.writeln(g.deferred_lines[line_index])
		}
	}
}

fn (mut g Parser) write_all_deferred_scopes() {
	if g.deferred_block_starts.len > 0 {
		g.write_deferred_blocks_from(0)
	}
}

fn (g &Parser) deferred_scopes_source() string {
	mut lines := []string{}
	for block_index := g.deferred_block_starts.len - 1; block_index >= 0; block_index-- {
		line_start := g.deferred_block_starts[block_index]
		line_end := if block_index + 1 < g.deferred_block_starts.len {
			g.deferred_block_starts[block_index + 1]
		} else {
			g.deferred_lines.len
		}
		for line_index in line_start .. line_end {
			lines << g.deferred_lines[line_index]
		}
	}
	return lines.join(' ')
}

fn (mut g Parser) parse_loop_block_body() !FastcLoopBlockResult {
	g.loop_defer_block_starts << g.deferred_block_starts.len
	g.loop_has_breaks << false
	terminates := g.parse_block_body()!
	has_reachable_break := g.loop_has_breaks.last()
	g.loop_has_breaks.delete_last()
	g.loop_defer_block_starts.delete_last()
	return FastcLoopBlockResult{
		terminates:          terminates
		has_reachable_break: has_reachable_break
	}
}

fn (mut g Parser) parse_match_statement() !bool {
	g.expect(.key_match)!
	subject := g.read_expression([token.Token.lcbr])!
	subject_type := fastc_normalize_inferred_type(g.last_expression_type)
	if subject == '' || subject_type == '' {
		return g.unsupported('unverifiable match subject')
	}
	subject_tokens := g.last_expression.clone()
	g.expect(.lcbr)!
	subject_name := g.temporary_name('match')
	g.write_line('__typeof__((${subject})) ${subject_name} = (${subject});')
	is_string := g.underlying_alias_type(subject_type).trim_right('*') == 'string'
	// A sum type or interface subject dispatches on the boxed `_typ` tag; each
	// branch names a variant/implementer type. When the subject is a plain local,
	// the branch body sees it smart-cast to the matched concrete type.
	is_boxed := g.is_boxed_type(subject_type)
	boxed_access := if subject_type.ends_with('*') { '->' } else { '.' }
	mut subject_local := ''
	if is_boxed && subject_tokens.len == 1 && subject_tokens[0].tok == .name
		&& subject_tokens[0].lit in g.locals {
		subject_local = subject_tokens[0].lit
	} else if is_boxed && subject_tokens.len == 2 && subject_tokens[0].tok in [.key_mut, .amp]
		&& subject_tokens[1].tok == .name && subject_tokens[1].lit in g.locals {
		subject_local = subject_tokens[1].lit
	}
	mut subject_member_path := ''
	if is_boxed && subject_tokens.len >= 3 && subject_tokens.len % 2 == 1
		&& subject_tokens[0].tok == .name && subject_tokens[0].lit in g.locals {
		mut is_member_chain := true
		mut path := subject_tokens[0].lit
		for i := 1; i + 1 < subject_tokens.len; i += 2 {
			if subject_tokens[i].tok != .dot || subject_tokens[i + 1].tok != .name {
				is_member_chain = false
				break
			}
			path += '.' + subject_tokens[i + 1].lit
		}
		if is_member_chain {
			subject_member_path = path
		}
	}
	mut branch_index := 0
	mut all_terminate := true
	mut has_else := false
	mut handled_cases := map[string]bool{}
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished match statement')
		}
		is_else := g.tok == .key_else
		has_else = has_else || is_else
		mut values := []string{}
		mut values_are_conditions := []bool{}
		mut variant_types := []string{}
		if is_else {
			g.next()
		} else if is_boxed {
			for {
				variant_key := g.read_match_type_key() or {
					return g.unsupported('match type value')
				}
				variant_cname := fastc_c_declared_type_name(variant_key)
				if variant_cname in handled_cases {
					return g.unsupported('duplicate match case `${variant_cname}`')
				}
				handled_cases[variant_cname] = true
				variant_types << variant_cname
				values << '${subject_name}${boxed_access}_typ == __v_typeid_${variant_cname}'
				if g.tok != .comma {
					break
				}
				g.next()
			}
		} else {
			for {
				mut value := ''
				mut value_tokens := []FastcExpressionToken{}
				mut case_key := ''
				mut value_is_condition := false
				if g.tok == .dot {
					g.next()
					if g.tok != .name && !g.tok.is_keyword() {
						return g.unsupported('match enum value')
					}
					value = '${subject_type.trim_right('*')}__${g.lit}'
					g.next()
				} else {
					value = g.read_expression([token.Token.comma, token.Token.lcbr, token.Token.dotdot,
						token.Token.ellipsis])!
					if value == '' {
						return g.unsupported('empty match branch value')
					}
					value_tokens = g.last_expression.clone()
					if g.tok in [.dotdot, .ellipsis] {
						start := value
						start_key := g.normalized_match_case_key(value_tokens, start)
						g.next()
						finish := g.read_expression([token.Token.comma, token.Token.lcbr])!
						finish_tokens := g.last_expression.clone()
						case_key = 'range:${start_key}..${g.normalized_match_case_key(finish_tokens,
							finish)}'
						value = '((${subject_name}) >= (${start}) && (${subject_name}) <= (${finish}))'
						value_is_condition = true
					}
				}
				if case_key == '' {
					case_key = g.normalized_match_case_key(value_tokens, value)
				}
				if case_key in handled_cases {
					return g.unsupported('duplicate match case `${value}`')
				}
				handled_cases[case_key] = true
				values << value
				values_are_conditions << value_is_condition
				if g.tok != .comma {
					break
				}
				g.next()
			}
		}
		g.expect(.lcbr)!
		if is_else {
			g.write_line('else {')
		} else {
			mut conditions := []string{}
			for value_index, value in values {
				if is_boxed || (value_index < values_are_conditions.len
					&& values_are_conditions[value_index]) {
					conditions << '(${value})'
				} else if is_string {
					conditions << 'builtin__string_eq(${subject_name}, ${value})'
				} else {
					conditions << '((${subject_name}) == (${value}))'
				}
			}
			prefix := if branch_index == 0 { 'if' } else { 'else if' }
			g.write_line('${prefix} (${conditions.join(' || ')}) {')
		}
		g.indent++
		mut smartcast_saved := FastcLocal{}
		mut member_smartcast_saved := FastcMemberSmartcast{}
		mut had_member_smartcast := false
		mut member_smartcast_active := false
		common_numeric_type := fastc_match_common_numeric_variant(variant_types)
		smartcast_active := is_boxed && !is_else
			&& (variant_types.len == 1 || common_numeric_type != '') && subject_local != ''
		if smartcast_active {
			variant_cname := if common_numeric_type != '' {
				common_numeric_type
			} else {
				variant_types[0]
			}
			smartcast_value := if common_numeric_type != '' {
				fastc_match_multi_variant_value(subject_name, boxed_access, variant_types,
					common_numeric_type)
			} else {
				'*(${variant_cname} *)${subject_name}${boxed_access}_object'
			}
			g.write_line('${variant_cname} ${fastc_c_identifier(subject_local)} = ${smartcast_value};')
			smartcast_saved = g.locals[subject_local] or { FastcLocal{} }
			g.locals[subject_local] = FastcLocal{
				is_mut: smartcast_saved.is_mut
				typ:    variant_cname
			}
		}
		if is_boxed && !is_else && variant_types.len == 1 && subject_member_path != '' {
			variant_cname := variant_types[0]
			member_smartcast_name := g.temporary_name('smartcast_member')
			g.write_line('${variant_cname} *${member_smartcast_name} = (${variant_cname} *)${subject_name}${boxed_access}_object;')
			member_smartcast_saved = g.member_smartcasts[subject_member_path] or {
				FastcMemberSmartcast{}
			}
			had_member_smartcast = subject_member_path in g.member_smartcasts
			g.member_smartcasts[subject_member_path] = FastcMemberSmartcast{
				typ:    variant_cname + '*'
				source: member_smartcast_name
			}
			member_smartcast_active = true
		}
		terminates := g.parse_block_body()!
		if smartcast_active {
			g.locals[subject_local] = smartcast_saved
		}
		if member_smartcast_active {
			if had_member_smartcast {
				g.member_smartcasts[subject_member_path] = member_smartcast_saved
			} else {
				g.member_smartcasts.delete(subject_member_path)
			}
		}
		if !terminates {
			all_terminate = false
		}
		g.indent--
		g.write_line('}')
		branch_index++
	}
	g.next()
	g.skip_semicolons()
	return has_else && all_terminate
}

// read_match_type_key reads a type-name match branch value (`Dog`, `mod.Type`,
// `int`, or `[]T`) and resolves it to the type-id suffix / smart-cast C spelling
// used for sum-type / interface dispatch.
fn (mut g Parser) read_match_type_key() ?string {
	if g.tok == .lsbr {
		// `[]T` dynamic-array variant. Its boxed id and smart-cast type are the
		// composite spelling `Array_<elem>`; register it so the type gets a typedef
		// and a `__v_typeid_`.
		g.next()
		if g.tok != .rsbr {
			return none
		}
		g.next()
		element_key := g.read_match_type_key() or { return none }
		array_c := fastc_array_c_type(fastc_c_declared_type_name(element_key))
		fastc_register_composite_type(array_c, mut g.composite_types)
		return array_c
	}
	if g.tok != .name {
		return none
	}
	first := g.lit
	g.next()
	if first == 'map' && g.tok == .lsbr {
		// `map[K]V` variant → composite `Map_<K>_<V>`, registered like `[]T`.
		g.next()
		key_key := g.read_match_type_key() or { return none }
		if g.tok != .rsbr {
			return none
		}
		g.next()
		value_key := g.read_match_type_key() or { return none }
		map_c := fastc_map_c_type(fastc_c_declared_type_name(key_key),
			fastc_c_declared_type_name(value_key))
		fastc_register_composite_type(map_c, mut g.composite_types)
		return map_c
	}
	if g.tok == .dot {
		g.next()
		if g.tok != .name {
			return none
		}
		type_name := g.lit
		g.next()
		module_name := g.imports[first] or { first }
		return fastc_type_key(module_name, type_name)
	}
	if key := fastc_resolve_declared_type_key(g.module_name, first, g.imports, g.declared_types) {
		return key
	}
	// A primitive variant (`int`, `bool`, ...) is not a declared type; its own
	// spelling is the type-id suffix and the smart-cast C type.
	if fastc_primitive_c_type(first) != none {
		return first
	}
	return none
}

fn (mut g Parser) parse_return() !bool {
	g.next()
	if g.tok == .name && g.lit == 'sql' && 'sql' !in g.locals {
		mut lookahead := g.s
		if lookahead.scan() == .name {
			return g.parse_orm_sql_select_return()
		}
	}
	if g.tok == .dollar {
		mut lookahead := g.s
		if lookahead.scan() == .name && lookahead.lit == 'veb' {
			return g.parse_veb_html_return()
		}
	}
	if g.tok == .semicolon || g.tok == .rcbr {
		g.consume_statement_end()
		g.write_all_deferred_scopes()
		g.write_line(if g.in_main {
			'return 0;'
		} else if g.return_type == 'Option' {
			'return (Option){0};'
		} else {
			'return;'
		})
		return true
	}
	if g.selfhost && (g.return_type.trim_right('*') == 'MultiReturn'
		|| (g.return_type == 'Option' && g.option_return_type == 'MultiReturn')) {
		mut values := []string{}
		mut value_types := []string{}
		for {
			value :=
				g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			if value == '' {
				return g.unsupported('empty multi-return value')
			}
			values << value
			value_types << fastc_normalize_inferred_type(g.last_expression_type)
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		mut evaluated_values := []string{cap: values.len}
		for value in values {
			if g.deferred_block_starts.len == 0 {
				evaluated_values << value
				continue
			}
			temporary := g.temporary_name('return')
			g.write_line('__typeof__((${value})) ${temporary} = (${value});')
			evaluated_values << temporary
		}
		g.write_all_deferred_scopes()
		if g.return_type == 'Option' && values.len == 1 && value_types[0] == 'Option' {
			g.write_line('return ${evaluated_values[0]};')
			return true
		}
		if g.return_type == 'Option' && values.len == 1
			&& value_types[0].trim_right('*') == 'IError' {
			g.write_line('return (Option){.err=${evaluated_values[0]}, .state=1};')
			return true
		}
		multi_value := if values.len == 1 && value_types[0] == 'MultiReturn' {
			evaluated_values[0]
		} else {
			mut packed_values := []string{cap: values.len}
			for value in evaluated_values {
				packed_values << 'V_FASTC_MULTI_VALUE((${value}))'
			}
			'(MultiReturn){.values={${packed_values.join(', ')}}}'
		}
		if g.return_type == 'Option' {
			g.write_line('return (Option){.data=v_fastc_interface_box(&${multi_value}, sizeof(MultiReturn)), .state=0};')
		} else {
			g.write_line('return ${multi_value};')
		}
		return true
	}
	previous_expected_type := g.expected_expression_type
	if g.selfhost {
		g.expected_expression_type = if g.return_type == 'Option'
			&& g.option_return_type !in ['', 'MultiReturn'] {
			g.option_return_type
		} else {
			g.return_type
		}
	}
	mut expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.expected_expression_type = previous_expected_type
	mut actual_type := g.last_expression_type
	if g.selfhost && g.return_type == '' {
		g.consume_statement_end()
		g.write_return_expression(expression)
		return true
	}
	contextual_return_type := if g.return_type == 'Option' && g.option_return_type != '' {
		g.option_return_type
	} else {
		g.return_type
	}
	if g.selfhost && actual_type == '' && g.last_expression.len == 2
		&& g.last_expression[0].tok == .dot && g.last_expression[1].tok == .name
		&& g.declared_kinds[g.semantic_type_key(contextual_return_type)] == .enum_ {
		expression = '${contextual_return_type.trim_right('*')}__${g.last_expression[1].lit}'
		actual_type = contextual_return_type
	}
	if g.selfhost && g.return_type !in ['Option', 'MultiReturn']
		&& g.declared_kinds[g.semantic_type_key(g.return_type)] != .interface_
		&& !fastc_types_share_lowering_representation(actual_type, g.return_type)
		&& !g.selfhost_types_share_lowering_representation(actual_type, g.return_type) {
		actual_type = g.return_type
	}
	if g.selfhost && g.declared_kinds[g.semantic_type_key(g.return_type)] == .interface_
		&& g.declared_kinds[g.semantic_type_key(actual_type)] != .interface_ {
		expression = g.interface_value_expression(g.return_type, actual_type, expression)
		actual_type = g.return_type
	}
	if g.selfhost && g.return_type == 'Option' && actual_type.trim_right('*') == 'IError' {
		expression = '(Option){.err=${expression}, .state=1}'
		actual_type = 'Option'
	} else if g.selfhost && g.return_type == 'Option' && actual_type != 'Option' {
		actual_base := fastc_normalize_inferred_type(actual_type)
		expression = '(Option){.data=${fastc_box_expression(actual_base, expression)}, .state=0}'
		actual_type = 'Option'
	}
	g.consume_statement_end()
	g.write_return_expression(expression)
	return true
}

fn (mut g Parser) write_return_expression(expression string) {
	if g.deferred_block_starts.len == 0 {
		g.write_line('return ${expression};')
		return
	}
	temporary := g.temporary_name('return')
	g.write_line('__typeof__((${expression})) ${temporary} = (${expression});')
	g.write_all_deferred_scopes()
	g.write_line('return ${temporary};')
}

fn (g &Parser) interface_value_expression(interface_type string, actual_type string, expression string) string {
	// Normalize so a boxed primitive literal (`Any(42)`) gets a concrete C type and
	// a matching `__v_typeid_int` rather than the pseudo type `integer literal`.
	// This is a no-op for declared types.
	normalized := g.underlying_alias_type(fastc_normalize_inferred_type(actual_type))
	actual_base := normalized.trim_right('*')
	actual_key := g.semantic_type_key(normalized)
	object := if fastc_is_pointer_type(normalized) {
		'(void*)(${expression})'
	} else {
		fastc_box_expression(actual_base, expression)
	}
	return '(${interface_type}){._object=${object}, ._typ=__v_typeid_${fastc_c_declared_type_name(actual_key)}, ._methods=NULL}'
}

fn (mut g Parser) parse_mutable_declaration() ! {
	g.next()
	if g.tok != .name && !g.tok.is_keyword() {
		return g.unsupported('mutable declaration')
	}
	name := g.lit
	g.next()
	if g.selfhost && g.tok == .comma {
		g.parse_parallel_assignment([name], true, true)!
		return
	}
	if g.tok != .decl_assign {
		return g.unsupported('`mut` statement without `:=`')
	}
	g.parse_declaration_after_name(name, true)!
}

fn (mut g Parser) parse_simple_statement() ! {
	if g.tok == .key_assert {
		return g.parse_assert_statement()
	}
	if g.tok == .name || (g.tok == .key_shared && g.shared_token_is_identifier(.unknown)) {
		name := g.lit
		global_key := fastc_global_key(g.module_name, name)
		is_global := global_key in g.globals
		statement_local := g.locals[name] or { FastcLocal{} }
		is_known_local := name in g.locals
		c_target := if is_global {
			g.globals[global_key]
		} else if local := g.locals[name] {
			if local.is_reference {
				'(*${fastc_c_identifier(name)})'
			} else {
				fastc_c_identifier(name)
			}
		} else {
			fastc_c_identifier(name)
		}
		g.next()
		if name == 'panic' && g.tok == .lpar {
			g.next()
			previous_expected_type := g.expected_expression_type
			g.expected_expression_type = 'string'
			argument := g.read_expression([token.Token.rpar])!
			g.expected_expression_type = previous_expected_type
			if argument == '' {
				return g.unsupported('empty panic argument')
			}
			panic_argument := g.render_call_argument_expression(g.last_expression, 'string') or {
				argument
			}
			g.expect(.rpar)!
			g.consume_statement_end()
			g.write_line('builtin__panic(${panic_argument});')
			return
		}
		if name == 'sql' && !is_known_local && !is_global && g.tok == .name {
			// `sql db { ... }` ORM statement: lower it to metadata construction plus
			// the connection method call.
			return g.parse_orm_sql_statement()
		}
		if g.selfhost && g.tok == .colon {
			g.next()
			g.skip_semicolons()
			g.write_line('${fastc_c_identifier(name)}:')
			return
		}
		if g.selfhost && g.tok == .comma {
			g.parse_parallel_assignment([name], false, false)!
			return
		}
		if g.tok == .decl_assign {
			g.parse_declaration_after_name(name, false)!
			return
		}
		if g.selfhost && g.tok == .left_shift {
			local := g.locals[name] or { return g.unsupported('append to unknown name `${name}`') }
			if !local.is_mut {
				return g.unsupported('append to immutable name `${name}`')
			}
			_ := g.array_element_type(local.typ) or {
				return g.unsupported('append to non-array `${name}` of type `${local.typ}`')
			}
			g.next()
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			value_type := fastc_normalize_inferred_type(g.last_expression_type)
			is_array_append := value_type == local.typ
			g.consume_statement_end()
			c_name := fastc_c_identifier(name)
			array_target := if local.typ.ends_with('*') {
				'(array *)${c_name}'
			} else {
				'(array *)&${c_name}'
			}
			value_name := g.temporary_name('push_value')
			g.write_line('__typeof__((${value})) ${value_name} = (${value});')
			if is_array_append {
				g.write_line('builtin__array_push_many(${array_target}, ${value_name}.data, ${value_name}.len);')
			} else {
				g.write_line('builtin__array_push(${array_target}, &${value_name});')
			}
			return
		}
		if !g.selfhost && (g.tok.is_assignment() || g.tok in [.inc, .dec]) && !is_global
			&& (!is_known_local || !statement_local.is_mut) {
			return g.unsupported('mutation of immutable or unknown name `${name}`')
		}
		g.validate_expression_name(name, .unknown)!
		if g.tok.is_assignment() {
			if !g.selfhost
				&& g.tok in [.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
				return g.unsupported('shift expressions')
			}
			if !g.selfhost && g.tok in [.div_assign, .mod_assign] {
				return g.unsupported('division or modulo expressions')
			}
			operator := g.tok
			expected_type := if is_global {
				g.global_types[global_key]
			} else if local := g.locals[name] {
				if local.is_reference { local.typ.trim_right('*') } else { local.typ }
			} else {
				''
			}
			g.next()
			if operator == .assign && g.tok == .name && g.lit == 'sql' && 'sql' !in g.locals {
				mut sql_lookahead := g.s
				if sql_lookahead.scan() == .name {
					// `x = sql db { select ... }` (assignment, not `:=`): lower like the
					// declaration form but assign into the existing target.
					g.parse_orm_sql_select_assignment(name)!
					return
				}
			}
			previous_expected_type := g.expected_expression_type
			if g.selfhost && expected_type != '' {
				g.expected_expression_type = expected_type
			}
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			g.expected_expression_type = previous_expected_type
			if value.len == 0 {
				return g.unsupported('empty assignment to `${name}`')
			}
			if g.selfhost && name == '_' && operator == .assign {
				g.consume_statement_end()
				g.write_line('(void)(${value});')
				return
			}
			mut actual_type := g.last_expression_type
			if g.selfhost && actual_type == '' {
				actual_type = expected_type
			}
			mut resolved_expected_type := if g.selfhost && expected_type == '' {
				actual_type
			} else {
				expected_type
			}
			if g.selfhost && resolved_expected_type == 'int'
				&& !fastc_is_numeric_expression_type(actual_type) && name in g.locals {
				resolved_expected_type = actual_type
				g.locals[name] = FastcLocal{
					is_mut:       statement_local.is_mut
					is_reference: statement_local.is_reference
					typ:          actual_type
				}
			}
			expected_layout_type := g.underlying_alias_type(resolved_expected_type)
			actual_layout_type := g.underlying_alias_type(actual_type)
			if g.in_generic_placeholder && expected_layout_type == 'voidptr' && operator != .assign {
				// Arithmetic on an erased type parameter is not valid C (`void * +=
				// void *`). The concrete on-demand instance is emitted separately;
				// make the placeholder parser fall back to its inert stub.
				return g.unsupported('compound assignment on an erased generic type')
			}
			if operator == .plus_assign && expected_layout_type == 'string'
				&& actual_layout_type == 'string' {
				g.consume_statement_end()
				concatenation := if g.selfhost {
					'builtin__string_plus(${c_target},${value})'
				} else {
					'builtin__string_plus_many(2, (string[]){${c_target},${value}})'
				}
				g.write_line('${c_target}=${concatenation};')
				return
			}
			if overloaded := g.render_overloaded_assignment(c_target, value,
				resolved_expected_type, operator)
			{
				g.consume_statement_end()
				g.write_line('${overloaded};')
				return
			}
			g.consume_statement_end()
			if operator == .right_shift_unsigned_assign {
				shift := g.render_unsigned_right_shift_assignment(c_target, value,
					resolved_expected_type) or {
					return g.unsupported('unsigned right shift assignment on type `${resolved_expected_type}`')
				}
				g.write_line('${shift};')
				return
			}
			mut assigned_value := value
			if g.selfhost && operator == .assign && resolved_expected_type == 'Option'
				&& actual_type != 'Option' {
				if actual_type.trim_right('*') == 'IError' {
					assigned_value = '(Option){.err=${value}, .state=1}'
				} else {
					payload_type := if statement_local.option_value_type != '' {
						statement_local.option_value_type
					} else {
						fastc_normalize_inferred_type(actual_type)
					}
					payload_value := g.render_call_argument_expression(g.last_expression,
						payload_type) or { value }
					assigned_value = fastc_option_success_expression(payload_type, payload_value)
				}
			} else if g.selfhost && operator == .assign
				&& g.should_box_variant(resolved_expected_type, actual_type) {
				// A concrete struct assigned to an interface variable is boxed with its
				// type id so the dispatch functions can recover the receiver.
				assigned_value = g.interface_value_expression(resolved_expected_type, actual_type,
					value)
			} else if g.selfhost && operator == .assign && resolved_expected_type == 'voidptr'
				&& actual_type !in ['', 'voidptr', 'nil']
				&& !fastc_expression_is_zero(g.last_expression)
				&& !fastc_is_pointer_type(actual_type) {
				// An unmonomorphized imported generic stores `T` as `voidptr`. Preserve a
				// concrete value assigned through that slot by copying it into boxed storage.
				box_value := g.temporary_name('generic_box')
				assigned_value = '({ ${actual_type} ${box_value} = (${value}); v_fastc_interface_box(&${box_value}, sizeof(${actual_type})); })'
			} else if g.selfhost && operator == .assign && actual_type == 'voidptr'
				&& (resolved_expected_type.trim_right('*') in g.struct_fields
				|| resolved_expected_type.trim_right('*').starts_with('Array_')
				|| resolved_expected_type.trim_right('*').starts_with('Map_')
				|| resolved_expected_type.trim_right('*').starts_with('FixedArray_')) {
				assigned_value = '*((${resolved_expected_type} *)(${value}))'
			}
			g.write_line('${c_target}${operator.str()}${assigned_value};')
			return
		}
		expression := g.read_statement_expression_with_prefix(name, [token.Token.comma,
			token.Token.semicolon, token.Token.rcbr, token.Token.arrow])!
		if g.selfhost && g.tok == .arrow {
			// Channel send `<chan> <- <value>`: push the value through the channel.
			// Channels are the type-erased `void*` stub, so this compiles to a
			// `try_push` of the value's address (non-blocking, matching the builtin).
			g.next()
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			value_type := fastc_normalize_inferred_type(g.last_expression_type)
			g.consume_statement_end()
			element_type := if value_type == '' { 'int' } else { value_type }
			send_tmp := g.temporary_name('chan_send')
			g.write_line('${element_type} ${send_tmp} = (${value});')
			g.write_line('builtin__chan_try_push((chan)(${expression}), &${send_tmp});')
			return
		}
		if g.selfhost && g.tok == .comma {
			g.parse_parallel_expression_assignment(expression, g.last_expression.clone(),
				g.last_expression_type)!
			return
		}
		if !g.last_expression_is_statement() {
			if g.or_value_capture {
				g.capture_or_value(expression)
				g.consume_statement_end()
				return
			}
			return g.unsupported('value-only expression statement')
		}
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	expression := g.read_statement_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('statement `${g.token_source()}`')
	}
	if g.last_expression.len == 0 && g.last_expression_type.starts_with(fastc_thread_type_prefix) {
		// Fire-and-forget `spawn f()` as a statement: the handle is discarded, so
		// detach the thread. It then releases its own resources on completion (the
		// run wrapper frees the packed arguments), rather than leaking a joinable.
		g.consume_statement_end()
		handle := g.temporary_name('thread')
		g.write_line('{ ${g.last_expression_type} ${handle} = ${expression}; pthread_detach(${handle}.handle); }')
		return
	}
	if g.selfhost && g.last_expression_is_statement() {
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	if g.or_value_capture {
		g.capture_or_value(expression)
		g.consume_statement_end()
		return
	}
	return g.unsupported('value-only expression statement')
}

fn (mut g Parser) parse_assert_statement() ! {
	g.next()
	condition := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
	if condition == '' {
		return g.unsupported('empty assert condition')
	}
	mut message := if g.selfhost { '_S("assertion failed")' } else { '"assertion failed"' }
	if g.tok == .comma {
		g.next()
		message = g.read_expression([token.Token.semicolon, token.Token.rcbr])!
		if message == '' {
			return g.unsupported('empty assert message')
		}
	}
	g.consume_statement_end()
	if g.selfhost {
		g.write_line('if (!(${condition})) { builtin__panic(${message}); }')
	} else {
		g.write_line('if (!(${condition})) { fputs(${message}, stderr); abort(); }')
	}
}

// capture_or_value records a trailing bare value expression as an `or`-block's fallback
// value (see the `or_value_capture` path in expr.v). It re-renders through the argument
// pipeline when the block's expected value type is known, so enum-shorthand/boxing
// fallbacks (`or { .none }`, a variant literal, …) still type correctly.
fn (mut g Parser) capture_or_value(expression string) {
	mut value := expression
	if g.or_value_expected_type != '' && g.last_expression.len > 0 {
		if contextual := g.render_call_argument_expression(g.last_expression,
			g.or_value_expected_type)
		{
			value = contextual
		}
	}
	g.or_value_captured = value
}

fn (mut g Parser) parse_parallel_assignment(initial_names []string, initial_mut bool, force_declaration bool) ! {
	mut names := initial_names.clone()
	mut mutability := []bool{len: initial_names.len, init: initial_mut}
	for g.tok == .comma {
		g.next()
		mut is_mut := false
		if g.tok == .key_mut {
			is_mut = true
			g.next()
		}
		if g.tok != .name && !(g.tok == .key_shared && g.shared_token_is_identifier(.unknown)) {
			return g.unsupported('parallel assignment target')
		}
		names << g.lit
		mutability << is_mut
		g.next()
	}
	is_declaration := force_declaration || g.tok == .decl_assign
	if g.tok !in [.decl_assign, .assign] {
		return g.unsupported('parallel assignment operator `${g.token_source()}`')
	}
	g.next()
	// `a, b := opt_tuple() or { x, y }`: an option whose value is a multi-return tuple,
	// with a TUPLE `or` fallback. The or-block's comma would otherwise trip the value
	// reader as a parallel assignment, so lower it here.
	if g.selfhost && names.len > 1 && g.parallel_rhs_is_option_tuple_or() {
		return g.parse_parallel_option_tuple(names, mutability, is_declaration)
	}
	g.last_multi_return_types = []string{}
	mut values := []string{}
	mut value_types := []string{}
	for {
		item := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		if item == '' {
			return g.unsupported('empty parallel assignment')
		}
		values << item
		value_types << g.last_expression_type
		if g.tok != .comma {
			break
		}
		g.next()
	}
	value := values[0]
	if value == '' {
		return g.unsupported('empty parallel assignment')
	}
	g.consume_statement_end()
	if values.len > 1 {
		if values.len != names.len {
			return g.unsupported('parallel assignment with ${names.len} targets and ${values.len} values')
		}
		assignment_targets := if is_declaration {
			[]FastcRenderedExpression{}
		} else {
			g.validate_parallel_assignment_targets(names)!
		}
		mut temporaries := []string{cap: values.len}
		for item in values {
			temporary := g.temporary_name('parallel')
			g.write_line('__typeof__((${item})) ${temporary} = (${item});')
			temporaries << temporary
		}
		if is_declaration {
			for i, name in names {
				if name == '_' {
					continue
				}
				value_type := if value_types[i] == '' {
					'int'
				} else {
					fastc_normalize_inferred_type(value_types[i])
				}
				g.write_line('${value_type} ${fastc_c_identifier(name)} = ${temporaries[i]};')
				g.locals[name] = FastcLocal{
					is_mut: mutability[i]
					typ:    value_type
				}
			}
		} else {
			for i, name in names {
				if name == '_' {
					continue
				}
				g.write_line('${assignment_targets[i].source} = ${temporaries[i]};')
			}
		}
		return
	}
	mut component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len == 0 {
		component_types = g.last_multi_return_types.clone()
	}
	assignment_targets := if is_declaration {
		[]FastcRenderedExpression{}
	} else {
		g.validate_parallel_assignment_targets(names)!
	}
	temporary := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${temporary} = (${value});')
	for i, name in names {
		if name == '_' {
			continue
		}
		if is_declaration {
			component_type := if i < component_types.len { component_types[i] } else { 'usize' }
			c_name := fastc_c_identifier(name)
			g.write_line('${component_type} ${c_name} = (${component_type}){0};')
			g.write_line('memcpy(&${c_name}, ${temporary}.values[${i}].data, sizeof(${c_name}));')
			g.locals[name] = FastcLocal{
				is_mut: mutability[i]
				typ:    component_type
			}
		} else {
			c_name := assignment_targets[i].source
			g.write_line('memcpy(&${c_name}, ${temporary}.values[${i}].data, sizeof(${c_name}));')
		}
	}
}

// parallel_rhs_is_option_tuple_or reports whether the RHS of a parallel `:=` (the
// scanner is positioned at its first token) has the shape `<expr> or { … , … }` — an
// option unwrapped with a comma-separated TUPLE fallback. Bounded to the current
// statement (stops at a top-level comma, a newline, or the statement end).
fn (g &Parser) parallel_rhs_is_option_tuple_or() bool {
	mut probe := g.s
	mut tok := g.tok
	mut prev_end := g.s.pos
	mut depth := 0
	for {
		match tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				if depth == 0 {
					return false
				}
				depth--
			}
			.comma {
				if depth == 0 {
					// A top-level comma before any `or` means a plain multi-value RHS.
					return false
				}
			}
			.key_or {
				if depth == 0 {
					break
				}
			}
			.semicolon, .eof {
				return false
			}
			else {}
		}
		prev_end = probe.pos
		tok = probe.scan()
		if depth == 0 && probe.src[prev_end..probe.pos].contains('\n') {
			return false
		}
	}
	if probe.scan() != .lcbr {
		return false
	}
	mut block_depth := 1
	for block_depth > 0 {
		t := probe.scan()
		if t == .eof {
			return false
		}
		match t {
			.lpar, .lsbr, .lcbr {
				block_depth++
			}
			.rpar, .rsbr, .rcbr {
				block_depth--
			}
			.comma {
				if block_depth == 1 {
					return true
				}
			}
			else {}
		}
	}
	return false
}

// parse_parallel_option_tuple lowers `a, b := opt_tuple() or { x, y }`: declare each
// target, then on the option's failure state assign the fallback tuple, else unbox the
// `MultiReturn` component into each target.
fn (mut g Parser) parse_parallel_option_tuple(names []string, mutability []bool, is_declaration bool) ! {
	option_expr := g.read_expression([token.Token.key_or, token.Token.semicolon, token.Token.rcbr])!
	component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len < names.len {
		return g.unsupported('parallel option tuple with ${names.len} targets and ${component_types.len} components')
	}
	g.expect(.key_or)!
	g.expect(.lcbr)!
	previous_err := g.locals['err'] or { FastcLocal{} }
	had_err := 'err' in g.locals
	g.locals['err'] = FastcLocal{
		typ: 'IError'
	}
	mut fallbacks := []string{}
	for g.tok != .rcbr && g.tok != .eof {
		g.skip_semicolons()
		if g.tok == .rcbr {
			break
		}
		fallback := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		fallbacks << fallback
		if g.tok in [.comma, .semicolon] {
			g.next()
		}
	}
	if had_err {
		g.locals['err'] = previous_err
	} else {
		g.locals.delete('err')
	}
	g.expect(.rcbr)!
	g.consume_statement_end()
	if fallbacks.len != names.len {
		return g.unsupported('parallel option tuple fallback with ${fallbacks.len} values for ${names.len} targets')
	}
	mut assignment_targets := []FastcRenderedExpression{}
	if !is_declaration {
		assignment_targets = g.validate_parallel_assignment_targets(names)!
	}
	guard := g.temporary_name('opt_guard')
	multi_return := g.temporary_name('multi_return')
	if is_declaration {
		for i, name in names {
			if name == '_' {
				continue
			}
			component_type := fastc_normalize_inferred_type(component_types[i])
			g.write_line('${component_type} ${fastc_c_identifier(name)} = (${component_type}){0};')
			g.locals[name] = FastcLocal{
				is_mut: mutability[i]
				typ:    component_type
			}
		}
	}
	g.write_line('Option ${guard} = (${option_expr});')
	g.write_line('if (${guard}.state) {')
	g.indent++
	g.write_line('IError err = ${guard}.err;')
	for i, name in names {
		if name == '_' {
			continue
		}
		target := if is_declaration {
			fastc_c_identifier(name)
		} else {
			assignment_targets[i].source
		}
		g.write_line('${target} = (${fallbacks[i]});')
	}
	g.indent--
	g.write_line('} else {')
	g.indent++
	g.write_line('MultiReturn ${multi_return} = *((MultiReturn *)${guard}.data);')
	for i, name in names {
		if name == '_' {
			continue
		}
		target := if is_declaration {
			fastc_c_identifier(name)
		} else {
			assignment_targets[i].source
		}
		g.write_line('memcpy(&${target}, ${multi_return}.values[${i}].data, sizeof(${target}));')
	}
	g.indent--
	g.write_line('}')
}

fn (g &Parser) validate_parallel_assignment_targets(names []string) ![]FastcRenderedExpression {
	mut targets := []FastcRenderedExpression{cap: names.len}
	for name in names {
		if name == '_' {
			targets << FastcRenderedExpression{}
			continue
		}
		mut target := FastcRenderedExpression{}
		if local := g.locals[name] {
			if !local.is_mut {
				return g.unsupported('parallel assignment to immutable name `${name}`')
			}
			target = FastcRenderedExpression{
				source: if local.is_reference {
					'(*${fastc_c_identifier(name)})'
				} else {
					fastc_c_identifier(name)
				}
				typ:    if local.is_reference { local.typ.trim_right('*') } else { local.typ }
			}
		} else {
			global_key := fastc_global_key(g.module_name, name)
			global_name := g.globals[global_key] or {
				return g.unsupported('parallel assignment to unknown name `${name}`')
			}
			target = FastcRenderedExpression{
				source: global_name
				typ:    g.global_types[global_key]
			}
		}
		targets << target
	}
	return targets
}

fn (mut g Parser) parse_parallel_expression_assignment(first_source string, first_tokens []FastcExpressionToken, first_type string) ! {
	mut targets := []FastcRenderedExpression{}
	targets << g.validate_parallel_expression_assignment_target(first_source, first_tokens,
		first_type)!
	for g.tok == .comma {
		g.next()
		target_source := g.read_expression([token.Token.comma, token.Token.assign])!
		if target_source == '' {
			return g.unsupported('empty parallel assignment target')
		}
		targets << g.validate_parallel_expression_assignment_target(target_source,
			g.last_expression.clone(), g.last_expression_type)!
	}
	if g.tok != .assign {
		return g.unsupported('parallel assignment operator `${g.token_source()}`')
	}
	g.next()
	g.last_multi_return_types = []string{}
	mut values := []string{}
	for {
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		if value == '' {
			return g.unsupported('empty parallel assignment')
		}
		values << value
		if g.tok != .comma {
			break
		}
		g.next()
	}
	g.consume_statement_end()
	if values.len > 1 {
		if values.len != targets.len {
			return g.unsupported('parallel assignment with ${targets.len} targets and ${values.len} values')
		}
		mut temporaries := []string{cap: values.len}
		for value in values {
			temporary := g.temporary_name('parallel')
			g.write_line('__typeof__((${value})) ${temporary} = (${value});')
			temporaries << temporary
		}
		for i, target in targets {
			if target.source != '' {
				g.write_line('${target.source} = ${temporaries[i]};')
			}
		}
		return
	}
	temporary := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${temporary} = (${values[0]});')
	for i, target in targets {
		if target.source == '' {
			continue
		}
		g.write_line('memcpy(&${target.source}, ${temporary}.values[${i}].data, sizeof(${target.source}));')
	}
}

fn (g &Parser) validate_parallel_expression_assignment_target(source string, tokens []FastcExpressionToken, typ string) !FastcRenderedExpression {
	if tokens.len == 1 && tokens[0].tok == .name {
		mut names := []string{}
		names << tokens[0].lit
		targets := g.validate_parallel_assignment_targets(names)!
		return targets[0]
	}
	mut mutation_tokens := tokens.clone()
	mutation_tokens << FastcExpressionToken{
		tok: .assign
		lit: '='
	}
	g.validate_expression_mutation_lvalue(mutation_tokens)!
	return FastcRenderedExpression{
		source: source
		typ:    typ
	}
}

fn (g &Parser) multi_return_types_for_expression(tokens []FastcExpressionToken) []string {
	mut expression_end := tokens.len
	for expression_end > 0 && tokens[expression_end - 1].tok == .semicolon {
		expression_end--
	}
	if expression_end > 0 && tokens[expression_end - 1].tok == .not {
		expression_end--
	}
	expression_tokens := tokens[..expression_end]
	if expression_tokens.len < 3 {
		return []string{}
	}
	if expression_tokens.len >= 5 && expression_tokens.last().tok == .rpar {
		for i := expression_tokens.len - 2; i >= 2; i-- {
			if expression_tokens[i].tok != .name || expression_tokens[i - 1].tok != .dot
				|| expression_tokens[i + 1].tok != .lpar {
				continue
			}
			method_close := fastc_matching_rpar(expression_tokens, i + 1) or { continue }
			if method_close != expression_tokens.len - 1 {
				continue
			}
			if static_key := g.static_function_key_for_call(expression_tokens, i) {
				if signature := g.functions[static_key] {
					return signature.return_types.clone()
				}
			}
			receiver_start := fastc_method_receiver_start(expression_tokens, i - 1)
			receiver_type := g.infer_expression_type(expression_tokens[receiver_start..i - 1]) or {
				''
			}
			if receiver_type == '' {
				break
			}
			method_key, _ := g.resolve_method(receiver_type, expression_tokens[i].lit)
			if signature := g.functions[method_key] {
				return signature.return_types.clone()
			}
			break
		}
	}
	mut name_index := 0
	mut open_index := 1
	if expression_tokens.len >= 4 && expression_tokens[0].tok == .name
		&& expression_tokens[1].tok == .dot && expression_tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if expression_tokens[name_index].tok !in [.name, .key_select]
		|| expression_tokens[open_index].tok != .lpar {
		return []string{}
	}
	close := fastc_matching_rpar(expression_tokens, open_index) or { return []string{} }
	if close != expression_tokens.len - 1 {
		return []string{}
	}
	function_key := if name_index == 2 && expression_tokens[0].lit !in g.imports
		&& expression_tokens[0].lit != 'C' {
		receiver_type := g.infer_expression_type(expression_tokens[..1]) or { return []string{} }
		g.method_function_key(receiver_type, expression_tokens[name_index].lit)
	} else {
		g.function_key_for_call(expression_tokens, name_index)
	}
	signature := g.functions[function_key] or { return []string{} }
	return signature.return_types.clone()
}

fn (g &Parser) option_value_type_for_expression(tokens []FastcExpressionToken) string {
	if map_lookup := g.render_map_lookup_option_expression(tokens) {
		return map_lookup.typ
	}
	if array_lookup := g.render_array_lookup_option_expression(tokens) {
		return array_lookup.typ
	}
	if generic_type := g.erased_generic_option_value_type_for_expression(tokens) {
		return generic_type
	}
	// An explicit option cast (`?T(value)` / `?T(none)`) carries its payload type in
	// the cast even though every option shares the erased C type `Option`.
	if tokens.len >= 4 && tokens[0].tok == .question && tokens.last().tok == .rpar {
		for open in 2 .. tokens.len - 1 {
			if tokens[open].tok != .lpar {
				continue
			}
			close := fastc_matching_rpar(tokens, open) or { continue }
			if close == tokens.len - 1 {
				return g.type_from_expression_tokens(tokens[1..open]) or { '' }
			}
			break
		}
	}
	for open in 1 .. tokens.len - 1 {
		if tokens[open].tok != .lsbr || tokens[open - 1].tok != .name {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close + 1 >= tokens.len || tokens[close + 1].tok != .lpar {
			continue
		}
		mut normalized := tokens[..open].clone()
		normalized << tokens[close + 1..]
		return g.option_value_type_for_expression(normalized)
	}
	// A bare option variable (`if x := opt`): recover the wrapped value type recorded
	// when the variable was declared, since its C type is the type-erased `Option`.
	if tokens.len == 1 && tokens[0].tok == .name {
		local := g.locals[tokens[0].lit] or { return '' }
		return local.option_value_type
	}
	// A member access to an option FIELD (`if mut v := x.f {` where `f ?T`): the field's
	// erased C type is `Option`, so recover the wrapped value type recorded on the field.
	if tokens.len >= 3 && tokens.last().tok == .name && tokens[tokens.len - 2].tok == .dot {
		receiver_type := g.infer_expression_type(tokens[..tokens.len - 2]) or { '' }
		if receiver_type != '' {
			receiver_layout := fastc_normalize_inferred_type(receiver_type).trim_right('*')
			field_name := tokens.last().lit
			if fields := g.struct_field_info[receiver_layout] {
				for field in fields {
					if field.name == field_name && field.option_value_type != '' {
						return field.option_value_type
					}
				}
			}
		}
	}
	// A method call whose receiver spans several tokens (`qb.conn.select(...)`):
	// find the trailing `.method(args)` and resolve the receiver's type so the
	// method's option payload type is known. The single-token and module-qualified
	// forms below cannot see past the first receiver token.
	if tokens.len >= 5 && tokens.last().tok == .rpar {
		for i := tokens.len - 2; i >= 2; i-- {
			if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
				continue
			}
			method_close := fastc_matching_rpar(tokens, i + 1) or { continue }
			if method_close != tokens.len - 1 {
				continue
			}
			receiver_start := fastc_method_receiver_start(tokens, i - 1)
			receiver_type := g.infer_expression_type(tokens[receiver_start..i - 1]) or { '' }
			if receiver_type == '' {
				break
			}
			if field := g.struct_field_metadata(receiver_type, tokens[i].lit) {
				if field.is_function && field.fn_option_value_type != '' {
					return field.fn_option_value_type
				}
			}
			method_key, _ := g.resolve_method(receiver_type, tokens[i].lit)
			if method_key in g.functions {
				method_signature := g.functions[method_key]
				return method_signature.option_type
			}
			break
		}
	}
	if tokens.len < 3 {
		return ''
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok !in [.name, .key_select] || tokens[open_index].tok != .lpar {
		return ''
	}
	close := fastc_matching_rpar(tokens, open_index) or { return '' }
	if close != tokens.len - 1 {
		return ''
	}
	if name_index == 0 {
		// Calling an option-returning function-pointer parameter (`f(x) or {…}`).
		if local := g.locals[tokens[0].lit] {
			if local.fn_option_value_type != '' {
				return local.fn_option_value_type
			}
		}
	}
	if static_key := g.static_function_key_for_call(tokens, name_index) {
		static_signature := g.functions[static_key]
		return static_signature.option_type
	}
	function_key := if name_index == 2 && tokens[0].lit !in g.imports && tokens[0].lit != 'C' {
		receiver_type := g.infer_expression_type(tokens[..1]) or { return '' }
		method_key, _ := g.resolve_method(receiver_type, tokens[name_index].lit)
		method_key
	} else {
		g.function_key_for_call(tokens, name_index)
	}
	signature := if function_key in g.functions {
		g.functions[function_key]
	} else {
		g.mono_functions[function_key] or { return '' }
	}
	return signature.option_type
}

fn (g &Parser) erased_generic_option_value_type_for_expression(tokens []FastcExpressionToken) ?string {
	if tokens.len < 5 || tokens.last().tok != .rpar {
		return none
	}
	for i := tokens.len - 2; i >= 2; i-- {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if call_end != tokens.len - 1 {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		if receiver_tokens.len < 3 || receiver_tokens.last().tok != .name
			|| receiver_tokens[receiver_tokens.len - 2].tok != .dot {
			return none
		}
		owner_type := g.infer_expression_type(receiver_tokens[..receiver_tokens.len - 2]) or {
			return none
		}
		field := g.struct_field_metadata(owner_type, receiver_tokens.last().lit) or { return none }
		if field.generic_argument_type == '' {
			return none
		}
		receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
		method_key, _ := g.resolve_method(receiver_type, tokens[i].lit)
		signature := g.functions[method_key] or { return none }
		if signature.return_type == 'Option' && signature.option_type == 'voidptr' {
			return field.generic_argument_type
		}
		return none
	}
	return none
}

fn (g &Parser) last_expression_is_statement() bool {
	return g.expression_tokens_are_statement(g.last_expression)
}

fn (g &Parser) expression_tokens_are_statement(expression_tokens []FastcExpressionToken) bool {
	tokens := if g.selfhost && expression_tokens.len > 0 && expression_tokens.last().tok == .not {
		expression_tokens[..expression_tokens.len - 1]
	} else {
		expression_tokens
	}
	for open in 1 .. tokens.len - 1 {
		if tokens[open].tok != .lsbr || tokens[open - 1].tok != .name {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close + 1 >= tokens.len || tokens[close + 1].tok != .lpar {
			continue
		}
		mut normalized := tokens[..open].clone()
		normalized << tokens[close + 1..]
		return g.expression_tokens_are_statement(normalized)
	}
	if g.selfhost {
		for item in tokens {
			if item.is_statement {
				return true
			}
		}
	}
	if g.selfhost && fastc_expression_tokens_contain_assignment_or_mutation(tokens) {
		return true
	}
	if g.selfhost && fastc_expression_tokens_contain(tokens, .left_shift) {
		return true
	}
	if g.selfhost && fastc_expression_tokens_contain_statement_method(tokens) {
		return true
	}
	if tokens.len >= 4 {
		for i in 2 .. tokens.len - 1 {
			if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
				continue
			}
			call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			if call_end != tokens.len - 1 {
				continue
			}
			receiver_start := fastc_method_receiver_start(tokens, i - 1)
			receiver_type := g.infer_expression_type(tokens[receiver_start..i - 1]) or { continue }
			if tokens[i].lit == 'wait' && receiver_type.starts_with(fastc_thread_type_prefix) {
				// Generated thread waiters are absent from g.functions, but joining
				// a void-returning spawn is still a valid standalone statement.
				return true
			}
			resolved_method_key, _ := g.resolve_method(receiver_type, tokens[i].lit)
			if resolved_method_key in g.functions || resolved_method_key in g.mono_functions
				|| g.struct_member_type(receiver_type, tokens[i].lit) != '' {
				return true
			}
		}
	}
	if tokens.len == 2 && tokens[0].tok == .name && tokens[1].tok in [.inc, .dec] {
		return true
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name
		&& (tokens[0].lit in g.imports || (tokens[0].lit == 'C' && g.has_declared_c_function())) {
		name_index = 2
		open_index = 3
	}
	if tokens.len <= open_index + 1 || tokens[name_index].tok !in [.name, .key_select]
		|| tokens[open_index].tok != .lpar {
		return false
	}
	call_close := fastc_matching_rpar(tokens, open_index) or { return false }
	if call_close != tokens.len - 1 {
		return false
	}
	name := tokens[name_index].lit
	function_key := g.function_key_for_call(tokens, name_index)
	if name_index == 0 {
		if local := g.locals[name] {
			if local.fn_return_type != '' {
				return true
			}
		}
	}
	return function_key in g.functions || (name_index == 0 && name in ['print', 'println'])
}

fn (mut g Parser) parse_declaration_after_name(name string, is_mut bool) ! {
	if !g.selfhost && name in g.locals {
		return g.unsupported('redeclaration of `${name}`')
	}
	g.next()
	if g.tok == .name && g.lit == 'sql' && 'sql' !in g.locals {
		mut lookahead := g.s
		if lookahead.scan() == .name {
			return g.parse_orm_sql_select_declaration(name, is_mut)
		}
	}
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('empty declaration')
	}
	option_value_type := if g.selfhost {
		if g.last_expression.len == 0 {
			g.last_option_value_type
		} else {
			g.option_value_type_for_expression(g.last_expression)
		}
	} else {
		''
	}
	g.consume_statement_end()
	// GNU typeof is unevaluated and is supported by bundled TinyCC. It lets the
	// direct path preserve V's `:=` without running any inference or type checker.
	c_name := fastc_c_identifier(name)
	if expression.starts_with('"') {
		// C's typeof preserves a literal's array type instead of applying the usual
		// pointer decay. The spelling alone is enough to lower this case.
		g.write_line('string ${c_name} = (${expression});')
	} else {
		g.write_line('__typeof__((${expression})) ${c_name} = (${expression});')
	}
	local_type := if g.selfhost && g.last_expression_type == '' {
		'int'
	} else {
		fastc_normalize_inferred_type(g.last_expression_type)
	}
	function_alias := g.functions[local_type] or { FastcFunctionSignature{} }
	g.locals[name] = FastcLocal{
		is_mut:               is_mut
		typ:                  local_type
		option_value_type:    option_value_type
		fn_return_type:       function_alias.return_type
		fn_option_value_type: function_alias.option_type
	}
}

fn fastc_normalize_inferred_type(typ string) string {
	return match typ {
		'integer literal', 'negative integer literal' { 'int' }
		'float literal' { 'f64' }
		'nil' { 'voidptr' }
		else { typ }
	}
}

fn (mut g Parser) consume_statement_end() {
	if g.tok == .semicolon {
		g.next()
	}
}

fn (g &Parser) token_source() string {
	if g.lit.len > 0 {
		return g.lit
	}
	return g.tok.str()
}

fn (mut g Parser) write_line(line string) {
	if g.capturing_defer {
		indented_line := '\t'.repeat(g.indent) + line
		g.captured_defer_lines << indented_line
		return
	}
	for _ in 0 .. g.indent {
		g.out.write_u8(`\t`)
	}
	g.out.writeln(line)
}

// parse_orm_sql_statement lowers a `sql db { ... }` ORM statement into ordinary V
// source (runtime metadata construction plus a Connection method call) and
// re-scans that source. The `sql` keyword has already been consumed, so g.tok is
// positioned at the connection expression.
fn (mut g Parser) parse_orm_sql_statement() ! {
	// The lowering references `orm.Table`/`orm.QueryData`/`orm.Primitive`, so the
	// `orm` module must be in scope for those to resolve. Files that use `sql` without
	// importing `orm` (V normally injects it for them) fall back to the diagnostic
	// until FastC injects the import in a pre-pass.
	if 'orm' !in g.imports && g.module_name != 'orm' {
		return g.unsupported('ORM `sql` statements (comptime query generation is unavailable)')
	}
	db_source, block_source, unwrap := g.capture_orm_sql_block()!
	mut trailing := unwrap
	if g.tok == .key_or {
		or_source := g.capture_orm_or_block()!
		trailing = ' or { ${or_source} }'
	}
	lowering := g.build_orm_lowering(db_source, block_source, trailing)!
	// Re-scan the generated V as ordinary source. A fresh C block scopes the
	// temporaries so several `sql` blocks in one function cannot collide.
	saved_tok := g.tok
	saved_lit := g.lit
	saved_s := g.s
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_sql', lowering.len)
	file.index_lines_without_digest(lowering)
	g.s = scanner.new_scanner(g.prefs, .normal)
	g.s.init(file, lowering)
	g.next()
	g.write_line('{')
	g.indent++
	outer_locals := g.locals.clone()
	for g.tok != .eof {
		g.parse_statement()!
		g.skip_semicolons()
	}
	g.locals = outer_locals.clone()
	g.indent--
	g.write_line('}')
	g.s = saved_s
	g.tok = saved_tok
	g.lit = saved_lit
}

// capture_orm_sql_block splits `db { ... }[!]` into the connection expression
// source, the raw query-block body, and any trailing `!` propagation marker,
// consuming all of them from the main scanner.
fn (mut g Parser) capture_orm_sql_block() !(string, string, string) {
	db_start := g.s.pos
	mut depth := 0
	for g.tok != .eof {
		if g.tok == .lpar || g.tok == .lsbr {
			depth++
		} else if g.tok == .rpar || g.tok == .rsbr {
			depth--
		} else if g.tok == .lcbr && depth == 0 {
			break
		}
		g.next()
	}
	if g.tok != .lcbr {
		return g.unsupported('ORM `sql`: expected `{` after the connection')
	}
	db_source := g.s.src[db_start..g.s.pos].trim_space()
	g.next() // consume `{`
	block_start := g.s.pos
	mut block_depth := 0
	mut block_end := -1
	for g.tok != .eof {
		if g.tok == .lcbr {
			block_depth++
		} else if g.tok == .rcbr {
			if block_depth == 0 {
				block_end = g.s.pos
				break
			}
			block_depth--
		}
		g.next()
	}
	if block_end < 0 {
		return g.unsupported('ORM `sql`: unterminated query block')
	}
	block_source := g.s.src[block_start..block_end]
	g.next() // consume `}`
	mut trailing := ''
	if g.tok == .not {
		trailing = '!'
		g.next()
	}
	return db_source, block_source, trailing
}

// build_orm_lowering parses one ORM query block and returns the V source it lowers
// to. Only `insert` is handled so far; other operations report as unsupported.
fn (g &Parser) build_orm_lowering(db_source string, block_source string, trailing string) !string {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_query', block_source.len)
	file.index_lines_without_digest(block_source)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, block_source)
	mut tok := s.scan()
	if tok != .name {
		return g.unsupported('ORM `sql`: empty query block')
	}
	operation := s.lit
	if operation == 'insert' {
		// `insert <value> into <Table>`
		tok = s.scan()
		value_start := s.pos
		mut value_end := -1
		mut table_name := ''
		for tok != .eof {
			if tok == .name && s.lit == 'into' {
				value_end = s.pos
				tok = s.scan()
				if tok == .name {
					table_name = s.lit
				}
				break
			}
			tok = s.scan()
		}
		if value_end < 0 || table_name == '' {
			return g.unsupported('ORM `sql`: malformed `insert`')
		}
		value_source := block_source[value_start..value_end].trim_space()
		return g.build_orm_insert(db_source, value_source, table_name, trailing)
	}
	if operation == 'update' {
		return g.build_orm_update(db_source, block_source, trailing)
	}
	if operation == 'delete' {
		return g.build_orm_delete(db_source, block_source, trailing)
	}
	if operation == 'create' {
		return g.build_orm_create(db_source, block_source, trailing)
	}
	if operation == 'drop' {
		return g.build_orm_drop(db_source, block_source, trailing)
	}
	return g.unsupported('ORM `sql` `${operation}` (not lowered yet)')
}

// fastc_orm_where_op maps a V comparison token to the `orm.OperationKind` variant
// name for a `where` clause, or '' for an operator the update lowering cannot express.
fn fastc_orm_where_op(tok token.Token) string {
	return match tok {
		.eq { 'eq' }
		.ne { 'neq' }
		.gt { 'gt' }
		.lt { 'lt' }
		.ge { 'ge' }
		.le { 'le' }
		else { '' }
	}
}

// build_orm_update emits the V source for `update <Table> set <f> = <v>, ... where
// <f> <op> <v> [&&/|| ...]`. The `set` pairs fill the `data` QueryData (one boxed
// Primitive per assignment) and the `where` conditions fill the `where` QueryData
// (Primitive value + OperationKind + is_and joiners), then it calls the connection's
// `update`. Values are captured as source spans and boxed via `orm.Primitive(<expr>)`.
fn (g &Parser) build_orm_update(db_source string, block_source string, trailing string) !string {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_update', block_source.len)
	file.index_lines_without_digest(block_source)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, block_source)
	mut tok := s.scan()
	for tok == .semicolon {
		tok = s.scan()
	}
	if tok != .name || s.lit != 'update' {
		return g.unsupported('ORM `sql`: expected `update`')
	}
	tok = s.scan()
	if tok != .name {
		return g.unsupported('ORM `sql`: expected a row type after `update`')
	}
	table_name := s.lit
	tok = s.scan()
	if tok != .name || s.lit != 'set' {
		return g.unsupported('ORM `sql`: expected `set` in `update`')
	}
	tok = s.scan()
	mut set_fields := []string{}
	mut set_values := []string{}
	for tok != .eof {
		if tok != .name {
			return g.unsupported('ORM `sql`: expected a field name in `set`')
		}
		field_name := s.lit
		tok = s.scan()
		if tok != .assign {
			return g.unsupported('ORM `sql`: expected `=` in `set`')
		}
		tok = s.scan()
		value_start := s.pos
		mut value_end := s.offset
		mut depth := 0
		for tok != .eof {
			if tok in [.lpar, .lsbr, .lcbr] {
				depth++
			} else if tok in [.rpar, .rsbr, .rcbr] {
				depth--
			} else if depth == 0 && tok == .comma {
				break
			} else if depth == 0 && tok == .name && s.lit == 'where' {
				break
			}
			value_end = s.offset
			tok = s.scan()
		}
		set_fields << field_name
		set_values << block_source[value_start..value_end].trim_space()
		if tok == .comma {
			tok = s.scan()
			continue
		}
		break
	}
	if set_fields.len == 0 {
		return g.unsupported('ORM `sql`: `update` has no `set` assignments')
	}
	where_lines, _ := g.build_orm_where_lines(mut s, block_source, tok)!
	mut out := "mut __orm_table := orm.Table{ name: '${table_name}' }\n"
	out += 'mut __orm_data := orm.QueryData{}\n'
	for i, field_name in set_fields {
		out += "__orm_data.fields << '${field_name}'\n"
		out += '__orm_set_${i} := orm.Primitive(${set_values[i]})\n'
		out += '__orm_data.data << __orm_set_${i}\n'
	}
	out += where_lines
	out += '${db_source}.update(__orm_table, __orm_data, __orm_where)${trailing}\n'
	return out
}

// build_orm_where_lines parses an optional `where <f> <op> <v> [&&/|| ...]` clause
// (the scanner is positioned on the token where the clause would start, `first_tok`)
// and returns the V source that fills a `__orm_where` QueryData with one boxed
// Primitive, OperationKind, and is_and joiner per condition. An absent `where`
// yields an empty QueryData. Shared by `update` and `delete`.
fn (g &Parser) build_orm_where_lines(mut s scanner.Scanner, block_source string, first_tok token.Token) !(string, token.Token) {
	mut tok := first_tok
	mut where_fields := []string{}
	mut where_ops := []string{}
	mut where_values := []string{}
	mut where_is_and := []string{}
	// Parenthesized groups as [start_field_index, end_field_index] pairs, closed in
	// post-order (inner group before outer), mirroring V's ORM QueryData.parentheses.
	mut where_paren_starts := []int{}
	mut where_paren_ends := []int{}
	if tok == .name && s.lit == 'where' {
		tok = s.scan()
		// `is_and[i]` is the operator (&& true / || false) between condition i and i+1 in
		// source order; a boolean tree's explicit `(` / `)` become parentheses groups.
		mut paren_stack := []int{}
		mut pending_is_and := ''
		for tok != .eof {
			if tok == .semicolon {
				tok = s.scan()
				continue
			}
			if tok == .lpar {
				paren_stack << where_fields.len
				tok = s.scan()
				continue
			}
			if tok == .rpar {
				if paren_stack.len > 0 {
					start := paren_stack.last()
					paren_stack.delete_last()
					if where_fields.len > start {
						where_paren_starts << start
						where_paren_ends << where_fields.len - 1
					}
				}
				tok = s.scan()
				continue
			}
			if tok == .and {
				pending_is_and = 'true'
				tok = s.scan()
				continue
			}
			if tok == .logical_or {
				pending_is_and = 'false'
				tok = s.scan()
				continue
			}
			if tok == .name && s.lit in ['order', 'limit', 'offset'] {
				break
			}
			if tok != .name {
				return g.unsupported('ORM `sql`: expected a field name in `where`')
			}
			where_field := s.lit
			tok = s.scan()
			op := fastc_orm_where_op(tok)
			if op == '' {
				return g.unsupported('ORM `sql`: unsupported `where` operator')
			}
			tok = s.scan()
			value_start := s.pos
			mut value_end := s.offset
			mut depth := 0
			for tok != .eof {
				if tok in [.lpar, .lsbr, .lcbr] {
					depth++
				} else if tok in [.rpar, .rsbr, .rcbr] {
					if depth == 0 {
						// A `)` at the value's top level closes a `where` group, not a
						// sub-expression; stop without consuming it.
						break
					}
					depth--
				} else if depth == 0 && (tok == .and || tok == .logical_or || tok == .semicolon) {
					break
				} else if depth == 0 && tok == .name && s.lit in ['order', 'limit', 'offset'] {
					// A trailing `order by` / `limit` / `offset` clause ends the condition;
					// do not swallow it into the value.
					break
				}
				value_end = s.offset
				tok = s.scan()
			}
			if where_fields.len > 0 {
				where_is_and << if pending_is_and == '' { 'true' } else { pending_is_and }
			}
			where_fields << where_field
			where_ops << op
			where_values << block_source[value_start..value_end].trim_space()
			pending_is_and = ''
		}
	}
	mut out := 'mut __orm_where := orm.QueryData{}\n'
	for i, field_name in where_fields {
		out += "__orm_where.fields << '${field_name}'\n"
		out += '__orm_wval_${i} := orm.Primitive(${where_values[i]})\n'
		out += '__orm_where.data << __orm_wval_${i}\n'
		out += '__orm_where.kinds << orm.OperationKind.${where_ops[i]}\n'
	}
	for is_and in where_is_and {
		out += '__orm_where.is_and << ${is_and}\n'
	}
	for i in 0 .. where_paren_starts.len {
		out += '__orm_paren_${i} := [${where_paren_starts[i]}, ${where_paren_ends[i]}]\n'
		out += '__orm_where.parentheses << __orm_paren_${i}\n'
	}
	return out, tok
}

// build_orm_delete emits the V source for `delete from <Table> [where ...]`: it
// builds the where QueryData (shared with `update`) and calls the connection's
// `delete`.
fn (g &Parser) build_orm_delete(db_source string, block_source string, trailing string) !string {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_delete', block_source.len)
	file.index_lines_without_digest(block_source)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, block_source)
	mut tok := s.scan()
	for tok == .semicolon {
		tok = s.scan()
	}
	if tok != .name || s.lit != 'delete' {
		return g.unsupported('ORM `sql`: expected `delete`')
	}
	tok = s.scan()
	if tok != .name || s.lit != 'from' {
		return g.unsupported('ORM `sql`: expected `from` in `delete`')
	}
	tok = s.scan()
	if tok != .name {
		return g.unsupported('ORM `sql`: expected a row type after `from`')
	}
	table_name := s.lit
	tok = s.scan()
	where_lines, _ := g.build_orm_where_lines(mut s, block_source, tok)!
	mut out := "mut __orm_table := orm.Table{ name: '${table_name}' }\n"
	out += where_lines
	out += '${db_source}.delete(__orm_table, __orm_where)${trailing}\n'
	return out
}

// build_orm_insert emits the V source for `insert <value> into <Table>`: it fills
// an orm.QueryData with one boxed Primitive per row-struct field, then calls the
// connection's `insert` method.
fn (g &Parser) build_orm_insert(db_source string, value_source string, table_name string, trailing string) !string {
	type_key := fastc_resolve_declared_type_key(g.module_name, table_name, g.imports,
		g.declared_types) or { return g.unsupported('ORM `sql`: unknown row type `${table_name}`') }
	c_type := fastc_c_declared_type_name(type_key)
	fields := g.struct_field_info[c_type]
	if fields.len == 0 {
		return g.unsupported('ORM `sql`: row type `${table_name}` has no fields')
	}
	mut out := "mut __orm_table := orm.Table{ name: '${table_name}' }\n"
	out += 'mut __orm_data := orm.QueryData{}\n'
	for i, field in fields {
		if field.is_skip {
			// `@[skip]` fields are not persisted, mirroring V's ORM.
			continue
		}
		out += "__orm_data.fields << '${field.name}'\n"
		// An enum field is persisted as its integer backing.
		field_value := if g.is_enum_type_name(field.typ) {
			'int(${value_source}.${field.name})'
		} else {
			'${value_source}.${field.name}'
		}
		// Box the field value through an intermediate declaration: a cast on a
		// declaration's right-hand side is boxed into the `orm.Primitive` sum type,
		// whereas a cast nested directly in the `<<` append renders as a raw C cast.
		out += '__orm_value_${i} := orm.Primitive(${field_value})\n'
		out += '__orm_data.data << __orm_value_${i}\n'
	}
	out += '${db_source}.insert(__orm_table, __orm_data)${trailing}\n'
	return out
}

// fastc_orm_parse_table_op scans `<op> table <Table>` and returns the row type name.
// Shared by `create` and `drop`.
fn (g &Parser) fastc_orm_parse_table_op(block_source string, op string) !string {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_${op}', block_source.len)
	file.index_lines_without_digest(block_source)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, block_source)
	mut tok := s.scan()
	for tok == .semicolon {
		tok = s.scan()
	}
	if tok != .name || s.lit != op {
		return g.unsupported('ORM `sql`: expected `${op}`')
	}
	tok = s.scan()
	if tok != .name || s.lit != 'table' {
		return g.unsupported('ORM `sql`: expected `table` in `${op}`')
	}
	tok = s.scan()
	if tok != .name {
		return g.unsupported('ORM `sql`: expected a row type after `table`')
	}
	return s.lit
}

// build_orm_create emits the V source for `create table <Table>`: it builds an
// orm.TableField per struct field (name + orm type index; attrs are not yet recorded
// by FastC, so they are empty) and calls the connection's `create`.
fn (g &Parser) build_orm_create(db_source string, block_source string, trailing string) !string {
	table_name := g.fastc_orm_parse_table_op(block_source, 'create')!
	type_key := fastc_resolve_declared_type_key(g.module_name, table_name, g.imports,
		g.declared_types) or { return g.unsupported('ORM `sql`: unknown row type `${table_name}`') }
	c_type := fastc_c_declared_type_name(type_key)
	fields := g.struct_field_info[c_type]
	if fields.len == 0 {
		return g.unsupported('ORM `sql`: row type `${table_name}` has no fields')
	}
	mut out := "mut __orm_table := orm.Table{ name: '${table_name}' }\n"
	out += 'mut __orm_fields := []orm.TableField{}\n'
	for i, field in fields {
		if field.is_skip {
			// `@[skip]` fields are not persisted (no column), mirroring V's ORM.
			continue
		}
		type_idx := fastc_builtin_type_idx(field.typ) or {
			if field.typ == 'time__Time' {
				// orm.time_ (-2): the ORM renders this as a DATETIME/timestamp column.
				-2
			} else if g.is_enum_type_name(field.typ) {
				// Enums are stored as their integer backing → an INTEGER column (`int`).
				8
			} else {
				return g.unsupported('ORM `sql`: create field `${field.name}` of type `${field.typ}` is not yet supported')
			}
		}
		out += "__orm_tf_${i} := orm.TableField{ name: '${field.name}', typ: ${type_idx} }\n"
		out += '__orm_fields << __orm_tf_${i}\n'
	}
	out += '${db_source}.create(__orm_table, __orm_fields)${trailing}\n'
	return out
}

// build_orm_drop emits the V source for `drop table <Table>`: it builds the Table and
// calls the connection's `drop`.
fn (g &Parser) build_orm_drop(db_source string, block_source string, trailing string) !string {
	table_name := g.fastc_orm_parse_table_op(block_source, 'drop')!
	mut out := "mut __orm_table := orm.Table{ name: '${table_name}' }\n"
	out += '${db_source}.drop(__orm_table)${trailing}\n'
	return out
}

// fastc_orm_field_zero returns the V zero literal for a column type the select
// row-parser can unbox from a `Primitive` (scalars, string, `time.Time`), or none for
// a type it cannot yet handle (other structs, arrays, …).
// fastc_orm_enum_v_name returns the V type name to cast an integer column value to/from
// for an enum field (enums are stored as their integer backing in V's ORM), or none if
// the field is not an enum. A C-qualified name (`mod__Enum`) is respelled `mod.Enum`.
fn (g &Parser) fastc_orm_enum_v_name(field_typ string) ?string {
	if !g.is_enum_type_name(field_typ) {
		return none
	}
	return field_typ.replace('__', '.')
}

fn fastc_orm_field_zero(c_type string) ?string {
	if c_type == 'string' {
		return "''"
	}
	if c_type in ['f32', 'f64'] {
		return '0.0'
	}
	if c_type == 'bool' {
		return 'false'
	}
	if fastc_primitive_c_type(c_type) != none {
		return '0'
	}
	if c_type == 'time__Time' {
		return 'time.Time{}'
	}
	return none
}

// fastc_orm_field_match_type returns the V type name to use as the `match` variant
// when unboxing a column Primitive (`time__Time` → `time.Time`; scalars/string are
// spelled the same in V and C).
fn fastc_orm_field_match_type(c_type string) string {
	if c_type == 'time__Time' {
		return 'time.Time'
	}
	return c_type
}

// build_orm_select_lowering emits the V source for `select from <Table>`: it builds
// an orm.SelectConfig, calls the connection's `select` (→ `[][]Primitive`), then
// parses each row back into a `<Table>` by unboxing every column Primitive to its
// field type via a `match`. The generated block ends with the `[]<Table>` result as
// its final statement, so the caller can wrap it as a C statement-expression value.
fn (g &Parser) build_orm_select_lowering(db_source string, block_source string, trailing string, or_source string) !(string, string) {
	if trailing != '!' && or_source == '' {
		return g.unsupported('ORM `sql`: `select` must be unwrapped with `!` or `or { ... }`')
	}
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_select', block_source.len)
	file.index_lines_without_digest(block_source)
	mut s := scanner.new_scanner(g.prefs, .normal)
	s.init(file, block_source)
	mut tok := s.scan()
	for tok == .semicolon {
		tok = s.scan()
	}
	if tok == .key_select {
		tok = s.scan()
	} else {
		return g.unsupported('ORM `sql`: expected `select`')
	}
	// Optional aggregate: `select count from ...`.
	mut is_count := false
	if tok == .name && s.lit == 'count' {
		is_count = true
		tok = s.scan()
	}
	// `select [count] from <Table>` — `from` scans as a name.
	if tok != .name || s.lit != 'from' {
		return g.unsupported('ORM `sql`: expected `from` in `select`')
	}
	tok = s.scan()
	if tok != .name {
		return g.unsupported('ORM `sql`: expected a row type after `from`')
	}
	table_name := s.lit
	tok = s.scan()
	// Optional `where ...` clause (shared with update/delete).
	where_lines, after_where := g.build_orm_where_lines(mut s, block_source, tok)!
	tok = after_where
	// Optional `order by <field> [desc|asc]`.
	mut order_field := ''
	mut order_desc := false
	if tok == .name && s.lit == 'order' {
		tok = s.scan()
		if tok != .name || s.lit != 'by' {
			return g.unsupported('ORM `sql`: expected `by` after `order`')
		}
		tok = s.scan()
		if tok != .name {
			return g.unsupported('ORM `sql`: expected a field name after `order by`')
		}
		order_field = s.lit
		tok = s.scan()
		if tok == .name && s.lit == 'desc' {
			order_desc = true
			tok = s.scan()
		} else if tok == .name && s.lit == 'asc' {
			tok = s.scan()
		}
	}
	// Optional `limit <expr>` / `offset <expr>` (their values ride in the data
	// QueryData, mirroring V's ORM).
	mut limit_value := ''
	mut offset_value := ''
	if tok == .name && s.lit == 'limit' {
		tok = s.scan()
		limit_start := s.pos
		mut limit_end := s.offset
		for tok != .eof {
			if tok == .name && s.lit == 'offset' {
				break
			}
			if tok == .semicolon {
				break
			}
			limit_end = s.offset
			tok = s.scan()
		}
		limit_value = block_source[limit_start..limit_end].trim_space()
	}
	if tok == .name && s.lit == 'offset' {
		tok = s.scan()
		offset_start := s.pos
		mut offset_end := s.offset
		for tok != .eof {
			if tok == .semicolon {
				break
			}
			offset_end = s.offset
			tok = s.scan()
		}
		offset_value = block_source[offset_start..offset_end].trim_space()
	}
	for tok == .semicolon {
		tok = s.scan()
	}
	if tok != .eof {
		return g.unsupported('ORM `sql`: unsupported clause near the end of `select`')
	}
	type_key := fastc_resolve_declared_type_key(g.module_name, table_name, g.imports,
		g.declared_types) or { return g.unsupported('ORM `sql`: unknown row type `${table_name}`') }
	c_type := fastc_c_declared_type_name(type_key)
	fields := g.struct_field_info[c_type]
	if fields.len == 0 {
		return g.unsupported('ORM `sql`: row type `${table_name}` has no fields')
	}
	// The SelectConfig: a row select names its columns; `count` sets the aggregate.
	mut out := where_lines
	out += 'mut __orm_config := orm.SelectConfig{\n'
	out += "\ttable:   orm.Table{ name: '${table_name}' }\n"
	if is_count {
		out += '\taggregate_kind: orm.AggregateKind.count\n'
	} else {
		for field in fields {
			if field.is_skip {
				continue
			}
			if g.is_enum_type_name(field.typ) {
				continue
			}
			if fastc_orm_field_zero(field.typ) == none {
				return g.unsupported('ORM `sql`: select field `${field.name}` of type `${field.typ}` is not yet supported')
			}
		}
		mut field_list := []string{}
		for field in fields {
			if field.is_skip {
				// `@[skip]` fields are not columns, mirroring V's ORM.
				continue
			}
			field_list << "'${field.name}'"
		}
		out += '\tfields:  [${field_list.join(', ')}]\n'
	}
	out += "\tprimary: 'id'\n"
	if order_field != '' {
		out += '\thas_order:  true\n'
		out += "\torder:      '${order_field}'\n"
		out += '\torder_type: orm.OrderType.${if order_desc { 'desc' } else { 'asc' }}\n'
	}
	if limit_value != '' {
		out += '\thas_limit:  true\n'
	}
	if offset_value != '' {
		out += '\thas_offset: true\n'
	}
	out += '}\n'
	// The result: a `[]<Table>` for a row select, or an `int` for `count`.
	result_c_type := if is_count { 'int' } else { fastc_array_c_type(c_type) }
	empty_init := if is_count { '0' } else { '[]${table_name}{}' }
	// The success-branch body that turns `__orm_rows` (`[][]Primitive`) into the result.
	mut build := ''
	if is_count {
		build += 'if __orm_rows.len > 0 {\n'
		build += '\t__orm_row0 := __orm_rows[0]\n'
		build += '\tif __orm_row0.len > 0 {\n'
		build += '\t\t__orm_cnt := __orm_row0[0]\n'
		build += '\t\t__orm_result = match __orm_cnt { int { __orm_cnt } i64 { int(__orm_cnt) } else { 0 } }\n'
		build += '\t}\n'
		build += '}\n'
	} else {
		// Unbox each column Primitive via a `match` on a plain local (the subject must
		// be a local for the smart-cast), then build the row via a struct literal so no
		// immutable field is mutated.
		build += 'for __orm_row in __orm_rows {\n'
		mut inits := []string{}
		// `col` tracks the DB column index, which counts only persisted (non-`@[skip]`)
		// fields; a skipped field consumes no column and is left at its zero value in the
		// row literal.
		mut col := 0
		for field in fields {
			if field.is_skip {
				continue
			}
			if enum_name := g.fastc_orm_enum_v_name(field.typ) {
				// An enum column is stored as an integer; cast it back to the enum.
				build += '\t__orm_col_${col} := __orm_row[${col}]\n'
				build += '\t__orm_field_${col} := match __orm_col_${col} { int { ${enum_name}(__orm_col_${col}) } i64 { ${enum_name}(int(__orm_col_${col})) } else { ${enum_name}(0) } }\n'
				inits << '${field.name}: __orm_field_${col}'
				col++
				continue
			}
			zero := fastc_orm_field_zero(field.typ) or {
				return g.unsupported('ORM `sql`: select field `${field.name}` unsupported')
			}
			match_type := fastc_orm_field_match_type(field.typ)
			build += '\t__orm_col_${col} := __orm_row[${col}]\n'
			build += '\t__orm_field_${col} := match __orm_col_${col} { ${match_type} { __orm_col_${col} } else { ${zero} } }\n'
			inits << '${field.name}: __orm_field_${col}'
			col++
		}
		build += '\t__orm_item := ${table_name}{ ${inits.join(', ')} }\n'
		build += '\t__orm_result << __orm_item\n'
		build += '}\n'
	}
	out += 'mut __orm_result := ${empty_init}\n'
	// The data QueryData carries the limit/offset values (limit first), if any.
	out += 'mut __orm_select_data := orm.QueryData{}\n'
	if limit_value != '' {
		out += '__orm_limit_val := orm.Primitive(${limit_value})\n'
		out += '__orm_select_data.data << __orm_limit_val\n'
	}
	if offset_value != '' {
		out += '__orm_offset_val := orm.Primitive(${offset_value})\n'
		out += '__orm_select_data.data << __orm_offset_val\n'
	}
	if trailing == '!' {
		// `!` propagates a query error to the caller.
		out += '__orm_rows := ${db_source}.select(__orm_config, __orm_select_data, __orm_where)!\n'
		out += build
	} else {
		// `or { <expr> }` uses the fallback value when the query fails.
		out += 'if __orm_rows := ${db_source}.select(__orm_config, __orm_select_data, __orm_where) {\n'
		out += build
		out += '} else {\n'
		if fastc_orm_or_source_starts_with_exit(or_source, g.prefs) {
			// A diverging fallback belongs to the containing function/loop; emitting it
			// after an assignment (`result = return ...`) produces invalid V and C.
			out += '\t${or_source}\n'
		} else {
			out += '\t__orm_result = ${or_source}\n'
		}
		out += '}\n'
	}
	return out, result_c_type
}

fn fastc_orm_or_source_starts_with_exit(source string, prefs &pref.Preferences) bool {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_or', source.len)
	file.index_lines_without_digest(source)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	return s.scan() in [.key_return, .key_break, .key_continue]
}

// capture_orm_or_block consumes an `or { <expr> }` unwrap after a `sql { ... }` block
// (if present) and returns the block's source (the fallback value expression). An
// absent `or` returns ''. The `!` unwrap is captured separately by capture_orm_sql_block.
fn (mut g Parser) capture_orm_or_block() !string {
	if g.tok != .key_or {
		return ''
	}
	g.next() // consume `or`
	if g.tok != .lcbr {
		return g.unsupported('ORM `sql`: expected `{` after `or`')
	}
	g.next() // consume `{`
	block_start := g.s.pos
	mut depth := 0
	mut block_end := -1
	for g.tok != .eof {
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			if depth == 0 {
				block_end = g.s.pos
				break
			}
			depth--
		}
		g.next()
	}
	if block_end < 0 {
		return g.unsupported('ORM `sql`: unterminated `or` block')
	}
	source := g.s.src[block_start..block_end]
	g.next() // consume `}`
	return source.trim_space()
}

// emit_orm_lowering_statements re-scans generated ORM lowering V (which builds the
// query and its result into `__orm_result`) as ordinary statements, writing to the
// live `g.out`. The scanner/token state is saved and restored around the swap.
fn (mut g Parser) emit_orm_lowering_statements(lowering string) ! {
	saved_tok := g.tok
	saved_lit := g.lit
	saved_s := g.s
	outer_locals := g.locals.clone()
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('orm_lowering', lowering.len)
	file.index_lines_without_digest(lowering)
	g.s = scanner.new_scanner(g.prefs, .normal)
	g.s.init(file, lowering)
	g.next()
	for g.tok != .eof {
		g.parse_statement()!
		g.skip_semicolons()
	}
	g.locals = outer_locals.clone()
	g.s = saved_s
	g.tok = saved_tok
	g.lit = saved_lit
}

// parse_orm_sql_select_declaration lowers `<name> := sql db { select ... } <unwrap>`.
// It declares `<name>` (a `[]<Table>` or `int` for `count`), then emits the lowering
// (which builds the result into `__orm_result`) inside a fresh C block that assigns
// `__orm_result` to `<name>`.
fn (mut g Parser) parse_orm_sql_select_declaration(name string, is_mut bool) ! {
	if 'orm' !in g.imports && g.module_name != 'orm' {
		return g.unsupported('ORM `sql` expressions (comptime query generation is unavailable)')
	}
	// Consume `sql` so the scanner sits on the connection expression, matching the
	// statement path (`parse_orm_sql_statement`) that `capture_orm_sql_block` expects.
	g.next()
	db_source, block_source, trailing := g.capture_orm_sql_block()!
	or_source := g.capture_orm_or_block()!
	lowering, result_type := g.build_orm_select_lowering(db_source, block_source, trailing,
		or_source)!
	g.consume_statement_end()
	// The `[]<Table>` result array is often not otherwise constructed in the program,
	// so register it as a composite type to force its `Array_<Table>` typedef.
	fastc_register_composite_type(result_type, mut g.composite_types)
	c_name := fastc_c_identifier(name)
	g.write_line('${result_type} ${c_name};')
	g.write_line('{')
	g.indent++
	g.emit_orm_lowering_statements(lowering)!
	g.write_line('${c_name} = __orm_result;')
	g.indent--
	g.write_line('}')
	g.locals[name] = FastcLocal{
		is_mut: is_mut
		typ:    result_type
	}
}

// parse_orm_sql_select_assignment lowers `<name> = sql db { select ... } <unwrap>`
// (assignment to an existing target), emitting the lowering into a fresh C block that
// assigns `__orm_result` to `<name>`.
fn (mut g Parser) parse_orm_sql_select_assignment(name string) ! {
	if 'orm' !in g.imports && g.module_name != 'orm' {
		return g.unsupported('ORM `sql` expressions (comptime query generation is unavailable)')
	}
	g.next() // consume `sql`
	db_source, block_source, trailing := g.capture_orm_sql_block()!
	or_source := g.capture_orm_or_block()!
	lowering, result_type := g.build_orm_select_lowering(db_source, block_source, trailing,
		or_source)!
	g.consume_statement_end()
	fastc_register_composite_type(result_type, mut g.composite_types)
	c_name := fastc_c_identifier(name)
	g.write_line('{')
	g.indent++
	g.emit_orm_lowering_statements(lowering)!
	g.write_line('${c_name} = __orm_result;')
	g.indent--
	g.write_line('}')
}

// parse_orm_sql_select_return lowers `return sql db { select ... } <unwrap>`: it emits
// the lowering into a fresh C block and returns `__orm_result` from it.
fn (mut g Parser) parse_orm_sql_select_return() !bool {
	if 'orm' !in g.imports && g.module_name != 'orm' {
		return g.unsupported('ORM `sql` expressions (comptime query generation is unavailable)')
	}
	g.next() // consume `sql`
	db_source, block_source, trailing := g.capture_orm_sql_block()!
	or_source := g.capture_orm_or_block()!
	lowering, result_type := g.build_orm_select_lowering(db_source, block_source, trailing,
		or_source)!
	g.consume_statement_end()
	g.write_line('{')
	g.indent++
	g.emit_orm_lowering_statements(lowering)!
	g.write_all_deferred_scopes()
	return_source := if g.return_type == 'Option' {
		fastc_option_success_expression(result_type, '__orm_result')
	} else {
		'__orm_result'
	}
	g.write_line('return ${return_source};')
	g.indent--
	g.write_line('}')
	return true
}
