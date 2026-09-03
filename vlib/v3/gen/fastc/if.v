module fastc

import v3.scanner
import v3.token

fn (g &Parser) optional_function_field_for_expression(tokens []FastcExpressionToken) ?FastcStructField {
	if tokens.len < 3 || tokens[tokens.len - 2].tok != .dot || tokens.last().tok != .name {
		return none
	}
	receiver_type := g.infer_expression_type(tokens[..tokens.len - 2]) or { return none }
	field := g.struct_field_metadata(receiver_type, tokens.last().lit) or { return none }
	if !field.is_optional_function {
		return none
	}
	return field
}

fn (g &Parser) or_block_has_statements() bool {
	if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
		return false
	}
	if g.tok == .name && g.lit == 'panic' {
		return true
	}
	if g.tok in [.dollar, .key_return, .key_if, .key_for, .key_match, .key_mut, .key_defer,
		.key_break, .key_continue] {
		return true
	}
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := g.tok
	mut depth := 0
	for tok != .eof {
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			if depth == 0 {
				return false
			}
			depth--
		} else if depth == 0 && tok == .key_return {
			return true
		} else if depth == 0 && (tok.is_assignment() || tok in [.inc, .dec]) {
			// A top-level assignment or `++`/`--` makes the block a statement block
			// (`or { x = '' }`), not a trailing value expression.
			return true
		} else if depth == 0 && tok == .semicolon {
			next_token := lookahead.scan()
			if fastc_token_continues_expression(next_token) {
				// A `;` auto-inserted mid multi-line expression (`a\n || b`) is a
				// continuation, not a statement boundary; keep scanning past it.
				tok = lookahead.scan()
				continue
			}
			return next_token != .rcbr
		}
		tok = lookahead.scan()
	}
	return false
}

// parse_if_prescan_leading_smartcasts registers a temporary member smart-cast for each
// leading `[mut] local is Variant` conjunct of an if-condition (simple boxed-local
// subjects, single-name variants, joined by top-level `&&`), so a following guard or the
// interpolation inside one reads the narrowed field while the condition is (re)rendered.
// It does not consume input (a probe scanner walks ahead) and returns the names it
// registered so the caller can drop them afterwards.
fn (mut g Parser) parse_if_prescan_leading_smartcasts() []string {
	mut registered := []string{}
	mut probe := g.s
	mut tok := g.tok
	mut lit := g.lit
	// Buffer the whole condition's tokens (bounded by the block `{` / `;`), tracking bracket
	// depth so only TOP-LEVEL operators split the boolean structure.
	mut toks := []token.Token{}
	mut lits := []string{}
	mut depth := 0
	mut has_top_and := false
	mut has_top_or := false
	for {
		if depth == 0 && tok in [token.Token.lcbr, .eof] {
			break
		}
		if depth == 0 && tok == .semicolon {
			// An auto-inserted `;` at a line break before a binary continuation (`x is A\n || …`)
			// is not the condition's end; skip it so the whole multi-line condition is scanned.
			// Missing this would treat `x is A` as the whole condition and wrongly narrow `x`.
			next := probe.scan()
			next_lit := probe.lit
			if !fastc_token_continues_expression(next) {
				break
			}
			tok = next
			lit = next_lit
			continue
		}
		if depth == 0 && tok == .and {
			has_top_and = true
		}
		if depth == 0 && tok == .logical_or {
			has_top_or = true
		}
		if tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tok in [.rpar, .rsbr, .rcbr] {
			if depth > 0 {
				depth--
			}
		}
		toks << tok
		lits << lit
		tok = probe.scan()
		lit = probe.lit
	}
	// A mix of top-level `&&` and `||` has no single short-circuit guarantee across the whole
	// condition, so no prescan narrowing is safe.
	if has_top_and && has_top_or {
		return []string{}
	}
	// A subject also read through an explicit `right as SI` in the same condition is narrowed by
	// that cast (with the conjunction narrowing) already; a prescan member smart-cast on the same
	// name would collide with the `as` rendering, so exclude those names.
	mut as_names := map[string]bool{}
	for i in 0 .. toks.len - 1 {
		if toks[i] == .name && toks[i + 1] == .key_as {
			as_names[lits[i]] = true
		}
	}
	// A top-level `||` chain narrows on NEGATED `is`: `x !is T || … x …` evaluates a later operand
	// only when `x !is T` was false, i.e. `x` IS `T` there. A `&&` chain (or a single conjunct)
	// narrows on positive `is`: `x is T && … x …`. Either way a member read buried in a later
	// operand's `or`-block synthetic (`tc.structs[field_type.name]`) then narrows correctly.
	split_tok := if has_top_or { token.Token.logical_or } else { token.Token.and }
	is_tok := if has_top_or { token.Token.not_is } else { token.Token.key_is }
	mut start := 0
	mut d := 0
	for i, item in toks {
		if item in [.lpar, .lsbr, .lcbr] {
			d++
			continue
		}
		if item in [.rpar, .rsbr, .rcbr] {
			if d > 0 {
				d--
			}
			continue
		}
		if d == 0 && item == split_tok {
			g.prescan_register_is_conjunct(toks[start..i], lits[start..i], as_names, is_tok, mut registered)
			start = i + 1
		}
	}
	g.prescan_register_is_conjunct(toks[start..], lits[start..], as_names, is_tok, mut registered)
	return registered
}

// prescan_register_is_conjunct registers a member smart-cast when the conjunct is exactly a bare
// `[mut] [&] name <is_tok> Variant`; any other shape (a call, a `!` test, a member `is`) is skipped.
fn (mut g Parser) prescan_register_is_conjunct(toks []token.Token, lits []string, as_names map[string]bool, is_tok token.Token, mut registered []string) {
	mut i := 0
	for i < toks.len && toks[i] in [.key_mut, .amp] {
		i++
	}
	if i + 3 != toks.len || toks[i] != .name || toks[i + 1] != is_tok || toks[i + 2] != .name {
		return
	}
	subject := lits[i]
	if subject in as_names {
		return
	}
	variant_name := lits[i + 2]
	if local := g.locals[subject] {
		boxed := fastc_normalize_inferred_type(local.typ)
		if g.is_boxed_type(boxed) && subject !in g.member_smartcasts {
			if variant_key := g.resolve_declared_type_key(variant_name) {
				variant_c := fastc_c_declared_type_name(variant_key)
				access := if boxed.ends_with('*') { '->' } else { '.' }
				g.member_smartcasts[subject] = FastcMemberSmartcast{
					typ: variant_c + '*'
					source: '((${variant_c} *)${g.local_c_name(subject)}${access}_object)'
				}
				registered << subject
			}
		}
	}
}

// compute_bool_is_implications returns the member smart-casts a bool local would imply when true,
// given the tokens of its RHS. Only a top-level `&&` chain (no top-level `||`) qualifies: a true
// bool then guarantees every `is` conjunct held. Bare `subject is Variant` conjuncts contribute a
// smart-cast; a bare bool-local conjunct inherits its own recorded implications.
fn (g &Parser) compute_bool_is_implications(tokens []FastcExpressionToken) []FastcBoolImplication {
	mut depth := 0
	for t in tokens {
		match t.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.logical_or {
				if depth == 0 {
					return []
				}
			}
			else {}
		}
	}
	mut implications := []FastcBoolImplication{}
	for conjunct in fastc_split_top_level_and(tokens) {
		if imp := g.bool_conjunct_implication(conjunct) {
			implications << imp
		} else if conjunct.len == 1 && conjunct[0].tok == .name {
			if inherited := g.locals[conjunct[0].lit] {
				for inner in inherited.bool_implications {
					implications << inner
				}
			}
		}
	}
	return implications
}

// bool_conjunct_implication returns the smart-cast a lone `[mut] name is Variant` conjunct implies.
fn (g &Parser) bool_conjunct_implication(conjunct []FastcExpressionToken) ?FastcBoolImplication {
	mut i := 0
	for i < conjunct.len && conjunct[i].tok in [.key_mut, .amp] {
		i++
	}
	if i + 3 != conjunct.len || conjunct[i].tok != .name || conjunct[i + 1].tok != .key_is || conjunct[i + 2].tok != .name {
		return none
	}
	subject := conjunct[i].lit
	variant_name := conjunct[i + 2].lit
	local := g.locals[subject] or { return none }
	boxed := fastc_normalize_inferred_type(local.typ)
	if !g.is_boxed_type(boxed) {
		return none
	}
	variant_key := g.resolve_declared_type_key(variant_name) or { return none }
	variant_c := fastc_c_declared_type_name(variant_key)
	access := if boxed.ends_with('*') { '->' } else { '.' }
	return FastcBoolImplication{
		subject: subject
		smartcast: FastcMemberSmartcast{
			typ: variant_c + '*'
			source: '((${variant_c} *)${g.local_c_name(subject)}${access}_object)'
		}
	}
}

// apply_bool_implication_smartcasts, when the expression about to be read starts with `boolvar &&`
// where `boolvar` is a bool local carrying `is` implications and the chain has no top-level `||`,
// registers those implied member smart-casts and returns the names to delete after the read — so
// `ok && x.field` reads `x`'s narrowed variant field. See [[fastc-compile-cmd-v]].
fn (mut g Parser) apply_bool_implication_smartcasts(stops []token.Token) []string {
	if g.tok != .name {
		return []
	}
	local := g.locals[g.lit] or { return [] }
	if local.bool_implications.len == 0 {
		return []
	}
	mut probe := g.s
	mut tok := g.tok
	mut depth := 0
	mut saw_top_and := false
	for {
		if depth == 0 && tok == .semicolon {
			// Skip an auto-inserted `;` before a binary continuation (`ok\n && x.field`); checked
			// before `stops` (which includes `.semicolon`) so a line-wrapped chain is not cut short.
			next := probe.scan()
			if !fastc_token_continues_expression(next) {
				break
			}
			tok = next
			continue
		}
		if depth == 0 && (tok in stops || tok == .eof) {
			break
		}
		if depth == 0 && tok == .logical_or {
			return []
		}
		if depth == 0 && tok == .and {
			saw_top_and = true
		}
		if tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tok in [.rpar, .rsbr, .rcbr] {
			if depth > 0 {
				depth--
			}
		}
		tok = probe.scan()
	}
	if !saw_top_and {
		return []
	}
	mut registered := []string{}
	for imp in local.bool_implications {
		if imp.subject !in g.member_smartcasts {
			g.member_smartcasts[imp.subject] = imp.smartcast
			registered << imp.subject
		}
	}
	return registered
}

// detect_option_none_narrowings returns the option locals a condition guarantees to be
// non-none inside the then-branch: each top-level `name != none` conjunct (the caller
// having already excluded a top-level `||`), where `name` is an option-typed local. Such
// a local is stored as the erased `Option`, so its wrapped value is reached through
// `*((Base *)name.data)` — the source a member smart-cast then rewrites `name.field` onto.
fn (g &Parser) detect_option_none_narrowings(cond_tokens []FastcExpressionToken) []string {
	mut names := []string{}
	mut depth := 0
	for i := 0; i + 2 < cond_tokens.len; i++ {
		tok := cond_tokens[i].tok
		if tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 {
			continue
		}
		if tok == .name && cond_tokens[i + 1].tok == .ne && cond_tokens[i + 2].tok == .key_none {
			if i > 0 && cond_tokens[i - 1].tok == .dot {
				// `x.f != none` narrows a member, not the bare local — out of scope here.
				continue
			}
			name := cond_tokens[i].lit
			if local := g.locals[name] {
				if local.option_value_type != '' && name !in names {
					names << name
				}
			}
		}
	}
	return names
}

fn (mut g Parser) parse_if() !bool {
	g.next()
	// `if a, b := opt_fn() { ... }` unwraps an option whose value is a multi-return
	// tuple and binds each component. The comma would otherwise trip the condition
	// reader as a parallel assignment, so detect the `name (, name)+ :=` prefix here.
	if g.tok == .name || (g.tok == .key_shared && g.shared_token_is_identifier(.key_if)) {
		mut probe := g.s
		mut guard_names := [g.lit]
		mut is_multi_guard := false
		for {
			t := probe.scan()
			if t == .comma {
				if probe.scan() !in [.name, .key_shared] {
					break
				}
				guard_names << probe.lit
			} else if t == .decl_assign {
				is_multi_guard = guard_names.len >= 2
				break
			} else {
				break
			}
		}
		if is_multi_guard {
			return g.parse_if_multi_return_guard(guard_names)
		}
	}
	// A guard (or an interpolation inside one) may read a field of a subject narrowed by
	// an earlier `is` in the SAME condition (`if x is T && '${x.field}' !in m`). The streaming
	// reader renders that field — including ones buried in an `or`-block synthetic like
	// `(m[x.field] or {})` that later top-level narrowing can no longer reach into — before any
	// smart-cast is known. So narrow the leading `is` subjects UP FRONT for the read; on failure
	// (a subject the prescan cannot narrow) retry once without them.
	saved_s := g.s
	saved_tok := g.tok
	saved_lit := g.lit
	mut prescan_registered := g.parse_if_prescan_leading_smartcasts()
	mut condition := g.read_condition_expression([token.Token.semicolon, token.Token.lcbr]) or {
		for prescan_name in prescan_registered {
			g.member_smartcasts.delete(prescan_name)
		}
		prescan_registered = []string{}
		g.s = saved_s
		g.tok = saved_tok
		g.lit = saved_lit
		g.read_condition_expression([token.Token.semicolon, token.Token.lcbr])!
	}
	// The narrowing was only needed while rendering the condition; drop it so the ordinary
	// per-branch smart-cast setup below owns it.
	for prescan_name in prescan_registered {
		g.member_smartcasts.delete(prescan_name)
	}
	if condition.len == 0 {
		return g.unsupported('empty if condition')
	}
	// `if local is Variant { ... }` on a boxed sum-type/interface local smart-casts
	// `local` to the concrete variant inside the then-branch, so its fields and
	// methods resolve. Only the exact `local is Variant` form (no `&&`/`||`) binds.
	cond_tokens := g.last_expression
	mut smartcast_name := ''
	mut smartcast_type := ''
	mut smartcast_tmp := ''
	mut smartcast_boxed_type := ''
	mut smartcast_negated := false
	mut local_start := 0
	if cond_tokens.len > 0 && cond_tokens[0].tok in [.key_mut, .amp] {
		// `if mut local is Variant`: the `mut` renders as a leading `&`/`mut` token.
		local_start = 1
	}
	// A top-level `||` (`x is A || x is B`) means no single variant is guaranteed in the
	// branch, so `x` must not be shadowed to the first tested variant.
	mut condition_has_top_level_or := false
	// A top-level `&&` matters for the negated `!is` fall-through narrowing: `x !is A && x !is B`
	// exits only when `x` is neither, so past the block `x` is `A` OR `B`, not a single variant.
	mut condition_has_top_level_and := false
	mut or_scan_depth := 0
	for item in cond_tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { or_scan_depth++ }
			.rpar, .rsbr, .rcbr { or_scan_depth-- }
			.logical_or {
				if or_scan_depth == 0 {
					condition_has_top_level_or = true
				}
			}
			.and {
				if or_scan_depth == 0 {
					condition_has_top_level_and = true
				}
			}
			else {}
		}
	}
	first_is_negated := cond_tokens.len >= local_start + 2 && cond_tokens[local_start + 1].tok == .not_is
	// A top-level `||` forbids the positive in-branch shadow (`x is A || x is B` guarantees no
	// single variant in the then-branch). But a NEGATED first operand with a terminating block
	// narrows in the FALL-THROUGH instead: `x !is A || … { return }` reaches past the block only
	// when `x` IS `A` (De Morgan), so allow detection there — only the after-block narrowing (which
	// requires `smartcast_negated`) consumes it; the positive shadows stay gated off.
	if (!condition_has_top_level_or || first_is_negated) && cond_tokens.len >= local_start + 3 && cond_tokens[local_start].tok == .name && cond_tokens[local_start + 1].tok in [
		.key_is,
		.not_is,
	] {
		smartcast_negated = cond_tokens[local_start + 1].tok == .not_is
		if local := g.locals[cond_tokens[local_start].lit] {
			boxed := fastc_normalize_inferred_type(local.typ)
			if g.is_boxed_type(boxed) {
				// The variant type spans from just after `is` up to the first top-level
				// `&&`/`||`; a trailing boolean guard (`x is T && cond`) must not be handed
				// to the type resolver, which would otherwise reject the whole slice.
				mut type_end := cond_tokens.len
				mut guard_depth := 0
				for i := local_start + 2; i < cond_tokens.len; i++ {
					match cond_tokens[i].tok {
						.lpar, .lsbr, .lcbr { guard_depth++ }
						.rpar, .rsbr, .rcbr { guard_depth-- }
						.and, .logical_or {
							if guard_depth == 0 {
								type_end = i
								break
							}
						}
						else {}
					}
				}
				if type_end == local_start + 3 && cond_tokens[local_start + 2].tok == .name {
					if variant_key := g.resolve_declared_type_key(cond_tokens[local_start + 2].lit) {
						smartcast_name = cond_tokens[local_start].lit
						smartcast_type = fastc_c_declared_type_name(variant_key)
						smartcast_boxed_type = local.typ
						smartcast_tmp = g.temporary_name('smartcast_subject')
					} else if fastc_primitive_c_type(cond_tokens[local_start + 2].lit) != none {
						// A primitive sum-type variant (`if v is u64`) narrows to that
						// primitive; its own spelling is the smart-cast C type and tag.
						smartcast_name = cond_tokens[local_start].lit
						smartcast_type = cond_tokens[local_start + 2].lit
						smartcast_boxed_type = local.typ
						smartcast_tmp = g.temporary_name('smartcast_subject')
					}
				} else if resolved_target := g.type_from_expression_tokens(cond_tokens[local_start + 2..type_end]) {
					// A variant target that spans several tokens: a composite type
					// (`x is []string` / `x is map[K]V`) or a module-qualified variant
					// (`x is ast.IfExpr`). Composites must be registered; a qualified
					// variant already resolves to its declared C type.
					target := fastc_normalize_inferred_type(resolved_target)
					if target.starts_with('Array_') || target.starts_with('Map_') {
						fastc_register_composite_type(target, mut g.composite_types)
						smartcast_name = cond_tokens[local_start].lit
						smartcast_type = target
						smartcast_boxed_type = local.typ
						smartcast_tmp = g.temporary_name('smartcast_subject')
					} else if target != '' {
						smartcast_name = cond_tokens[local_start].lit
						smartcast_type = target
						smartcast_boxed_type = local.typ
						smartcast_tmp = g.temporary_name('smartcast_subject')
					}
				}
			}
		}
	}
	if smartcast_name != '' && local_start == 1 {
		// `if mut x is T`: the leading `mut`/`&` defeats the general `is` rendering, so
		// build the runtime tag check explicitly here.
		access := if smartcast_boxed_type.ends_with('*') { '->' } else { '.' }
		condition = '((${g.local_c_name(smartcast_name)}${access}_typ) == __v_typeid_${smartcast_type})'
	}
	// A boxed member (`if mut holder.writer is File`, or a conjunction such as
	// `a.x is T && b.y is U`) needs the same concrete view as a boxed local, but its
	// qualified source spelling cannot be shadowed. Keep a branch-scoped member-path
	// rewrite per test instead, backed by a pointer to each boxed object.
	member_smartcast_plans, member_smartcast_condition := g.detect_member_smartcasts(cond_tokens, condition)
	if member_smartcast_condition != '' {
		condition = member_smartcast_condition
	}
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	mut guard_is_mut := false
	mut guard_function := FastcStructField{}
	mut guard_function_source := ''
	mut guard_erased_generic := false
	// `if mut x := opt {` / `if mut x := map[k] {`: the leading `mut` (which renders to
	// a `&` token here) shifts the name one token to the right. `& <name> :=` at the
	// start of an if condition can only come from such a `mut` guard.
	guard_name_index := if g.selfhost && g.last_expression.len >= 1 && g.last_expression[0].tok in [
		token.Token.key_mut,
		token.Token.amp,
	] {
		1
	} else {
		0
	}
	if g.selfhost && g.last_expression.len >= guard_name_index + 3 && g.last_expression[guard_name_index].tok == .name && g.last_expression[guard_name_index + 1].tok == .decl_assign {
		right_tokens := g.last_expression[guard_name_index + 2..]
		if function_field := g.optional_function_field_for_expression(right_tokens) {
			guard_name = g.last_expression[guard_name_index].lit
			guard_type = 'voidptr'
			guard_is_mut = guard_name_index == 1
			guard_function = function_field
			guard_function_source = g.render_member_receiver(right_tokens) or { '' }
			if guard_function_source != '' {
				condition = '${guard_function_source} != NULL'
				g.last_expression_type = 'bool'
			}
		} else if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[guard_name_index].lit
			guard_type = map_lookup.typ
			guard_is_mut = guard_name_index == 1
			guard_option = g.temporary_name('if_guard')
			g.write_line('Option ${guard_option} = (${map_lookup.source});')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		} else if array_lookup := g.render_array_lookup_option_expression(right_tokens) {
			// `if x := arr[i] {` succeeds only when the index is in bounds, so the array
			// access must lower to the bounds-checked Option form, not a raw element read.
			guard_name = g.last_expression[guard_name_index].lit
			guard_type = array_lookup.typ
			guard_is_mut = guard_name_index == 1
			guard_option = g.temporary_name('if_guard')
			g.write_line('Option ${guard_option} = (${array_lookup.source});')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[guard_name_index].lit
				guard_type = option_type
				guard_is_mut = guard_name_index == 1
				guard_option = g.temporary_name('if_guard')
				guard_erased_generic = g.erased_generic_option_value_type_for_expression(right_tokens) != none
				right_source := condition.all_after(':=').trim_space()
				g.write_line('Option ${guard_option} = (${right_source});')
				condition = '${guard_option}.state == 0'
				g.last_expression_type = 'bool'
			}
		}
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	if smartcast_name != '' && !smartcast_negated {
		// Copy the boxed value out before the branch so the shadowing cast below
		// does not reference its own uninitialized declaration.
		g.write_line('${smartcast_boxed_type} ${smartcast_tmp} = ${g.local_c_name(smartcast_name)};')
	}
	for plan in member_smartcast_plans {
		boxed_zero := if plan.boxed_type.ends_with('*') {
			'NULL'
		} else {
			'(${plan.boxed_type}){0}'
		}
		g.write_line('${plan.boxed_type} ${plan.boxed_tmp} = ${boxed_zero};')
	}
	g.write_line('if (${condition}) {')
	g.indent++
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		if guard_function_source != '' {
			parameters := if guard_function.fn_parameter_types.len == 0 {
				'void'
			} else {
				guard_function.fn_parameter_types.join(', ')
			}
			return_type := if guard_function.fn_return_type == '' {
				'void'
			} else {
				guard_function.fn_return_type
			}
			name := fastc_c_identifier(guard_name)
			g.write_line('${return_type} (*${name})(${parameters}) = (${return_type} (*)(${parameters}))(${guard_function_source});')
			g.locals[guard_name] = FastcLocal{
				is_mut: guard_is_mut
				typ: 'voidptr'
				fn_return_type: return_type
				fn_option_value_type: guard_function.fn_option_value_type
			}
		} else if guard_erased_generic {
			name := fastc_c_identifier(guard_name)
			if guard_type.ends_with('*') {
				g.write_line('${guard_type} ${name} = *((${guard_type} *)${guard_option}.data);')
			} else {
				g.write_line('${guard_type} ${name} = **((${guard_type} **)${guard_option}.data);')
			}
			g.locals[guard_name] = FastcLocal{
				is_mut: guard_is_mut
				typ: guard_type
			}
		} else {
			function_alias := g.functions[guard_type] or { FastcFunctionSignature{} }
			g.write_line('${guard_type} ${fastc_c_identifier(guard_name)} = *((${guard_type} *)${guard_option}.data);')
			g.locals[guard_name] = FastcLocal{
				is_mut: guard_is_mut
				typ: guard_type
				fn_return_type: function_alias.return_type
				fn_option_value_type: function_alias.option_type
			}
		}
	}
	previous_smartcast := g.locals[smartcast_name] or { FastcLocal{} }
	had_smartcast := smartcast_name in g.locals
	if smartcast_name != '' && !smartcast_negated {
		// A pointer subject (`node &ast.Node`) reaches its boxed payload through `->`.
		tmp_access := if smartcast_boxed_type.ends_with('*') { '->' } else { '.' }
		smartcast_is_reference := local_start == 1 || smartcast_boxed_type.ends_with('*') || cond_tokens[local_start].is_mut_argument
		// The narrowed value goes into a UNIQUELY named temporary, not a shadow of the subject's
		// own C name, so a `defer` body (rendered at function scope) still binds the original.
		branch_cast := g.temporary_name('if_cast')
		branch_type := if smartcast_is_reference { smartcast_type + '*' } else { smartcast_type }
		branch_value := if smartcast_is_reference {
			'(${smartcast_type} *)${smartcast_tmp}${tmp_access}_object'
		} else {
			'*((${smartcast_type} *)${smartcast_tmp}${tmp_access}_object)'
		}
		g.write_line('${branch_type} ${branch_cast} = ${branch_value};')
		g.locals[smartcast_name] = FastcLocal{
			is_mut: previous_smartcast.is_mut
			is_reference: smartcast_is_reference
			typ: branch_type
			c_name: branch_cast
			smartcast_origin_type: smartcast_boxed_type
			smartcast_origin_source: smartcast_tmp
		}
	}
	mut previous_member_smartcasts := map[string]FastcMemberSmartcast{}
	mut had_member_smartcasts := map[string]bool{}
	mut previous_local_plans := map[string]FastcLocal{}
	mut had_local_plans := map[string]bool{}
	for plan in member_smartcast_plans {
		if plan.path == smartcast_name {
			// The bare local is also shadowed as a concrete-variant value below, which
			// already covers both `${name}` and `${name}.field`; a member-path pointer
			// rewrite would instead expose it as a pointer to later statements.
			continue
		}
		plan_access := if plan.boxed_type.ends_with('*') { '->' } else { '.' }
		if plan.path in g.locals && !plan.path.contains('.') && !plan.path.contains('[') {
			// A second bare local narrowed in the same condition (`a is X && b is Y`) must be
			// shadowed as a concrete-variant VALUE, exactly like the primary subject above.
			// A member-smartcast pointer is only consulted by member-access reads, so plain
			// reads of the local (an arithmetic operand, a cast) would otherwise keep the raw
			// boxed value.
			if plan.path !in previous_local_plans {
				had_local_plans[plan.path] = plan.path in g.locals
				previous_local_plans[plan.path] = g.locals[plan.path] or { FastcLocal{} }
			}
			plan_shadow := g.temporary_name('if_cast')
			plan_is_reference := plan.boxed_type.ends_with('*')
			plan_type := if plan_is_reference { plan.type_c + '*' } else { plan.type_c }
			plan_value := if plan_is_reference {
				'(${plan.type_c} *)${plan.boxed_tmp}${plan_access}_object'
			} else {
				'*((${plan.type_c} *)${plan.boxed_tmp}${plan_access}_object)'
			}
			g.write_line('${plan_type} ${plan_shadow} = ${plan_value};')
			previous_plan_local := g.locals[plan.path] or { FastcLocal{} }
			g.locals[plan.path] = FastcLocal{
				is_mut: previous_plan_local.is_mut
				is_reference: plan_is_reference
				typ: plan_type
				c_name: plan_shadow
				smartcast_origin_type: plan.boxed_type
				smartcast_origin_source: plan.boxed_tmp
			}
			continue
		}
		if plan.path !in previous_member_smartcasts {
			had_member_smartcasts[plan.path] = plan.path in g.member_smartcasts
			previous_member_smartcasts[plan.path] = g.member_smartcasts[plan.path] or {
				FastcMemberSmartcast{}
			}
		}
		g.write_line('${plan.type_c} *${plan.member_tmp} = (${plan.type_c} *)${plan.boxed_tmp}${plan_access}_object;')
		g.member_smartcasts[plan.path] = FastcMemberSmartcast{
			typ: plan.type_c + '*'
			source: plan.member_tmp
		}
	}
	// `if opt != none { opt.field }`: narrow each option local guaranteed non-none in the
	// branch to its wrapped value, read through the erased `Option`'s `.data` payload.
	if !condition_has_top_level_or {
		for opt_name in g.detect_option_none_narrowings(cond_tokens) {
			if opt_name == smartcast_name || opt_name in previous_member_smartcasts {
				continue
			}
			if local := g.locals[opt_name] {
				base := local.option_value_type
				had_member_smartcasts[opt_name] = opt_name in g.member_smartcasts
				previous_member_smartcasts[opt_name] = g.member_smartcasts[opt_name] or {
					FastcMemberSmartcast{}
				}
				g.member_smartcasts[opt_name] = FastcMemberSmartcast{
					typ: base
					source: '(*((${base} *)${fastc_c_identifier(opt_name)}.data))'
				}
			}
		}
	}
	then_terminates := g.parse_block_body()!
	if guard_name != '' {
		if had_guard {
			g.locals[guard_name] = previous_guard
		} else {
			g.locals.delete(guard_name)
		}
	}
	if smartcast_name != '' {
		if had_smartcast {
			g.locals[smartcast_name] = previous_smartcast
		} else {
			g.locals.delete(smartcast_name)
		}
	}
	for path, present in had_member_smartcasts {
		if present {
			g.member_smartcasts[path] = previous_member_smartcasts[path]
		} else {
			g.member_smartcasts.delete(path)
		}
	}
	for path, present in had_local_plans {
		if present {
			g.locals[path] = previous_local_plans[path]
		} else {
			g.locals.delete(path)
		}
	}
	g.indent--
	if g.tok != .key_else {
		g.write_line('}')
		if smartcast_negated && smartcast_name != '' && then_terminates && !condition_has_top_level_and && !previous_smartcast.is_mut {
			// `if x !is Variant { return }`: reaching past the block proves `x` IS the
			// variant, so narrow it to a value shadow scoped to the rest of the block,
			// mirroring the positive `is` branch but in the fall-through path. A `mut`
			// subject is excluded: a later `x = …` reassignment widens the smart-cast in V,
			// but the value shadow would keep pointing at the stale narrowed variant.
			access := if smartcast_boxed_type.ends_with('*') { '->' } else { '.' }
			branch_cast := g.temporary_name('if_cast')
			g.write_line('${smartcast_type} ${branch_cast} = *((${smartcast_type} *)${g.local_c_name(smartcast_name)}${access}_object);')
			g.set_scoped_local(smartcast_name, FastcLocal{
				is_mut: previous_smartcast.is_mut
				typ: smartcast_type
				c_name: branch_cast
				smartcast_origin_type: smartcast_boxed_type
				smartcast_origin_source: g.local_c_name(smartcast_name)
			})
		}
		return false
	}
	g.next()
	if g.tok == .key_if {
		g.write_line('} else {')
		g.indent++
		previous_err := g.locals['err'] or { FastcLocal{} }
		had_err := 'err' in g.locals
		if guard_option != '' {
			g.write_line('IError err = ${guard_option}.err;')
			g.locals['err'] = FastcLocal{
				typ: 'IError'
			}
		}
		else_terminates := g.parse_if()!
		if guard_option != '' {
			if had_err {
				g.locals['err'] = previous_err
			} else {
				g.locals.delete('err')
			}
		}
		g.indent--
		g.write_line('}')
		return then_terminates && else_terminates
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	previous_err := g.locals['err'] or { FastcLocal{} }
	had_err := 'err' in g.locals
	if guard_option != '' {
		g.write_line('IError err = ${guard_option}.err;')
		g.locals['err'] = FastcLocal{
			typ: 'IError'
		}
	}
	else_terminates := g.parse_block_body()!
	if guard_option != '' {
		if had_err {
			g.locals['err'] = previous_err
		} else {
			g.locals.delete('err')
		}
	}
	g.indent--
	g.write_line('}')
	return then_terminates && else_terminates
}

// parse_if_multi_return_guard lowers `if a, b := opt_fn() { ... }`: the option's
// value is a multi-return tuple, so on success the boxed `MultiReturn` is copied out
// of the option and each component is bound as a local inside the then-branch.
fn (mut g Parser) parse_if_multi_return_guard(names []string) !bool {
	for g.tok != .decl_assign && g.tok != .eof {
		g.next()
	}
	g.expect(.decl_assign)!
	rhs := g.read_expression([token.Token.semicolon, token.Token.lcbr])!
	if rhs == '' {
		return g.unsupported('empty multi-return option guard')
	}
	component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len < names.len {
		return g.unsupported('multi-return option guard component types')
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	guard_option := g.temporary_name('if_guard')
	g.write_line('Option ${guard_option} = (${rhs});')
	g.write_line('if (${guard_option}.state == 0) {')
	g.indent++
	multi_return := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${multi_return} = *((MultiReturn *)${guard_option}.data);')
	mut previous_locals := []FastcLocal{cap: names.len}
	mut had_locals := []bool{cap: names.len}
	for i, name in names {
		existing := name in g.locals
		previous_locals << (g.locals[name] or { FastcLocal{} })
		had_locals << existing
		if name == '_' {
			continue
		}
		component_type := fastc_normalize_inferred_type(component_types[i])
		c_name := fastc_c_identifier(name)
		g.write_line('${component_type} ${c_name} = (${component_type}){0};')
		g.write_line('memcpy(&${c_name}, V_FASTC_MULTI_SOURCE(${multi_return}.values[${i}], sizeof(${c_name})), sizeof(${c_name}));')
		g.locals[name] = FastcLocal{
			typ: component_type
		}
	}
	then_terminates := g.parse_block_body()!
	for i, name in names {
		if had_locals[i] {
			g.locals[name] = previous_locals[i]
		} else {
			g.locals.delete(name)
		}
	}
	g.indent--
	if g.tok != .key_else {
		g.write_line('}')
		return false
	}
	g.next()
	if g.tok == .key_if {
		g.write_line('} else {')
		g.indent++
		else_terminates := g.parse_if()!
		g.indent--
		g.write_line('}')
		return then_terminates && else_terminates
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	else_terminates := g.parse_block_body()!
	g.indent--
	g.write_line('}')
	return then_terminates && else_terminates
}

fn (g &Parser) if_starts_final_block_expression() bool {
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut prev := token.Token.unknown
	mut tok := lookahead.scan()
	for {
		// Scan to the block-opening `{`, skipping any `unsafe { … }` that appears in the
		// condition itself (`if x != unsafe { nil } { … }`), whose brace would otherwise
		// be mistaken for the block.
		for tok != .eof {
			if tok == .lcbr {
				if prev == .key_unsafe {
					tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or {
						return false
					}
					prev = .rcbr
					continue
				}
				break
			}
			prev = tok
			tok = lookahead.scan()
		}
		if tok != .lcbr {
			return false
		}
		tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
		for tok == .semicolon {
			tok = lookahead.scan()
		}
		if tok != .key_else {
			return false
		}
		tok = lookahead.scan()
		for tok == .semicolon {
			tok = lookahead.scan()
		}
		if tok == .key_if {
			prev = .key_if
			tok = lookahead.scan()
			continue
		}
		if tok != .lcbr {
			return false
		}
		tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
		break
	}
	for tok == .semicolon {
		tok = lookahead.scan()
	}
	return tok == .rcbr
}

// FastcMemberSmartcastPlan is one `member.path is Type` test detected in an `if`
// condition. A condition may carry several (`a.x is T && b.y is U`); each one gets
// its boxed subject captured before the branch and a concrete pointer inside it.
struct FastcMemberSmartcastPlan {
	path       string
	type_c     string
	boxed_type string
	source     string
	boxed_tmp  string
	member_tmp string
}

// fastc_strip_outer_parens removes fully-matching outer parentheses from a rendered C
// expression (the streaming reader wraps a whole condition in one pair), so its top-level
// operators become visible to a split.
fn fastc_strip_outer_parens(source string) string {
	mut s := source.trim_space()
	for s.len >= 2 && s[0] == `(` && s[s.len - 1] == `)` {
		mut depth := 0
		mut quote := u8(0)
		mut escaped := false
		mut matches := true
		for i := 0; i < s.len; i++ {
			c := s[i]
			if quote != 0 {
				if escaped {
					escaped = false
				} else if c == `\\` {
					escaped = true
				} else if c == quote {
					quote = 0
				}
				continue
			}
			if c == `'` || c == `"` {
				quote = c
			} else if c == `(` {
				depth++
			} else if c == `)` {
				depth--
				if depth == 0 && i != s.len - 1 {
					matches = false
					break
				}
			}
		}
		if !matches {
			break
		}
		s = s[1..s.len - 1].trim_space()
	}
	return s
}

// fastc_split_top_level_c_and splits a rendered C condition on its top-level `&&`
// operators, respecting parentheses/brackets/braces and string and char literals, so a
// guard conjunct can reuse its already-rendered spelling.
fn fastc_split_top_level_c_and(source string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	mut quote := u8(0)
	mut escaped := false
	for i := 0; i < source.len; i++ {
		c := source[i]
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		if c == `'` || c == `"` {
			quote = c
		} else if c in [`(`, `[`, `{`] {
			depth++
		} else if c in [`)`, `]`, `}`] {
			depth--
		} else if depth == 0 && c == `&` && i + 1 < source.len && source[i + 1] == `&` {
			parts << source[start..i]
			i++
			start = i + 1
		}
	}
	parts << source[start..]
	return parts
}

// fastc_split_top_level_and splits condition tokens on top-level `&&` operators.
fn fastc_split_top_level_and(tokens []FastcExpressionToken) [][]FastcExpressionToken {
	mut parts := [][]FastcExpressionToken{}
	mut depth := 0
	mut start := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.and {
				if depth == 0 {
					parts << tokens[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << tokens[start..]
	return parts
}

// fastc_first_top_level_is returns the index of the first top-level `is` in tokens.
fn fastc_first_top_level_is(tokens []FastcExpressionToken) int {
	mut depth := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.key_is {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return -1
}

// render_guard_boolean_expression lowers a compound boolean guard conjunct (`(op == .decl
// && x.is_mut) || op == .assign`) that reads narrowed subjects and enum shorthands. Splitting
// on the top-level `||`/`&&` lets each leaf resolve through render_guard_comparison (which
// applies the enum-shorthand type) or render_member_receiver (the smart-cast pointer), which
// the raw renderer alone loses when re-rendering the conjunct outside the streaming reader.
fn (g &Parser) render_guard_boolean_expression(tokens []FastcExpressionToken) ?string {
	inner := fastc_strip_paren_tokens(tokens)
	if inner.len == 0 {
		return none
	}
	if idx := fastc_top_level_boolean_split(inner, .logical_or) {
		left := g.render_guard_boolean_expression(inner[..idx]) or { return none }
		right := g.render_guard_boolean_expression(inner[idx + 1..]) or { return none }
		return '((${left}) || (${right}))'
	}
	if idx := fastc_top_level_boolean_split(inner, .and) {
		left := g.render_guard_boolean_expression(inner[..idx]) or { return none }
		right := g.render_guard_boolean_expression(inner[idx + 1..]) or { return none }
		return '((${left}) && (${right}))'
	}
	if inner[0].tok == .not {
		operand := g.render_guard_boolean_expression(inner[1..]) or { return none }
		return '(!(${operand}))'
	}
	if comparison := g.render_guard_comparison(inner) {
		return comparison
	}
	if boolean_is := g.render_boolean_is_expression(inner) {
		return boolean_is.source
	}
	if member := g.render_member_receiver(inner) {
		return member
	}
	if raw := g.render_raw_expression_tokens(inner) {
		if special := g.render_special_expression(inner, raw) {
			return special.source
		}
		return raw
	}
	return none
}

// detect_member_smartcasts finds every `member.path is Type` test in a (possibly
// `&&`-conjoined) condition, records a plan per test, and rewrites `condition` to
// the conjunction of the runtime type checks plus any boolean guards. Returns an
// empty list (leaving `condition` untouched) when the condition is not a member
// smartcast, so simple-local smartcasts and ordinary conditions are unaffected.
fn (mut g Parser) detect_member_smartcasts(cond_tokens []FastcExpressionToken, rendered_condition string) ([]FastcMemberSmartcastPlan, string) {
	conjuncts := fastc_split_top_level_and(cond_tokens)
	rendered_conjuncts := fastc_split_top_level_c_and(fastc_strip_outer_parens(rendered_condition))
	aligned := rendered_conjuncts.len == conjuncts.len
	mut plans := []FastcMemberSmartcastPlan{}
	mut condition_parts := []string{}
	mut previous := map[string]FastcMemberSmartcast{}
	mut previously_present := map[string]bool{}
	mut ok := true
	for i, conjunct in conjuncts {
		mut cstart := 0
		if conjunct.len > 0 && conjunct[0].tok in [.key_mut, .amp] {
			cstart = 1
		}
		is_idx := fastc_first_top_level_is(conjunct)
		if is_idx > cstart && is_idx + 1 < conjunct.len {
			left_tokens := conjunct[cstart..is_idx]
			// A member/local subject (`x.f is T`, `x is T`, or an indexed chain
			// `right.args[0].expr is T`) is a smart-cast; a non-chain left operand
			// (`unalias(x) is T`) is instead a boolean guard, handled below.
			if path := fastc_indexed_member_chain_path(left_tokens) {
				mut left_type := fastc_normalize_inferred_type(g.infer_expression_type(left_tokens) or {
					''
				})
				if member_type := g.infer_member_access_type(left_tokens, 0, left_tokens.len) {
					left_type = fastc_normalize_inferred_type(member_type)
				}
				target_type := g.type_from_expression_tokens(conjunct[is_idx + 1..]) or { '' }
				if left_type != '' && target_type != '' && g.is_boxed_type(left_type) {
					if left_source := g.render_member_receiver(left_tokens) {
						normalized_target := fastc_normalize_inferred_type(target_type)
						boxed_tmp := g.temporary_name('smartcast_subject')
						member_tmp := g.temporary_name('smartcast_member')
						// A pointer subject (`node &ast.Node`, or a nested variant already
						// narrowed to `Variant*`) reaches its boxed `_typ`/`_object` through
						// `->`, not `.`.
						access := if left_type.ends_with('*') { '->' } else { '.' }
						plans << FastcMemberSmartcastPlan{
							path: path
							type_c: normalized_target
							boxed_type: left_type
							source: left_source
							boxed_tmp: boxed_tmp
							member_tmp: member_tmp
						}
						// Assign the boxed temporary inside this conjunct. Hoisting the member read
						// before the whole condition would violate short-circuiting for guards such
						// as `args.len == 1 && args[0].expr is CallExpr`.
						condition_parts << '(((${boxed_tmp} = (${left_source}))${access}_typ) == __v_typeid_${normalized_target})'
						// Register temporarily so a following guard conjunct reads the
						// concrete variant; restored below before the caller sets it up.
						if path !in previous {
							previously_present[path] = path in g.member_smartcasts
							previous[path] = g.member_smartcasts[path] or { FastcMemberSmartcast{} }
						}
						g.member_smartcasts[path] = FastcMemberSmartcast{
							typ: normalized_target + '*'
							source: '((${normalized_target} *)${boxed_tmp}${access}_object)'
						}
						continue
					}
				}
				ok = false
				break
			}
		}
		// A parenthesized group that itself narrows (`(x.f is T && x.f.g !in [...])`) must be
		// rendered through the flow-sensitive narrowing renderer so its own left `is` conjunct
		// registers the smart-cast its right operand reads. render_boolean_is_expression below
		// splits the group but cannot register that narrowing (it is non-mut), leaving the
		// membership/comparison subject read through the boxed value.
		if g.boolean_expression_has_narrowing(conjunct[cstart..]) {
			if guard := g.render_narrowing_boolean_expression(conjunct[cstart..]) {
				condition_parts << '(${guard})'
				continue
			}
		}
		// A guard built from `is` tests on non-name operands (`unalias(x) is Array`),
		// possibly combined with `&&`/`||`, reads through the earlier smart-casts. The
		// streaming reader cannot render such a test, so try it before reusing the reader's
		// aligned rendering.
		if guard := g.render_boolean_is_expression(conjunct[cstart..]) {
			condition_parts << '(${guard.source})'
			continue
		}
		// A non-`is` conjunct is a boolean guard. When it does NOT touch a narrowed
		// subject, the streaming reader's own rendering of this conjunct (which already
		// resolved enum shorthands and constants) is reused by splitting the rendered
		// condition the same way as the tokens.
		uses_smartcast := g.expression_uses_member_smartcast(conjunct[cstart..])
		if !uses_smartcast && aligned {
			condition_parts << '(${rendered_conjuncts[i].trim_space()})'
			continue
		}
		// A comparison guard that reads a narrowed subject through a plain member/index chain
		// (`x.args[0].expr.typ > 0`) must be rendered before the general member receiver path.
		// Besides indexed chains, this resolves an enum shorthand such as
		// `left is PrefixExpr && left.op == .mul` against the narrowed field's enum type.
		// A call-bearing operand (`sym(x).kind != .placeholder`) is left to the raw/special path,
		// whose call handling the comparison renderer does not reproduce.
		if uses_smartcast && !fastc_expression_tokens_contain(conjunct[cstart..], .lpar) {
			if guard := g.render_guard_comparison(conjunct[cstart..]) {
				condition_parts << '(${guard})'
				continue
			}
		}
		// A method call on a narrowed member (`right.typ.has_flag(.generic)`) needs
		// both method lowering and the active smartcast receiver. The general member
		// renderer below only handles the receiver chain and can leave the call raw.
		if uses_smartcast && fastc_tokens_are_plain_call(fastc_strip_paren_tokens(conjunct[cstart..]))
			&& !fastc_expression_tokens_contain(conjunct[cstart..], .key_as) {
			if raw := g.render_raw_expression_tokens(conjunct[cstart..]) {
				if special := g.render_special_expression(conjunct[cstart..], raw) {
					condition_parts << '(${special.source})'
					continue
				}
			}
		}
		// A guard that reads the narrowed subject (`expr.kind == .constant`) must be
		// re-rendered so that access goes through the concrete-variant pointer: a plain
		// member chain (`x.f.g`), a parenthesized cast field access (`(x.f as T).g`), or
		// the general expression path (a call/comparison).
		if guard := g.render_member_receiver(conjunct[cstart..]) {
			condition_parts << '(${guard})'
			continue
		}
		if guard := g.render_as_cast_member_access(conjunct[cstart..]) {
			condition_parts << '(${guard.source})'
			continue
		}
		// An embedded `(x as T)` cast in the guard (a method-call receiver `(x as T).m()`)
		// is lowered to a synthetic atom first so the `as` does not reach the raw renderer.
		if rewritten := g.rewrite_embedded_as_casts(conjunct[cstart..]) {
			if raw := g.render_raw_expression_tokens(rewritten) {
				if special := g.render_special_expression(rewritten, raw) {
					condition_parts << '(${special.source})'
				} else {
					condition_parts << '(${raw})'
				}
				continue
			}
		}
		// A compound boolean guard (`(op == .decl && x.is_mut) || op == .assign`) must be
		// split so each leaf resolves its enum shorthand and narrowed field reads; the raw
		// path below would leave `.decl` and the boxed member access unresolved.
		strip := fastc_strip_paren_tokens(conjunct[cstart..])
		if fastc_top_level_boolean_split(strip, .logical_or) != none || fastc_top_level_boolean_split(strip, .and) != none {
			if guard := g.render_guard_boolean_expression(conjunct[cstart..]) {
				condition_parts << '(${guard})'
				continue
			}
		}
		if raw := g.render_raw_expression_tokens(conjunct[cstart..]) {
			if special := g.render_special_expression(conjunct[cstart..], raw) {
				condition_parts << '(${special.source})'
				continue
			}
		}
		if guard := g.render_guard_comparison(conjunct[cstart..]) {
			condition_parts << '(${guard})'
			continue
		}
		if aligned {
			condition_parts << '(${rendered_conjuncts[i].trim_space()})'
			continue
		}
		ok = false
		break
	}
	for path, present in previously_present {
		if present {
			g.member_smartcasts[path] = previous[path]
		} else {
			g.member_smartcasts.delete(path)
		}
	}
	if !ok || plans.len == 0 {
		return []FastcMemberSmartcastPlan{}, ''
	}
	return plans, condition_parts.join(' && ')
}

// match_starts_final_block_expression reports whether a `match` at the current
// token is the final expression of the enclosing block (its `{ ... }` body is
// immediately followed by the block's closing `}`), so it is a value rather than a
// standalone statement. Mirrors if_starts_final_block_expression for `match`.
fn (g &Parser) match_starts_final_block_expression() bool {
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := lookahead.scan()
	// Skip the match subject up to the opening `{` of the match body.
	for tok !in [.lcbr, .eof] {
		tok = lookahead.scan()
	}
	if tok != .lcbr {
		return false
	}
	tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
	for tok == .semicolon {
		tok = lookahead.scan()
	}
	return tok == .rcbr
}

fn fastc_option_success_expression(value_type string, expression string) string {
	base := fastc_normalize_inferred_type(value_type)
	return '(Option){.data=${fastc_box_expression(base, expression)}, .state=0}'
}

fn fastc_box_expression(value_type string, expression string) string {
	return '({ ${value_type} __v_fastc_box_value = (${expression}); v_fastc_interface_box(&__v_fastc_box_value, sizeof(${value_type})); })'
}

// read_if_expression_multi_return_guard lowers `if a, b := opt_multi() { x } else { y }`
// used as an expression: the option is unwrapped, its multi-return components bound in
// the then-branch, and the selected branch's value produced as a statement-expression.
fn (mut g Parser) read_if_expression_multi_return_guard(names []string, branch_expected_type string, outer_expected_type string) !string {
	for g.tok != .decl_assign && g.tok != .eof {
		g.next()
	}
	g.expect(.decl_assign)!
	rhs := g.read_expression([token.Token.semicolon, token.Token.lcbr])!
	component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len < names.len {
		return g.unsupported('multi-return option guard component types')
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	guard_option := g.temporary_name('if_guard')
	multi_return := g.temporary_name('multi_return')
	mut binds := 'MultiReturn ${multi_return} = *((MultiReturn *)${guard_option}.data); '
	mut previous_locals := []FastcLocal{}
	mut had_locals := []bool{}
	for i, name in names {
		had_locals << (name in g.locals)
		previous_locals << (g.locals[name] or { FastcLocal{} })
		if name == '_' {
			continue
		}
		component_type := fastc_normalize_inferred_type(component_types[i])
		c_name := fastc_c_identifier(name)
		binds += '${component_type} ${c_name} = (${component_type}){0}; memcpy(&${c_name}, V_FASTC_MULTI_SOURCE(${multi_return}.values[${i}], sizeof(${c_name})), sizeof(${c_name})); '
		g.locals[name] = FastcLocal{
			typ: component_type
		}
	}
	g.expected_expression_type = branch_expected_type
	then_expr := if g.tok == .key_return {
		g.read_return_expression_branch()!
	} else {
		g.read_block_expression_value()!
	}
	then_type := g.last_expression_type
	for i, name in names {
		if had_locals[i] {
			g.locals[name] = previous_locals[i]
		} else {
			g.locals.delete(name)
		}
	}
	g.skip_semicolons()
	g.expect(.rcbr)!
	if g.tok != .key_else {
		return g.unsupported('if expression without `else`')
	}
	g.next()
	g.expected_expression_type = branch_expected_type
	mut else_expr := ''
	if g.tok == .key_if {
		else_expr = g.read_if_expression()!
	} else {
		g.expect(.lcbr)!
		else_expr = if g.tok == .key_return {
			g.read_return_expression_branch()!
		} else {
			g.read_block_expression_value()!
		}
		g.skip_semicolons()
		g.expect(.rcbr)!
	}
	else_type := g.last_expression_type
	result_type := if then_type != '' {
		fastc_normalize_inferred_type(then_type)
	} else if else_type != '' {
		fastc_normalize_inferred_type(else_type)
	} else if outer_expected_type != '' {
		outer_expected_type
	} else {
		'int'
	}
	g.last_expression_type = result_type
	g.expected_expression_type = outer_expected_type
	g.last_expression = []FastcExpressionToken{}
	result_var := g.temporary_name('if_result')
	return '({ Option ${guard_option} = (${rhs}); ${result_type} ${result_var}; if (${guard_option}.state == 0) { ${binds}${result_var} = (${then_expr}); } else { ${result_var} = (${else_expr}); } ${result_var}; })'
}

// fastc_plan_is_bare_local reports whether a member-smart-cast plan narrows a bare local
// variable (not a member/indexed chain), which is shadowed as a concrete-variant VALUE rather
// than exposed as a member-smart-cast pointer.
fn (g &Parser) fastc_plan_is_bare_local(plan FastcMemberSmartcastPlan) bool {
	return plan.path in g.locals && !plan.path.contains('.') && !plan.path.contains('[')
}

// fastc_common_sum_type returns a declared sum type that has BOTH `a` and `b` among its leaf
// variants (used to unify differing if/match branch variants into one boxed value), or none.
fn (g &Parser) fastc_common_sum_type(a string, b string) ?string {
	an := fastc_trim_pointer_suffix(fastc_normalize_inferred_type(a))
	bn := fastc_trim_pointer_suffix(fastc_normalize_inferred_type(b))
	if an == '' || bn == '' || an == bn {
		return none
	}
	// Only unify DECLARED struct/enum variants. Two primitive branches (`abs64(x)` vs `u32(0)`)
	// can share a numeric sum type in the builtin, but boxing them without an explicit expected
	// type would corrupt ordinary numeric if-expressions.
	if an in ['bool', 'string', 'rune', 'voidptr', 'char'] || bn in ['bool', 'string', 'rune',
		'voidptr', 'char'] || fastc_is_numeric_expression_type(an) || fastc_is_numeric_expression_type(bn) {
		return none
	}
	for sum_type, _ in g.sum_types {
		leaves := g.sum_type_leaf_variants(sum_type)
		if an in leaves && bn in leaves {
			return sum_type
		}
	}
	return none
}

fn (mut g Parser) read_if_expression() !string {
	outer_expected_type := g.expected_expression_type
	branch_expected_type := if outer_expected_type != '' {
		outer_expected_type
	} else if g.declared_kinds[g.semantic_type_key(g.return_type)] == .enum_ {
		g.return_type
	} else {
		''
	}
	g.expect(.key_if)!
	// `pos := if a, _, _ := opt_multi() { … } else { … }`: a multi-return option guard
	// used as an if-expression value.
	if g.tok == .name {
		mut probe := g.s
		mut guard_names := [g.lit]
		mut is_multi_guard := false
		for {
			t := probe.scan()
			if t == .comma {
				if probe.scan() !in [.name, .key_shared] {
					break
				}
				guard_names << probe.lit
			} else if t == .decl_assign {
				is_multi_guard = guard_names.len >= 2
				break
			} else {
				break
			}
		}
		if is_multi_guard {
			return g.read_if_expression_multi_return_guard(guard_names, branch_expected_type, outer_expected_type)
		}
	}
	g.expected_expression_type = ''
	mut condition := g.read_condition_expression([token.Token.semicolon, token.Token.lcbr])!
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	mut guard_source := ''
	if g.selfhost && g.last_expression.len >= 3 && g.last_expression[0].tok == .name && g.last_expression[1].tok == .decl_assign {
		right_tokens := g.last_expression[2..]
		if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[0].lit
			guard_type = map_lookup.typ
			guard_source = map_lookup.source
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[0].lit
				guard_type = option_type
				guard_source = condition.all_after(':=').trim_space()
			}
		}
		if guard_name != '' {
			guard_option = g.temporary_name('if_guard')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		}
	}
	// A boxed-member smart-cast in an if-*expression* condition (`if sym.info is FnType {
	// &sym.info.func }`) needs the same concrete-variant view as the statement form; the
	// tmp declarations are emitted inline in the wrapping statement-expression below.
	member_smartcast_plans, member_smartcast_condition := g.detect_member_smartcasts(g.last_expression, condition)
	use_member_smartcasts := guard_name == '' && member_smartcast_condition != ''
	mut smartcast_decls := ''
	if use_member_smartcasts {
		condition = member_smartcast_condition
		for plan in member_smartcast_plans {
			// A pointer subject (`mut node ast.TypeDecl` is `TypeDecl*`) reaches its boxed
			// payload through `->`, not `.`.
			plan_access := if plan.boxed_type.ends_with('*') { '->' } else { '.' }
			if g.fastc_plan_is_bare_local(plan) {
				// A bare-local subject is shadowed as a concrete-variant VALUE, so a whole-value
				// read of the local (`if x is T { x }` returning it) yields the variant — not the
				// raw boxed subject, which would mis-box on the way out.
				smartcast_decls += '${plan.boxed_type} ${plan.boxed_tmp} = ${plan.source}; ${plan.type_c} ${plan.member_tmp} = *((${plan.type_c} *)${plan.boxed_tmp}${plan_access}_object); '
			} else {
				smartcast_decls += '${plan.boxed_type} ${plan.boxed_tmp} = ${plan.source}; ${plan.type_c} *${plan.member_tmp} = (${plan.type_c} *)${plan.boxed_tmp}${plan_access}_object; '
			}
		}
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		g.locals[guard_name] = FastcLocal{
			typ: guard_type
		}
	}
	g.expected_expression_type = branch_expected_type
	mut previous_member_smartcasts := map[string]FastcMemberSmartcast{}
	mut had_member_smartcasts := map[string]bool{}
	mut previous_local_shadows := map[string]FastcLocal{}
	mut had_local_shadows := map[string]bool{}
	if use_member_smartcasts {
		for plan in member_smartcast_plans {
			if g.fastc_plan_is_bare_local(plan) {
				if plan.path !in previous_local_shadows {
					had_local_shadows[plan.path] = plan.path in g.locals
					previous_local_shadows[plan.path] = g.locals[plan.path] or { FastcLocal{} }
				}
				previous_plan_local := g.locals[plan.path] or { FastcLocal{} }
				g.locals[plan.path] = FastcLocal{
					is_mut: previous_plan_local.is_mut
					typ: plan.type_c
					c_name: plan.member_tmp
				}
				continue
			}
			if plan.path !in previous_member_smartcasts {
				had_member_smartcasts[plan.path] = plan.path in g.member_smartcasts
				previous_member_smartcasts[plan.path] = g.member_smartcasts[plan.path] or {
					FastcMemberSmartcast{}
				}
			}
			g.member_smartcasts[plan.path] = FastcMemberSmartcast{
				typ: plan.type_c + '*'
				source: plan.member_tmp
			}
		}
	}
	mut then_expression := if g.tok == .key_return {
		g.read_return_expression_branch()!
	} else {
		g.read_block_expression_value()!
	}
	if use_member_smartcasts {
		for path, present in had_local_shadows {
			if present {
				g.locals[path] = previous_local_shadows[path]
			} else {
				g.locals.delete(path)
			}
		}
		for path, present in had_member_smartcasts {
			if present {
				g.member_smartcasts[path] = previous_member_smartcasts[path]
			} else {
				g.member_smartcasts.delete(path)
			}
		}
	}
	if guard_name != '' {
		then_expression = '({ ${guard_type} ${fastc_c_identifier(guard_name)} = *((${guard_type} *)${guard_option}.data); ${then_expression}; })'
		if had_guard {
			g.locals[guard_name] = previous_guard
		} else {
			g.locals.delete(guard_name)
		}
	}
	mut then_type := g.last_expression_type
	mut then_option_value_type := if then_type == 'Option' {
		if g.last_expression.len > 0 {
			g.option_value_type_for_expression(g.last_expression)
		} else {
			g.last_option_value_type
		}
	} else {
		''
	}
	if enum_expression := g.expected_enum_shorthand_expression() {
		then_expression = enum_expression
		then_type = g.return_type
	}
	g.skip_semicolons()
	g.expect(.rcbr)!
	if g.tok != .key_else {
		return g.unsupported('if expression without `else`')
	}
	g.next()
	mut else_expression := ''
	mut else_type := ''
	mut else_option_value_type := ''
	// With no outer expected type (`x := if … {} else {}`), the then branch's inferred type is
	// the best expected type for the else branch, so a bare `[]` / `{}` there lowers to a typed
	// empty collection instead of raw C (which the `else_type == ''` fallback would wrap in an
	// invalid `(void)([])`).
	else_branch_expected := if branch_expected_type != '' {
		branch_expected_type
	} else if then_type !in ['', 'Option'] {
		fastc_normalize_inferred_type(then_type)
	} else {
		''
	}
	if g.tok == .key_if {
		g.expected_expression_type = else_branch_expected
		else_expression = g.read_if_expression()!
		else_type = g.last_expression_type
		else_option_value_type = g.last_option_value_type
	} else {
		g.expect(.lcbr)!
		g.expected_expression_type = else_branch_expected
		else_expression = if g.tok == .key_return {
			g.read_return_expression_branch()!
		} else {
			g.read_block_expression_value()!
		}
		else_type = g.last_expression_type
		if else_type == 'Option' {
			else_option_value_type = if g.last_expression.len > 0 {
				g.option_value_type_for_expression(g.last_expression)
			} else {
				g.last_option_value_type
			}
		}
		if enum_expression := g.expected_enum_shorthand_expression() {
			else_expression = enum_expression
			else_type = g.return_type
		}
		g.skip_semicolons()
		g.expect(.rcbr)!
	}
	if g.selfhost && g.return_type == 'Option' && outer_expected_type != 'Option' {
		if then_type.trim_right('*') == 'IError' && else_type !in ['', 'IError'] {
			then_expression = '({ return (Option){.err=${then_expression}, .state=1}; (${fastc_normalize_inferred_type(else_type)}){0}; })'
			then_type = else_type
		} else if else_type.trim_right('*') == 'IError' && then_type !in ['', 'IError'] {
			else_expression = '({ return (Option){.err=${else_expression}, .state=1}; (${fastc_normalize_inferred_type(then_type)}){0}; })'
			else_type = then_type
		}
	}
	if g.selfhost && outer_expected_type == 'Option' {
		if then_type != 'Option' {
			then_option_value_type = fastc_normalize_inferred_type(then_type)
			then_expression = g.option_branch_expression(then_type, then_expression)
			then_type = 'Option'
		}
		if else_type != 'Option' {
			else_option_value_type = fastc_normalize_inferred_type(else_type)
			else_expression = g.option_branch_expression(else_type, else_expression)
			else_type = 'Option'
		}
	} else if g.selfhost && then_type == 'Option' && else_type !in ['', 'Option'] {
		else_option_value_type = fastc_normalize_inferred_type(else_type)
		else_base := fastc_normalize_inferred_type(else_type)
		else_expression = '(Option){.data=${fastc_box_expression(else_base, else_expression)}, .state=0}'
		else_type = 'Option'
	} else if g.selfhost && else_type == 'Option' && then_type !in ['', 'Option'] {
		then_option_value_type = fastc_normalize_inferred_type(then_type)
		then_base := fastc_normalize_inferred_type(then_type)
		then_expression = '(Option){.data=${fastc_box_expression(then_base, then_expression)}, .state=0}'
		then_type = 'Option'
	}
	if g.selfhost && then_type == '' && else_type != '' {
		resolved_type := fastc_normalize_inferred_type(else_type)
		then_expression = '({ (void)(${then_expression}); (${resolved_type}){0}; })'
		then_type = resolved_type
		else_type = resolved_type
	} else if g.selfhost && else_type == '' && then_type != '' {
		resolved_type := fastc_normalize_inferred_type(then_type)
		else_expression = '({ (void)(${else_expression}); (${resolved_type}){0}; })'
		then_type = resolved_type
		else_type = resolved_type
	}
	// A member smart-cast branch (`if x.obj is Var { x.obj } else { Var{…} }`) reads the narrowed
	// member as a variant POINTER, while the other branch is a by-value variant; deref the pointer
	// branch so the ternary selects between two value-typed branches.
	if g.selfhost && then_type.ends_with('*') && else_type != '' && then_type.trim_right('*') == else_type {
		then_expression = '*(${then_expression})'
		then_type = else_type
	} else if g.selfhost && else_type.ends_with('*') && then_type != '' && else_type.trim_right('*') == then_type {
		else_expression = '*(${else_expression})'
		else_type = then_type
	}
	if then_type == else_type {
		g.last_expression_type = then_type
	} else if g.selfhost && then_type == '' {
		g.last_expression_type = else_type
	} else if g.selfhost && else_type == '' {
		g.last_expression_type = then_type
	} else if g.selfhost && fastc_is_integer_expression_type(then_type) && fastc_is_integer_expression_type(else_type) {
		g.last_expression_type = if then_type == 'integer literal' { else_type } else { then_type }
	} else {
		g.last_expression_type = if outer_expected_type != '' {
			outer_expected_type
		} else if then_type != '' {
			then_type
		} else {
			else_type
		}
	}
	g.last_option_value_type = if g.last_expression_type == 'Option' && then_option_value_type != '' && then_option_value_type == else_option_value_type {
		then_option_value_type
	} else {
		''
	}
	// When the if-expression's value is a boxed sum type / interface, its branches may be
	// DIFFERENT variants (`if x is T { x } else { empty }`), which C cannot select between in one
	// ternary. Box each variant branch into the boxed type so the ternary picks between two
	// identically-typed boxes. The target box is the expected type when known, else the sum type
	// that has both branch variants — recovered here because a call-argument if-expression is
	// read before its parameter type is applied, so the caller would otherwise box the whole
	// mismatched ternary.
	if g.selfhost && then_type != '' && else_type != '' {
		then_norm := fastc_normalize_inferred_type(then_type)
		else_norm := fastc_normalize_inferred_type(else_type)
		mut expected_box := ''
		if outer_expected_type != '' && g.is_boxed_type(fastc_normalize_inferred_type(outer_expected_type)) {
			expected_box = fastc_normalize_inferred_type(outer_expected_type)
		} else if then_norm != else_norm {
			// One branch may already BE the sum type (`ast.empty_expr` is an `Expr`) while the
			// other is a bare variant; box the variant into that sum type. Otherwise fall back to
			// a sum type that has both as variants.
			if g.is_boxed_type(else_norm) && g.should_box_variant(else_norm, then_type) {
				expected_box = else_norm
			} else if g.is_boxed_type(then_norm) && g.should_box_variant(then_norm, else_type) {
				expected_box = then_norm
			} else {
				expected_box = g.fastc_common_sum_type(then_type, else_type) or { '' }
			}
		}
		if expected_box != '' {
			if fastc_normalize_inferred_type(then_type) != expected_box && g.should_box_variant(expected_box, then_type) {
				then_expression = g.interface_value_expression(expected_box, then_type, then_expression)
				then_type = expected_box
			}
			if fastc_normalize_inferred_type(else_type) != expected_box && g.should_box_variant(expected_box, else_type) {
				else_expression = g.interface_value_expression(expected_box, else_type, else_expression)
				else_type = expected_box
			}
			if then_type == expected_box || else_type == expected_box {
				g.last_expression_type = expected_box
			}
		}
	}
	g.expected_expression_type = outer_expected_type
	g.last_expression = []FastcExpressionToken{}
	conditional := '((${condition}) ? (${then_expression}) : (${else_expression}))'
	wrapped := if smartcast_decls != '' {
		'({ ${smartcast_decls}${conditional}; })'
	} else {
		conditional
	}
	return if guard_option == '' {
		wrapped
	} else {
		'({ Option ${guard_option} = (${guard_source}); ${wrapped}; })'
	}
}

fn (g &Parser) option_branch_expression(value_type string, expression string) string {
	if value_type.trim_right('*') == 'IError' {
		return '(Option){.err=${expression}, .state=1}'
	}
	if value_type == 'voidptr' && g.option_return_type != 'voidptr' {
		return '(Option){.err=(IError){._object=(voidptr)(${expression})}, .state=1}'
	}
	return fastc_option_success_expression(value_type, expression)
}

fn (mut g Parser) read_return_expression_branch() !string {
	g.expect(.key_return)!
	if g.return_type.trim_right('*') == 'MultiReturn' {
		mut values := []string{}
		for {
			value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			values << 'V_FASTC_MULTI_VALUE((${value}))'
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		g.last_expression_type = ''
		g.last_expression = []FastcExpressionToken{}
		return '({ return ${fastc_multi_return_literal(values)}; 0; })'
	}
	value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	value_type := g.last_expression_type
	g.consume_statement_end()
	g.last_expression = []FastcExpressionToken{}
	// This `return` propagates out of the enclosing function, so coerce its value to that
	// function's return type exactly as parse_return does — an `IError` becomes the error state of
	// an `Option`, and a bare value is wrapped into the success state — rather than emitting a raw
	// `return <IError>` that TinyCC rejects.
	mut return_value := value
	if g.selfhost && g.return_type == 'Option' && value_type.trim_right('*') == 'IError' {
		return_value = '(Option){.err=${value}, .state=1}'
	} else if g.selfhost && g.return_type == 'Option' && value_type !in ['', 'Option'] {
		value_base := fastc_normalize_inferred_type(value_type)
		return_value = '(Option){.data=${fastc_box_expression(value_base, value)}, .state=0}'
	}
	g.last_expression_type = ''
	return '({ return ${return_value}; 0; })'
}

fn (g &Parser) expected_enum_shorthand_expression() ?string {
	if !g.selfhost || g.last_expression_type != '' || g.last_expression.len != 2 || g.last_expression[0].tok != .dot || g.last_expression[1].tok != .name || g.declared_kinds[g.semantic_type_key(g.return_type)] != .enum_ {
		return none
	}
	return '${g.return_type.trim_right('*')}__${g.last_expression[1].lit}'
}
