module fastc

import strings
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_smartcast_gate_parser(selfhost bool, mode int) Parser {
	prefs := pref.new_preferences()
	mut holder_fields := flat.TypeTextBindings{}
	holder_fields.set('subject', 'Choice')
	mut record_fields := flat.TypeTextBindings{}
	record_fields.set('count', 'int')
	field_info := {
		'Holder': [FastcStructField{ name: 'subject', typ: flat.source_type_id('Choice') }]
		'Record': [FastcStructField{ name: 'count', typ: flat.source_type_int }]
	}
	return Parser{
		prefs:                    prefs
		s:                        scanner.new_scanner(prefs, .normal)
		selfhost:                 selfhost
		in_mono_drain:            mode == 1
		in_generic_placeholder:   mode == 2
		numeric_ops_are_builtin:  true
		primitive_cmps_builtin:   true
		out:                      strings.new_builder(0)
		protos:                   strings.new_builder(0)
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_expression_type:     flat.source_type_int
		last_option_value_type:   flat.source_type_string
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		expected_expression_type: flat.source_type_bool
		declared_types:           {
			'Holder': true
			'Record': true
			'Choice': true
		}
		declared_kinds:           {
			'Holder': .struct_
			'Record': .struct_
			'Choice': .alias_
		}
		sum_types:                {
			'Choice': true
		}
		struct_fields:            {
			'Holder': holder_fields
			'Record': record_fields
		}
		struct_field_info:        field_info
		struct_field_lookup:      fastc_build_struct_field_lookup(field_info)
		locals:                   {
			'holder': FastcLocal{ typ: flat.source_type_id('Holder') }
			'value':  FastcLocal{ typ: flat.source_type_id('Choice') }
			'count':  FastcLocal{ typ: flat.source_type_int }
			'flag':   FastcLocal{ typ: flat.source_type_bool }
			'text':   FastcLocal{ typ: flat.source_type_string }
			'items':  FastcLocal{ typ: flat.source_type_id('Array_Unknown') }
		}
	}
}

fn fastc_smartcast_gate_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('smartcast_gate.v', source.len), source)
	mut result := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		result << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return result
}

fn fastc_smartcast_gate_compare_state(mut fast Parser, mut slow Parser) {
	assert fast.temp_id == slow.temp_id
	assert fast.spawn_helpers == slow.spawn_helpers
	assert fast.member_smartcasts == slow.member_smartcasts
	assert fast.composite_types == slow.composite_types
	assert fast.fixed_array_types.ids == slow.fixed_array_types.ids
	assert fast.locals == slow.locals
	assert fast.functions == slow.functions
	assert fast.mono_functions == slow.mono_functions
	assert fast.protos.str() == slow.protos.str()
	assert fast.out.str() == slow.out.str()
	assert fast.last_expression == slow.last_expression
	assert fast.last_expression_type == slow.last_expression_type
	assert fast.last_option_value_type == slow.last_option_value_type
	assert fast.last_multi_return_types == slow.last_multi_return_types
	assert fast.expected_expression_type == slow.expected_expression_type
	assert fast.tok == slow.tok && fast.lit == slow.lit
	assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos
	assert fast.comparison_memo == slow.comparison_memo
	assert fast.type_memo == slow.type_memo
	assert fast.declared_type_key_memo == slow.declared_type_key_memo
	assert fast.unqualified_key_memo == slow.unqualified_key_memo
	assert fast.global_key_memo == slow.global_key_memo
	assert fast.c_function_name_memo == slow.c_function_name_memo
}

fn test_fastc_smartcast_gate_skips_only_observably_inert_singleton_passes() {
	for mode in 0 .. 3 {
		for source, rendered in {
			'flag':                     'flag'
			'count > 3':                '((count) > 3)'
			'!flag':                    '!flag'
			'flag || count > 3':        '(flag || count > 3)'
			'call(count)':              'fixture__call(count)'
			'items[0] > count':         'array_get(items, 0) > count'
			'holder.subject.count > 3': 'holder.subject.count > 3'
			'text == "plain"':          'string_eq(text, _S("plain"))'
			'(count > 3)':              '((((count > 3))))'
			'':                         ''
		} {
			mut fast := fastc_smartcast_gate_parser(true, mode)
			mut slow := fastc_smartcast_gate_parser(true, mode)
			tokens := fastc_smartcast_gate_tokens(source)
			assert fast.can_skip_member_smartcast_detection(tokens, rendered), source
			plans, rewritten := fast.detect_member_smartcasts(tokens, rendered)
			old_plans, old_rewritten := slow.fastc_detect_member_smartcasts_before_gate(tokens, rendered)
			assert plans == old_plans && rewritten == old_rewritten, source
			assert plans.len == 0 && rewritten == ''
			fastc_smartcast_gate_compare_state(mut fast, mut slow)
		}
	}
}

fn test_fastc_smartcast_gate_keeps_type_tests_conjunctions_and_render_misalignment() {
	for source, rendered in {
		'value is Record':                    'value._typ == __v_typeid_Record'
		'holder.subject is Record':           'holder.subject._typ == __v_typeid_Record'
		'value is Record && value.count > 0': 'value._typ == __v_typeid_Record && value.count > 0'
		'value !is Record':                   'value._typ != __v_typeid_Record'
		'(value is Record)':                  '(value._typ == __v_typeid_Record)'
		'flag && count > 0':                  '(flag && count > 0)'
		'flag':                               'first && second'
		'!flag':                              'nested((first && second))'
		'text':                               '_S("quoted && text")'
		'items.str()':                        'first && second'
	} {
		mut fast := fastc_smartcast_gate_parser(true, 0)
		mut slow := fastc_smartcast_gate_parser(true, 0)
		tokens := fastc_smartcast_gate_tokens(source)
		assert !fast.can_skip_member_smartcast_detection(tokens, rendered), source
		plans, rewritten := fast.detect_member_smartcasts(tokens, rendered)
		old_plans, old_rewritten := slow.fastc_detect_member_smartcasts_before_gate(tokens, rendered)
		assert plans == old_plans && rewritten == old_rewritten, source
		if source == 'items.str()' {
			// A failed generated array str() leaves a declaration behind. The
			// rendered alignment guard must keep the complete old fallback.
			assert fast.protos.len > 0
			assert fast.spawn_helpers.len == 0
		}
		fastc_smartcast_gate_compare_state(mut fast, mut slow)
	}
	for selfhost in [false, true] {
		mut fast := fastc_smartcast_gate_parser(selfhost, 0)
		mut slow := fastc_smartcast_gate_parser(selfhost, 0)
		fast.member_smartcasts['other.field'] = FastcMemberSmartcast{ typ: flat.source_type_int, source: 'narrowed' }
		slow.member_smartcasts['other.field'] = FastcMemberSmartcast{ typ: flat.source_type_int, source: 'narrowed' }
		tokens := fastc_smartcast_gate_tokens('flag')
		assert !fast.can_skip_member_smartcast_detection(tokens, 'flag')
		plans, rewritten := fast.detect_member_smartcasts(tokens, 'flag')
		old_plans, old_rewritten := slow.fastc_detect_member_smartcasts_before_gate(tokens, 'flag')
		assert plans == old_plans && rewritten == old_rewritten
		fastc_smartcast_gate_compare_state(mut fast, mut slow)
	}
	ordinary := fastc_smartcast_gate_parser(false, 0)
	assert !ordinary.can_skip_member_smartcast_detection(fastc_smartcast_gate_tokens('flag'), 'flag')
}

// Snapshot of the complete pre-guard detector; preserve failed-render side effects.
fn (mut g Parser) fastc_detect_member_smartcasts_before_gate(cond_tokens []FastcExpressionToken, rendered_condition string) ([]FastcMemberSmartcastPlan, string) {
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
				mut left_type := fastc_normalize_inferred_type(g.infer_expression_type(left_tokens) or { u32(0) })
				if member_type_source_id := g.infer_member_access_type(left_tokens, 0, left_tokens.len) {
					member_type := member_type_source_id

					left_type = fastc_normalize_inferred_type(member_type)
				}
				if left_tokens.len == 1 {
					if origin := g.narrowed_local_origin(left_tokens[0].lit) {
						left_type = origin.typ
					}
				}
				target_type := (g.type_from_expression_tokens(conjunct[is_idx + 1..]) or { u32(0) })
				if left_type != u32(0) && flat.source_type_name(target_type) != '' && g.is_boxed_type(left_type) {
					if rendered_left := g.render_member_receiver(left_tokens) {
						mut left_source := rendered_left
						if left_tokens.len == 1 {
							if origin := g.narrowed_local_origin(left_tokens[0].lit) {
								left_source = origin.source
							}
						}
						value_type := fastc_normalize_inferred_type(target_type)
						normalized_target := flat.source_type_name(value_type).trim_right('*')
						boxed_tmp := g.temporary_name('smartcast_subject')
						member_tmp := g.temporary_name('smartcast_member')
						// A pointer subject (`node &ast.Node`, or a nested variant already
						// narrowed to `Variant*`) reaches its boxed `_typ`/`_object` through
						// `->`, not `.`.
						access := if flat.source_type_name(left_type).ends_with('*') {
							'->'
						} else {
							'.'
						}
						plans << FastcMemberSmartcastPlan{
							path:       path
							type_c:     flat.source_type_id(normalized_target)
							value_type: value_type
							boxed_type: left_type
							source:     left_source
							boxed_tmp:  boxed_tmp
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
							typ:        flat.source_type_id(normalized_target + '*')
							value_type: value_type
							source:     '((${normalized_target} *)${boxed_tmp}${access}_object)'
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
		uses_smartcast := g.member_smartcasts.len > 0
			&& g.expression_uses_member_smartcast(conjunct[cstart..])
		if !uses_smartcast && aligned {
			condition_parts << '(${rendered_conjuncts[i].trim_space()})'
			continue
		}
		// A comparison guard that reads a narrowed subject through a plain member/index chain
		// (`x.args[0].expr.typ_text() > 0`) must be rendered before the general member receiver path.
		// Besides indexed chains, this resolves an enum shorthand such as
		// `left is PrefixExpr && left.op == .mul` against the narrowed field's enum type.
		// A call-bearing operand (`sym(x).kind != .placeholder`) is left to the raw/special path,
		// whose call handling the comparison renderer does not reproduce.
		if uses_smartcast
			&& !fastc_expression_tokens_contain_range(conjunct, cstart, conjunct.len, .lpar) {
			if guard := g.render_guard_comparison(conjunct[cstart..]) {
				condition_parts << '(${guard})'
				continue
			}
		}
		// A method call on a narrowed member (`right.typ_text().has_flag(.generic)`) needs
		// both method lowering and the active smartcast receiver. The general member
		// renderer below only handles the receiver chain and can leave the call raw.
		if uses_smartcast && fastc_tokens_are_plain_call(fastc_strip_paren_tokens(conjunct[cstart..]))
			&& !fastc_expression_tokens_contain_range(conjunct, cstart, conjunct.len, .key_as) {
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
