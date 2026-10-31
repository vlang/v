module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_bool_implication_trigger_parser() Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:          prefs
		s:              scanner.new_scanner(prefs, .normal)
		selfhost:       true
		declared_types: {
			flat.source_type_id('Record'): true
		}
		declared_kinds: {
			flat.source_type_id('Record'): .struct_
			flat.source_type_id('Choice'): .alias_
		}
		sum_types:      {
			'Choice': true
		}
		locals:         {
			'subject': FastcLocal{ typ: flat.source_type_id('Choice') }
			'count':   FastcLocal{ typ: flat.source_type_int }
			'ok':      FastcLocal{
				typ:               flat.source_type_bool
				bool_implications: [FastcBoolImplication{
					subject:   'subject'
					smartcast: FastcMemberSmartcast{ typ: flat.source_type_id('Record*'), source: 'saved' }
				}]
			}
			'plain':   FastcLocal{ typ: flat.source_type_bool }
		}
	}
}

fn fastc_bool_implication_trigger_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('bool_implication_trigger.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		tokens << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return tokens
}

fn test_fastc_bool_implication_trigger_preserves_complete_old_metadata() {
	for source in ['', 'count', '17', 'true', 'plain', 'ok', 'count + 1', 'call(ok)', '(ok)',
		'subject.field', 'items[0]', 'Record{}', 'subject !is Record', 'subject is Record',
		'count is Record', 'mut subject is Record', '&subject is Record', 'ok && plain', 'plain && ok',
		'subject is Record && ok', 'ok && subject is Record', '(subject is Record)', '(ok && plain)',
		'ok || plain', 'subject is Record || plain', 'ok && (plain || ok)', '[ok && plain]',
		'Record{flag: ok}'] {
		g := fastc_bool_implication_trigger_parser()
		old := fastc_bool_implication_trigger_parser()
		tokens := fastc_bool_implication_trigger_tokens(source)
		actual := g.compute_bool_is_implications(tokens)
		expected := old.fastc_compute_bool_is_implications_before_trigger(tokens)
		assert actual == expected, source
		assert g.member_smartcasts == old.member_smartcasts
		assert g.locals == old.locals
		assert g.temp_id == old.temp_id && g.temp_id == 0
		assert g.spawn_helpers == old.spawn_helpers
		assert g.declared_type_key_memo == old.declared_type_key_memo
		assert g.unqualified_key_memo == old.unqualified_key_memo
		assert g.type_memo == old.type_memo
	}
	g := fastc_bool_implication_trigger_parser()
	assert g.compute_bool_is_implications(fastc_bool_implication_trigger_tokens('ok')).len == 1
	assert g.compute_bool_is_implications(fastc_bool_implication_trigger_tokens('subject is Record')).len == 1
	assert g.compute_bool_is_implications(fastc_bool_implication_trigger_tokens('subject is Record && ok')).len == 2
	assert g.compute_bool_is_implications(fastc_bool_implication_trigger_tokens('ok || subject is Record')).len == 0
}

// Complete pre-trigger function; inherited and direct is implications remain active.
fn (g &Parser) fastc_compute_bool_is_implications_before_trigger(tokens []FastcExpressionToken) []FastcBoolImplication {
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
