module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_scalar_argument_test_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('scalar_argument.v', source.len), source)
	kind := scan.scan()
	return [FastcExpressionToken{ tok: kind, lit: scan.lit }]
}

fn test_fastc_scalar_call_arguments_keep_existing_literal_and_local_spellings() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs:    prefs
		s:        scanner.new_scanner(prefs, .normal)
		selfhost: true
		locals:   {
			'number': FastcLocal{ typ: flat.source_type_int }
			'short':  FastcLocal{ typ: flat.source_type_int }
			'text':   FastcLocal{ typ: flat.source_type_string, c_name: '__renamed_text' }
		}
	}
	for source, type_name in {
		'17':                 'int'
		'0xffffffffffffffff': 'u64'
		'1.25':               'f64'
		'"hello"':            'string'
		'`λ`':                'rune'
		'true':               'bool'
		'false':              'bool'
		'number':             'int'
		'short':              'int'
		'text':               'string'
	} {
		tokens := fastc_scalar_argument_test_tokens(source)
		expected := flat.source_type_id(type_name)
		fast := g.render_selfhost_scalar_call_argument(tokens, expected) or { panic('missing scalar ${source}') }
		// These exact-type scalar arguments need none of the later boxing,
		// pointer, option, fixed-array or interface coercions.
		slow := g.render_membership_candidate(tokens, expected) or { panic('missing fallback ${source}') }
		assert fast == slow, source
	}
}

fn test_fastc_scalar_call_argument_leaves_coercions_and_narrowings_to_general_dispatch() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs:    prefs
		s:        scanner.new_scanner(prefs, .normal)
		selfhost: true
		locals:   {
			'value':      FastcLocal{ typ: flat.source_type_int }
			'reference':  FastcLocal{ typ: flat.source_type_id('int*'), is_reference: true }
			'optional':   FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_int }
			'origin':     FastcLocal{ typ: flat.source_type_int, smartcast_origin_type: flat.source_type_id('Scalar') }
			'other_type': FastcLocal{ typ: flat.source_type_u64 }
			'alias':      FastcLocal{ typ: flat.source_type_id('Alias') }
			'C':          FastcLocal{ typ: flat.source_type_int }
		}
	}
	for source in ['reference', 'optional', 'origin', 'other_type', 'alias', 'C', 'absent', 'nil'] {
		assert g.render_selfhost_scalar_call_argument(fastc_scalar_argument_test_tokens(source), flat.source_type_int) == none, source
	}
	for expected in [u32(0), flat.source_type_voidptr, flat.source_type_id('int*'), fastc_type_option,
		flat.source_type_id('Scalar'), flat.source_type_id('Array_int')] {
		assert g.render_selfhost_scalar_call_argument(fastc_scalar_argument_test_tokens('value'), expected) == none
	}
	for token_value in [
		FastcExpressionToken{ tok: .number, lit: '0', typ: flat.source_type_voidptr },
		FastcExpressionToken{ tok: .name, lit: 'value', is_mut_argument: true },
		FastcExpressionToken{ tok: .name, lit: 'value', source: 'lowered_value', typ: flat.source_type_int },
		FastcExpressionToken{ tok: .name, lit: 'value', typ: flat.source_type_u64 },
	] {
		assert g.render_selfhost_scalar_call_argument([token_value], flat.source_type_int) == none
	}
}

fn test_fastc_scalar_call_argument_preserves_active_member_smartcasts_and_nonselfhost_dispatch() {
	prefs := pref.new_preferences()
	for selfhost in [true, false] {
		g := Parser{
			prefs:             prefs
			s:                 scanner.new_scanner(prefs, .normal)
			selfhost:          selfhost
			locals:            {
				'value': FastcLocal{ typ: flat.source_type_int }
			}
			member_smartcasts: {
				'value': FastcMemberSmartcast{}
			}
		}
		assert g.render_selfhost_scalar_call_argument(fastc_scalar_argument_test_tokens('value'), flat.source_type_int) == none
	}
	nonselfhost := Parser{
		prefs:  prefs
		s:      scanner.new_scanner(prefs, .normal)
		locals: {
			'value': FastcLocal{ typ: flat.source_type_int }
		}
	}
	assert nonselfhost.render_selfhost_scalar_call_argument(fastc_scalar_argument_test_tokens('value'), flat.source_type_int) == none
	assert nonselfhost.render_selfhost_scalar_call_argument(fastc_scalar_argument_test_tokens('1'), flat.source_type_int) == none
}
