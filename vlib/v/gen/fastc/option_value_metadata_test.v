module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_singleton_option_test_parser() Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:  prefs
		s:      scanner.new_scanner(prefs, .normal)
		locals: {
			'optional': FastcLocal{
				typ:               flat.source_type_id('Option')
				option_value_type: flat.source_type_string
			}
			'ordinary': FastcLocal{ typ: flat.source_type_int }
		}
	}
}

fn test_fastc_singleton_option_metadata_preserves_token_and_local_precedence() {
	g := fastc_singleton_option_test_parser()
	for name, expected in {
		'optional': 'string'
		'ordinary': ''
		'absent':   ''
	} {
		tokens := [FastcExpressionToken{ tok: .name, lit: name }]
		assert g.option_value_type_for_expression(tokens) == flat.source_type_id(expected), name
	}
	// Tokens synthesized by an earlier lowering retain their own payload metadata.
	for name in ['optional', 'ordinary', 'absent'] {
		tokens := [FastcExpressionToken{
			tok:               .name
			lit:               name
			option_value_type: flat.source_type_u64
		}]
		assert g.option_value_type_for_expression(tokens) == flat.source_type_u64, name
	}
	assert g.option_value_type_for_expression([]) == u32(0)
	for kind in [token.Token.number, .string, .char, .key_true, .key_false, .key_none] {
		assert g.option_value_type_for_expression([FastcExpressionToken{ tok: kind }]) == u32(0)
	}
}

fn test_fastc_parenthesized_singleton_option_metadata_keeps_the_wrapped_value() {
	g := fastc_singleton_option_test_parser()
	for name, expected in {
		'optional': 'string'
		'ordinary': ''
		'absent':   ''
	} {
		tokens := [FastcExpressionToken{ tok: .lpar }, FastcExpressionToken{ tok: .lpar },
			FastcExpressionToken{ tok: .name, lit: name }, FastcExpressionToken{ tok: .rpar },
			FastcExpressionToken{ tok: .rpar }]
		assert g.option_value_type_for_expression(tokens) == flat.source_type_id(expected), name
	}
}
