module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn test_fastc_overloaded_binary_dispatch_leaves_single_tokens_unchanged() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs:    prefs
		s:        scanner.new_scanner(prefs, .normal)
		selfhost: true
		locals:   {
			'pointer': FastcLocal{ typ: flat.source_type_id('int*') }
		}
	}
	assert g.render_overloaded_binary_expression([]) == none
	for kind in [token.Token.name, .number, .string, .char, .key_true, .key_false, .key_nil, .key_none,
		.not, .mul, .amp, .lpar, .rpar, .assign] {
		assert g.render_overloaded_binary_expression([FastcExpressionToken{ tok: kind, lit: 'value' }]) == none
		assert g.render_overloaded_binary_expression([FastcExpressionToken{ tok: kind, lit: 'value', source: 'lowered()', typ: flat.source_type_int }]) == none
	}
	// Two-token pointer dereferences still need this lowering.
	value := g.render_overloaded_binary_expression([FastcExpressionToken{ tok: .mul },
		FastcExpressionToken{ tok: .name, lit: 'pointer' }]) or { panic('missing dereference') }
	assert value.source == '*(pointer)'
	assert value.typ == flat.source_type_int
}
