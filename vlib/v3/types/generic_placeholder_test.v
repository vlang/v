module types

import v3.flat

fn test_generic_placeholder_token_prescreen_is_conservative() {
	for text in ['T', 'os.T', '[]T', 'map[string]V', '?map[K][]V', 'Box[T]', 'fn (value T) !U'] {
		assert generic_placeholder_token_prescreen(text), text
	}
	for text in ['', 'string', 'SomeType', 'HTTP', 'os.SomeType', 'map[string]int',
		'fn (value UUID) Result'] {
		assert !generic_placeholder_token_prescreen(text), text
	}
}

fn test_type_text_generic_placeholder_prescreen_keeps_results() {
	a := &flat.FlatAst{}
	tc := TypeChecker.new(a)
	assert tc.type_text_has_generic_placeholder('T')
	assert tc.type_text_has_generic_placeholder('os.T')
	assert tc.type_text_has_generic_placeholder('map[string][]T')
	assert !tc.type_text_has_generic_placeholder('SomeType')
	assert !tc.type_text_has_generic_placeholder('map[string]HTTP')
	assert tc.type_text_has_unbound_generic_placeholder('map[K]V', ['V'])
	assert !tc.type_text_has_unbound_generic_placeholder('map[string]int', [])
}
