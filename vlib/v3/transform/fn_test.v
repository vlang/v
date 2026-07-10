module transform

fn test_flattened_generic_receiver_short_variants() {
	assert flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}
