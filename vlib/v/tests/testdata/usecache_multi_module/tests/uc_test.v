import mymod

fn test_sumtype_is_across_tus() {
	// sumtype value tagged inside the cached module, `is`-checked here
	s := mymod.make_circle(3)
	assert s is mymod.Circle
	// `is`-check inside the cached module on a value held here
	assert mymod.classify(s) == 'circle'
}

fn test_interface_dispatch_across_tus() {
	sp := mymod.make_speaker()
	assert sp.speak() == 'woof'
	assert mymod.greet(sp) == 'woof'
	assert mymod.speaker_type(sp).len > 0
}

fn test_closures_created_in_cached_module() {
	g := mymod.make_getter(42)
	assert g() == 42
	assert mymod.call_getter_here(7) == 7
}

fn test_module_consts_and_array_growth() {
	assert mymod.push_many() == 10
	assert mymod.big_cap == 1000
}

fn test_float_formatting_reads_cached_const_tables() {
	assert '${2.5 + 0.25}' == '2.75'
	assert 12345.678.str() == '12345.678'
}
