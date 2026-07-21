module c

fn test_late_source_does_not_reemit_multiline_header_context() {
	header := '#if defined(HEADER_IMPL)\n' + 'typedef struct { int value; } header_value;\n' +
		'#endif'
	directives := [
		header,
		'#ifdef __APPLE__',
		'#define OBJC_HELPER 1',
		'#include "/tmp/helper.m"',
		'#undef OBJC_HELPER',
		'#endif',
	]
	emission := c_source_directive_emission(directives, map[string]bool{})

	assert 0 !in emission.emit_late
	for i in 1 .. directives.len {
		assert i in emission.emit_late
	}
	assert 3 in emission.skip_early
}

fn test_multiline_inlined_c_function_definition_is_collected() {
	mut g := FlatGen.new()
	g.collect_inlined_c_fns('bool qrcodegen_encodeText(const char *text, uint8_t tempBuffer[],\n' +
		'\tenum qrcodegen_Ecc ecl, bool boostEcl) {\n' + '\treturn text != 0 && boostEcl;\n' + '}')
	g.collect_inlined_c_fns('void declared_with_anon_param(\n' + '\tstruct { int value; } item);')

	assert 'qrcodegen_encodeText' in g.inlined_c_fns
	assert 'declared_with_anon_param' !in g.inlined_c_fns
}

fn test_cache_tracks_omitted_native_function_definitions() {
	mut g := FlatGen.new()
	g.cache_split = true
	g.collect_inlined_c_fns_for_cache('int native_source_fn(void) { return 1; }', true, false)
	g.collect_inlined_c_fns_for_cache('int native_header_fn(void) { return 2; }', false, true)
	g.collect_inlined_c_fns_for_cache('#ifdef FONTSTASH_IMPLEMENTATION\nint omitted_header_fn(void) { return 4; }\n#endif',
		false, true)
	g.collect_inlined_c_fns_for_cache('#ifndef FOO_IMPLEMENTATION\nint else_implementation_fn(void);\n#else\nint else_implementation_fn(void) { return 5; }\n#endif',
		false, true)
	g.collect_inlined_c_fns_for_cache('static int native_static_fn(void) { return 3; }', false,
		true)

	assert 'native_source_fn' in g.cache_omitted_c_fns
	assert 'native_header_fn' !in g.cache_omitted_c_fns
	assert 'omitted_header_fn' in g.cache_omitted_c_fns
	assert 'else_implementation_fn' in g.cache_omitted_c_fns
	assert g.should_emit_c_extern_decl('else_implementation_fn')
	assert 'native_static_fn' in g.inlined_c_static_fns
}

fn test_cache_extern_declaration_avoids_tgmath_macro_expansion() {
	assert c_cache_safe_extern_decl('exp', 'double exp(double x);', true) == 'double (exp)(double x);'
	assert c_cache_safe_extern_decl('exp', 'double exp(double x);', false) == 'double exp(double x);'
	assert c_cache_safe_extern_decl('custom', 'int custom(int x);', true) == 'int custom(int x);'
}

fn test_builtin_abi_compat_macros_precede_late_c_source() {
	mut g := FlatGen.new()
	g.has_builtins = true
	g.add_c_directive('main', '#include "/tmp/helper.m"', false)
	g.preamble()
	g.emit_c_source_directives()
	code := g.sb.str()
	alias_pos := code.index('#define builtin__string_clone string__clone') or { -1 }
	source_pos := code.index('#include "/tmp/helper.m"') or { -1 }

	assert alias_pos >= 0
	assert source_pos > alias_pos
}
