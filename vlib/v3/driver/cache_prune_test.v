module driver

fn test_cache_function_reference_counts_scans_source_once() {
	candidates := {
		'alpha__one': true
		'beta__two':  true
	}
	counts := cache_function_reference_counts('void alpha__one(void); alpha__one(); beta__two(); beta__two_extra(); alpha__one();',
		candidates)
	assert counts['alpha__one'] == 2
	assert counts['beta__two'] == 1
}

fn test_c_source_references_identifiers_ignores_comments_strings_and_longer_names() {
	identifiers := {
		'local_helper': true
	}
	assert c_source_references_identifiers('int call(void) { return local_helper(); }', identifiers)
	assert c_source_references_identifiers('#define CALL_LOCAL() local_helper()\n', identifiers)
	assert !c_source_references_identifiers('// local_helper()\n/* local_helper */\nconst char *name = "local_helper";\nint local_helper_extra(void);\n',
		identifiers)
}

fn test_v_c_identifiers_accepts_spaced_and_commented_selectors() {
	assert v_c_identifiers('C /* selector */ . helper()\nC\n.\nother()\nC // line comment\n. line_helper()') == [
		'helper',
		'other',
		'line_helper',
	]
	assert v_c_identifiers('// C.fake()\nC.real_value') == [
		'real_value',
	]
}

fn test_cache_c_source_definitely_active_code_filters_conditional_definitions() {
	source := 'int always_active(void) { return 1; }\n#if FEATURE\nstatic int bundled_api(void) { return 2; }\n#else\nint library_api(void) { return 3; }\n#endif\n'
	mut disabled_macros := cache_local_c_flag_macros(['-DFEATURE=0'])
	disabled := cache_c_source_definitely_active_code(source, mut disabled_macros)
	assert disabled.contains('always_active')
	assert !disabled.contains('bundled_api')
	assert disabled.contains('library_api')

	mut enabled_macros := cache_local_c_flag_macros(['-DFEATURE=1'])
	enabled := cache_c_source_definitely_active_code(source, mut enabled_macros)
	assert enabled.contains('always_active')
	assert enabled.contains('bundled_api')
	assert !enabled.contains('library_api')

	mut unknown_macros := cache_local_c_flag_macros([])
	unknown := cache_c_source_definitely_active_code(source, mut unknown_macros)
	assert unknown.contains('always_active')
	assert !unknown.contains('bundled_api')
	assert !unknown.contains('library_api')
}

fn test_prune_cached_native_function_prototypes_resolves_cache_guards() {
	state := &V3ModuleCacheState{
		native_declared_functions: {
			'owner': {
				'active_api': true
			}
		}
	}
	source := '#ifndef V3CACHE_PROGRAM_UNIT\nint active_api(void);\n#endif\n#ifndef V3CACHE_PROGRAM_UNIT\nint library_api(void);\n#endif\nint active_api(void);\n'
	pruned := prune_cached_native_function_prototypes(source, state, ['owner'])
	assert !pruned.contains('active_api')
	assert pruned.contains('int library_api(void);')
	assert !pruned.contains('V3CACHE_PROGRAM_UNIT')
}
