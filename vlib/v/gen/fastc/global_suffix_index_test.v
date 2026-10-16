module fastc

import v.flat
import v.pref
import v.scanner

fn fastc_suffix_legacy_global_key(g &Parser, name string) ?string {
	key := fastc_global_key(g.module_name, name)
	if key in g.globals { return key }
	builtin_key := fastc_global_key('builtin', name)
	if builtin_key in g.globals { return builtin_key }
	suffix := '.${name}'
	for global_key, _ in g.globals {
		if global_key == name || global_key.ends_with(suffix) { return global_key }
	}
	return none
}

fn test_fastc_global_suffix_index_preserves_exact_suffix_precedence_and_missing_types() {
	prefs := pref.new_preferences()
	globals := {
		'foreign.shared': 'foreign__shared'
		'alpha.shared':   'alpha__shared'
		'beta.shared':    'beta__shared'
		'shared':         'shared'
		'one.deep.name':  'one__deep__name'
		'two.deep.name':  'two__deep__name'
		'untyped.name':   'untyped__name'
		'odd..last':      'odd__last'
		'odd.':           'odd__empty'
		'':               'empty'
	}
	mut types := flat.TypeTextBindings{}
	for i, key in globals.keys() {
		if key != 'untyped.name' { types.set_id(key, u32(i)) }
	}
	types.set_id('typed.name', flat.source_type_int)
	types.set_id('type_only.value', flat.source_type_string)
	index := fastc_global_suffix_index(globals, types)
	mut names := ['', 'absent', 'ared', 'hared', '.name', 'name.', 'untyped', 'value']
	for key in globals.keys() {
		for i in 0 .. key.len + 1 {
			names << key[i..]
			names << key[..i]
		}
	}
	for key in types.keys() {
		names << key
		names << key.all_after_last('.')
	}
	for module_name in ['alpha', 'beta', 'builtin', 'main', 'missing', 'one.deep'] {
		mut g := Parser{ prefs: prefs, s: scanner.new_scanner(prefs, .normal), module_name: module_name, globals: globals, global_types: types, global_suffix_index: index }
		// A separate parser without an index executes the original lookup path,
		// including the memo wrapper's existing empty-key miss sentinel.
		legacy := Parser{ prefs: prefs, s: scanner.new_scanner(prefs, .normal), module_name: module_name, globals: globals, global_types: types }
		for name in names {
			assert g.unqualified_global_key_impl(name) or { '<none>' } == legacy.unqualified_global_key_impl(name) or { '<none>' }, '${module_name}/${name}'
			assert g.resolve_cross_module_global_type(name) or { u32(0xffffffff) } == legacy.resolve_cross_module_global_type(name) or { u32(0xffffffff) }, '${module_name}/${name}'
		}
	}
}

fn test_fastc_global_suffix_index_keeps_insertion_order_after_deletion() {
	prefs := pref.new_preferences()
	mut globals := {
		'first.value':  'first'
		'second.value': 'second'
	}
	globals.delete('first.value')
	globals['first.value'] = 'first_again'
	g := Parser{ prefs: prefs, s: scanner.new_scanner(prefs, .normal), module_name: 'other', globals: globals, global_suffix_index: fastc_global_suffix_index(globals, flat.TypeTextBindings{}) }
	assert g.unqualified_global_key_impl('value') or { panic('missing value') } == fastc_suffix_legacy_global_key(&g, 'value') or { panic('missing legacy value') }
}

fn test_fastc_global_suffix_index_declaration_initializers_use_current_tables() {
	prefs := pref.new_preferences()
	mut globals := map[string]string{}
	mut g := Parser{
		prefs:                        prefs
		s:                            scanner.new_scanner(prefs, .normal)
		globals:                      globals
		module_name:                  'main'
		declaration_initializer_mode: true
		global_suffix_index:          fastc_global_suffix_index(map[string]string{}, flat.TypeTextBindings{})
	}
	globals['foreign.later'] = 'foreign__later'
	g.global_types.set_id('foreign.later', flat.source_type_int)
	g.global_types.set_id('types.later_type', flat.source_type_string)
	assert g.unqualified_global_key_impl('later') or { panic('missing later global') } == 'foreign.later'
	assert g.resolve_cross_module_global_type('later') or { panic('missing later type') } == flat.source_type_int
	assert g.resolve_cross_module_global_type('later_type') or { panic('missing type-only declaration') } == flat.source_type_string
}

fn test_fastc_global_suffix_index_survives_monomorphization_context_switch() {
	prefs := pref.new_preferences()
	globals := {
		'alpha.value': 'alpha__value'
		'beta.value':  'beta__value'
	}
	mut g := Parser{
		prefs:               prefs
		s:                   scanner.new_scanner(prefs, .normal)
		module_name:         'alpha'
		globals:             globals
		global_suffix_index: fastc_global_suffix_index(globals, flat.TypeTextBindings{})
	}
	assert g.unqualified_global_key('value') or { panic('missing alpha') } == 'alpha.value'
	g.parse_mono_instance('', FastcGenericMethodSource{ module_name: 'beta', path: 'suffix_beta.v' }) or { panic(err) }
	assert g.global_suffix_index.ready
	assert g.unqualified_global_key('value') or { panic('missing beta') } == 'beta.value'
}
