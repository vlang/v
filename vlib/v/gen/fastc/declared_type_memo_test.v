module fastc

import v.flat
import v.pref
import v.scanner

fn fastc_declared_memo_test_parser(module_name string) Parser {
	prefs := pref.new_preferences()
	declared_types := {
		flat.source_type_id('alpha.Record'):     true
		flat.source_type_id('beta.Record'):      true
		flat.source_type_id('alpha.Private'):    true
		flat.source_type_id('beta.Imported'):    true
		flat.source_type_id('Qualified.Nested'): true
		flat.source_type_id('OnlyBuiltin'):      true
	}
	return Parser{
		prefs:                     prefs
		s:                         scanner.new_scanner(prefs, .normal)
		module_name:               module_name
		imports:                   {
			fastc_selective_import_key('Imported'): 'beta'
		}
		declared_types:            declared_types
		declared_type_key_by_name: fastc_declared_type_key_by_name(declared_types)
		declared_type_c_names:     fastc_declared_type_c_names(declared_types)
	}
}

fn test_fastc_declared_type_memo_preserves_resolution_and_negative_results() {
	g := fastc_declared_memo_test_parser('alpha')
	for _ in 0 .. 2 {
		assert g.resolve_declared_type_key('Record') or { panic('missing local record') } == flat.source_type_id('alpha.Record')
		assert g.resolve_declared_type_key('Private') or { panic('missing local type') } == flat.source_type_id('alpha.Private')
		assert g.resolve_declared_type_key('Imported') or { panic('missing selective import') } == flat.source_type_id('beta.Imported')
		assert g.resolve_declared_type_key('Qualified__Nested') or { panic('missing C spelling') } == flat.source_type_id('Qualified.Nested')
		assert g.resolve_declared_type_key('OnlyBuiltin') or { panic('missing builtin type') } == flat.source_type_id('OnlyBuiltin')
		assert g.resolve_declared_type_key('ordinary_value_name') == none
		assert g.resolve_declared_type_key('record') == none
		assert g.resolve_declared_type_key('') == none
	}
	for name in ['Record', 'Private', 'Imported', 'Qualified__Nested', 'OnlyBuiltin',
		'ordinary_value_name', 'record', ''] {
		cached := g.resolve_declared_type_key(name) or { u32(0) }
		uncached := g.resolve_declared_type_key_uncached(name) or { u32(0) }
		assert cached == uncached, name
	}
	missing := g.declared_type_key_memo[flat.source_type_id('ordinary_value_name')] or { panic('uncached missing type') }
	assert !missing.found
}

fn test_fastc_declared_type_memo_resets_when_monomorphization_switches_modules() {
	mut g := fastc_declared_memo_test_parser('alpha')
	assert g.resolve_declared_type_key('Record') or { panic('missing alpha type') } == flat.source_type_id('alpha.Record')
	assert g.resolve_declared_type_key('Private') or { panic('missing private type') } == flat.source_type_id('alpha.Private')
	assert g.resolve_declared_type_key('ordinary_value_name') == none
	assert g.declared_type_key_memo.len == 3
	g.parse_mono_instance(FastcSourceFile{
		path:   'declared_memo_beta.v'
		header: FastcSourceHeader{
			module_name: 'beta'
		}
	}) or { panic(err) }
	assert g.declared_type_key_memo.len == 0
	assert g.resolve_declared_type_key('Record') or { panic('missing beta type') } == flat.source_type_id('beta.Record')
	assert g.resolve_declared_type_key('Private') == none
	assert g.resolve_declared_type_key('ordinary_value_name') == none
	g.parse_mono_instance(FastcSourceFile{
		path:   'declared_memo_alpha.v'
		header: FastcSourceHeader{
			module_name: 'alpha'
		}
	}) or { panic(err) }
	assert g.declared_type_key_memo.len == 0
	assert g.resolve_declared_type_key('Private') or { panic('missing restored private type') } == flat.source_type_id('alpha.Private')
}

fn test_fastc_lookup_only_type_probes_do_not_intern_misses() {
	missing := 'fastc_lookup_only_missing_42c6'
	marker := '#Cstruct#${missing}'
	private_id := flat.source_type_id('fastc_lookup_only.Private')
	declared := {
		u32(0):     false
		private_id: false
	}
	kinds := {
		u32(0):     FastcDeclaredTypeKind.enum_
		private_id: FastcDeclaredTypeKind.alias_
	}
	assert fastc_source_type_key_exists(declared, '')
	assert !fastc_source_type_key_value(declared, '')
	assert fastc_declared_kind_exists(kinds, '')
	assert fastc_declared_kind_value(kinds, '') == .enum_
	assert fastc_source_type_key_exists(declared, 'fastc_lookup_only.Private')
	assert !fastc_source_type_key_value(declared, 'fastc_lookup_only.Private')
	assert !fastc_source_type_key_exists(declared, marker)
	assert !fastc_source_type_key_value(declared, marker)
	assert !fastc_declared_kind_exists(kinds, marker)
	assert fastc_declared_kind_value(kinds, marker) == .struct_
	assert flat.source_type_existing_id(marker) == none
	mut g := fastc_declared_memo_test_parser('alpha')
	g.imports[fastc_selective_import_key(missing)] = 'beta'
	assert g.resolve_declared_type_key_uncached(missing) == none
	assert flat.source_type_existing_id(missing) == none
	assert fastc_resolve_declared_type_key('alpha', missing, g.imports, g.declared_types) == none
	assert flat.source_type_existing_id('alpha.${missing}') == none
	assert flat.source_type_existing_id('beta.${missing}') == none
	assert g.resolve_declared_type_key(missing) == none
	raw_id := flat.source_type_existing_id(missing) or { panic('negative memo key missing') }
	entry := g.declared_type_key_memo[raw_id] or { panic('negative memo result missing') }
	assert !entry.found
	assert flat.source_type_existing_id('alpha.${missing}') == none
	assert flat.source_type_existing_id('beta.${missing}') == none
}
