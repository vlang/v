module fastc

import v.pref
import v.scanner

fn fastc_declared_memo_test_parser(module_name string) Parser {
	prefs := pref.new_preferences()
	declared_types := {
		'alpha.Record':     true
		'beta.Record':      true
		'alpha.Private':    true
		'beta.Imported':    true
		'Qualified.Nested': true
		'OnlyBuiltin':      true
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
		assert g.resolve_declared_type_key('Record') or { panic('missing local record') } == 'alpha.Record'
		assert g.resolve_declared_type_key('Private') or { panic('missing local type') } == 'alpha.Private'
		assert g.resolve_declared_type_key('Imported') or { panic('missing selective import') } == 'beta.Imported'
		assert g.resolve_declared_type_key('Qualified__Nested') or { panic('missing C spelling') } == 'Qualified.Nested'
		assert g.resolve_declared_type_key('OnlyBuiltin') or { panic('missing builtin type') } == 'OnlyBuiltin'
		assert g.resolve_declared_type_key('ordinary_value_name') == none
		assert g.resolve_declared_type_key('record') == none
		assert g.resolve_declared_type_key('') == none
	}
	for name in ['Record', 'Private', 'Imported', 'Qualified__Nested', 'OnlyBuiltin',
		'ordinary_value_name', 'record', ''] {
		cached := g.resolve_declared_type_key(name) or { '<missing>' }
		uncached := g.resolve_declared_type_key_uncached(name) or { '<missing>' }
		assert cached == uncached, name
	}
	missing := g.declared_type_key_memo['ordinary_value_name'] or { panic('uncached missing type') }
	assert !missing.found
}

fn test_fastc_declared_type_memo_resets_when_monomorphization_switches_modules() {
	mut g := fastc_declared_memo_test_parser('alpha')
	assert g.resolve_declared_type_key('Record') or { panic('missing alpha type') } == 'alpha.Record'
	assert g.resolve_declared_type_key('Private') or { panic('missing private type') } == 'alpha.Private'
	assert g.resolve_declared_type_key('ordinary_value_name') == none
	assert g.declared_type_key_memo.len == 3
	g.parse_mono_instance(FastcSourceFile{
		path:   'declared_memo_beta.v'
		header: FastcSourceHeader{
			module_name: 'beta'
		}
	}) or { panic(err) }
	assert g.declared_type_key_memo.len == 0
	assert g.resolve_declared_type_key('Record') or { panic('missing beta type') } == 'beta.Record'
	assert g.resolve_declared_type_key('Private') == none
	assert g.resolve_declared_type_key('ordinary_value_name') == none
	g.parse_mono_instance(FastcSourceFile{
		path:   'declared_memo_alpha.v'
		header: FastcSourceHeader{
			module_name: 'alpha'
		}
	}) or { panic(err) }
	assert g.declared_type_key_memo.len == 0
	assert g.resolve_declared_type_key('Private') or { panic('missing restored private type') } == 'alpha.Private'
}
