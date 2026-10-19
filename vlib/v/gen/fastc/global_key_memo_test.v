module fastc

import v.pref
import v.scanner

fn fastc_global_memo_test_parser(module_name string, globals map[string]string) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:       prefs
		s:           scanner.new_scanner(prefs, .normal)
		module_name: module_name
		globals:     globals
	}
}

fn test_fastc_global_key_memo_preserves_module_builtin_and_fallback_precedence() {
	globals := {
		'foreign_one.fallback': 'foreign_one__fallback'
		'foreign_two.fallback': 'foreign_two__fallback'
		'shared':               'shared'
		'alpha.shared':         'alpha__shared'
		'beta.shared':          'beta__shared'
	}
	alpha := fastc_global_memo_test_parser('alpha', globals)
	for _ in 0 .. 2 {
		assert alpha.unqualified_global_key('shared') or { panic('missing local global') } == 'alpha.shared'
		assert alpha.unqualified_global_key('fallback') or { panic('missing foreign global') } == 'foreign_one.fallback'
	}
	other := fastc_global_memo_test_parser('other', globals)
	for _ in 0 .. 2 {
		assert other.unqualified_global_key('shared') or { panic('missing builtin global') } == 'shared'
	}
	// A miss is cached as a present empty value, distinct from an unqueried name.
	assert alpha.unqualified_global_key('absent') == none
	assert alpha.unqualified_global_key('absent') == none
	missing := alpha.global_key_memo['absent'] or { panic('uncached missing global') }
	assert missing == ''
	local := alpha.global_key_memo['shared'] or { panic('uncached local global') }
	assert local == 'alpha.shared'
}

fn test_fastc_global_key_memo_resets_when_monomorphization_switches_modules() {
	mut g := fastc_global_memo_test_parser('alpha', {
		'shared':       'shared'
		'alpha.shared': 'alpha__shared'
		'beta.shared':  'beta__shared'
	})
	assert g.unqualified_global_key('shared') or { panic('missing alpha global') } == 'alpha.shared'
	assert g.unqualified_global_key('absent') == none
	assert g.global_key_memo.len == 2
	// Exercise the actual context switch used by synthetic generic instances.
	g.parse_mono_instance(FastcSourceFile{
		path:   'global_memo_beta.v'
		header: FastcSourceHeader{
			module_name: 'beta'
		}
	}) or { panic(err) }
	assert g.module_name == 'beta'
	assert g.global_key_memo.len == 0
	assert g.unqualified_global_key('shared') or { panic('missing beta global') } == 'beta.shared'
	assert g.unqualified_global_key('absent') == none
	assert g.global_key_memo.len == 2
	g.parse_mono_instance(FastcSourceFile{
		path:   'global_memo_main.v'
		header: FastcSourceHeader{
			module_name: 'main'
		}
	}) or { panic(err) }
	assert g.global_key_memo.len == 0
	assert g.unqualified_global_key('shared') or { panic('missing main global') } == 'shared'
}
