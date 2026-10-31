module fastc

import v.pref
import v.scanner

fn fastc_name_memo_test_parser(names map[string]string) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:            prefs
		s:                scanner.new_scanner(prefs, .normal)
		function_c_names: names
	}
}

// Keep the prior eager resolver as an independent oracle for map precedence.
fn fastc_name_memo_test_eager(g &Parser, key string, conventional string) string {
	if cached := g.c_function_name_memo[key] {
		return cached
	}
	c_name := if key == 'main' && !g.prefs.is_test {
		fastc_c_main_name(g.prefs)
	} else if compact_name := g.function_c_names[key] {
		compact_name
	} else {
		conventional
	}
	mut w := unsafe { &Parser(g) }
	w.c_function_name_memo[key] = c_name
	return c_name
}

fn test_fastc_lazy_function_names_match_previous_cold_and_warm_mapping() {
	keys := ['plain', 'other.function', 'C.printf', 'C.struct.member', 'malloc', 'exit', 'int_str',
		'byte', 'v_fastc_helper', 'complex', 'mktemp', '@keyword', 'other.Type.@static@method',
		'array.[]', '']
	for names in [map[string]string{}, {
		'other.function': 'v_f17'
		'C.printf':       'override_c'
		'complex':        'override_collision'
		'':               ''
	}] {
		g := fastc_name_memo_test_parser(names)
		original := fastc_name_memo_test_parser(names)
		for _ in 0 .. 3 {
			for key in keys {
				expected := fastc_name_memo_test_eager(original, key, fastc_c_function_name_for_key(key))
				assert g.c_function_name_for_key(key) == expected
				assert g.c_function_name_memo[key] or { panic('missing cached function name') } == expected
			}
		}
		assert g.c_function_name_memo.len == keys.len
	}
}

struct FastcMethodNameMemoCase {
	key         string
	module_name string
	receiver    string
	method      string
	expected    string
}

fn test_fastc_lazy_method_names_preserve_receiver_and_operator_spelling() {
	cases := [
		FastcMethodNameMemoCase{'Thing.run', '', 'Thing*', 'run', 'Thing_run'},
		FastcMethodNameMemoCase{'main.Thing.+', 'main', 'Thing**', '+', '__v3_internal_symbol_op_Thing_plus'},
		FastcMethodNameMemoCase{'other.deep.Thing.[]=', 'other.deep', 'other__deep__Thing*', '[]=', '__v3_internal_symbol_op_other__deep__Thing_op_index_set'},
		FastcMethodNameMemoCase{'json2.Array_Any.str', 'json2', 'Array_json2__Any*', 'str', 'json2__Array_Any_str'},
		FastcMethodNameMemoCase{'json2.Array_Map_Any.str', 'json2', 'Array_Map_json2__Any*', 'str', 'json2__Array_Map_Any_str'},
		FastcMethodNameMemoCase{'sync.Channel.try_push', 'sync', 'sync__Channel*', 'try_push', 'sync__Channel_try_push'},
		FastcMethodNameMemoCase{'odd.name', 'other', 'Thing', '@method', 'other__Thing__v_method'},
		FastcMethodNameMemoCase{'', '', '', '', '_'},
	]
	for names in [map[string]string{}, {
		'sync.Channel.try_push': 'v_f21'
		'':                      ''
	}] {
		g := fastc_name_memo_test_parser(names)
		original := fastc_name_memo_test_parser(names)
		for _ in 0 .. 3 {
			for item in cases {
				fallback := fastc_method_c_name(item.module_name, item.receiver, item.method)
				assert fallback == item.expected
				expected := fastc_name_memo_test_eager(original, item.key, fallback)
				assert g.c_method_name(item.key, item.module_name, item.receiver, item.method) == expected
				assert g.c_function_name_memo[item.key] or { panic('missing cached method name') } == expected
			}
		}
		assert g.c_function_name_memo.len == cases.len
	}
}

fn test_fastc_lazy_names_preserve_existing_memo_and_compact_override_precedence() {
	g := fastc_name_memo_test_parser({
		'override': 'v_f2'
		'empty':    ''
	})
	assert g.c_function_name_or('override', 'ignored') == 'v_f2'
	assert g.c_method_name('override', 'other', 'Different', 'method') == 'v_f2'
	assert g.c_function_name_for_key('override') == 'v_f2'
	assert g.c_method_name('empty', 'other', 'Different', 'method') == ''
	assert g.c_function_name_for_key('empty') == ''
	assert g.c_function_name_or('empty', 'ignored') == ''
	assert g.c_function_name_or('custom', 'custom_fallback') == 'custom_fallback'
	assert g.c_function_name_for_key('custom') == 'custom_fallback'
	assert g.c_method_name('custom', 'other', 'Different', 'method') == 'custom_fallback'
	assert g.c_method_name('same_key', 'first', 'Receiver', 'run') == 'first__Receiver_run'
	assert g.c_method_name('same_key', 'second', 'Other', '+') == 'first__Receiver_run'
	assert g.c_function_name_for_key('same_key') == 'first__Receiver_run'
	// A parser checkpoint sees the same existing memo entries and preserves their precedence.
	checkpoint := g
	assert checkpoint.c_function_name_for_key('custom') == 'custom_fallback'
	assert checkpoint.c_method_name('empty', 'other', 'Different', 'method') == ''
	// Independent parser instances retain separate memo ownership.
	other := fastc_name_memo_test_parser(map[string]string{})
	assert other.c_function_name_for_key('custom') == 'main__custom'
	assert other.c_method_name('same_key', 'second', 'Other', '+') == '__v3_internal_symbol_op_second__Other_plus'
}

fn test_fastc_lazy_names_preserve_target_entry_and_test_main_resolution() {
	for target_os in ['macos', 'windows'] {
		for is_test in [false, true] {
			for has_override in [false, true] {
				mut prefs := pref.new_preferences()
				prefs.target = pref.target_from(target_os, 'amd64') or { panic(err) }
				prefs.is_test = is_test
				mut overrides := map[string]string{}
				if has_override {
					overrides['main'] = 'test_entry_override'
				}
				g := Parser{
					prefs:            prefs
					s:                scanner.new_scanner(prefs, .normal)
					function_c_names: overrides
				}
				expected := if !is_test {
					if target_os == 'windows' { 'wmain' } else { 'main' }
				} else if has_override {
					'test_entry_override'
				} else {
					'main'
				}
				for _ in 0 .. 3 {
					assert g.c_function_name_for_key('main') == expected
					assert g.c_function_name_or('main', 'ignored_fallback') == expected
					assert g.c_method_name('main', 'other', 'Receiver', 'run') == expected
				}
			}
		}
	}
}

fn test_fastc_lazy_main_entry_name_keeps_existing_memo_precedence() {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	mut g := Parser{
		prefs:            prefs
		s:                scanner.new_scanner(prefs, .normal)
		function_c_names: {
			'main': 'ignored_override'
		}
	}
	g.c_function_name_memo['main'] = 'already_cached'
	assert g.c_function_name_for_key('main') == 'already_cached'
	assert g.c_method_name('main', 'other', 'Receiver', 'run') == 'already_cached'
	assert g.c_function_name_or('main', 'ignored_fallback') == 'already_cached'
}
