import os
import v3.flat
import v3.parser
import v3.pref
import v3.transform
import v3.types

fn parse_building_v_erasure_source(name string, source string) (&flat.FlatAst, &types.TypeChecker) {
	src := os.join_path(os.temp_dir(), 'v3_building_v_erasure_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	return a, &tc
}

fn fn_ids_by_name(a &flat.FlatAst, names []string) map[string]int {
	mut ids := map[string]int{}
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value in names {
			ids[node.value] = i
		}
	}
	for name in names {
		assert name in ids
	}
	return ids
}

fn test_building_v_erasure_keeps_transitive_generic_helpers() {
	mut a, mut tc := parse_building_v_erasure_source('transitive_helpers', '
module token

fn new_keywords_matcher_trie[T](value T) T {
	return token.kept_helper(value)
}

fn kept_helper[T](value T) T {
	return kept_nested(value)
}

fn kept_nested[T](value T) T {
	return value
}

fn erased_helper[T](value T) T {
	return value
}
')
	names := ['new_keywords_matcher_trie', 'kept_helper', 'kept_nested', 'erased_helper']
	ids := fn_ids_by_name(a, names)
	_ = transform.erase_generic_templates(mut a, tc, map[string]bool{})
	assert a.nodes[ids['new_keywords_matcher_trie']].kind == .fn_decl
	assert a.nodes[ids['kept_helper']].kind == .fn_decl
	assert a.nodes[ids['kept_nested']].kind == .fn_decl
	assert a.nodes[ids['erased_helper']].kind == .empty
}
