module c

import os
import v.flat
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn generate_import_cache_reuse_source(mut g FlatGen) string {
	source_path := os.join_path(os.temp_dir(), 'v3_import_cache_reuse_${os.getpid()}.v')
	os.write_file(source_path, 'fn main() {
	println(1)
}
') or {
		panic(err)
	}
	defer {
		os.rm(source_path) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(source_path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used_fns := markused.mark_used(a, tc)
	// Serial generation: it has no function dispatch that resets lookup caches.
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

// One FlatGen may generate more than one program. The import front caches keep
// answers, including misses, from the previous program's checker; a new
// generation must not resolve imports through them.
fn test_import_caches_do_not_survive_into_the_next_generation() {
	mut first_a := flat.FlatAst.new()
	mut first_tc := types.TypeChecker.new(&first_a)
	first_tc.file_imports['shared.v\nmodel'] = 'first.model'
	mut g := FlatGen.new()
	g.a = &first_a
	g.tc = &first_tc
	assert g.cached_file_import('shared.v', 'model')? == 'first.model'
	assert g.file_selective_import_candidates('shared.v', 'Item') == none

	c_source := generate_import_cache_reuse_source(mut g)
	assert c_source.contains('int main('), c_source
	// The same file/alias keys now resolve through the new program's checker.
	g.tc.file_imports['shared.v\nmodel'] = 'second.model'
	g.tc.file_selective_imports['shared.v\nItem'] = ['second.Item']
	assert g.cached_file_import('shared.v', 'model')? == 'second.model'
	assert g.file_selective_import_candidates('shared.v', 'Item')? == ['second.Item']
}
