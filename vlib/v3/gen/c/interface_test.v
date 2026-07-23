module c

import os
import v3.flat
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

fn test_collect_interface_impls_includes_boxed_maps() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.interfaces['Any'] = []string{}
	g.interface_boxed_types['Any::map[string]int'] = true
	g.interface_boxed_types_done = true

	g.collect_interface_impls()

	assert 'map[string]int' in g.iface_impls['Any']
	assert g.iface_type_ids['Any::map[string]int'] > 0
}

fn test_incremental_cgen_collects_interface_type_ids() {
	source_path := os.join_path(os.temp_dir(), 'v3_incremental_cgen_interface_ids.v')
	os.write_file(source_path, 'module main

interface Speaker {
	speak() string
}

struct Cat {}

fn (cat Cat) speak() string {
	_ = cat
	return "cat"
}

fn selected_speaker() Speaker {
	return Cat{}
}

fn main() {
	println(selected_speaker().speak())
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
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[source_path] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used_fns := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	g.set_cache_split(true)
	g.set_program_body_only(true)
	g.set_incremental_fn_names({
		'selected_speaker': true
	})
	c_source := g.gen_with_used_options(a, used_fns, &tc, true)
	assert g.iface_type_ids['Speaker::Cat'] > 0
	assert c_source.contains('selected_speaker'), c_source
	assert !c_source.contains('._typ = 0'), c_source
}
