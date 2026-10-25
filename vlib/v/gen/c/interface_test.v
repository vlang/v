module c

import os
import v.flat
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_collect_interface_impls_includes_boxed_maps() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.interfaces['Any'] = []string{}
	g.interface_boxed_types[interface_dispatch_boxed_type_key(flat.source_type_id('Any'), flat.source_type_id('map[string]int'))] = true
	g.interface_boxed_types_done = true

	g.collect_interface_impls()

	assert flat.source_type_id('map[string]int') in g.iface_impls['Any']
	assert g.iface_type_ids[interface_dispatch_boxed_type_key(flat.source_type_id('Any'), flat.source_type_id('map[string]int'))] > 0
}

fn test_interface_init_typ_id_uses_canonical_source_value() {
	mut a := flat.FlatAst.new()
	iface_id := flat.source_type_id('Speaker')
	concrete_id := flat.source_type_id('Cat')
	obj := a.add_val(.ident, 'cat')
	object_start := a.children.len
	a.children << obj
	field := a.add_node(flat.Node{
		kind:           .field_init
		value:          '_object'
		children_start: i32(object_start)
		children_count: 1
	})
	init_start := a.children.len
	a.children << field
	init := a.add_node(flat.Node{
		kind:           .struct_init
		value:          'Speaker'
		children_start: i32(init_start)
		children_count: 1
	})
	// Canonical type operands retain their spelling only as a source ID.
	assert a.node(init).value == ''
	assert a.node(init).source_value_id() == iface_id
	mut tc := types.TypeChecker.new(&a)
	tc.cur_scope.insert('cat', types.Type(types.Struct{ name_id: concrete_id }))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.interfaces['Speaker'] = []string{}
	g.iface_type_ids[interface_dispatch_boxed_type_key(iface_id, concrete_id)] = 7

	assert (g.interface_init_typ_id(*a.node(init)) or { 0 }) == 7
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
	assert g.iface_type_ids[interface_dispatch_boxed_type_key(flat.source_type_id('Speaker'), flat.source_type_id('Cat'))] > 0
	assert c_source.contains('selected_speaker'), c_source
	assert !c_source.contains('._typ = 0'), c_source
}

fn test_incremental_cgen_populates_shared_parameter_index() {
	source_path := os.join_path(os.temp_dir(), 'v3_incremental_cgen_shared_params.v')
	os.write_file(source_path, 'module main

struct State {
	value int
}

fn read(shared state State) int {
	rlock state {
		return state.value
	}
}

fn selected_read(shared state State) int {
	return read(shared state)
}

fn main() {
	shared state := State{}
	println(selected_read(shared state))
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
		'selected_read': true
	})
	c_source := g.gen_with_used_options(a, used_fns, &tc, true)
	assert g.fn_param_is_shared('read', 0)
	assert c_source.contains('selected_read'), c_source
}

fn test_shared_library_exports_its_interface_dispatchers() {
	source_path := os.join_path(os.temp_dir(), 'v3_shared_interface_exports.v')
	os.write_file(source_path, 'module main

interface Speaker {
	speak() string
}

struct Cat {}

fn (cat Cat) speak() string {
	_ = cat
	return "cat"
}

@[export: "make_speaker"]
fn make_speaker() Speaker {
	return Cat{}
}
') or {
		panic(err)
	}
	defer {
		os.rm(source_path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.is_shared = true
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
	g.set_shared(true)
	c_source := g.gen_with_used_options(a, used_fns, &tc, true)
	// A program that loads this library cannot dispatch `Cat` itself, so the
	// library exports its dispatcher, keyed by the type tag it boxed `Cat` with.
	id := g.iface_type_id(flat.source_type_id('Speaker'), flat.source_type_id('Cat'))
	assert id > 0
	assert c_source.contains('_v_interface_exports[] = {'), c_source
	assert c_source.contains('{"Speaker", "speak", ${id}u, (void*)Speaker__speak},'), c_source
}
