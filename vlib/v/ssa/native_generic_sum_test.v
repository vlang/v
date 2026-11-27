module ssa

import os
import v.flat
import v.parser
import v.pref
import v.types

fn test_generic_sum_layouts_keep_concrete_variants() {
	path := os.join_path(os.vtmp_dir(), 'ssa_generic_sum_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'module main
struct Empty {}
struct Node[T] {
    value T
    left Tree[T]
    right Tree[T]
}
type Tree[T] = Empty | &Node[T]
type Value[T] = T | bool
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	assert tc.errors.len == 0, tc.errors.str()
	mut m := Module.new()
	mut b := Builder{
		a:        a
		tc:       &tc
		m:        m
		i32_type: m.type_store.get_int(32)
		u32_type: m.type_store.get_uint(32)
		i8_type:  m.type_store.get_int(8)
		i64_type: m.type_store.get_int(64)
		f64_type: m.type_store.get_float(64)
	}
	b.register_types()
	for scalar in ['int', 'f64'] {
		// Start with the struct so its recursive sum sees an unfinished Node layout.
		node := b.resolve_type(flat.source_type_id('Node[${scalar}]'))
		tree_name := flat.source_type_id('Tree[${scalar}]')
		tree := b.resolve_type(tree_name)
		assert m.type_store.types[tree].field_names == ['typ', '_pointer_variant_is_owned', '_payload']
		assert m.type_size(tree) == 16
		assert m.type_store.types[node].fields == [
			b.resolve_type(flat.source_type_id(scalar)),
			tree,
			tree,
		]
		assert m.type_size(node) == 40
		payload := m.type_store.types[tree].fields.last()
		assert m.type_store.types[payload].fields[1] == m.type_store.get_ptr(node)
		assert b.sum_variant_index(tree_name, flat.source_type_id('Empty')) == 1
		assert b.sum_variant_index(tree_name, flat.source_type_id('&Node[${scalar}]')) == 2
	}
	value := b.resolve_type(flat.source_type_id('Value[[3]i64]'))
	layout := m.type_store.types[value]
	assert layout.field_names == ['typ', '_payload']
	payload := layout.fields.last()
	assert m.type_store.types[payload].fields[0] == m.type_store.get_array(b.i64_type, 3)
	assert m.type_size(payload) == 24
	assert m.type_size(value) == 32
}
