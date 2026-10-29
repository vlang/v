module c

import v.flat
import v.types

fn test_c_header_type_probes_do_not_retain_unused_declarations() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	name := 'C.cgen_header_probe_unregistered'
	source := 'typedef struct cgen_header_probe_unregistered { int value; } cgen_header_probe_unregistered;'
	assert flat.source_type_existing_id(name) == none
	assert !g.c_source_defines_used_c_type(source)
	assert flat.source_type_existing_id(name) == none
	id := flat.source_type_id(name)
	tc.structs[id] = []types.StructField{}
	assert g.c_source_defines_used_c_type(source)
	tc.structs.delete(id)
	tc.type_aliases.set_id(id, 0)
	assert g.c_source_defines_used_c_type(source)
}

fn test_module_enum_probes_keep_short_fallback_without_retaining_misses() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'dep')
	a.children << base
	selector := flat.Node{
		kind:           .selector
		value:          'CgenUnregisteredEnumProbe'
		children_start: 0
		children_count: 1
	}
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.modules['dep'] = 'outer.enum_probe_module'
	qualified := 'enum_probe_module.CgenUnregisteredEnumProbe'
	assert flat.source_type_existing_id(qualified) == none
	assert flat.source_type_existing_id(selector.value) == none
	assert !g.is_module_qualified_enum(selector)
	assert flat.source_type_existing_id(qualified) == none
	assert flat.source_type_existing_id(selector.value) == none
	tc.enum_names[flat.source_type_id(selector.value)] = true
	assert g.is_module_qualified_enum(selector)
	assert flat.source_type_existing_id(qualified) == none
}
