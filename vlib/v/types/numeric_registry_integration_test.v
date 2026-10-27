module types

import os
import v.flat
import v.parser
import v.pref

fn test_numeric_nominal_registries_collect_resolve_and_detach_worker_metadata() {
	path := os.join_path(os.vtmp_dir(), 'v3_numeric_registries_${os.getpid()}.v')
	os.write_file(path, 'module registryfixture
struct Item {
	value int
}
struct Box[T] {
	value T
}
type ItemAlias = Item
type Choice = Item | string
enum Kind {
	first
	second
}
interface Parent {
	parent() int
}
interface Contract {
	Parent
	label string
}
') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.errors.len == 0, tc.errors.str()
	item_id := flat.source_type_id('registryfixture.Item')
	box_id := flat.source_type_id('registryfixture.Box')
	alias_id := flat.source_type_id('registryfixture.ItemAlias')
	choice_id := flat.source_type_id('registryfixture.Choice')
	kind_id := flat.source_type_id('registryfixture.Kind')
	parent_id := flat.source_type_id('registryfixture.Parent')
	contract_id := flat.source_type_id('registryfixture.Contract')

	assert tc.source_declares_type_in_scope(flat.source_type_id('Item'), path, 'registryfixture')
	assert tc.source_declares_type_in_scope(flat.source_type_id('Box'), path, 'registryfixture')
	assert scope_type_key(path, 'registryfixture', flat.source_type_id('Item')) in tc.concrete_type_scope_keys
	assert scope_type_key(path, 'registryfixture', flat.source_type_id('Box')) !in tc.concrete_type_scope_keys
	assert scope_type_key(path, 'registryfixture', flat.source_type_id('Kind')) !in tc.concrete_type_scope_keys
	assert tc.struct_modules[item_id] == 'registryfixture'
	assert tc.struct_files[item_id] == path
	assert tc.struct_generic_params[box_id] == ['T']
	assert item_id in tc.first_type_declaration_ids
	assert a.nodes[tc.first_type_declaration_ids[item_id]].kind == .struct_decl
	assert tc.type_aliases.type_id(alias_id)? == item_id
	assert tc.type_alias_modules[alias_id] == 'registryfixture'
	assert tc.sum_types.type_ids(choice_id)? == [item_id, flat.source_type_string]
	assert tc.enum_names[kind_id]
	assert tc.enum_fields[kind_id] == ['first', 'second']
	assert tc.interface_names[contract_id]
	assert tc.interface_embeds.type_ids(contract_id)? == [parent_id]
	assert tc.interface_abstract_methods[parent_id] == ['parent']
	assert tc.interface_fields[contract_id].map(it.name) == ['label']

	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.register_file_import('dep', 'registryfixture')
	assert tc.qualify_resolution_type_name(flat.source_type_id('dep.Item')) == item_id
	assert tc.qualify_resolution_type_text(flat.source_type_id('dep.Box[dep.Item]')) == flat.source_type_id('registryfixture.Box[registryfixture.Item]')
	for name, expected in {
		'dep.Item':     item_id
		'dep.Choice':   choice_id
		'dep.Kind':     kind_id
		'dep.Contract': contract_id
	} {
		assert tc.parse_resolution_type(flat.source_type_id(name)).source_type_id() == expected, name
	}
	alias_type := tc.parse_resolution_type(flat.source_type_id('dep.ItemAlias'))
	assert alias_type is Alias
	if alias_type is Alias {
		assert alias_type.name_id == alias_id
		assert alias_type.base_type.source_type_id() == item_id
	}
	assert tc.struct_field_type(item_id, 'value')?.source_type_id() == flat.source_type_int
	assert tc.struct_field_type(flat.source_type_id('registryfixture.Box[int]'), 'value')?.source_type_id() == flat.source_type_int

	mut worker := tc.fork_for_parallel_transform(a)
	defer { worker.free_parallel_transform_caches() }
	worker.cur_file = tc.cur_file
	worker.cur_module = tc.cur_module
	assert worker.parse_resolution_type(flat.source_type_id('dep.Item')).source_type_id() == item_id
	worker.clone_transform_metadata_owned()
	worker.declared_type_scope_keys.delete(scope_type_key(path, 'registryfixture', flat.source_type_id('Item')))
	assert !worker.source_declares_type_in_scope(flat.source_type_id('Item'), path, 'registryfixture')
	assert tc.source_declares_type_in_scope(flat.source_type_id('Item'), path, 'registryfixture')
	// Mutate existing list elements too: independent map headers alone cannot
	// protect the master's collected fields and metadata from a worker write.
	worker_struct_field := worker.structs[item_id][0]
	worker.structs[item_id][0] = StructField{
		...worker_struct_field
		name: 'worker_value'
	}
	worker.struct_modules[item_id] = 'worker'
	worker.struct_files[item_id] = 'worker.v'
	worker.struct_generic_params[box_id][0] = 'WorkerT'
	worker.sum_types.set_ids(choice_id, [flat.source_type_bool])
	worker.enum_fields[kind_id][0] = 'worker_first'
	worker_interface_field := worker.interface_fields[contract_id][0]
	worker.interface_fields[contract_id][0] = StructField{
		...worker_interface_field
		name: 'worker_label'
	}
	worker.interface_abstract_methods[parent_id][0] = 'worker_parent'
	worker.interface_embeds.set_ids(contract_id, []u32{})
	assert worker.structs[item_id][0].name == 'worker_value'
	assert worker.sum_types.type_ids(choice_id)? == [flat.source_type_bool]
	assert worker.interface_embeds.type_ids(contract_id)?.len == 0
	assert tc.structs[item_id][0].name == 'value'
	assert tc.struct_modules[item_id] == 'registryfixture'
	assert tc.struct_files[item_id] == path
	assert tc.struct_generic_params[box_id] == ['T']
	assert tc.sum_types.type_ids(choice_id)? == [item_id, flat.source_type_string]
	assert tc.enum_fields[kind_id] == ['first', 'second']
	assert tc.interface_fields[contract_id][0].name == 'label'
	assert tc.interface_abstract_methods[parent_id] == ['parent']
	assert tc.interface_embeds.type_ids(contract_id)? == [parent_id]
	assert tc.errors.len == 0, tc.errors.str()
	assert worker.errors.len == 0, worker.errors.str()
}
