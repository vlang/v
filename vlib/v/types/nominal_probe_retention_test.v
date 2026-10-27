module types

import v.flat

fn test_c_alias_cast_probe_does_not_intern_ordinary_or_missing_c_calls() {
	mut a := flat.FlatAst.new()
	ordinary := a.add_val(.ident, 'nominal_probe_owner')
	c_base := a.add_val(.ident, 'C')
	mut tc := TypeChecker.new(&a)
	call := flat.Node{ kind: .call, children_count: 2 }
	for base in [ordinary, c_base] {
		start := a.children.len
		a.children << base
		callee := flat.Node{
			kind:           .selector
			value:          '__nominal_probe_missing_c_alias__'
			children_start: start
			children_count: 1
		}
		missing := 'C.__nominal_probe_missing_c_alias__'
		assert flat.source_type_existing_id(missing) == none
		assert !tc.check_c_alias_cast_call(flat.NodeId(0), call, callee)
		assert flat.source_type_existing_id(missing) == none
	}
}

fn test_generic_param_qualification_probes_preserve_misses_and_registered_types() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'nominal_probe_retention.v'
	tc.cur_module = '__nominal_probe_generic_module__'
	bare_id := flat.source_type_id('__NominalProbeGenericType__')
	qualified := '__nominal_probe_generic_module__.__NominalProbeGenericType__'
	assert flat.source_type_existing_id(qualified) == none
	assert tc.generic_param_type_text(bare_id) == bare_id
	assert flat.source_type_existing_id(qualified) == none
	qualified_id := flat.source_type_id(qualified)
	tc.structs[qualified_id] = []StructField{}
	assert tc.generic_param_type_text(bare_id) == qualified_id
}
