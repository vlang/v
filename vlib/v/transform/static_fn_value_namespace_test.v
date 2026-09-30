module transform

import v.flat
import v.types

fn test_static_fn_value_fallback_rejects_local_root_without_checker_metadata() {
	root := 'staticfnref'
	mut a := flat.FlatAst.new()
	root_id := a.add_val(.ident, root)
	type_start := a.children.len
	a.children << root_id
	type_id := a.add_node(flat.Node{
		kind:           .selector
		value:          'MyStruct'
		children_start: type_start
		children_count: 1
	})
	value_start := a.children.len
	a.children << type_id
	value_id := a.add_node(flat.Node{
		kind:           .selector
		value:          'new'
		children_start: value_start
		children_count: 1
	})
	name := flat.encode_static_type_method_name('staticfnref.MyStruct', 'new')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['staticfnref.MyStruct'] = StructInfo{}
	t.fn_ret_types[name] = 'staticfnref.MyStruct'
	// No checker expression types or resolved function value entry, as in a
	// cloned generic body. An unbound namespace still names the static fn.
	assert tc.expr_type(value_id) == none
	assert tc.resolved_fn_value_name(value_id) == none
	resolved := t.static_fn_value_name(value_id, a.node(value_id)) or {
		assert false, 'unbound namespace `${root}` must name the static fn'
		return
	}
	assert resolved == name
	// A local binding with the same spelling makes this an ordinary field.
	t.set_var_type(root, 'Wrapper')
	assert t.static_assoc_fn_name(type_id, 'new') == none
	assert t.static_fn_value_name(value_id, a.node(value_id)) == none
	t.unset_var_type(root)
	tc.const_types[root] = types.Type(types.Struct{ name: 'Wrapper' })
	assert t.static_fn_value_name(value_id, a.node(value_id)) == none
	tc.const_types.delete(root)
	t.globals[root] = 'Wrapper'
	assert t.static_fn_value_name(value_id, a.node(value_id)) == none
	t.cur_file = 'namespace.v'
	tc.file_imports[file_import_key(t.cur_file, root)] = 'staticfnref'
	assert t.static_fn_value_name(value_id, a.node(value_id)) == none
	t.globals.delete(root)
	// An unrelated module's same-named constant must not hide an import alias.
	tc.const_types['transitive.${root}'] = types.int_
	t.const_suffixes[root] = 'transitive.${root}'
	assert t.static_fn_value_name(value_id, a.node(value_id)) or { '' } == name
}
