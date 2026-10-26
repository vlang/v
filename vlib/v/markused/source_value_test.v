module markused

import v.flat
import v.types

fn test_factory_source_operands_preserve_original_names() {
	mut a := flat.FlatAst.new()
	ident := a.add_node(flat.Node{
		kind:         .ident
		source_value: flat.source_type_id('dep.Record')
	})
	init := a.add_node(flat.Node{
		kind:         .struct_init
		source_value: flat.source_type_id('dep.Record')
	})
	array := a.add_node(flat.Node{
		kind:         .array_init
		source_value: flat.source_type_id('[2]dep.Record')
	})
	mut tc := types.TypeChecker.new(&a)
	collector := CallCollector{ a: &a, tc: &tc }
	assert collector.qualified_expr_name(ident) == 'dep.Record'
	assert collector.generic_factory_type_arg(ident) == flat.source_type_id('dep.Record')
	assert collector.generic_factory_type_arg(init) == flat.source_type_id('dep.Record')
	assert collector.generic_factory_type_arg(array) == flat.source_type_id('[2]dep.Record')
}
