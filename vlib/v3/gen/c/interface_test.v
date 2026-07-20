module c

import v3.flat
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
