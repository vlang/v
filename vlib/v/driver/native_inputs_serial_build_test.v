// vtest vflags: -d v3_no_parallel
module driver

import v.flat
import v.gen.c as cgen
import v.pref

fn test_serial_build_bypasses_native_header_cache() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, '/project/main.v')
	a.add_node(flat.Node{ kind: .directive, value: 'insert', typ: '"native.h"' })
	inputs := cgen.cache_native_inputs(&a, @VEXEROOT, pref.host_target(), []string{}, map[string]string{},
		map[string]bool{})
	assert inputs.user_supplied != ''
}
