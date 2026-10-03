// vtest vflags: -d v3_no_parallel
module driver

import v.flat

fn test_serial_build_bypasses_native_header_cache() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{ kind: .directive, value: 'insert', typ: '"native.h"' })
	assert ast_has_external_c_inputs(&a, []string{})
}
