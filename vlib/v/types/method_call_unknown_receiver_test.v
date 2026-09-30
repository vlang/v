module types

import os
import v.flat
import v.parser
import v.pref

// A method call is typed from its receiver. While the receiver's type is not known
// yet (it is resolved again once its scope is), the call's type must stay unknown
// rather than become that of a free function sharing the method's name, which
// would then be cached: `res.write(...) or {}` failed as "does not return an
// Option" when a C or V function `write` existed.
fn test_method_call_on_unknown_receiver_is_not_typed_as_a_free_function() {
	path := os.join_path(os.vtmp_dir(), 'v3_method_unknown_receiver_${os.getpid()}.v')
	os.write_file(path, 'module main
fn write(fd int, buf voidptr, count u64) int {
	return 0
}
fn main() {
	_ = res.write(0, unsafe { nil }, 0, 10)
}
')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	mut found := false
	for i, node in a.nodes {
		if node.kind != .call || node.children_count == 0 {
			continue
		}
		callee := a.child_node(&node, 0)
		if callee.kind != .selector || callee.value != 'write' {
			continue
		}
		found = true
		typ := tc.resolve_type(flat.NodeId(i))
		assert typ is Unknown, typ.name()
	}
	assert found
}
