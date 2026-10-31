module types

import os
import v.parser
import v.pref

@[unsafe]
fn test_typed_global_initializers_reject_direct_null_reference_constants() {
	path := os.join_path(os.vtmp_dir(), 'null_global_reference_${os.getpid()}.v')
	os.write_file(path, 'struct Item { id int }
type ItemRef = &Item
__global (
    null_zero &Item = 0
    null_nil &Item = nil
    null_cast &Item = voidptr(0)
    null_parenthesized &Item = (voidptr(0))
    null_raw &u8 = 0
    null_alias ItemRef = voidptr(0)
)
fn main() {}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.enable_globals = true
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 6, tc.errors.str()
	assert tc.errors.filter(it.msg == '`nil` is only allowed in `unsafe` code').len == 1
	assert tc.errors.filter(it.msg.contains('outside `unsafe`, use `unsafe { nil }`')).len == 5
}
