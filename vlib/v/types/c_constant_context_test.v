module types

import os
import v.parser
import v.pref

fn test_c_integer_constant_adopts_numeric_alias_call_context() {
	path := os.join_path(os.vtmp_dir(), 'v3_c_constant_context_${os.getpid()}.c.v')
	os.write_file(path, 'module main\n\ntype Flag = usize\n\nfn consume(value Flag) {}\n\nfn main() { consume(C.TEST_FLAG) }\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}
